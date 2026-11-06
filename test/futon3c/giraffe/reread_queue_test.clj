(ns futon3c.giraffe.reread-queue-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.giraffe.reread-queue :as queue])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-db []
  (let [dir (.toFile (Files/createTempDirectory "giraffe-reread-"
                                                (make-array FileAttribute 0)))]
    (io/file dir "requests.sqlite")))

(defn- item [sha]
  {:id :M-one :source-pin {:path "/code/futon/holes/M-one.md" :sha256 sha}})

(defn- string-item [sha]
  {:id "M-one" :source-pin {:path "/code/futon/holes/M-one.md" :sha256 sha}})

(defn- coverage [sha]
  {:members [{:id "M-one" :status :read
              :source {:path "/code/futon/holes/M-one.md" :sha256 sha}}]})

(deftest changed-reading-is-durable-idempotent-and-reconstructible
  (let [db (temp-db)
        store (queue/open-store db)]
    (queue/reconcile! store [(item "new")] (coverage "old") 10)
    (queue/reconcile! store [(item "new")] (coverage "old") 20)
    (is (= [{:target_id "M-one" :source_sha "new" :old_sha "old"
             :source_path "/code/futon/holes/M-one.md" :state :pending
             :requested_at_ms 10 :updated_at_ms 10 :lease_until_ms nil
             :superseded_by_sha nil :superseded_reason nil :attempts 0
             :last_error nil :landed_digest nil :claimed_by nil}]
           (queue/requests store)))
    (testing "the database is a cache of authority, not the only copy"
      (doseq [suffix ["" "-wal" "-shm"]
              :let [file (io/file (str db suffix))]
              :when (.exists file)]
        (is (.delete file)))
      (let [rebuilt (queue/open-store db)]
        (queue/reconcile! rebuilt [(item "new")] (coverage "old") 30)
        (is (= [["M-one" "new" :pending]]
               (mapv (juxt :target_id :source_sha :state)
                     (queue/requests rebuilt))))))))

(deftest newer-edit-supersedes-prior-source
  (let [store (queue/open-store (temp-db))]
    (queue/reconcile! store [(item "sha-2")] (coverage "sha-1") 10)
    (queue/reconcile! store [(item "sha-3")] (coverage "sha-1") 20)
    (is (= [["sha-2" :superseded "sha-3"] ["sha-3" :pending nil]]
           (mapv (juxt :source_sha :state :superseded_by_sha)
                 (queue/requests store))))))

(deftest string-target-id-retains-old-source-pin
  (let [store (queue/open-store (temp-db))]
    (queue/reconcile! store [(string-item "new")] (coverage "old") 10)
    (is (= "old" (:old_sha (first (queue/requests store)))))))

(deftest lease-expiry-is-reconciled
  (let [store (queue/open-store (temp-db))]
    (queue/reconcile! store [(item "new")] (coverage "old") 10)
    (is (= :claimed (:state (queue/claim! store 20 100 1 "reader"))))
    (queue/reconcile! store [(item "new")] (coverage "old") 119)
    (is (= :claimed (:state (first (queue/requests store)))))
    (queue/reconcile! store [(item "new")] (coverage "old") 120)
    (is (= :pending (:state (first (queue/requests store)))))))

(deftest source-revert-supersedes-pending-request
  (let [store (queue/open-store (temp-db))]
    (queue/reconcile! store [(item "changed")] (coverage "read-sha") 10)
    (queue/reconcile! store [(item "read-sha")] (coverage "read-sha") 20)
    (let [row (first (queue/requests store))]
      (is (= :superseded (:state row)))
      (is (= :source-returned-to-read (:superseded_reason row)))
      (is (= "read-sha" (:superseded_by_sha row))))))

(deftest never-read-item-is-pending-once-with-no-old-sha
  (let [store (queue/open-store (temp-db))
        ;; Coverage can retain a non-admitted member and its historical pin;
        ;; :unread-new, not member absence, defines the capped work class.
        result (queue/reconcile! store [(item "new")]
                                 {:members [{:id "M-one" :status :ambiguous
                                             :source {:path "/old/M-one.md"
                                                      :sha256 "old"}}]}
                                 10)]
    (is (= [:M-one] (get-in result [:drift :unread-new])))
    (is (= [[:pending nil "new"]]
           (mapv (juxt :state :old_sha :source_sha) (queue/requests store))))
    (let [request (queue/claim! store 15 100 1 "reader")]
      (queue/settle! store request {:status :landed :digest "generation-1"} 16 3))
    (queue/reconcile! store [(item "new")]
                      {:members [{:id "M-one" :status :read
                                  :source {:path "/code/futon/holes/M-one.md"
                                           :sha256 "new"}}]}
                      20)
    (is (= [[:landed "generation-1"]]
           (mapv (juxt :state :landed_digest) (queue/requests store))))))

(deftest new-item-claim-cap-is-fail-closed
  (let [store (queue/open-store (temp-db))
        items [(item "new")
               {:id :M-two :source-pin {:path "/code/M-two.md" :sha256 "two"}}]]
    (queue/reconcile! store items {:members []} 10)
    (is (nil? (queue/claim! store 20 100 0 "reader")))
    (is (= :claimed (:state (queue/claim! store 20 100 1 "reader"))))
    (is (= 1 (count (filter #(= :pending (:state %)) (queue/requests store)))))))

(deftest two-readers-hold-distinct-claims-and-leases
  (let [store (queue/open-store (temp-db))
        items [(item "one")
               {:id :M-two :source-pin {:path "/code/M-two.md" :sha256 "two"}}]]
    (queue/reconcile! store items {:members []} 10)
    (let [claims @(future
                    [(queue/claim! store 20 100 1 "zai-2")
                     (queue/claim! store 20 200 1 "kimi-1")])]
      (is (= #{"zai-2" "kimi-1"} (set (map :claimed_by claims))))
      (is (= #{120 220} (set (map :lease_until_ms claims)))))))
