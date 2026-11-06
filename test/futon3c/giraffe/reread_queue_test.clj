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
             :last_error nil :landed_digest nil}]
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
    (is (= :claimed (:state (queue/claim! store 20 100))))
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
