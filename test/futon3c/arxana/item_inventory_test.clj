(ns futon3c.arxana.item-inventory-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [futon2.aif.mission-registry :as registry]
            [futon3c.arxana.item-inventory :as sut]
            [futon3c.arxana.store :as store]
            [futon3c.arxana.store-http :as http]))

(deftest file-stamps-invalidate-and-shared-statuses-apply
  (let [root (.toFile (java.nio.file.Files/createTempDirectory "inventory-test" (make-array java.nio.file.attribute.FileAttribute 0)))
        repo (io/file root "toy") dir (io/file repo "holes") f (io/file dir "M-a.md") calls (atom 0)
        cfg {:code-root (str root) :repos [(str repo)]}]
    (.mkdirs dir) (spit f "OPEN")
    (try
      (with-redefs [registry/load-missions-from-repos (fn [_] (swap! calls inc)
                                                       {:missions [{:id "M-a" :path (str f) :source {:sha256 "toy"} :status-class :open :status-line "OPEN"}]})
                    registry/load-excursions (fn [_] {:excursions [{:id "E-b" :status-class :complete :status-line "DONE"}]})
                    registry/load-tickets (fn [_] {:tickets [{:id "T-c" :status-class :live}]})]
        (is (= {"M-a" :open "E-b" :closed "T-c" :open}
               (into {} (map (juxt :id :state)) (sut/current-items cfg))))
        (sut/current-items cfg) (is (= 1 @calls))
        (spit f "OPEN changed") (sut/current-items cfg) (is (= 2 @calls)))
      (finally (doseq [x (reverse (file-seq root))] (io/delete-file x))))))

(deftest dry-run-only-proposes-missing-records
  (with-redefs [sut/current-items (fn [_] [{:id "M-a"} {:id "E-b"}])
                sut/publication-plan (fn [xs] [{:missing (mapv :id xs)}])
                store/get-entity (fn [_ id] (if (= id (:id (sut/item-record "M-a")))
                                               {:ok (sut/item-record "M-a")}
                                               {:error {:error/category :missing}}))
                http/append-batch! (fn [& _] (throw (AssertionError. "dry-run wrote")))]
    (is (= ["E-b"] (:missing (sut/sync-inventory! {:graph-store :stub}))))))

(deftest plan-pairs-items-with-exact-file-provenance
  (with-redefs-fn {#'sut/git! (fn [_ & args] (if (= "rev-parse" (first args)) "toy-commit" ""))}
    (fn []
      (let [plan (sut/publication-plan [{:id "M-a" :path "/invented/holes/M-a.md"
                                         :source {:sha256 "toy-sha"}}])
            rows (:records (first plan)) a (:record (second rows))]
        (is (= [:clusion/assert :evidence/append] (mapv :operation rows)))
        (is (= (:id (:record (first rows))) (:subject-clusion-id a)))
        (is (= :exact (:basis a)))
        (is (= "futon3c/item-inventory" (:producer a)))
        (is (= [{:path "/invented/holes/M-a.md" :git/commit "toy-commit"
                 :sha256 "toy-sha" :worktree-dirty? false}] (:source-refs a)))))))

(deftest incremental-sync-writes-one-batch-only-for-missing-items
  (let [item {:id "M-new"} calls (atom [])]
    (with-redefs [store/get-entity (fn [& _] {:error {:error/category :missing}})
                  sut/publication-plan
                  (fn [_] [{:message-id "inventory/new"
                            :records [{:operation :clusion/assert :record (sut/item-record "M-new")}
                                      {:operation :evidence/append :record {:id "attestation"}}]}])
                  http/append-batch! (fn [_ message rows]
                                       (swap! calls conj [message rows]) {:ok {}})]
      (is (= {:checked ["M-new"] :written ["M-new"] :batch-count 1}
             (sut/sync-items! {:graph-store :stub} [item])))
      (is (= 1 (count @calls)))
      (is (= [:clusion/assert :evidence/append]
             (mapv :operation (second (first @calls)))))))
  (let [calls (atom []) item {:id "M-present"}]
    (with-redefs [store/get-entity (fn [& _] {:ok (sut/item-record "M-present")})
                  http/append-batch! (fn [& _] (swap! calls conj :write) {:ok {}})]
      (is (= [] (:written (sut/sync-items! {:graph-store :stub} [item]))))
      (is (empty? @calls)))))
