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
             (select-keys (sut/sync-items! {:graph-store :stub} [item])
                          [:checked :written :batch-count])))
      (is (= 1 (count @calls)))
      (is (= [:clusion/assert :evidence/append]
             (mapv :operation (second (first @calls)))))))
  (let [calls (atom []) item {:id "M-present"}]
    (with-redefs [store/get-entity (fn [& _] {:ok (sut/item-record "M-present")})
                  http/append-batch! (fn [& _] (swap! calls conj :write) {:ok {}})]
      (is (= [] (:written (sut/sync-items! {:graph-store :stub} [item]))))
      (is (empty? @calls)))))

(deftest supersession-plan-writes-one-and-counts-nonclaims
  (let [root (.toFile (java.nio.file.Files/createTempDirectory "supersession-plan" (make-array java.nio.file.attribute.FileAttribute 0)))
        dir (io/file root "holes")
        mk (fn [id status body sha]
             (let [f (io/file dir (str id ".md"))]
               (.mkdirs dir) (spit f body)
               {:id id :path (str f) :status-line status :source {:sha256 sha}}))
        one (mk "M-old" "SUPERSEDED" "superseded by `M-new`" "sha-a")
        none (mk "M-none" "SUPERSEDED" "no declared successor" "sha-n")
        several (mk "M-many" "SUPERSEDED" "superseded by `M-a`; superseded by `M-b`" "sha-m")
        unknown (mk "M-unknown" "SUPERSEDED" "superseded by `M-absent`" "sha-u")
        targets (mapv #(mk % "OPEN" "invented" (str "sha-" %)) ["M-new" "M-a" "M-b"])
        inventory (into [one none several unknown] targets)]
    (try
      (with-redefs-fn {#'sut/git! (fn [& _] "toy-commit")}
        (fn []
          (let [a (sut/supersession-plan inventory [one none several unknown])
                again (sut/supersession-plan inventory [one none several unknown])
                changed (sut/supersession-plan inventory [(assoc-in one [:source :sha256] "sha-b")])
                claim (get-in a [:rows 0 :record]) att (get-in a [:rows 1 :record])]
            (is (= {:written 1 :no-successor 1 :ambiguous 1 :unknown-successor 1}
                   (select-keys a [:written :no-successor :ambiguous :unknown-successor])))
            (is (= [:supersedes 1] (:type-revision claim)))
            (is (= [:successor :predecessor] (mapv :role (:endpoints claim))))
            (is (= {:name :supersession-census :version 1} (:method att)))
            (is (= "sha-a" (get-in att [:content :file/sha256])))
            (is (= (:message-id a) (:message-id again)))
            (is (not= (:message-id a) (:message-id changed))))))
      (finally (doseq [f (reverse (file-seq root))] (io/delete-file f))))))

(deftest incremental-sync-combines-item-and-supersession-in-one-batch
  (let [root (.toFile (java.nio.file.Files/createTempDirectory "supersession-sync" (make-array java.nio.file.attribute.FileAttribute 0)))
        dir (io/file root "holes") old-file (io/file dir "M-old.md")
        old {:id "M-old" :path (str old-file) :status-line "SUPERSEDED" :source {:sha256 "old-sha"}}
        new {:id "M-new" :path (str (io/file dir "M-new.md")) :status-line "OPEN" :source {:sha256 "new-sha"}}
        calls (atom [])]
    (try
      (.mkdirs dir) (spit old-file "superseded by `M-new`") (spit (:path new) "invented")
      (with-redefs-fn {#'sut/git! (fn [& _] "toy-commit")
                       #'store/get-entity (fn [_ id]
                                            (if (= id (:id (sut/item-record "M-old")))
                                              {:ok (sut/item-record "M-old")}
                                              {:error {:error/category :missing}}))
                       #'http/append-batch! (fn [_ message rows]
                                             (swap! calls conj [message rows]) {:ok {}})}
        (fn []
          (let [out (sut/sync-items! {:graph-store :stub
                                      :inventory-items-fn (constantly [old new])}
                                     [old new])]
            (is (= 1 (:batch-count out)))
            (is (= 1 (count @calls)))
            (is (= 1 (get-in out [:supersession :written]))))))
      (finally (doseq [f (reverse (file-seq root))] (io/delete-file f))))))
