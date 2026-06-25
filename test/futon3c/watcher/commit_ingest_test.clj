(ns futon3c.watcher.commit-ingest-test
  (:require [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.test :refer [deftest is testing]]
            [futon3c.agency.registry :as registry]
            [futon3c.watcher.commit-ingest :as sut]))

(defn- tmp-dir []
  (doto (java.io.File/createTempFile "commit-ingest-" "")
    (.delete)
    (.mkdirs)))

(defn- run-git! [repo & args]
  (let [{:keys [exit err]} (apply sh "git" "-C" (.getPath repo) args)]
    (is (zero? exit) err)))

(defn- fixture-repo-with-mission-commit []
  (let [repo (tmp-dir)
        f (io/file repo "src/demo.clj")]
    (run-git! repo "init")
    (run-git! repo "config" "user.email" "agent@example.test")
    (run-git! repo "config" "user.name" "Agent Test")
    (.mkdirs (.getParentFile f))
    (spit f "(ns demo)\n")
    (run-git! repo "add" ".")
    (run-git! repo "commit" "-m" "Implement demo" "-m" "Mission: E-mealy-style-transducer")
    repo))

(defn- delete-tree! [root]
  (doseq [f (reverse (file-seq root))]
    (.delete f)))

(deftest ingest-new-commits-records-head-cursor
  (testing "live ingestion anchors the in-memory cursor at HEAD"
    (let [recorded (atom nil)]
      (with-redefs [sut/last-indexed-commit-sha (fn [_] "old-side-tip")
                    sut/list-commits (fn [_ since]
                                       (is (= "old-side-tip" since))
                                       [{:sha "older-mainline"}
                                        {:sha "latest-non-merge"}])
                    sut/current-head-sha (fn [_] "merge-head")
                    sut/ingest-commits-batch! (fn [_]
                                                {:n-ingested 2
                                                 :latest-sha "latest-non-merge"
                                                 :n-failed 0
                                                 :n-blocks 0
                                                 :n-mana-credited 0})
                    sut/record-last-ingested! (fn [repo-label sha]
                                                (reset! recorded [repo-label sha]))]
        (is (= {:n-ingested 2
                :latest-sha "latest-non-merge"
                :n-failed 0
                :n-blocks 0
                :n-mana-credited 0}
               (sut/ingest-new-commits! {:repo-root "/tmp/repo"
                                         :repo-label "demo"
                                         :file->structure (constantly nil)})))
        (is (= ["demo" "merge-head"] @recorded))))))

(deftest ingest-new-commits-falls-back-to-latest-commit-when-head-missing
  (testing "HEAD lookup failure still preserves the previous latest-sha behaviour"
    (let [recorded (atom nil)]
      (with-redefs [sut/last-indexed-commit-sha (constantly "old")
                    sut/list-commits (constantly [{:sha "only-new"}])
                    sut/current-head-sha (constantly nil)
                    sut/ingest-commits-batch! (constantly {:n-ingested 1
                                                           :latest-sha "only-new"
                                                           :n-failed 0
                                                           :n-blocks 0
                                                           :n-mana-credited 0})
                    sut/record-last-ingested! (fn [repo-label sha]
                                                (reset! recorded [repo-label sha]))]
        (sut/ingest-new-commits! {:repo-root "/tmp/repo"
                                  :repo-label "demo"
                                  :file->structure (constantly nil)})
        (is (= ["demo" "only-new"] @recorded))))))

(deftest parses-mission-trailer-from-real-commit
  (let [repo (fixture-repo-with-mission-commit)]
    (try
      (let [commit (first (sut/list-commits (.getPath repo)))]
        (is (= "E-mealy-style-transducer" (:mission commit)))
        (is (= "Implement demo" (:subject commit))))
      (finally
        (delete-tree! repo)))))

(deftest commit-mission-edge-flag-off-has-no-store-write
  (with-redefs [sut/commit-mission-edges-enabled? (constantly false)
                sut/post-hyperedge! (fn [& _]
                                      (throw (ex-info "store write should not happen" {})))]
    (is (nil? (sut/ingest-commit-mission-edge!
               ["v05" "phase-3" "demo"]
               {"repo" "demo" "phase" 3}
               {:sha "abc123" :mission "M-demo"})))))

(deftest mission-trailer-emits-trailer-provenance-edge
  (let [repo (fixture-repo-with-mission-commit)
        posted (atom [])]
    (try
      (let [commit (first (sut/list-commits (.getPath repo)))]
        (with-redefs [sut/commit-mission-edges-enabled? (constantly true)
                      sut/post-hyperedge! (fn [hx-type endpoints labels props]
                                            (swap! posted conj {:hx-type hx-type
                                                               :endpoints endpoints
                                                               :labels labels
                                                               :props props})
                                            {:ok? true})]
          (is (= {:ok? true}
                 (sut/ingest-commit-mission-edge!
                  ["v05" "phase-3" "demo"]
                  {"repo" "demo" "phase" 3}
                  commit)))))
      (is (= 1 (count @posted)))
      (is (= sut/commit-mission-edge-type (:hx-type (first @posted))))
      (is (= ["E-mealy-style-transducer"]
             (rest (:endpoints (first @posted)))))
      (is (= "trailer"
             (get-in (first @posted) [:props "relation/provenance"])))
      (finally
        (delete-tree! repo)))))

(deftest trailer-attribution-wins-over-session-heuristic
  (with-redefs [sut/resolve-session-for-commit (fn [_]
                                                (throw (ex-info "heuristic should not run" {})))
                registry/registry-status (constantly {:agents {"agent"
                                                               {:session-id "s1"
                                                                :mission-id "M-other"}}})]
    (is (= {:mission-id "M-trailer"
            :relation/provenance "trailer"}
           (sut/commit-mission-attribution {:mission "M-trailer" :ts 100})))))

(deftest session-heuristic-attribution-uses-registry-mission
  (with-redefs [sut/resolve-session-for-commit (constantly "s1")
                registry/registry-status (constantly {:agents {"agent"
                                                               {:session-id "s1"
                                                                :mission-id "M-registry"}}})]
    (is (= {:mission-id "M-registry"
            :session-id "s1"
            :relation/provenance "session-heuristic"}
           (sut/commit-mission-attribution {:ts 100})))))
