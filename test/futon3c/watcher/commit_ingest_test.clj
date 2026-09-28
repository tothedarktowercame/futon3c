(ns futon3c.watcher.commit-ingest-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [babashka.http-client :as http]
            [cheshire.core :as json]
            [futon3c.agency.registry :as registry]
            [futon3c.watcher.commit-ingest :as sut]
            [futon3c.watcher.write-pace :as write-pace]
            [futon3c.test-support.git-fixture :as git-fixture]))

(deftest cold-start-cursor-asks-store-for-one-latest-commit
  (let [seen-url (atom nil)]
    (with-redefs [http/get (fn [url _]
                             (reset! seen-url url)
                             {:status 200
                              :body (pr-str {:hyperedges
                                             [{:hx/id "hx:code/v05/commit:abc123"}]})})]
      (is (= "abc123" (sut/last-indexed-commit-sha-from-store "demo")))
      (is (str/includes? @seen-url "repo=demo"))
      (is (str/includes? @seen-url "latest=true&limit=1")))))

(deftest files-changed-scopes-and-strips-subtree
  (testing "with a subtree, git show is pathspec-scoped and the subtree/ prefix is stripped"
    (let [seen (atom nil)]
      (with-redefs [sut/run-git (fn [repo & args]
                                  (reset! seen {:repo repo :args (vec args)})
                                  "M\tmfuton/src/a.clj\nA\tmfuton/src/b.clj\n")]
        (is (= ["src/a.clj" "src/b.clj"] (sut/files-changed "/gh" "sha1" "mfuton")))
        (is (= "/gh" (:repo @seen)))
        (is (= ["show" "--name-status" "--format=" "sha1" "--" "mfuton"] (:args @seen))))))
  (testing "without a subtree, paths are verbatim and no pathspec is added"
    (let [seen (atom nil)]
      (with-redefs [sut/run-git (fn [_repo & args]
                                  (reset! seen (vec args))
                                  "M\tsrc/a.clj\n")]
        (is (= ["src/a.clj"] (sut/files-changed "/repo" "sha1")))
        (is (= ["show" "--name-status" "--format=" "sha1"] @seen))))))

(deftest list-commits-scopes-subtree-pathspec
  (testing "list-commits appends -- <subtree> so only subtree-touching commits are walked"
    (let [seen (atom nil)]
      (with-redefs [sut/run-git (fn [_repo & args] (reset! seen (vec args)) "")]
        (sut/list-commits "/gh" nil "mfuton")
        (is (= ["--" "mfuton"] (take-last 2 @seen))))
      (with-redefs [sut/run-git (fn [_repo & args] (reset! seen (vec args)) "")]
        (sut/list-commits "/repo" nil)
        (is (not= "--" (last @seen)))))))

(defn- tmp-dir []
  (doto (java.io.File/createTempFile "commit-ingest-" "")
    (.delete)
    (.mkdirs)))

(defn- run-git! [repo & args]
  (let [{:keys [exit err]} (apply git-fixture/git-result repo args)]
    (is (zero? exit) err)))

(defn- fixture-repo-with-mission-commit []
  (let [repo (tmp-dir)
        f (io/file repo "src/demo.clj")]
    (run-git! repo "init")
    (.mkdirs (.getParentFile f))
    (spit f "(ns demo)\n")
    (run-git! repo "add" ".")
    (run-git! repo "-c" "user.name=Agent Test" "-c" "user.email=agent@example.test"
              "commit" "-m" "Implement demo" "-m" "Mission: E-mealy-style-transducer")
    (run-git! repo "show" "-s" "--format=%an" "HEAD")
    (run-git! repo "show" "-s" "--format=%ae" "HEAD")
    repo))

(defn- delete-tree! [root]
  (doseq [f (reverse (file-seq root))]
    (.delete f)))

(deftest ingest-new-commits-records-head-cursor
  (testing "live ingestion anchors the in-memory cursor at HEAD"
    (let [recorded (atom nil)]
      (with-redefs [sut/last-indexed-commit-sha (fn [_] "old-side-tip")
                    sut/list-commits (fn [_ since & _]
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

(deftest post-hyperedges-batches-and-falls-back
  (let [missing-at (ns-resolve 'futon3c.watcher.commit-ingest '!batch-route-missing-at)
        items [["code/v05/var" ["r/a"] ["r"] {"var/qname" "a"}]
               ["code/v05/var" ["r/b"] ["r"] {"var/qname" "b"}]]]
    (testing "one batch request, one result per item in post-hyperedge!'s shape"
      (reset! @missing-at nil)
      (let [posts (atom [])]
        (with-redefs [write-pace/pace! (fn [] nil)
                      http/post (fn [url opts]
                                  (swap! posts conj [url (json/parse-string (:body opts))])
                                  {:status 200
                                   :body (pr-str {:ok true :results [{:ok true :hx/id "hx:a"}
                                                                     {:ok true :hx/id "hx:b"}]})})]
          (let [rs (binding [sut/*valid-time-ms* 1790000000000] (sut/post-hyperedges! items))]
            (is (= 1 (count @posts)))
            (is (str/ends-with? (ffirst @posts) "/api/alpha/hyperedges/batch"))
            (is (= [1790000000000 1790000000000]
                   (map #(get % "hx/valid-time") (get (second (first @posts)) "hyperedges"))))
            (is (= [true true] (map :ok? rs)))
            (is (= ["hx:a" "hx:b"] (map (comp :hx/id :body) rs)))))))
    (testing "a store without the route gets single posts, and is remembered"
      (reset! @missing-at nil)
      (let [urls (atom [])]
        (with-redefs [write-pace/pace! (fn [] nil)
                      http/post (fn [url _]
                                  (swap! urls conj url)
                                  (if (str/ends-with? url "/batch")
                                    {:status 404 :body "{}"}
                                    {:status 200 :body "{\"hx/id\":\"hx:x\"}"}))]
          (is (every? :ok? (sut/post-hyperedges! items)))
          (is (every? :ok? (sut/post-hyperedges! items)))
          (is (= 1 (count (filter #(str/ends-with? % "/batch") @urls))) "not retried")
          (is (= 4 (count (remove #(str/ends-with? % "/batch") @urls))))
          (reset! @missing-at (- (System/currentTimeMillis) (* 11 60 1000)))
          (sut/post-hyperedges! items)
          (is (= 2 (count (filter #(str/ends-with? % "/batch") @urls)))
              "asked again once the recheck interval has passed"))))
    (testing "a failed batch falls back to single posts for that chunk"
      (reset! @missing-at nil)
      (let [urls (atom [])]
        (with-redefs [write-pace/pace! (fn [] nil)
                      http/post (fn [url _]
                                  (swap! urls conj url)
                                  (if (str/ends-with? url "/batch")
                                    {:status 500 :body "{:ok false}"}
                                    {:status 200 :body "{\"hx/id\":\"hx:x\"}"}))]
          (is (every? :ok? (sut/post-hyperedges! items)))
          (is (nil? @@missing-at) "a 500 does not mark the route missing"))))
    (reset! @missing-at nil)))
