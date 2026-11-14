(ns futon3c.watcher.commit-ingest-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [babashka.http-client :as http]
            [cheshire.core :as json]
            [futon3c.agency.artifact-activation :as artifact-activation]
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
      (with-redefs [sut/last-indexed-commit-sha (fn [& _] "old-side-tip")
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

(deftest doc-section-hook-submits-mission-sections-only
  (let [submitted (atom [])
        errors (atom [])
        commit {:sha "abc" :ts 1790635000}]
    (with-redefs [sut/files-changed (fn [_ _]
                                     ["holes/missions/M-demo.md" "src/demo.clj"])
                  sut/run-git (fn [_ _ spec]
                                (case spec
                                  "abc:holes/missions/M-demo.md" "# One\nnew\n# Two\nsame"
                                  "abc^:holes/missions/M-demo.md" "# One\nold\n# Two\nsame"
                                  ""))
                  artifact-activation/submit-work!
                  (fn [store artifact text]
                    (swap! submitted conj [artifact text store]) :submitted)
                  artifact-activation/submit-error!
                  (fn [& args] (swap! errors conj args) :submitted)]
      (sut/process-doc-section-activations! "/repo" commit :the-store))
    (is (= 1 (count @submitted)))
    (is (= :the-store (nth (first @submitted) 2)) "records go to the given store, never nil")
    (is (= "One" (some-> @submitted ffirst :id (str/split #":") (nth 2))))
    (is (= "# One\nnew" (second (first @submitted))))
    (is (empty? @errors))))

(deftest doc-section-hook-ignores-non-futon3c-repos
  (let [tasks (atom [])]
    (with-redefs [artifact-activation/submit-task!
                  (fn [task] (swap! tasks conj task) :submitted)]
      (is (nil? (sut/submit-doc-section-activations!
                 "/repo" "futon2" {:sha "abc" :ts 1})))
      (is (empty? @tasks))
      (is (= :submitted (sut/submit-doc-section-activations!
                         "/repo" "futon3c" {:sha "abc" :ts 1})))
      (is (= :submitted (sut/submit-doc-section-activations!
                         "/repo" "futon3c-d" {:sha "abc" :ts 1}))
          "the label the live watcher uses")
      (is (= 2 (count @tasks))))))

(deftest doc-section-hook-caps-large-rewrites
  (let [submitted (atom [])
        errors (atom [])
        new-text (str/join "\n" (map #(str "# S" % "\nbody " %) (range 22)))]
    (with-redefs [sut/files-changed (fn [& _] ["holes/labs/M/BUILD-PLAN-x.md"])
                  sut/run-git (fn [_ _ spec]
                                (if (str/starts-with? spec "abc:") new-text ""))
                  artifact-activation/submit-work!
                  (fn [_ artifact _] (swap! submitted conj artifact) :submitted)
                  artifact-activation/submit-error!
                  (fn [_ artifact _ error]
                    (swap! errors conj [artifact error]) :submitted)]
      (sut/process-doc-section-activations! "/repo" {:sha "abc" :ts 1} :the-store))
    (is (= 20 (count @submitted)))
    (is (= :section-cap-exceeded (get-in @errors [0 1 :reason])))
    (is (= 22 (get-in @errors [0 1 :count])))))

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
                                  (is (= 120000 (:timeout opts)) "a batch waits for its transaction")
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
    (testing "a failed batch reports failed items and is NOT re-posted one by one"
      ;; 2026-11-14: the single-post fallback wrote a second version of every
      ;; edge whose timed-out batch had committed, and multiplied requests to
      ;; an overloaded store by 200.
      (reset! @missing-at nil)
      (doseq [failure [{:status 500 :body "{:ok false}"}
                       {:status -1 :body "timeout"}
                       {:status 503 :body "busy"}]]
        (let [urls (atom [])]
          (with-redefs [write-pace/pace! (fn [] nil)
                        http/post (fn [url _]
                                    (swap! urls conj url)
                                    (if (str/ends-with? url "/batch")
                                      failure
                                      {:status 200 :body "{\"hx/id\":\"hx:x\"}"}))]
            (let [rs (sut/post-hyperedges! items)]
              (is (= 2 (count rs)))
              (is (not-any? :ok? rs))
              (is (= [(:status failure) (:status failure)] (map :status rs))))
            (is (= 1 (count @urls)) (str "no single posts after " (:status failure)))
            (is (nil? @@missing-at) "a failure does not mark the route missing")))))
    (reset! @missing-at nil)))

(deftest doc-section-query-is-the-new-paragraph-only
  ;; An entry appended under a long heading: only the new paragraph is queried.
  (let [submitted (atom [])
        old "## INSTANTIATE\n\n**P1 done.** first entry\n\n**P2 done.** second entry"
        new (str old "\n\n**P3 done.** third entry")]
    (with-redefs [sut/files-changed (fn [& _] ["holes/missions/M-x.md"])
                  sut/run-git (fn [_ _ spec] (if (str/starts-with? spec "abc^:") old new))
                  artifact-activation/submit-work!
                  (fn [_ artifact text] (swap! submitted conj text) :submitted)]
      (sut/process-doc-section-activations! "/repo" {:sha "abc" :ts 1} :the-store))
    (is (= ["**P3 done.** third entry"] @submitted))))

(deftest doc-section-hook-without-a-live-backend-refuses
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Evidence backend unavailable"
                        (artifact-activation/live-backend))))

(deftest cursor-lookup-distinguishes-no-answer-from-no-commits
  (testing "a store that answers with no commit gives nil"
    (with-redefs [http/get (fn [_ _] {:status 200 :body (pr-str {:hyperedges []})})]
      (is (nil? (sut/last-indexed-commit-sha-from-store "demo")))))
  (testing "a timeout, error status or unreadable body is a failed lookup, not nil"
    (doseq [resp [{:status 504 :body "busy"} {:status 200 :body "<html>"} nil]]
      (with-redefs [http/get (fn [_ _] (if resp resp (throw (ex-info "timed out" {}))))]
        (is (= :futon3c.watcher.commit-ingest/lookup-failed
               (binding [*err* (java.io.StringWriter.)]
                 (sut/last-indexed-commit-sha-from-store "demo"))))))))

(defn- live-ingest [since commits head]
  (let [recorded (atom nil) listed (atom nil) batch (atom nil)]
    (with-redefs [sut/last-indexed-commit-sha (constantly since)
                  sut/list-commits (fn [_ s & _] (reset! listed s) commits)
                  sut/current-head-sha (constantly head)
                  sut/ingest-commits-batch! (fn [{:keys [commits]}]
                                              (reset! batch commits)
                                              {:n-ingested (count commits)
                                               :latest-sha (:sha (last commits))
                                               :n-failed 0 :n-blocks 0
                                               :n-mana-credited 0})
                  sut/record-last-ingested! (fn [label sha] (reset! recorded [label sha]))]
      (let [report (atom nil)
            log (with-out-str
                  (reset! report
                          (sut/ingest-new-commits! {:repo-root "/tmp/repo" :repo-label "demo"
                                                    :file->structure (constantly nil)})))]
        {:report @report :log log
         :recorded @recorded :listed @listed :batch @batch}))))

(deftest live-ingest-never-walks-whole-history
  (testing "an unanswered cursor lookup skips the cycle: no git log, no posts, no cursor"
    (let [{:keys [report listed batch recorded log]}
          (live-ingest :futon3c.watcher.commit-ingest/lookup-failed
                       [{:sha "a"}] "head")]
      (is (= :cursor-lookup-failed (:skipped report)))
      (is (nil? listed)) (is (nil? batch)) (is (nil? recorded))
      (is (str/includes? log "skipping this cycle"))))
  (testing "no known cursor seeds it at HEAD without ingesting history"
    (let [{:keys [report batch recorded log]} (live-ingest nil [{:sha "a"}] "head-sha-1")]
      (is (= :cursor-seeded-at-head (:skipped report)))
      (is (nil? batch))
      (is (= ["demo" "head-sha-1"] recorded))
      (is (str/includes? log "ingest-all-commits!"))))
  (testing "a long range is ingested in bounded steps, cursor at the last one handled"
    (let [commits (mapv (fn [i] {:sha (str "c" i)}) (range 120))
          {:keys [report batch recorded]} (live-ingest "old" commits "head")]
      (is (= sut/max-live-commits-per-cycle (count batch)))
      (is (= (subvec commits 0 sut/max-live-commits-per-cycle) batch))
      (is (= ["demo" (str "c" (dec sut/max-live-commits-per-cycle))] recorded))
      (is (= (- 120 sut/max-live-commits-per-cycle) (:remaining report))))))

(deftest a-running-batch-can-be-stopped
  ;; 2026-11-14: a 14,677-commit futon2 pass could not be stopped short of a
  ;; JVM restart; the batch loop never checked for a stop request.
  (let [posts (atom 0)
        stop-at (atom 3)
        stop-before @(ns-resolve 'futon3c.watcher.commit-ingest '!stop-batches-before-ms)]
   (try
    (with-redefs [write-pace/pace! (fn [] nil)
                  http/post (fn [_ _]
                              (when (= (swap! posts inc) @stop-at)
                                (sut/stop-running-batches!))
                              {:status 200 :body "{\"hx/id\":\"hx:x\"}"})
                  sut/files-changed (constantly [])
                  sut/submit-doc-section-activations! (fn [& _] nil)]
      (let [commits (mapv (fn [i] {:sha (str "s" i) :email "e" :name "n" :ts 1}) (range 20))
            batch (fn [] (sut/ingest-commits-batch!
                          {:commits commits :repo-root "/tmp/r" :repo-label "demo"
                           :file->structure (constantly nil)}))]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"stopped by stop-running-batches!"
                              (batch)))
        (is (= 3 @posts) "no post after the stop")
        (testing "a batch started after the stop runs normally"
          (Thread/sleep 2)
          (reset! stop-at -1)
          (reset! posts 0)
          (is (= 20 (:n-ingested (batch))))
          (is (pos? @posts)))))
    ;; leave no stop in force for posts made outside a batch by later tests
    (finally (reset! stop-before 0)))))

(deftest cursor-survives-restart-and-cold-start-walks-recent-commits
  ;; 2026-11-14: the store cursor query is a scan that failed (500 at 61 s)
  ;; on a healthy store, so every JVM start lost every repo's cursor.
  (let [dir (doto (java.io.File/createTempFile "cursor" "") (.delete) (.mkdirs))
        path (str dir "/cursors.edn")
        mem @(ns-resolve 'futon3c.watcher.commit-ingest '!last-ingested)
        saved @mem]
    (try
      (with-redefs-fn {(ns-resolve 'futon3c.watcher.commit-ingest 'cursor-file-path) path}
        (fn []
          (testing "a recorded cursor is written to the file and read back after a restart"
            (reset! mem {})
            (#'sut/record-last-ingested! "demo" "sha-1")
            (reset! mem {})                       ; restart: memory empty
            (with-redefs [http/get (fn [& _] (throw (ex-info "must not query the store" {})))]
              (is (= "sha-1" (sut/last-indexed-commit-sha "demo" "/tmp/repo")))))
          (testing "no cursor anywhere: walk newest-first, first commit the store has wins"
            (reset! mem {})
            (let [asked (atom [])]
              (with-redefs [sut/run-git (fn [& _] "c3\nc2\nc1\n")
                            http/get (fn [url _]
                                       (swap! asked conj (last (str/split url #":")))
                                       {:status (if (str/ends-with? url "c2") 200 404)})]
                (is (= "c2" (sut/last-indexed-commit-sha "other" "/tmp/repo")))
                (is (= ["c3" "c2"] @asked)))))
          (testing "store has none of them: nil; a failed answer or git failure: ::lookup-failed"
            (reset! mem {})
            (with-redefs [sut/run-git (fn [& _] "c2\nc1\n")
                          http/get (fn [_ _] {:status 404})]
              (is (nil? (sut/last-indexed-commit-sha "none" "/tmp/repo"))))
            (with-redefs [sut/run-git (fn [& _] "c2\nc1\n")
                          http/get (fn [_ _] {:status 500})]
              (is (= :futon3c.watcher.commit-ingest/lookup-failed
                     (sut/last-indexed-commit-sha "busy" "/tmp/repo"))))
            (with-redefs [sut/run-git (fn [& _] "")]
              (is (= :futon3c.watcher.commit-ingest/lookup-failed
                     (sut/last-indexed-commit-sha "nogit" "/tmp/repo")))))))
      (finally (reset! mem saved)))))

(deftest top-level-spans-locate-forms
  (testing "clojure: names after metadata, spans run to the next top-level form"
    (is (= [{:name "a" :start 2 :end 3} {:name "b" :start 4 :end 5} {:name "c" :start 6 :end 6}]
           (sut/top-level-spans "x.clj" (str "(ns x)\n(defn a []\n  1)\n(def ^:private b\n  2)\n"
                                               "(defn- ^{:doc \"d\"} c [] 3)\n")))))
  (testing "elisp"
    (is (= ["f" "v"] (map :name (sut/top-level-spans "x.el" "(defun f ()\n  1)\n(defvar v 2)\n")))))
  (testing "python: decorators belong to the def; top-level code ends a span"
    (is (= [{:name "f" :start 2 :end 4} {:name "K" :start 6 :end 7}]
           (sut/top-level-spans "x.py" (str "import os\n@dec\ndef f():\n    return 1\nX = 2\n"
                                              "class K:\n    pass\n")))))
  (testing "an unknown language gives nil, not an empty claim"
    (is (nil? (sut/top-level-spans "x.md" "# hi\n")))))

(deftest changed-line-ranges-read-zero-context-hunks
  (is (= [[3 3] [10 12] [7 7]]
         (sut/changed-line-ranges "@@ -3 +3 @@\n-x\n+y\n@@ -9,0 +10,3 @@\n+a\n@@ -8,2 +7,0 @@\n-z\n"))))

(deftest edits-edges-go-only-to-touched-vars
  ;; 2026-11-14: a commit got an edits edge to every var in each file it
  ;; changed; most of futon1b's hyperedge write volume.
  (let [repo (tmp-dir)
        f (io/file repo "src/demo.clj")
        commit! (fn [msg] (run-git! repo "add" ".")
                  (run-git! repo "-c" "user.name=T" "-c" "user.email=t@example.test"
                            "commit" "-q" "-m" msg)
                  (git-fixture/git! repo "rev-parse" "HEAD"))]
    (try
      (run-git! repo "init" "-q")
      (.mkdirs (.getParentFile f))
      (spit f "(ns demo)\n(defn a [] 1)\n(defn b [] 2)\n(defn c [] 3)\n")
      (let [first-sha (commit! "add")
            _ (spit f "(ns demo)\n(defn a [] 1)\n(defn b [] 22)\n(defn c [] 3)\n")
            second-sha (commit! "edit b")
            posted (atom [])
            run (fn [sha]
                  (reset! posted [])
                  (with-redefs [sut/post-hyperedges! (fn [items] (swap! posted into items) [])]
                    (sut/ingest-edits-for-commit!
                     ["v05"] {} "r" {:sha sha}
                     (constantly ["demo/a" "demo/b" "demo/c"]) (str repo) nil))
                  (set (map (comp second second) @posted)))]
        (is (= #{"r/demo/b"} (run second-sha)) "only the edited var")
        (is (= #{"r/demo/a" "r/demo/b" "r/demo/c"} (run first-sha))
            "a commit that adds the file touches all its vars"))
      (finally (delete-tree! repo)))))
