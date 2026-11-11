(ns futon3c.agency.pattern-search-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.agency.pattern-index :as pidx]
            [futon3c.agency.pattern-search :as search])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def script-source
  (str "import json,sys,time\n"
       "mode=sys.argv[1]\n"
       "print(json.dumps({'resident':'ready'}),flush=True)\n"
       "for line in sys.stdin:\n"
       " r=json.loads(line)\n"
       " if mode=='hang': time.sleep(10)\n"
       " elif mode=='malformed': print('not-json',flush=True)\n"
       " else: print(json.dumps([{'id':r['query'],'title':'ok','score':1.0,'rank':1}]),flush=True)\n"))

(defn temp-script []
  (let [root (Files/createTempDirectory "pattern-search-test-"
                                        (make-array FileAttribute 0))
        path (.resolve root "fake.py")]
    (spit (.toFile path) script-source)
    {:root root :path (str path)}))

(defn delete-tree! [root]
  (doseq [file (reverse (file-seq (.toFile root)))]
    (io/delete-file file true)))

(use-fixtures :each
  (fn [f]
    (search/reset-state!)
    (try (f) (finally (search/reset-state!)))))

(deftest resident-answers-and-restarts-after-death
  (let [{:keys [root path]} (temp-script)]
    (try
      (binding [search/*resident-command* ["python3" "-u" path "normal"]
                search/*fallback-search* (fn [& _] (throw (ex-info "unexpected" {})))]
        (is (= "first" (:id (first (search/search "first" 1)))) (pr-str (search/stats)))
        (search/stop!)
        (is (= "second" (:id (first (search/search "second" 1)))) (pr-str (search/stats)))
        (is (= 1 (:restarts (search/stats))))
        (is (= 2 (:resident-hits (search/stats)))))
      (finally (delete-tree! root)))))

(deftest hung-resident-times-out-then-falls-back
  (let [{:keys [root path]} (temp-script)
        fallback-calls (atom [])]
    (try
      (binding [search/*resident-command* ["python3" "-u" path "hang"]
                search/*query-timeout-ms* 50
                search/*fallback-search*
                (fn [query top]
                  (swap! fallback-calls conj [query top])
                  [{:id "fallback" :score 0.5 :rank 1}])]
        (is (= "fallback" (:id (first (search/search "slow" 3)))))
        (is (= [["slow" 3]] @fallback-calls))
        (is (= 1 (:fallbacks (search/stats)))))
      (finally (delete-tree! root)))))

(deftest malformed-resident-output-falls-back
  (let [{:keys [root path]} (temp-script)]
    (try
      (binding [search/*resident-command* ["python3" "-u" path "malformed"]
                search/*fallback-search*
                (fn [& _] [{:id "fallback" :score 0.5 :rank 1}])]
        (is (= "fallback" (:id (first (search/search "bad" 1)))))
        (is (= 1 (:fallbacks (search/stats)))))
      (finally (delete-tree! root)))))

(deftest prewarm-starts-resident-before-first-search
  (let [{:keys [root path]} (temp-script)]
    (try
      (binding [search/*resident-command* ["python3" "-u" path "normal"]
                search/*fallback-search* (fn [& _] (throw (ex-info "unexpected" {})))]
        @(search/prewarm!)
        (is (= 0 (:resident-hits (search/stats))))
        ;; Only a process started by prewarm makes the next start a restart.
        (search/stop!)
        (is (= "q" (:id (first (search/search "q" 1)))))
        (is (= 1 (:restarts (search/stats)))))
      (finally (delete-tree! root)))))

;;; ------------------------------------------------------- source choice
;; M-repl-circus packet 3: the resident answers from the pattern-index
;; sidecar when usable? says so (or when it is merely behind), from today's
;; JSON otherwise; stats says which source answered.

(defn- temp-library []
  (let [root (.getAbsolutePath
              (doto (io/file (System/getProperty "java.io.tmpdir")
                             (str "ps-src-" (System/nanoTime)))
                .mkdirs))]
    (doseq [rel ["library/agent/handoff-preserves-context.flexiarg"
                 "library/proof-search/typed-hole-as-frontier.flexiarg"]]
      (let [dst (io/file root rel)]
        (.mkdirs (.getParentFile dst))
        (io/copy (io/file (System/getProperty "user.home")
                          "code/futon3" rel)
                 dst)))
    root))

(defn- stub-build! [lib db]
  (let [root (System/getProperty "user.home")
        proc (.start (doto (ProcessBuilder.
                            [(str root "/code/futon3a/.venv/bin/python3")
                             (str root "/code/futon3a/scripts/pattern_index.py")
                             "build" "--futon3-root" lib "--db" db
                             "--embed-stub"])
                       (.directory (io/file root "code/futon3a"))
                       (.redirectErrorStream true)))
        out (slurp (.getInputStream proc))]
    (is (zero? (.waitFor proc)) (str "stub build failed: " out))))

(deftest sidecar-chosen-when-usable
  (let [lib (temp-library)
        db (str lib "/sidecar.sqlite")]
    (stub-build! lib db)
    (binding [search/*sidecar-path* db
              search/*resident-command* nil]
      (let [source (search/choose-source)]
        (is (= :sidecar (:mode source)))
        (is (nil? (:stale? source))))
      (is (= :sidecar (:source (search/stats))))
      (is (= 1 (:sidecar (search/stats)))))))

(deftest sidecar-chosen-and-marked-when-merely-behind
  (let [lib (temp-library)
        db (str lib "/sidecar.sqlite")]
    (stub-build! lib db)
    ;; delete a library file: drift (one build behind) — serve, but count it
    (io/delete-file (io/file lib "library/agent/handoff-preserves-context.flexiarg"))
    (binding [search/*sidecar-path* db
              search/*library-root* lib
              search/*resident-command* nil
              pidx/*cache-ttl-ms* 0]
      (let [source (search/choose-source)]
        (is (= :sidecar (:mode source)))
        (is (:stale? source)))
      (is (= :sidecar-stale (:source (search/stats))))
      (is (= 1 (:sidecar-stale (search/stats)))))))

(deftest json-chosen-when-sidecar-missing
  (binding [search/*sidecar-path* (str (temp-library) "/no-such.sqlite")
            search/*resident-command* nil]
    (is (= :json (:mode (search/choose-source))))
    (is (= :json (:source (search/stats))))
    (is (= 1 (:json-path (search/stats))))))

(deftest resident-command-follows-source
  (let [lib (temp-library)
        db (str lib "/sidecar.sqlite")]
    (stub-build! lib db)
    (binding [search/*sidecar-path* db
              search/*library-root* lib
              search/*resident-command* nil]
      (is (some #{"--sidecar"} (#'search/resident-command))))
    (binding [search/*sidecar-path* (str lib "/missing.sqlite")
              search/*resident-command* nil]
      (is (some #{"--embeddings"} (#'search/resident-command))))))

(deftest choose-source-survives-a-stats-map-from-before-the-reload
  ;; !stats is a defonce: a live reload keeps the old map, which has no
  ;; :sidecar/:json-path keys.  claude-13, 2026-11-11: (update s :sidecar inc)
  ;; threw on nil there, so the live resident never started after the reload.
  (let [stats @#'futon3c.agency.pattern-search/!stats
        before @stats]
    (try
      (reset! stats {:resident-hits 0 :fallbacks 0 :restarts 0 :last-latency-ms nil})
      (is (#{:sidecar :json} (:mode (futon3c.agency.pattern-search/choose-source))))
      (finally (reset! stats before)))))

