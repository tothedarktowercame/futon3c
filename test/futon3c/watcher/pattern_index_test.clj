(ns futon3c.watcher.pattern-index-test
  "M-repl-circus packet 2: the sidecar watcher (debounce, one-more-build,
   off-switch) and the usable? freshness gate.  Builder stubbed except one
   test, which runs the real pattern_index.py build with --embed-stub
   (test-only flag) against a temp library of real library files."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon.pattern-index :as notions-index]
            [futon3c.agency.pattern-index :as pidx]
            [futon3c.transport.http :as http]
            [futon3c.watcher.pattern-index :as w]))

(def ^:private fixture-files
  ["library/cascade-construction/read-what-exists-first.flexiarg"
   "library/agent/handoff-preserves-context.flexiarg"
   "library/proof-search/typed-hole-as-frontier.flexiarg"])

(defn- temp-dir []
  (let [d (io/file (System/getProperty "java.io.tmpdir")
                   (str "pi-test-" (System/nanoTime)))]
    (.mkdirs d)
    (.getAbsolutePath d)))

(defn- make-library []
  (let [root (temp-dir)]
    (doseq [rel fixture-files]
      (let [dst (io/file root rel)]
        (.mkdirs (.getParentFile dst))
        (io/copy (io/file w/futon3-root rel) dst)))
    root))

(defn- wait-for-it
  "Wait until (pred) or ~10 s. Returns the last pred value."
  [pred]
  (loop [i 0]
    (let [v (pred)]
      (if (or v (> i 200)) v (do (Thread/sleep 50) (recur (inc i)))))))

(defn- with-stub-builder
  "Bind a stub builder: a shell script appending one line per call to LOG,
   sleeping SLEEP-MS per call. Run F with {:log path :calls (fn [] n)}."
  [sleep-ms f]
  (let [dir (temp-dir)
        log (str dir "/calls.log")
        script (str dir "/stub.sh")]
    ;; The call is logged when the build STARTS, so a test can act while
    ;; it is still running (logging after the sleep let the "change during
    ;; a build" test pass with the one-more rule removed).
    (spit script (str "#!/bin/sh\necho call >> " log "\n"
                      "sleep " (/ sleep-ms 1000.0) "\n"
                      "echo '{\"scanned\":3,\"changed\":1,\"deleted\":0,\"embedded\":1}'\n"))
    (.setExecutable (io/file script) true)
    (binding [w/*build-command* ["sh" script]
              w/*debounce-ms* 100]
      (f {:log log
          :calls (fn [] (let [f (io/file log)]
                          (if (.exists f)
                            (count (clojure.string/split-lines
                                    (slurp f))) 0)))}))))

(deftest create-triggers-one-build
  (with-stub-builder
   0
   (fn [{:keys [calls]}]
     (reset! w/last-result nil)
     (w/schedule!)
     (is (true? (wait-for-it #(pos? (calls)))) "one build runs")
     (Thread/sleep 400)
     (is (= 1 (calls)) "exactly one build"))))

(deftest ten-edits-debounce-to-one-build
  (with-stub-builder
   0
   (fn [{:keys [calls]}]
     (dotimes [_ 10] (w/schedule!))
     (is (true? (wait-for-it #(pos? (calls)))))
     (Thread/sleep 400)
     (is (= 1 (calls)) "ten edits inside the debounce window are one build"))))

(deftest change-during-build-schedules-exactly-one-more
  (with-stub-builder
   600
   (fn [{:keys [calls]}]
     (w/schedule!)
     (is (true? (wait-for-it #(pos? (calls)))) "first build started")
     ;; during the (600 ms) build: more changes
     (w/schedule!)
     (w/schedule!)
     (is (true? (wait-for-it #(= 2 (calls)))) "exactly one more build")
     (Thread/sleep 800)
     (is (= 2 (calls)) "no third build"))))

(deftest a-failed-build-is-retried-a-bounded-number-of-times
  (let [dir (temp-dir)
        log (str dir "/calls.log")
        script (str dir "/fail.sh")]
    ;; fails on its first two calls, then succeeds
    (spit script (str "#!/bin/sh\necho call >> " log "\n"
                      "n=$(wc -l < " log ")\n"
                      "if [ \"$n\" -le 2 ]; then exit 1; fi\n"
                      "echo '{\"scanned\":3,\"changed\":0,\"deleted\":0,\"embedded\":0}'\n"))
    (.setExecutable (io/file script) true)
    (let [calls #(let [f (io/file log)] (if (.exists f) (count (str/split-lines (slurp f))) 0))]
      (binding [w/*build-command* ["sh" script]
                w/*debounce-ms* 50
                w/*retry-base-ms* 100]
        (w/schedule!)
        (is (true? (wait-for-it #(= 3 (calls)))) "two failures, then one success")
        (is (= 0 (:exit @w/last-result)))
        (Thread/sleep 700)
        (is (= 3 (calls)) "no retry after the success")))))

(deftest retries-stop-after-three
  (let [dir (temp-dir)
        log (str dir "/calls.log")
        script (str dir "/fail.sh")]
    (spit script (str "#!/bin/sh\necho call >> " log "\nexit 1\n"))
    (.setExecutable (io/file script) true)
    (let [calls #(let [f (io/file log)] (if (.exists f) (count (str/split-lines (slurp f))) 0))]
      (binding [w/*build-command* ["sh" script]
                w/*debounce-ms* 50
                w/*retry-base-ms* 50]
        (w/schedule!)
        (is (true? (wait-for-it #(= 4 (calls)))) "the build and three retries")
        (Thread/sleep 800)
        (is (= 4 (calls)) "then nothing until the next change")))))

(deftest off-switch-stops-builds
  (let [off (io/file w/off-switch-path)]
    (.mkdirs (.getParentFile off))
    (spit off "test")
    (try
      (is (= :off (w/schedule!)) "schedule! is refused while off")
      (is (false? (wait-for-it #(boolean @w/last-result))) ; no build records
          "no build runs while off")
      (finally (.delete off)))))

;;; ------------------------------------------------------------- usable?

(defn- real-build! [lib-root db]
  (let [pb (doto (ProcessBuilder.
                  [(str w/futon3a-root "/.venv/bin/python3")
                   (str w/futon3a-root "/scripts/pattern_index.py") "build"
                   "--futon3-root" lib-root "--db" db "--embed-stub"])
             (.directory (io/file w/futon3a-root))
             (.redirectErrorStream true))
        proc (.start pb)
        out (slurp (.getInputStream proc))
        exit (.waitFor proc)]
    (is (zero? exit) (str "real build failed: " out))))

(deftest usable-gate-lifecycle
  (binding [pidx/*cache-ttl-ms* 0]
    (let [lib (make-library)
          db (str (temp-dir) "/sidecar.sqlite")
          gate (fn [] (pidx/usable? db {:library-root lib}))]
      (testing "before the first build: not usable (no sidecar)"
        (is (= :no-sidecar (:why (gate)))))
      (real-build! lib db)
      (testing "after a build: usable"
        (is (:usable? (gate)) (pr-str (gate))))
      (testing "the same automatic build writes the canonical sigil snapshot"
        (with-open [conn (java.sql.DriverManager/getConnection (str "jdbc:sqlite:" db))
                    st (.createStatement conn)
                    rs (.executeQuery st (str "SELECT count(*), "
                                              "(SELECT embedding_seq FROM sigil_checkpoint "
                                              "ORDER BY seq DESC LIMIT 1), "
                                              "(SELECT max(seq) FROM checkpoint) "
                                              "FROM pattern_sigil"))]
          (is (.next rs))
          (is (= 3 (.getLong rs 1)))
          (is (= (.getLong rs 2) (.getLong rs 3))))
        (is (.exists (io/file lib "resources/sigils/patterns-index.tsv"))))
      (binding [notions-index/*db-path* db
                notions-index/*checkpoint-ttl-ms* 0]
        (is (= 3 (count (#'http/load-patterns-sidecar))))
        (let [src (io/file w/futon3-root
                           "library/process/built-but-not-wired-invisibility.flexiarg")
              dst (io/file lib
                           "library/process/built-but-not-wired-invisibility.flexiarg")]
          (.mkdirs (.getParentFile dst))
          (io/copy src dst))
        (real-build! lib db)
        (is (= 4 (count (#'http/load-patterns-sidecar)))
            "HTTP sees the checkpoint move without a namespace reload"))
      (testing "a planted stale embedding row: not usable"
        (let [conn (java.sql.DriverManager/getConnection (str "jdbc:sqlite:" db))]
          (with-open [st (.createStatement conn)]
            (.execute st "UPDATE embedding SET file_sha256='x'"))
          (.close conn))
        (is (= :stale-embeddings (:why (gate)))))
      (testing "repaired by the next build"
        (real-build! lib db)
        (is (:usable? (gate))))
      (testing "a deleted library file is drift until the next build"
        (.delete (io/file lib (last fixture-files)))
        (let [r (gate)]
          (is (false? (:usable? r)))
          (is (= :drift (:why r))))
        (real-build! lib db)
        (is (:usable? (gate)) "after rebuild, the deletion is indexed away")))))
