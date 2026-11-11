(ns futon3c.watcher.pattern-index
  "Keep the pattern-index SQLite sidecar current when the library changes.

   M-repl-circus packet 2 (Joe, 2026-11-11: reindexing 'happens automatically
   in the future').  Copied from futon3c.watcher.scope-reingest's shape:
   debounced schedule!, one single-thread daemon executor, never on a request
   thread.  Differences: one pending build for the whole library (a burst of
   edits — e.g. the 81-file onboard commit a59c721 — is ONE build), and at
   most one build at a time (a change during a build schedules exactly one
   more).

   The build itself is futon3a's incremental builder,
   scripts/pattern_index.py build (packet 1, futon3a 5af594d5), run as a
   subprocess: the embedding model must not load into this JVM.  Pure
   deletions produce no fs event on the poll watcher, so a delete-only change
   is caught at the next build or by the drift figure in
   futon3c.agency.pattern-index/usable?.

   The file ~/code/data/minus40/pattern-index-off stops it (lu-off style)."

  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.util.concurrent Executors ScheduledExecutorService
            ScheduledFuture TimeUnit]))

(def ^:private home (System/getProperty "user.home"))

(def futon3-root
  (or (System/getenv "FUTON3_ROOT") (str home "/code/futon3")))

(def futon3a-root
  (or (System/getenv "FUTON3A_ROOT") (str home "/code/futon3a")))

(def off-switch-path
  (str home "/code/data/minus40/pattern-index-off"))

(def ^:dynamic *debounce-ms* 4000)

(def ^:private build-timeout-minutes
  "A full build takes ~31 s (2026-11-11); past this the build is stopped, so a
   hung builder cannot hold building? forever."
  10)

(def ^:dynamic *build-command*
  "The builder invocation; rebound in tests."
  [(str futon3a-root "/.venv/bin/python3")
   (str futon3a-root "/scripts/pattern_index.py") "build"
   "--futon3-root" futon3-root])

(defonce ^:private ^ScheduledExecutorService executor
  (Executors/newSingleThreadScheduledExecutor
   (reify java.util.concurrent.ThreadFactory
     (newThread [_ r]
       (doto (Thread. r "pattern-index-build") (.setDaemon true))))))

(defonce ^:private pending (atom nil))     ;; ScheduledFuture or nil
(defonce ^:private building? (atom false))
(defonce ^:private lock (Object.))

;; The most recent build: {:stats {} :exit n :stderr-tail "..." :finished-at
;; epoch-ms}. Read by agency.pattern-index/usable? wiring.
(defonce last-result (atom nil))

(defn off? []
  (.exists (io/file off-switch-path)))

(defn- stderr-tail [s]
  (let [ls (str/split-lines (or s ""))]
    (str/join "\n" (take-last 5 ls))))

(defn run-build!
  "Run one build synchronously.  Never throws; records into last-result."
  []
  (if (off?)
    (do (println "[pattern-index] build skipped: off-switch present")
        {:status :skipped :reason :off-switch})
    (try
      (let [pb (doto (ProcessBuilder. ^java.util.List (vec *build-command*))
                 (.directory (io/file futon3a-root)))
            proc (.start pb)
            ;; stderr is drained on its own thread: the model loader writes
            ;; progress bars there, and a full stderr pipe would block the
            ;; child while this thread waits on stdout.
            err-f (future (slurp (.getErrorStream proc)))
            out (slurp (.getInputStream proc))
            exit (if (.waitFor proc build-timeout-minutes TimeUnit/MINUTES)
                   (.exitValue proc)
                   ;; only the build process this watcher started
                   (do (.destroyForcibly proc) -2))
            err (deref err-f 10000 "")
            result {:exit exit
                    :stats (try (json/parse-string out true)
                                (catch Exception _ nil))
                    :stderr-tail (stderr-tail err)
                    :finished-at (System/currentTimeMillis)}]
        (reset! last-result result)
        (println (format "[pattern-index] build exit %d: %s"
                         exit (or (:stats result) (str/trim out))))
        result)
      (catch Throwable t
        (let [result {:exit -1 :error (.getMessage t)
                      :finished-at (System/currentTimeMillis)}]
          (reset! last-result result)
          (println "[pattern-index] build FAILED:" (.getMessage t))
          result)))))

(defn schedule!
  "Debounced async sidecar rebuild.  Returns immediately.

  The debounce task and the build share one thread (`executor` is single
  threaded), so a change that lands during a build is scheduled behind it
  and runs once the build ends; further changes in the meantime replace the
  pending task.  That is what gives \"a burst is one build\" and \"a change
  during a build is exactly one more\" (claude-13's review, 2026-11-11:
  an again?/building? handshake here could never take its mid-build branch,
  because the task that would set it cannot run while the build holds the
  thread)."
  []
  (if (off?)
    :off
    (locking lock
      (when-let [^ScheduledFuture old @pending]
        (.cancel old false))
      (let [run-task (bound-fn []
                       (locking lock (reset! pending nil))
                       (reset! building? true)
                       (try (run-build!)
                            (finally (reset! building? false))))
            task (reify Runnable (run [_] (run-task)))
            fut (.schedule executor task (long *debounce-ms*)
                           TimeUnit/MILLISECONDS)]
        (reset! pending fut)
        :scheduled))))

(defn library-pattern-path?
  "True when PATH is a .flexiarg/.multiarg/.arg under futon3 library/ or
   holes/ — the files the sidecar indexes."
  [path]
  (and (str/starts-with? (str path) futon3-root)
       (re-find #"/(library|holes)/" (str path))
       (re-find #"\.(flexiarg|multiarg|arg)$" (str path))))

(defn on-file-event!
  "Watcher hook: schedule a build when PATH is a library pattern file."
  [path]
  (when (library-pattern-path? path)
    (schedule!)))

(defn status []
  {:off? (off?)
   :pending (some? @pending)
   :building? @building?
   :last-result @last-result})
