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
(defonce ^:private again? (atom false))
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
            out (slurp (.getInputStream proc))
            err (slurp (.getErrorStream proc))
            exit (.waitFor proc)
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

(defn- run-build-loop!
  "One build; if a change landed during it, exactly one more.  The again?/
   building? handshake runs under the lock so a schedule! arriving between
   the last again? check and building? going false cannot lose a build."
  []
  (loop []
    (run-build!)
    (let [more (locking lock
                 (if @again?
                   (do (reset! again? false) true)
                   (do (reset! building? false) false)))]
      (when more (recur)))))

(defn schedule!
  "Debounced async sidecar rebuild.  Coalesces bursts into one build; a call
   during a running build schedules exactly one more.  Returns immediately."
  []
  (if (off?)
    :off
    (locking lock
      (when-let [^ScheduledFuture old @pending]
        (.cancel old false))
      (let [run-task (bound-fn []
                      (reset! pending nil)
                      (locking lock
                        (if @building?
                          (reset! again? true)
                          (do (reset! building? true)
                              (.execute executor
                                        ^Runnable (bound-fn [] (run-build-loop!)))))))
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
