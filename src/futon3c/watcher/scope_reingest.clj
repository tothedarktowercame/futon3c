(ns futon3c.watcher.scope-reingest
  "Keep a mission/excursion/campaign's SCOPE-SURFACE current when its doc lands.

   The file watcher ingests the *doc* (futon3c.watcher.file-ingest), but the
   scope hyperedges (eightfold-phase / loose-section / capability-scope / …)
   are a SEPARATE pipeline (mission_scope_detect.py → mission-scope-ingest).
   Without this trigger, a landed/edited doc's scope-surface silently drifts
   behind the filesystem — `mission-mode` then reads stale or absent scopes.

   This module closes that gap: on doc-land the watcher calls `schedule!`,
   which DEBOUNCES (coalesces rapid saves of the same doc) and runs the
   reingest ASYNC on a single background thread (one reingest at a time —
   gentle on the serving JVM). The work mirrors `mission-scope-reingest.sh`
   but in-process (no Drawbridge round-trip back into this JVM):
     1. re-detect (python)  → rewrites data/mission-scope-trees/<id>.json
     2. per-binder ingest   → mission-scope-ingest/-main  (the W2′ enrichment
                              path; a plain full-run skips enrichments)
     3. true-up             → retracts scopes of binder types the rewrite drop
   `-main` is System/exit-free (Drawbridge-safe), so it is safe to call here."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [cheshire.core :as json])
  (:import [java.util.concurrent Executors ScheduledExecutorService
            ScheduledFuture TimeUnit]))

(def ^:private home (System/getProperty "user.home"))
(def ^:private futon6-root (or (System/getenv "FUTON6_ROOT") (str home "/code/futon6")))
(def ^:private debounce-ms 4000)

(defn- doc->id [doc-path]
  (-> (io/file doc-path) .getName (str/replace #"\.md$" "")))

(defn- run-detect!
  "Run the python scope-detector over DOC-PATH (rewrites the scope-tree JSON).
   Returns {:exit <code> :out <combined stdout/stderr>}."
  [doc-path]
  (let [pb (doto (ProcessBuilder. ["python3" "scripts/mission_scope_detect.py"
                                   (.getAbsolutePath (io/file doc-path))])
             (.directory (io/file futon6-root))
             (.redirectErrorStream true))
        proc (.start pb)
        out (slurp (.getInputStream proc))]
    {:exit (.waitFor proc) :out out}))

(defn- binders-of
  "The distinct binder types in the freshly-detected scope-tree for ID."
  [id]
  (let [f (io/file futon6-root "data/mission-scope-trees" (str id ".json"))]
    (when (.exists f)
      (some-> (json/parse-string (slurp f))
              (get "scope-count-by-binder-type")
              keys))))

(defn reingest-now!
  "Synchronously re-detect + per-binder ingest + true-up the scope-surface for
   DOC-PATH. Never throws — logs and returns a result map. Exposed for the
   backlog/manual path as well as the debounced trigger."
  [doc-path]
  (let [id (doc->id doc-path)]
    (cond
      (not (.exists (io/file doc-path)))
      (do (println (format "[scope-reingest] %s skip: doc not found (%s)" id doc-path))
          {:status :skipped :id id :reason :no-doc})
      :else
      (try
        (let [main (requiring-resolve 'futon3c.scripts.mission-scope-ingest/-main)
              {:keys [exit out]} (run-detect! doc-path)]
          (if (not (zero? exit))
            (do (println (format "[scope-reingest] %s FAILED detect (exit %s): %s"
                                 id exit (str/trim (or out ""))))
                {:status :failed :id id :reason :detect})
            (let [binders (vec (binders-of id))]
              (doseq [b binders] (with-out-str (main "--binder" b id)))
              (with-out-str (main "--true-up" id))
              (println (format "[scope-reingest] %s ok (%d binders: %s)"
                                id (count binders) (str/join " " binders)))
              {:status :ok :id id :binders binders})))
        (catch Throwable t
          (println (format "[scope-reingest] %s FAILED: %s" id (.getMessage t)))
          {:status :failed :id id :error (.getMessage t)})))))

;; --- debounce: one pending task per path; single background worker thread ---

(defonce ^:private ^ScheduledExecutorService executor
  (Executors/newSingleThreadScheduledExecutor
   (reify java.util.concurrent.ThreadFactory
     (newThread [_ r]
       (doto (Thread. r "scope-reingest") (.setDaemon true))))))

(defonce ^:private pending (atom {}))        ;; doc-path -> ScheduledFuture
(defonce ^:private lock (Object.))

(defn schedule!
  "Debounced async scope-reingest for DOC-PATH. Coalesces rapid saves: a new
   call cancels the still-pending one and reschedules `debounce-ms` out. Returns
   immediately — the watcher cycle never blocks on the (seconds-long) reingest."
  [doc-path]
  (locking lock
    (when-let [^ScheduledFuture old (get @pending doc-path)]
      (.cancel old false))
    (let [task (reify Runnable
                 (run [_]
                   (swap! pending dissoc doc-path)
                   (reingest-now! doc-path)))
          fut (.schedule executor task (long debounce-ms) TimeUnit/MILLISECONDS)]
      (swap! pending assoc doc-path fut)))
  :scheduled)
