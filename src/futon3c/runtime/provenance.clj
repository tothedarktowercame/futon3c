(ns futon3c.runtime.provenance
  "Serving-JVM provenance: this JVM states what code it is.

  Ticket T-jvm-provenance-before-multi-jvm: the one-JVM rule did two jobs,
  and only the resource job expired. The surviving job — \"what code is
  running?\" — gets its answer here: a snapshot of the checkout this JVM
  was loaded from, plus an ordered record of every path live-loaded into
  the process since start. `load-file` leaves no trace in the working
  tree (the 2026-08-22 incident), so the trace lives in this process and
  is served where a caller can ask."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str])
  (:import [java.lang.management ManagementFactory]
           [java.time Instant]))

(def schema-version 1)

(defonce !reloaded-from
  ;; Ordered log of paths live-loaded into this process. Append-only and
  ;; process-local by nature: disk state cannot reconstruct what an
  ;; already-loaded Var body was (see futon3c.wm.code-identity).
  (atom []))

(defn record-reload!
  "Append one live-load event. `source` names the entry path (typically
  :clojure.core/load-file). Returns the recorded entry."
  [path source]
  (let [entry {:path (str path)
               :source (keyword source)
               :recorded-at (str (Instant/now))}]
    (swap! !reloaded-from conj entry)
    entry))

(defn reloaded-from []
  (vec (distinct (map :path @!reloaded-from))))

(defn reset-for-test! []
  (reset! !reloaded-from []))

(defn- repo-root
  "Absolute path of the futon3c working tree this namespace was loaded
  from, derived from its own classpath location. Nil when the source is
  not on the filesystem (e.g. a jar)."
  []
 (when-let [resource (io/resource "futon3c/runtime/provenance.clj")]
   (when (= "file" (.getProtocol resource))
     (let [path (-> resource io/as-file .getCanonicalPath)
           cut (str/last-index-of path "/src/futon3c/runtime/provenance.clj")]
       (when cut (subs path 0 cut))))))

(defn git-facts
  "Branch, head sha and dirty flag for the checkout at `root`.
  :ok? false and the git stderr surface when git is unavailable."
  [root]
  (let [run (fn [& args]
              (let [{:keys [exit out err]} (apply shell/sh "git" "-C" root args)]
                {:ok? (zero? exit) :value (str/trim out) :error (str/trim err)}))
        branch (run "rev-parse" "--abbrev-ref" "HEAD")
        head (run "rev-parse" "HEAD")
        status (run "status" "--porcelain")]
    {:ok? (and (:ok? branch) (:ok? head) (:ok? status))
     :branch (:value branch)
     :head-sha (:value head)
     :dirty? (and (:ok? status) (not (str/blank? (:value status))))
     :error (first (remove str/blank? [(:error branch) (:error head) (:error status)]))}))

(defn summarize
  "Pure assembly of the provenance snapshot the ticket specifies:
  :checkout :branch :head-sha :dirty? :reloaded-from :port :repo
  :started-at. Kept separate from the shell-reading `snapshot` so the
  shape is testable without touching git."
  [{:keys [checkout branch head-sha dirty? reloaded-from port repo
           started-at]}]
  {:schema-version schema-version
   :checkout checkout
   :branch branch
   :head-sha head-sha
   :dirty? (boolean dirty?)
   :reloaded-from (vec reloaded-from)
   :port port
   :repo repo
   :started-at started-at})

(defn jvm-started-at
  "JVM process start time from the runtime MX bean — the one start
  instant that is true of this process, not of a handler rebuild."
  []
  (-> (ManagementFactory/getRuntimeMXBean) .getStartTime Instant/ofEpochMilli str))

(defn snapshot
  "Provenance of this JVM: the checkout it was loaded from, what that
  tree held, and every path live-loaded since start. `port` (from the
  server config when known) lets a caller tell two JVMs of the same repo
  apart."
  ([] (snapshot nil))
  ([{:keys [port] :as _config}]
  (let [root (repo-root)
        facts (when root (git-facts root))]
    (summarize {:checkout root
                :branch (or (:branch facts) nil)
                :head-sha (or (:head-sha facts) nil)
                :dirty? (:dirty? facts)
                :reloaded-from (reloaded-from)
                :port port
                :repo "futon3c"
                :started-at (jvm-started-at)}))))

(defonce ^:private !load-recording-original (atom nil))
(defonce ^:private install-lock (Object.))
(defn install-load-recording!
  "Wrap clojure.core/load-file so every live load lands in !reloaded-from.
  Idempotent: the original var root is captured once (under a lock, since
  alter-var-root is not atomic with the capture); later calls (dev reloads
  of this namespace) are no-ops. The wrapper delegates fully — it records,
  it never refuses."
  []
  (locking install-lock
    (when (nil? @!load-recording-original)
      (let [original load-file]
        (alter-var-root
         #'clojure.core/load-file
         (fn [_current]
           (fn [arg]
             (record-reload! (str arg) :clojure.core/load-file)
             (original arg))))
        (reset! !load-recording-original original)))))

(defn load-recording-installed? []
  (some? @!load-recording-original))
