(ns futon3c.agency.bg-process
  "Durable background processes for REPL-inhabiting agents.

   PROBLEM (claude-1/claude-11 incident, 2026-06-27): an agent on a warm pouch
   (`claude --print`, M-kangaroo) that spawns an OS subprocess makes it a child of
   that pouch. The pouch is EPHEMERAL — LRU-evicted (`max-warm`, RAM-bound) or
   idle-reaped between turns — and tearing it down SIGTERM/SIGKILLs the claude
   process, whose shutdown reaps its background shells (even setsid-detached ones).
   So inter-turn OS work dies (\"the watcher got reaped on teardown again\").

   FIX: spawn the process as a child of the durable futon3c JVM instead. The JVM is
   never torn down between turns (I-0), so its child outlives any pouch eviction —
   at ZERO extra RAM (it is the same process the agent would have spawned, just
   re-parented). This is exactly how the legacy Claude Code CLI keeps background
   work alive: a long-lived parent owns it. Here the long-lived parent is the JVM,
   not the per-agent claude.

   Agents reach this over Drawbridge (the established agent->JVM channel), or via
   `scripts/bg.py`:
     (futon3c.agency.bg-process/launch! {:cmd \"…\" :agent-id \"claude-1\" :label \"…\"})
     (futon3c.agency.bg-process/status  \"bg-…\")
     (futon3c.agency.bg-process/tail    \"bg-…\" 40)
     (futon3c.agency.bg-process/kill!   \"bg-…\")
   Output (stdout+stderr) is captured to <out-dir>/<id>.log so it survives turns
   and is tailable.

   SCOPE: helper work only — backlog runners, reingest, builds, watchers. NOT for
   spawning agent/claude clones (I-1/I-2/I-3). v1 tracks tasks in-memory (defonce,
   survives Drawbridge reload); a JVM restart loses the registry but the processes
   themselves survive as init-reparented orphans with their log files intact."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.util.concurrent TimeUnit]))

(def ^:private out-dir "/tmp/futon3c-bg")

(defonce ^:private !tasks (atom {}))   ;; id -> task map (incl. raw :process)
(defonce ^:private !ctr (atom 0))

(defn- now-ms [] (System/currentTimeMillis))
(defn- now-iso [] (str (java.time.Instant/ofEpochMilli (now-ms))))

(defn- task-public
  "Public view of a task — drops the raw Process, computes live status/exit."
  [{:keys [^Process process] :as t}]
  (let [alive? (boolean (and process (.isAlive process)))
        exit (when (and process (not alive?))
               (try (.exitValue process) (catch Throwable _ nil)))]
    (-> t
        (dissoc :process)
        (assoc :alive? alive?
               :status (cond alive?         :running
                             (some? (:status t)) (:status t)
                             :else          :exited)
               :exit (or (:exit t) exit)))))

(defn launch!
  "Spawn CMD (a shell string) as a child of the JVM — durable across pouch
   teardown. Opts: :cmd (required), :agent-id, :label, :dir, :env (map of
   name->value). stdout+stderr are captured to <out-dir>/<id>.log. Returns the
   public task map incl. :id, :pid, :out-file."
  [{:keys [cmd agent-id label dir env]}]
  (when (str/blank? (str cmd))
    (throw (ex-info "bg-process/launch! requires a non-blank :cmd" {})))
  (.mkdirs (io/file out-dir))
  (let [id (str "bg-" (now-ms) "-" (swap! !ctr inc))
        out-file (str out-dir "/" id ".log")
        pb (doto (ProcessBuilder. ^java.util.List ["bash" "-lc" (str cmd)])
             (.redirectErrorStream true)
             (.redirectOutput (io/file out-file)))]
    (when (and dir (not (str/blank? (str dir))))
      (.directory pb (io/file (str dir))))
    (when (map? env)
      (let [m (.environment pb)]
        (doseq [[k v] env] (.put m (str k) (str v)))))
    (let [proc (.start pb)
          task {:id id
                :agent-id (some-> agent-id str not-empty)
                :label (some-> label str not-empty)
                :cmd (str cmd)
                :dir (some-> dir str not-empty)
                :pid (.pid proc)
                :out-file out-file
                :started-at (now-iso)
                :status :running
                :process proc}]
      (swap! !tasks assoc id task)
      ;; daemon completion watcher: record terminal status when the process exits,
      ;; so `status` reports :exited/:exit without the caller polling.
      (doto (Thread.
             ^Runnable
             (fn []
               (try
                 (.waitFor proc)
                 (swap! !tasks update id
                        (fn [t] (when t
                                  (assoc t :status :exited
                                         :exit (try (.exitValue proc) (catch Throwable _ nil))
                                         :finished-at (now-iso)))))
                 (catch Throwable _)))
             (str "bg-process-wait-" id))
        (.setDaemon true)
        (.start))
      (task-public task))))

(defn status
  "Public task map for ID, or nil."
  [id]
  (some-> (get @!tasks (str id)) task-public))

(defn list-tasks
  "All tracked tasks, or only those for AGENT-ID."
  ([] (mapv task-public (vals @!tasks)))
  ([agent-id]
   (->> (vals @!tasks)
        (filter #(= (some-> agent-id str not-empty) (:agent-id %)))
        (mapv task-public))))

(defn tail
  "Last N lines (default 40) of a task's captured output, or nil if none yet."
  ([id] (tail id 40))
  ([id n]
   (let [f (some-> (get @!tasks (str id)) :out-file io/file)]
     (when (and f (.exists f))
       (->> (str/split-lines (slurp f))
            (take-last (long n))
            (str/join "\n"))))))

(defn kill!
  "Terminate a task's process (SIGTERM, then SIGKILL after a 500ms grace)."
  [id]
  (when-let [{:keys [^Process process]} (get @!tasks (str id))]
    (when (and process (.isAlive process))
      (.destroy process)
      (when-not (.waitFor process 500 TimeUnit/MILLISECONDS)
        (.destroyForcibly process)))
    (swap! !tasks update (str id)
           #(when % (assoc % :status :killed :finished-at (now-iso))))
    (status id)))

(defn forget!
  "Drop a TERMINAL task from the registry (its log file is left on disk). No-op
   on a still-running task — kill! it first."
  [id]
  (let [{:keys [^Process process]} (get @!tasks (str id))]
    (if (and process (.isAlive process))
      {:ok false :reason :still-running :id (str id)}
      (do (swap! !tasks dissoc (str id))
          {:ok true :id (str id)}))))
