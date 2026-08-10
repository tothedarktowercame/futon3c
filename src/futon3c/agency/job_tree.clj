(ns futon3c.agency.job-tree
  "Launch-time discovery and Cyder projection for agent-owned OS jobs.

   `nohup`, `setsid`, and shell backgrounding deliberately sever ancestry, so
   an after-the-fact `pgrep` cannot attribute a process honestly.  The observer
   arms when an agent emits a Bash tool-use event, samples the known agent
   process's descendants while the command starts, and retains the identity of
   processes that remain alive after the Bash tool reports completion.  For a
   detached command launched through `ssh`, the same birth-window observation
   is performed on the named remote host; the local SSH launcher is not
   mistaken for the durable job.

   Monitor/task IDs are useful labels but are not the discovery mechanism.
   Each discovered job owns one state atom, exposed through Cyder and rendered
   under its agent + turn in `*tree*`."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [futon3c.blackboard :as bb]
            [futon3c.cyder :as cyder])
  (:import [java.lang ProcessHandle]
           [java.time Duration Instant]
           [java.util Optional]
           [java.util.concurrent Executors ScheduledExecutorService ThreadFactory
            TimeUnit]))

(defonce ^{:doc "{job-id -> state-atom}; terminal jobs remain briefly visible."}
  !jobs (atom {}))

(defonce ^{:doc "{tool-use-id -> launch probe}."}
  !probes (atom {}))

(def ^:private sample-ms 20)
(def ^:private post-result-grace-ms 250)
(def ^:private discovery-ceiling-ms 10000)
(def ^:private terminal-retention-ms (* 15 60 1000))

(defonce ^:private ^ScheduledExecutorService retention-executor
  (Executors/newSingleThreadScheduledExecutor
   (reify ThreadFactory
     (newThread [_ runnable]
       (doto (Thread. runnable "agency-job-tree-retention")
         (.setDaemon true))))))

(defonce ^:private !ticker (atom nil))

(defn- now [] (Instant/now))

(defn- optional-value [^Optional x]
  (when (.isPresent x) (.get x)))

(defn- process-info
  [pid]
  (when-let [^ProcessHandle h (some-> (ProcessHandle/of (long pid)) optional-value)]
    (let [info (.info h)]
      {:pid (.pid h)
       :alive? (.isAlive h)
       :parent-pid (some-> (.parent h) optional-value .pid)
       :started-at (some-> (.startInstant info) optional-value)
       :command (some-> (.command info) optional-value)
       :command-line (some-> (.commandLine info) optional-value)})))

(def ^:private ssh-options-with-argument
  #{"-B" "-b" "-c" "-D" "-E" "-e" "-F" "-I" "-i" "-J" "-L"
    "-l" "-m" "-O" "-o" "-p" "-Q" "-R" "-S" "-W" "-w"})

(defn- scanned-word [match]
  (or (nth match 1 nil) (nth match 2 nil) (nth match 3 nil)))

(defn- command-words [command]
  (mapv scanned-word
        (re-seq #"(?:^|\s)(?:'([^']*)'|\"([^\"]*)\"|(\S+))"
                (str command))))

(defn- ssh-target
  "Return the first ssh destination in COMMAND, handling the common options
   which consume a following argument.  Returns nil for ssh subcommands which
   have no destination (`-Q`)."
  [command]
  (let [words (command-words command)
        ssh-idx (first (keep-indexed
                        (fn [idx word]
                          (when (contains? #{"ssh" "/usr/bin/ssh"} word) idx))
                        words))]
    (when (some? ssh-idx)
      (loop [xs (subvec words (inc ssh-idx))]
        (when-let [word (first xs)]
          (cond
            (= word "--") (second xs)
            (= word "-Q") nil
            (contains? ssh-options-with-argument word) (recur (subvec xs (min 2 (count xs))))
            (str/starts-with? word "-") (recur (subvec xs 1))
            :else word))))))

(defn- remote-snapshot
  "Read a remote process table without a shell on the local side.  STARTED is
   retained verbatim as the PID-reuse guard.  Failure is explicit so a network
   outage never turns an unknown remote job into a false `completed` state.
   Full ARGS are needed only for launch attribution; steady-state session
   polling uses the compact executable name."
  ([host] (remote-snapshot host true))
  ([host include-args?]
   (try
    (let [columns (str "pid=,ppid=,sid=,pgid=,lstart=,"
                       (if include-args? "args=" "comm="))
          pb (doto (ProcessBuilder.
                    ^java.util.List
                    ["ssh" "-o" "BatchMode=yes" "-o" "ConnectTimeout=5"
                     (str host) "ps" "-eo" columns])
               (.redirectErrorStream true))
          proc (.start pb)
          output-future (future (slurp (.getInputStream proc)))]
      ;; Drain concurrently: Java command lines on the research hosts are
      ;; large enough to fill the subprocess pipe before `ps` can exit.
      (if-not (.waitFor proc 8 TimeUnit/SECONDS)
        (do (.destroyForcibly proc)
            (future-cancel output-future)
            {:ok? false :error :timeout})
        (let [output (deref output-future 1000 "")
              exit (.exitValue proc)]
          (if (zero? exit)
            {:ok? true
             :processes
             (->> (str/split-lines output)
                  (keep (fn [line]
                          (when-let [[_ pid ppid sid pgid started args]
                                     (re-matches
                                      #"\s*(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+([A-Z][a-z]{2}\s+[A-Z][a-z]{2}\s+\d+\s+\d\d:\d\d:\d\d\s+\d{4})\s+(.*)"
                                      line)]
                            {:pid (Long/parseLong pid)
                             :parent-pid (Long/parseLong ppid)
                             :session-id (Long/parseLong sid)
                             :process-group-id (Long/parseLong pgid)
                             :alive? true
                             :remote? true
                             :remote-host (str host)
                             :process-start started
                             :command-line args})))
                  vec)}
            {:ok? false :error :exit :exit exit :output output}))))
     (catch Throwable t
       {:ok? false :error :exception :message (.getMessage t)}))))

(defn- all-pids []
  (with-open [stream (ProcessHandle/allProcesses)]
    (->> (iterator-seq (.iterator stream))
         (map #(.pid ^ProcessHandle %))
         set)))

(defn- descendant-pids
  [root-pid]
  (if-let [^ProcessHandle root (some-> (ProcessHandle/of (long root-pid)) optional-value)]
    (with-open [stream (.descendants root)]
      (->> (iterator-seq (.iterator stream))
           (map #(.pid ^ProcessHandle %))
           set))
    #{}))

(defn- same-live-process?
  [{:keys [pid started-at]}]
  (when-let [current (process-info pid)]
    (and (:alive? current)
         (= started-at (:started-at current)))))

(defn- same-remote-process? [known current]
  (and (= (:pid known) (:pid current))
       (= (:process-start known) (:process-start current))))

(defn- command-preview [command]
  (let [s (-> (str command) (str/replace #"\s+" " ") str/trim)]
    (subs s 0 (min 160 (count s)))))

(def ^:private weak-command-tokens
  #{"bash" "sh" "nohup" "nice" "env" "usr" "bin" "dev" "null"
    "true" "false" "tmp" "home" "joe" "code" "ssh" "sshd" "setsid"
    "disown" "exec"})

(defn- command-tokens [command]
  (->> (re-seq #"[A-Za-z0-9_.-]+" (str command))
       (map str/lower-case)
       (remove weak-command-tokens)
       (filter #(>= (count %) 3))
       set))

(defn- command-match?
  [command info]
  (let [haystack (str/lower-case
                  (str (:command info) " " (:command-line info)))
        tokens (command-tokens command)]
    (and (seq tokens)
         (some #(str/includes? haystack %) tokens))))

(def ^:private launch-artifact-pattern
  #"(?i).+\.(?:bb|clj|cljc|cljs|jar|py|r|sh)$")

(defn- launch-identity-tokens
  "Return artifact names specific enough to attribute a remote launch.

   Remote shell commands often contain an entire here-doc. Treating ANY word
   in that source as a process signature is unsafe: `futon5`, `run`, or `work`
   can match an unrelated JVM or even a kernel worker. Script/artifact names
   survive into the durable process command line and form the narrow identity
   needed by the launch-window observer."
  [command]
  (->> (command-tokens command)
       (filter #(re-matches launch-artifact-pattern %))
       set))

(defn- user-session?
  "Kernel/system sessions are never agent-launched remote jobs. nil is kept
   for synthetic/test process records and older snapshots without SID."
  [{:keys [session-id]}]
  (or (nil? session-id) (> (long session-id) 1)))

(defn- identity-match? [identity-tokens info]
  (let [haystack (str/lower-case (str (:command-line info)))]
    (some #(str/includes? haystack %) identity-tokens)))

(defn- remote-candidates
  "Select remote processes born during the launch window.  A distinctive
   script/path token is the fallback for the narrow race where the remote PID
   appears while the baseline snapshot is in flight."
  [command baseline processes]
  (let [baseline-identities (set (map (juxt :pid :process-start) baseline))
        identity-tokens (launch-identity-tokens command)
        matches-launch? (if (seq identity-tokens)
                          #(identity-match? identity-tokens %)
                          #(command-match? command %))]
    (->> processes
         (filter user-session?)
         (filter matches-launch?)
         (filter (fn [proc]
                   (or (not (contains? baseline-identities
                                       [(:pid proc) (:process-start proc)]))
                       ;; Race recovery is deliberately narrower than ordinary
                       ;; birth-window matching. Only a launch artifact such as
                       ;; `e0_bridge.clj` may recover a process already visible
                       ;; in the asynchronous baseline; broad source words may
                       ;; not claim pre-existing sessions.
                       (and (seq identity-tokens)
                            (identity-match? identity-tokens proc)))))
         vec)))

(defn- remote-session-processes [session-ids processes]
  (->> processes
       (filter #(contains? session-ids (:session-id %)))
       vec))

(defn tree-snapshot
  "Pure snapshot of all active and retained terminal job atoms."
  []
  (->> @!jobs
       (map (fn [[id state]] (assoc @state :job/id id)))
       (sort-by (juxt :job/agent-id :job/started-at :job/id))
       vec))

(defn- elapsed-text [started-at ended-at]
  (when started-at
    (let [seconds (.getSeconds (Duration/between started-at (or ended-at (now))))]
      (if (< seconds 60)
        (str seconds "s")
        (format "%dm%02ds" (quot seconds 60) (mod seconds 60))))))

(defn format-tree
  "Render agent -> turn -> OS-job hierarchy."
  [jobs]
  (let [by-agent (group-by :job/agent-id jobs)
        lines
        (for [[agent agent-jobs] (sort-by key by-agent)
              :let [turn-groups (sort-by key (group-by :job/turn-id agent-jobs))]
              line
              (cons (str agent)
                    (mapcat
                     (fn [[turn turn-jobs]]
                       (cons
                        (str "  └─ " (or turn "<no-turn>"))
                        (map-indexed
                         (fn [idx job]
                           (let [last? (= idx (dec (count turn-jobs)))]
                             (str "     " (if last? "└─ " "├─ ")
                                  (name (:job/status job)) " "
                                  (or (:job/label job) (:job/id job))
                                  " [" (elapsed-text (:job/started-at job)
                                                     (:job/ended-at job)) "]"
                                  (when-let [host (:job/remote-host job)]
                                    (str " @" host))
                                  (when (seq (:job/pids job))
                                    (str " pid=" (str/join "," (:job/pids job)))))))
                         turn-jobs)))
                     turn-groups))]
          line)]
    (str "Agent job tree\n"
         (str/join (repeat 52 "─")) "\n"
         (if (seq jobs)
           (str/join "\n" lines)
           "(no captured background jobs)")
         "\n")))

(defn- known-emacs-sockets []
  (let [registry-var (requiring-resolve 'futon3c.agency.registry/!registry)
        registry @(var-get registry-var)]
    (->> registry
         (keep (fn [[_ agent]]
                 (or (get-in agent [:agent/metadata :emacs-socket])
                     (get-in agent [:metadata :emacs-socket]))))
         set
         (#(conj % nil)))))

(defn project!
  "Refresh `*tree*` on the service socket and every agent REPL socket.

   Resolve the registry atom lazily: registry -> agent-pouch -> job-tree is a
   valid load path, so a namespace-level registry dependency would introduce a
   cycle. `:async-key` gives this panel a coalescing lane independent of the
   high-frequency `*agents*` snapshot on the same socket."
  []
  (try
    (let [content (format-tree (tree-snapshot))
          sockets (known-emacs-sockets)]
      (doseq [socket sockets]
        (bb/blackboard! "*tree*" content
                        (cond-> {:width 72 :slot 1 :no-display true
                                 :async? true :async-key :job-tree}
                          socket (assoc :emacs-socket socket)))))
    (catch Throwable _ nil)))

(defn- update-job! [job-id f & args]
  (when-let [state (get @!jobs job-id)]
    (apply swap! state f args)
    (cyder/touch! job-id)
    (project!)
    @state))

(defn- terminate-local-process! [{:keys [pid started-at]}]
  (when-let [^ProcessHandle h (some-> (ProcessHandle/of (long pid)) optional-value)]
    (when (and (.isAlive h)
               (= started-at (some-> (.startInstant (.info h)) optional-value)))
      (with-open [stream (.descendants h)]
        (doseq [^ProcessHandle child (reverse (vec (iterator-seq (.iterator stream))))]
          (when (.isAlive child) (.destroyForcibly child))))
      (.destroyForcibly h))))

(defn- terminate-remote-process! [{:keys [remote-host pid] :as known}]
  (let [snapshot (remote-snapshot remote-host false)
        current (some #(when (same-remote-process? known %) %) (:processes snapshot))]
    (when current
      (try
        (let [proc (.start
                    (ProcessBuilder.
                     ^java.util.List
                     ["ssh" "-o" "BatchMode=yes" "-o" "ConnectTimeout=5"
                      remote-host "kill" "-TERM" (str pid)]))]
          (.waitFor proc 8 TimeUnit/SECONDS))
        (catch Throwable _ nil)))))

(defn- terminate-process! [proc]
  (if (:remote? proc)
    (terminate-remote-process! proc)
    (terminate-local-process! proc)))

(declare retain-terminal!)

(defn stop-job!
  "Stop one captured job by stable job id."
  [job-id]
  (when-let [state (get @!jobs job-id)]
    (doseq [proc (:job/processes @state)] (terminate-process! proc))
    (update-job! job-id assoc
                 :job/status :stopped :job/pids [] :job/ended-at (now))
    (retain-terminal! job-id)
    true))

(defn forget-job!
  "Remove one tracking record without signalling any captured process.

   This is the safe operations repair for a proven attribution false positive.
   `stop-job!` is intentionally different: it terminates the recorded process
   identities before retaining a terminal row."
  [job-id]
  (when (contains? @!jobs job-id)
    (swap! !jobs dissoc job-id)
    (cyder/deregister! job-id)
    (project!)
    true))

(defn- retain-terminal! [job-id]
  (.schedule retention-executor
             ^Runnable
             (fn []
               (swap! !jobs dissoc job-id)
               (cyder/deregister! job-id)
               (project!))
             terminal-retention-ms
             TimeUnit/MILLISECONDS))

(defn- watch-job! [job-id]
  (future
    (loop []
      (when-let [state (get @!jobs job-id)]
        (let [job @state
              remote-host (:job/remote-host job)
              remote-result (when remote-host
                              (remote-snapshot
                               remote-host
                               (not (seq (:job/remote-session-ids job)))))
              live (if remote-host
                     (when (:ok? remote-result)
                       (if (seq (:job/remote-session-ids job))
                         (remote-session-processes
                          (:job/remote-session-ids job)
                          (:processes remote-result))
                         (remote-candidates (:job/command job)
                                            (:job/remote-baseline job)
                                            (:processes remote-result))))
                     (filterv same-live-process? (:job/processes job)))
              unknown? (and remote-host (not (:ok? remote-result)))]
          (if (and (= :running (:job/status job))
                   (or unknown? (seq live)))
            (do
              ;; Heartbeats update the state/Cyder lease without projecting a
              ;; new Emacs snapshot every 500ms.  `*tree*` is refreshed at the
              ;; meaningful boundaries: discovery, stop, and completion.
              (swap! state
                     (fn [current]
                       ;; stop-job! may win while a remote `ps` is in flight.
                       ;; Never let that stale poll repopulate a terminal job.
                       (if (= :running (:job/status current))
                         (cond-> (assoc current
                                        :job/last-active (now)
                                        :job/remote-health
                                        (if unknown? :unreachable :reachable))
                           (seq live) (assoc :job/processes live
                                             :job/pids (mapv :pid live)))
                         current)))
              (cyder/touch! job-id)
              (Thread/sleep (if remote-host 2000 500))
              (recur))
            (when (= :running (:job/status job))
              (update-job! job-id assoc
                           :job/status :completed
                           :job/pids []
                           :job/ended-at (now))
              (retain-terminal! job-id))))))))

(defn- detached-command? [command]
  (boolean
   (or (re-find #"(?:^|[\s'\";&|])nohup(?:\s|$)" command)
       (re-find #"(?:^|[\s'\";&|])setsid(?:\s|$)" command)
       (re-find #"(?:^|[\s'\";&|])disown(?:\s|$)" command)
       ;; A shell background operator may be immediately followed by a quote
       ;; which closes an ssh payload.  Exclude redirects such as `2>&1`.
       (re-find #"(?<![>&])&(?![&0-9])" command))))

(defn- register-job!
  [{:keys [agent-id turn-id tool-use-id command captured baseline-pids
           remote-host remote-baseline]}]
  (let [ancestry-live (->> (vals @captured) (filter same-live-process?) vec)
        ancestry-pids (set (map :pid ancestry-live))
        ;; Global birth-window matching is only needed for explicitly detached
        ;; commands, whose child may reparent before the 20ms ancestry sample.
        ;; Applying it to ordinary foreground Bash calls can steal an unrelated
        ;; concurrent process with a generic token such as `curl`.
        detached? (detached-command? command)
        window-live (if (and detached? (nil? remote-host))
                      (->> (set/difference (all-pids) baseline-pids)
                           (keep process-info)
                           (filter same-live-process?)
                           (filter #(command-match? command %))
                           (remove #(contains? ancestry-pids (:pid %)))
                           vec)
                      [])
        baseline-result (when remote-host
                          (deref remote-baseline 8000
                                 {:ok? false :error :baseline-timeout}))
        remote-result (when remote-host (remote-snapshot remote-host))
        remote-live (when (and (:ok? baseline-result) (:ok? remote-result))
                      (remote-candidates command
                                         (:processes baseline-result)
                                         (:processes remote-result)))
        live (if (seq remote-live)
               remote-live
               (into ancestry-live window-live))
        attribution (cond
                      (seq remote-live) :remote-command-window
                      (seq ancestry-live) :observed-ancestry
                      (seq window-live) :command-window
                      :else nil)]
    (when (seq live)
      (let [job-id (str "job/" agent-id "/" tool-use-id)
            state (atom {:job/id job-id
                         :job/agent-id agent-id
                         :job/turn-id turn-id
                         :job/tool-use-id tool-use-id
                         :job/status :running
                         :job/attribution attribution
                         :job/label (command-preview command)
                         :job/command command
                         :job/processes live
                         :job/pids (mapv :pid live)
                         :job/started-at (now)
                         :job/last-active (now)})]
        (when (seq remote-live)
          (swap! state assoc
                 :job/remote-host remote-host
                 :job/remote-baseline (:processes baseline-result)
                 :job/remote-session-ids (set (map :session-id remote-live))
                 :job/remote-health :reachable))
        (swap! !jobs assoc job-id state)
        (cyder/deregister! job-id)
        (cyder/register!
         {:id job-id
          :type :background-job
          :layer :infra
          :stop-fn #(stop-job! job-id)
          :state-fn #(dissoc @state :job/processes)
          :metadata {:parent-agent agent-id
                     :parent-turn turn-id
                     :tool-use-id tool-use-id}})
        (project!)
        (watch-job! job-id)
        job-id))))

(defn- start-probe!
  [{:keys [agent-id turn-id root-pid tool-use-id command]}]
  (when (and root-pid tool-use-id (not (str/blank? command)))
    (let [detached? (detached-command? command)
          remote-host (when detached? (ssh-target command))
          baseline (descendant-pids root-pid)
          probe {:agent-id (str agent-id)
                 :turn-id (some-> turn-id str)
                 :root-pid (long root-pid)
                 :tool-use-id (str tool-use-id)
                 :command command
                 :baseline baseline
                 :baseline-pids (all-pids)
                 :remote-host remote-host
                 :remote-baseline (when remote-host
                                    (future (remote-snapshot remote-host)))
                 :captured (atom {})
                 :result-seen? (atom false)
                 :started-ms (System/currentTimeMillis)}]
      (swap! !probes assoc (str tool-use-id) probe)
      (future
        (loop []
          (when-let [current (get @!probes (str tool-use-id))]
            (let [new-pids (set/difference (descendant-pids root-pid) baseline)]
              (doseq [pid new-pids]
                (when-let [info (process-info pid)]
                  (swap! (:captured current) assoc pid info)))
              (if (or (and @(:result-seen? current)
                           (> (- (System/currentTimeMillis) (:result-ms current 0))
                              post-result-grace-ms))
                      (> (- (System/currentTimeMillis) (:started-ms current))
                         discovery-ceiling-ms))
                (do
                  (swap! !probes dissoc (str tool-use-id))
                  (register-job! current))
                (do (Thread/sleep sample-ms) (recur)))))))
      probe)))

(defn- finish-probe! [tool-use-id]
  (when-let [probe (get @!probes (str tool-use-id))]
    (reset! (:result-seen? probe) true)
    (swap! !probes update (str tool-use-id) assoc
           :result-ms (System/currentTimeMillis))
    true))

(defn- bash-tool-blocks [event]
  (when (= "assistant" (:type event))
    (->> (get-in event [:message :content])
         (filter #(and (= "tool_use" (:type %))
                       (= "Bash" (:name %)))))))

(defn- tool-result-ids [event]
  (when (= "user" (:type event))
    (->> (get-in event [:message :content])
         (filter #(= "tool_result" (:type %)))
         (keep :tool_use_id))))

(defn observe-event!
  "Observe one raw Claude stream event.

   ROOT-PID is the warm pouch or cold invoke PID. Tool-use arms discovery;
   tool-result closes the foreground interval. Only processes still alive after
   that boundary become background jobs."
  [{:keys [agent-id turn-id root-pid event]}]
  (doseq [block (bash-tool-blocks event)]
    (start-probe! {:agent-id agent-id
                   :turn-id (or (:turn-id event) turn-id)
                   :root-pid root-pid
                   :tool-use-id (:id block)
                   :command (or (get-in block [:input :command]) "")}))
  (doseq [tool-use-id (tool-result-ids event)]
    (finish-probe! tool-use-id))
  nil)

(defn clear!
  "Test/operations cleanup for captured jobs and launch probes."
  []
  (doseq [job-id (keys @!jobs)]
    (stop-job! job-id)
    (cyder/deregister! job-id))
  (reset! !jobs {})
  (reset! !probes {})
  (project!)
  true)

(defn start!
  "Initialise the live projection. Event observation itself is call-driven."
  []
  (when (or (nil? @!ticker)
            (.isCancelled ^java.util.concurrent.ScheduledFuture @!ticker)
            (.isDone ^java.util.concurrent.ScheduledFuture @!ticker))
    (reset! !ticker
            (.scheduleAtFixedRate
             retention-executor
             ^Runnable
             (fn []
               (when (some (fn [state] (= :running (:job/status @state)))
                           (vals @!jobs))
                 (project!)))
             1 1 TimeUnit/SECONDS)))
  (project!)
  {:running true :live-refresh-ms 1000 :jobs (count @!jobs)} )
