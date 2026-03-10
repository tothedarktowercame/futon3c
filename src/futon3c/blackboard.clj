(ns futon3c.blackboard
  "Blackboard — project structured state to Emacs via emacsclient.

   The blackboard gives peripherals a voice: while the agent works inside
   a constrained envelope, the blackboard projects live state onto the
   user's Emacs, making the agent's progress visible without breaking
   the peripheral boundary.

   Two layers:
   1. blackboard! — the primitive. Writes content to a named Emacs buffer
      via emacsclient --eval. Fire-and-forget; failures are logged, not thrown.
   2. render-blackboard — multimethod dispatched on peripheral-id. Each
      peripheral provides an adaptor that maps its internal state to a
      display string. Returns nil to skip projection.

   Blackboard entries can also be recorded as evidence (tagged :blackboard)
   for the evidence landscape — a bit like Clerk notebooks containing
   the results of evaluation.

   Design: emacsclient is the transport. No protocol to design, no
   endpoint to build. Emacs IS the sliding blackboard."
  (:require [clojure.string :as str]
            [futon3c.evidence.store :as estore])
  (:import [java.util UUID]))

(declare emit-blackboard-evidence!)

;; =============================================================================
;; Enable/disable — bind *enabled* to false in tests or non-interactive contexts
;; =============================================================================

(def ^:dynamic *enabled*
  "When false, project! is a no-op. Defaults to true.
   Bind to false in tests to avoid spawning emacsclient processes."
  true)

(defn- detect-emacs-socket
  "Auto-detect Emacs server socket from the process tree.
   Finds emacsclient processes with -s <socket>, sorted by PID (oldest first).
   If in a tmux session, prefers sockets from emacsclient siblings in the
   same session. Returns the first socket found, or nil."
  []
  (try
    (let [;; Find all emacsclient processes with -s arg, sorted by PID
          proc (-> (ProcessBuilder. ["pgrep" "-a" "emacsclient"])
                   (.redirectErrorStream true)
                   .start)
          output (slurp (.getInputStream proc))
          parsed (->> (str/split-lines output)
                      (keep (fn [line]
                              (when-let [[_ pid] (re-find #"^(\d+)\s" line)]
                                (when-let [[_ socket] (re-find #"-s\s+(\S+)" line)]
                                  {:pid (parse-long pid) :socket socket}))))
                      (sort-by :pid))
          ;; Try to narrow to our tmux session
          session-pids (try
                         (let [p (-> (ProcessBuilder.
                                       ["tmux" "list-panes" "-s" "-F" "#{pane_pid}"])
                                     (.redirectErrorStream true)
                                     .start)
                               out (str/trim (slurp (.getInputStream p)))]
                           (when (zero? (.waitFor p))
                             (set (map parse-long (str/split-lines out)))))
                         (catch Exception _ nil))
          ;; Filter to emacsclient whose PPID is a session pane
          in-session (when session-pids
                       (->> parsed
                            (filter (fn [{:keys [pid]}]
                                      (try
                                        (let [ppid (-> (str "/proc/" pid "/status")
                                                       slurp
                                                       (->> (re-find #"PPid:\s+(\d+)"))
                                                       second
                                                       parse-long)]
                                          (contains? session-pids ppid))
                                        (catch Exception _ false))))))]
      ;; Prefer session-scoped, fall back to all
      (-> (if (seq in-session) in-session parsed)
          first
          :socket))
    (catch Exception _ nil)))

(def !emacs-socket
  "Emacs server socket name. Auto-detected from process tree at load time,
   or set at runtime: (reset! bb/!emacs-socket \"workspace1\").
   Falls back to FUTON3C_EMACS_SOCKET or EMACS_SOCKET_NAME env vars."
  (atom (or (System/getenv "FUTON3C_EMACS_SOCKET")
            (System/getenv "EMACS_SOCKET_NAME")
            (detect-emacs-socket))))

;; =============================================================================
;; Emacsclient primitive
;; =============================================================================

(defn- escape-elisp-string
  "Escape a string for embedding in an elisp double-quoted string."
  [s]
  (-> s
      (str/replace "\\" "\\\\")
      (str/replace "\"" "\\\"")
      (str/replace "\n" "\\n")))

(defn- run-emacsclient!
  "Run emacsclient --eval with the given elisp. Returns {:ok bool :output str}.
   Non-blocking: uses a short timeout. Failures are swallowed (the blackboard
   is best-effort, never blocks the agent).
   Optional socket-override targets a specific Emacs daemon (e.g. \"workspace2\")."
  ([elisp] (run-emacsclient! elisp nil))
  ([elisp socket-override]
  (try
    (let [socket (or socket-override
                     @!emacs-socket
                     (System/getenv "FUTON3C_EMACS_SOCKET")
                     (System/getenv "EMACS_SOCKET_NAME"))
          cmd (if socket
                ["emacsclient" "-s" socket "--eval" elisp]
                ["emacsclient" "--eval" elisp])
          pb (ProcessBuilder. cmd)
          _ (.redirectErrorStream pb true)
          proc (.start pb)
          finished? (.waitFor proc 2 java.util.concurrent.TimeUnit/SECONDS)]
      (if finished?
        {:ok (zero? (.exitValue proc))
         :output (slurp (.getInputStream proc))}
        (do (.destroyForcibly proc)
            {:ok false :output "timeout"})))
    (catch Exception e
      {:ok false :output (.getMessage e)}))))

(defn blackboard!
  "Project content to a named Emacs buffer.

   buffer-name: e.g. \"*Blackboard*\" or \"*Mission: M-evidence-parity*\"
   content: string to display (plain text or fontified via elisp)

   Options:
   :side    — which side for the window (:right :bottom :left), default :right
   :width   — window width (for :left/:right), default 50
   :height  — window height (for :bottom), default 15
   :slot    — side-window slot index (keeps multiple panels stable)
   :no-display — if true, update buffer but don't force-display it
   :emacs-socket — target a specific Emacs daemon socket (e.g. \"workspace2\")

   Returns {:ok bool :output str}."
  ([buffer-name content]
   (blackboard! buffer-name content {}))
  ([buffer-name content opts]
   (let [escaped (escape-elisp-string content)
         buf-escaped (escape-elisp-string buffer-name)
         side (or (:side opts) 'right)
         width (or (:width opts) 50)
         height (or (:height opts) 15)
         slot (if (contains? opts :slot)
                (:slot opts)
                (cond
                  (= buffer-name "*agents*") 0
                  (str/starts-with? buffer-name "*invoke:") 1
                  :else nil))
         display? (not (:no-display opts))
         display-form (when display?
                        (let [slot-form (when (some? slot)
                                          (format " (slot . %s)" slot))]
                          (if (#{:bottom} side)
                            (format "(display-buffer-in-side-window buf '((side . bottom) (window-height . %d)%s))"
                                    height
                                    (or slot-form ""))
                            (format "(display-buffer-in-side-window buf '((side . %s) (window-width . %d)%s))"
                                    (name side)
                                    width
                                    (or slot-form "")))))
         post-elisp (:post-elisp opts)
         elisp (str "(let ((buf (get-buffer-create \"" buf-escaped "\")))"
                    "(with-current-buffer buf"
                    "(let ((inhibit-read-only t))"
                    "(erase-buffer)"
                    "(insert \"" escaped "\")"
                    "(goto-char (point-min))"
                    (or post-elisp "")
                    "))"
                    (or display-form "")
                    "nil)")]
     (run-emacsclient! elisp (:emacs-socket opts)))))

(defn blackboard-eval!
  "Run arbitrary elisp via emacsclient. For cases where text content
   isn't enough — e.g. opening magit, finding a file, arranging windows.

   Returns {:ok bool :output str}."
  ([elisp]
   (run-emacsclient! elisp))
  ([elisp opts]
   (run-emacsclient! elisp (:emacs-socket opts))))

;; =============================================================================
;; Per-peripheral render adaptors
;; =============================================================================

(defmulti render-blackboard
  "Render peripheral state to a blackboard string.

   Dispatched on peripheral-id (keyword). Each peripheral knows what's
   interesting about its own state. Returns a string, or nil to skip.

   The default implementation returns nil (not all peripherals need one)."
  (fn [peripheral-id _state] peripheral-id))

(defmethod render-blackboard :default [_ _] nil)

;; -----------------------------------------------------------------------------
;; :mission — obligation DAG, current phase, cycle progress
;; -----------------------------------------------------------------------------

(defn- format-obligation
  "Format a single obligation for blackboard display."
  [[id item]]
  (let [status (or (:item/status item) :unknown)
        label (or (:item/label item) (name id))
        status-char (case status
                      :done "+"
                      :partial "~"
                      :open " "
                      :blocked "!"
                      :abandoned "x"
                      "?")]
    (format "  [%s] %-12s %s" status-char (name status) label)))

(defn- format-mission-state
  "Format mission peripheral state for blackboard."
  [state]
  (let [mid (or (:mission-id state) "unknown")
        phase (or (:current-phase state) "setup")
        cycles (:cycles-completed state 0)
        steps (count (:steps state))
        ;; Try to get obligation summary from the latest step results
        latest-obligations (some (fn [{:keys [tool result]}]
                                   (when (and (= tool :mission-load)
                                              (:mission/obligations result))
                                     (:mission/obligations result)))
                                 (reverse (:steps state)))]
    (str "Mission: " mid "\n"
         "Phase: " (name phase) "  Cycles: " cycles "  Steps: " steps "\n"
         (when latest-obligations
           (str "\nObligations:\n"
                (str/join "\n" (map format-obligation latest-obligations))
                "\n")))))

(defmethod render-blackboard :mission [_ state]
  (format-mission-state state))

;; -----------------------------------------------------------------------------
;; :mission-control — portfolio view
;; -----------------------------------------------------------------------------

(defn- format-mission-control-state
  "Format mission-control peripheral state for blackboard."
  [state]
  (let [steps (count (:steps state))
        review (:latest-review state)
        missions (when review (:portfolio/missions review))
        summary (when review (:portfolio/summary review))
        ;; Count by status — missions may be a map {id -> entry} or a seq of entries
        entries (when missions
                  (if (map? missions) (vals missions) (seq missions)))
        by-status (when entries
                    (frequencies (map :mission/status entries)))
        complete (get by-status :complete 0)
        blocked (get by-status :blocked 0)
        in-progress (get by-status :in-progress 0)
        total (if missions (count missions) 0)]
    (str "Mission Control\n"
         "Steps: " steps "\n"
         (when missions
           (str "\nMissions: " total
                " (" complete " complete"
                (when (pos? in-progress) (str ", " in-progress " in-progress"))
                (when (pos? blocked) (str ", " blocked " blocked"))
                ")\n"))
         (when summary
           (str "\n" summary "\n")))))

(defmethod render-blackboard :mission-control [_ state]
  (format-mission-control-state state))

;; -----------------------------------------------------------------------------
;; :alfworld — game state, coordination events
;; -----------------------------------------------------------------------------

(defn- format-alfworld-state
  "Format ALFWorld peripheral state for blackboard."
  [state]
  (let [alf-state (:alfworld-state state)
        step-count (:step-count state 0)
        bells (count (:bells-sent state))
        whistles (count (:whistles-sent state))
        last-obs (or (:last-observation state)
                     (:observation alf-state)
                     "none")
        task-desc (or (:task-description state)
                      (:task alf-state)
                      "unknown")
        score (:score alf-state)
        done? (:done alf-state)
        won? (:won alf-state)]
    (str "ALFWorld\n"
         "Task: " task-desc "\n"
         "Steps: " step-count
         (when score (str "  Score: " score))
         (when done? (str "  " (if won? "WON" "LOST")))
         "\n"
         "Last: " (if (> (count (str last-obs)) 60)
                    (str (subs (str last-obs) 0 57) "...")
                    last-obs) "\n"
         (when (pos? bells)
           (str "Bells: " bells "\n"))
         (when (pos? whistles)
           (str "Whistles: " whistles "\n")))))

(defmethod render-blackboard :alfworld [_ state]
  (format-alfworld-state state))

;; -----------------------------------------------------------------------------
;; :proof — problem state, ledger summary, mode, cycle progress
;; -----------------------------------------------------------------------------

(defn- format-ledger-summary
  "Summarize ledger items by status."
  [steps]
  (let [ledger-result (some (fn [{:keys [tool result]}]
                              (when (and (= tool :ledger-query)
                                         (map? result)
                                         (:items result))
                                (:items result)))
                            (reverse steps))
        items (when ledger-result
                (if (map? ledger-result) (vals ledger-result) ledger-result))
        by-status (when (seq items)
                    (frequencies (map :item/status items)))]
    (when by-status
      (let [total (reduce + (vals by-status))]
        (str total " items: "
             (str/join ", "
                       (keep (fn [[status n]]
                               (when (pos? n) (str n " " (name status))))
                             by-status)))))))

(defn- format-proof-state
  "Format proof peripheral state for blackboard."
  [state]
  (let [problem-id (or (:problem-id state) "unknown")
        phase (or (:current-phase state) "setup")
        cycles (:cycles-completed state 0)
        steps (count (:steps state))
        mode (or (:proof/current-mode state) "SPEC")
        ;; Current cycle info
        current-cycle (some (fn [{:keys [tool result]}]
                              (when (and (= tool :cycle-get) (map? result))
                                result))
                            (reverse (:steps state)))
        blocker (:cycle/blocker-id current-cycle)
        ;; Ledger summary
        ledger-str (format-ledger-summary (:steps state))
        ;; TryHarder
        license (:proof/active-license state)
        falsify? (:proof/falsify-completed? state)]
    (str "Proof: " problem-id "\n"
         "Mode: " (name mode)
         (when falsify? " [FALSIFY done]")
         "  Phase: " (name phase)
         "  Cycles: " cycles "  Steps: " steps "\n"
         (when blocker
           (str "Blocker: " blocker "\n"))
         (when ledger-str
           (str "Ledger: " ledger-str "\n"))
         (when license
           (str "TryHarder: " (:license/id license)
                " target=" (:license/target-claim license)
                " timebox=" (:license/timebox-minutes license) "min\n")))))

(defmethod render-blackboard :proof [_ state]
  (format-proof-state state))

;; -----------------------------------------------------------------------------
;; :mentor — observation state, trigger status, interventions
;; -----------------------------------------------------------------------------

(defn- format-mentor-state
  "Format mentor peripheral state for blackboard.
   State shape: top-level has :cmap (conversation map), :handle, :problem-id,
   :channel, :interventions, :steps. The cmap holds :map/messages-seen,
   :map/topics, :map/effort, :map/digest, :triggers/fired, :map/last-seen-at."
  [state]
  (let [cmap (or (:cmap state) {})
        seen (get cmap :map/messages-seen 0)
        topics (get cmap :map/topics [])
        effort (get cmap :map/effort {})
        digest (get cmap :map/digest [])
        interventions (or (:interventions cmap) (:interventions state) [])
        int-count (count interventions)
        fired (or (:triggers/fired cmap) #{})
        last-seen (get cmap :map/last-seen-at)
        problem-id (or (:problem-id state)
                       (:mentor/problem cmap) "none")
        channel (or (:channel state)
                    (:mentor/channel cmap) "#math")
        handle (or (:handle state) (:mentor/handle cmap))
        version (get cmap :mentor/version 0)
        recent-digest (take-last 3 digest)
        recent-interventions (take-last 3 interventions)
        ;; Format last-seen as relative time
        last-seen-rel (when last-seen
                        (try
                          (let [then (.toEpochMilli (java.time.Instant/parse last-seen))
                                secs (quot (- (System/currentTimeMillis) then) 1000)]
                            (cond
                              (< secs 60) (str secs "s ago")
                              (< secs 3600) (str (quot secs 60) "m ago")
                              :else (str (quot secs 3600) "h ago")))
                          (catch Exception _ last-seen)))]
    (str "Mentor"
         (when handle (str " — " handle))
         "\n"
         (str/join (repeat 40 "─")) "\n"
         "Problem: " problem-id
         "  Channel: " channel "\n"
         "Seen: " seen " msgs"
         "  Map v" version "\n"
         (when last-seen-rel
           (str "Last msg: " last-seen-rel "\n"))
         "\n"
         ;; Topics
         (when (seq topics)
           (str "Topics (" (count topics) "):\n"
                (str/join "\n"
                  (map (fn [{:keys [id status by]}]
                         (str "  " (name id) " [" (name (or status :unknown)) "] by " by))
                       topics))
                "\n\n"))
         ;; Effort distribution
         (when (seq effort)
           (str "Effort:\n"
                (str/join "\n"
                  (map (fn [[agent topics]]
                         (let [topic-names (if (set? topics)
                                            (map name topics)
                                            (map name (vec topics)))]
                           (str "  " agent " (" (count topic-names) "): "
                                (str/join ", " (take 4 (sort topic-names)))
                                (when (> (count topic-names) 4) "..."))))
                       (sort-by (comp count val) > effort)))
                "\n\n"))
         ;; Triggers
         "Triggers: " (if (seq fired)
                        (str (count fired) " fired — " (str/join ", " (map name fired)))
                        "none fired")
         "\n"
         ;; Interventions
         "Interventions: " int-count
         (when (seq recent-interventions)
           (str "\n"
                (str/join "\n"
                  (map (fn [{:keys [trigger-id message]}]
                         (str "  [" (name trigger-id) "] "
                              (subs (str message) 0 (min 60 (count (str message))))
                              (when (> (count (str message)) 60) "...")))
                       recent-interventions))))
         "\n"
         ;; Task queue (from tickle-queue, resolved to avoid cyclic dep)
         (try
           (when-let [fmt-fn (resolve 'futon3c.agents.tickle-queue/format-queue)]
             (let [q-str (fmt-fn)]
               (when (and q-str (not (str/blank? q-str)))
                 (str "\n" q-str "\n"))))
           (catch Exception e
             (println "[blackboard] tickle-queue format-queue error:" (.getMessage e))
             nil))
         ;; Recent digest (last 3 conversation entries)
         (when (seq recent-digest)
           (str "\n" (str/join (repeat 40 "─")) "\n"
                "Recent:\n"
                (str/join "\n"
                  (map (fn [{:keys [speaker summary]}]
                         (let [preview (subs (str summary) 0
                                            (min 70 (count (str summary))))]
                           (str "  <" speaker "> " preview
                                (when (> (count (str summary)) 70) "..."))))
                       recent-digest))
                "\n")))))

(defmethod render-blackboard :mentor [_ state]
  (format-mentor-state state))

(defn project-mentor!
  "Project mentor state to the *mentor* blackboard buffer.
   Uses slot 2 (after *agents* slot 0, *invoke* slot 1).
   Reads emacs-socket from state :emacs-socket key, or falls back to
   the agent's registered socket in the registry."
  [state]
  (when *enabled*
    (try
      (when-let [content (render-blackboard :mentor state)]
        (let [socket (or (:emacs-socket state)
                         (try
                           (get-in @(resolve 'futon3c.agency.registry/!registry)
                                   [(:author state) :agent/metadata :emacs-socket])
                           (catch Exception _ nil)))]
          (blackboard! "*mentor*" content
                       (cond-> {:width 55 :slot 2}
                         socket (assoc :emacs-socket socket)))
          (emit-blackboard-evidence! :mentor state content))
        nil)
      (catch Throwable _ nil))))

;; -----------------------------------------------------------------------------
;; :arse — ArSE corpus query session
;; -----------------------------------------------------------------------------

(defn- format-arse-state
  "Format ArSE peripheral state for blackboard."
  [state]
  (let [queries (:queries state)
        q-count (count queries)
        problem-id (or (:problem-id state) "—")]
    (str "ArSE — Artificial Stack Exchange\n"
         "Problem: " problem-id
         "  Queries: " q-count "\n"
         (when (seq queries)
           (str "\nRecent queries:\n"
                (str/join "\n"
                  (map (fn [{:keys [query result-count sources]}]
                         (str "  \"" (subs query 0 (min 60 (count query)))
                              (when (> (count query) 60) "...")
                              "\" → " result-count " results"
                              (when sources (str " [" (str/join "," (map name sources)) "]"))))
                       (take-last 5 queries)))))
         "\n")))

(defmethod render-blackboard :arse [_ state]
  (format-arse-state state))

;; -----------------------------------------------------------------------------
;; :tickle — watchdog scan results, stall/page history
;; -----------------------------------------------------------------------------

(defn- format-tickle-conductor
  "Format FM conductor state for the tickle blackboard."
  [conductor]
  (when conductor
    (let [now-ms (System/currentTimeMillis)
          agents (:agents conductor)
          cycles (:cycles conductor 0)
          step-ms (:step-ms conductor)
          last-cycle (:last-cycle conductor)
          problem-id (:problem-id conductor "FM-001")
          rotation (:rotation conductor)
          cooldowns (:cooldowns conductor)
          idx (:idx conductor 0)
          next-agent (when (seq rotation)
                       (nth rotation (mod idx (count rotation))))]
      (str "FM Conductor — " problem-id "\n"
           (str/join (repeat 40 "─")) "\n"
           "Cycles: " cycles
           (when step-ms (str "  Step: " step-ms))
           "\n"
           (if-let [next-at (:next-at conductor)]
             (str "Next:   " next-at "\n")
             "")
           "\n"
           "Agents:\n"
           (if (seq agents)
             (str/join "\n"
               (map (fn [line] (str "  " line)) agents))
             (str/join "\n"
               (map (fn [a]
                      (let [marker (if (= a next-agent) "▶ " "  ")
                            cd-ms (get cooldowns a)
                            cd-text (if cd-ms
                                      (let [ago-s (quot (- now-ms cd-ms) 1000)]
                                        (str "(paged " ago-s "s ago)"))
                                      "ready")]
                        (str "  " marker a " → " cd-text)))
                    rotation)))
           "\n"
           (when last-cycle
             (str "\nLast: " (:target last-cycle) " "
                  (name (or (:action last-cycle) :unknown))
                  (when-let [text (:text last-cycle)]
                    (str "\n  \"" (subs text 0 (min 70 (count text)))
                         (when (> (count text) 70) "...") "\""))
                  (when (:at last-cycle)
                    (let [at-str (str (:at last-cycle))]
                      (str "\n  at " (if (> (count at-str) 19) (subs at-str 11 19) at-str))))
                  "\n"))))))

(defn- format-tickle-watchdog
  "Format tickle watchdog state for blackboard."
  [state]
  (let [cycles (:cycles-completed state 0)
        last-cycle (:last-cycle state)
        history (:recent-history state)
        interval-ms (:interval-ms state)
        threshold-s (:threshold-seconds state)]
    (str "Watchdog\n"
         (str/join (repeat 40 "─")) "\n"
         "Cycles: " cycles
         (when interval-ms (str "  Interval: " (quot interval-ms 1000) "s"))
         (when threshold-s (str "  Threshold: " threshold-s "s"))
         "\n"
         (when last-cycle
           (str "\nLast scan: " (:at last-cycle) "\n"
                "  Scanned: " (:scanned last-cycle 0)
                "  Stalled: " (count (:stalled last-cycle))
                "  Paged: " (count (:paged last-cycle))
                "  Escalated: " (count (:escalated last-cycle))
                "\n"
                (when (seq (:stalled last-cycle))
                  (str "  Stalled agents: " (str/join ", " (:stalled last-cycle)) "\n"))))
         (when (seq history)
           (str "\nRecent (" (count history) " cycles):\n"
                (str/join "\n"
                  (map (fn [{:keys [at stalled paged]}]
                         (str "  " at
                              (if (seq stalled)
                                (str " STALL [" (str/join "," stalled) "]"
                                     (when (seq paged) (str " paged:" (count paged))))
                                " ok")))
                       (reverse history)))
                "\n")))))

(defn- format-tickle-state
  "Format combined tickle state for blackboard.
   State may contain :conductor (FM conductor) and/or :watchdog sections."
  [state]
  (let [conductor (:conductor state)
        watchdog (:watchdog state)
        ;; Backwards compat: if state has :cycles-completed at top level, it's pure watchdog
        pure-watchdog? (and (not conductor) (:cycles-completed state))]
    (str "Tickle\n"
         (str/join (repeat 40 "═")) "\n\n"
         (if pure-watchdog?
           (format-tickle-watchdog state)
           (str (when conductor
                  (str (format-tickle-conductor conductor) "\n"))
                (when watchdog
                  (str (format-tickle-watchdog watchdog) "\n"))
                (when (and (not conductor) (not watchdog) (not pure-watchdog?))
                  "  (no active tickle processes)\n"))))))

(defmethod render-blackboard :tickle [_ state]
  (format-tickle-state state))

;; -----------------------------------------------------------------------------
;; :agents — registered agent status overview
;; -----------------------------------------------------------------------------

(defn- format-elapsed-secs
  "Format seconds as human-readable elapsed time."
  [secs]
  (cond
    (>= secs 3600) (str (quot secs 3600) "h" (mod (quot secs 60) 60) "m")
    (>= secs 60) (str (quot secs 60) "m" (mod secs 60) "s")
    :else (str secs "s")))

(defn- format-relative-time
  "Format an ISO instant string as relative time from now."
  [iso-str now-ms]
  (try
    (let [then (.toEpochMilli (java.time.Instant/parse iso-str))
          secs (quot (- now-ms then) 1000)]
      (cond
        (neg? secs) "just now"
        (< secs 60) (str secs "s ago")
        (< secs 3600) (str (quot secs 60) "m ago")
        (< secs 86400) (str (quot secs 3600) "h ago")
        :else (str (quot secs 86400) "d ago")))
    (catch Exception _ nil)))

(defn format-agent-status
  "Format agent registry status for blackboard display.
   Takes the registry-status map from registry/registry-status."
  [registry-status]
  (let [agents (:agents registry-status)
        ws-connected (vec (or (:ws-connected registry-status) []))
        ws-unregistered (vec (or (:ws-unregistered registry-status) []))
        now-ms (System/currentTimeMillis)]
    (str "Agents (" (:count registry-status) " registered"
         (when (seq ws-connected)
           (str ", " (count ws-connected) " ws-connected"))
         ")\n"
         (str/join "\n"
                   (map (fn [[aid info]]
                          (let [status (or (:status info) :idle)
                                type-str (some-> (:type info) name)
                                metadata (:metadata info)
                                remote? (:remote? metadata)
                                queued-jobs (long (or (:queued-jobs info) 0))
                                elapsed (when (and (= status :invoking)
                                                   (:invoke-started-at info))
                                          (try
                                            (let [started (.toEpochMilli
                                                            (java.time.Instant/parse
                                                              (:invoke-started-at info)))
                                                  secs (quot (- now-ms started) 1000)]
                                              (format-elapsed-secs secs))
                                            (catch Exception _ nil)))
                                activity (:invoke-activity info)
                                last-active-str (format-relative-time
                                                  (:last-active info) now-ms)]
                            (str "  " (name aid) " [" type-str
                                 (when remote? " remote")
                                 "] "
                                 (case status
                                   :invoking
                                   (str "INVOKING"
                                        (when elapsed (str " (" elapsed ")"))
                                        (when (pos? queued-jobs)
                                          (str " [" queued-jobs " queued]"))
                                        (when activity
                                          (str "\n    " activity))
                                        "\n    "
                                        (:invoke-prompt-preview info))
                                   :idle
                                   (str "idle"
                                        (when (or (pos? queued-jobs) last-active-str)
                                          (str " ("
                                               (str/join
                                                ", "
                                                (remove nil?
                                                        [(when (pos? queued-jobs)
                                                           (str queued-jobs " queued"))
                                                         last-active-str]))
                                               ")")))
                                   (name status)))))
                        (sort-by key agents)))
         (when (seq ws-unregistered)
           (str "\n\nWS Connected (Unregistered):\n"
                (str/join "\n" (map #(str "  " %) ws-unregistered))))
         "\n")))

(defn project-agents!
  "Project agent registry status to the *agents* blackboard buffer.
   Sends to ALL known Emacs sockets (extracted from agent metadata)
   so every workspace sees the full agent roster."
  [registry-status]
  (when *enabled*
    (try
      (let [content (format-agent-status registry-status)
            opts {:width 60 :slot 0}
            ;; Collect distinct sockets from agent metadata + default
            sockets (->> (:agents registry-status)
                         (keep (fn [[_ info]] (get-in info [:metadata :emacs-socket])))
                         (into #{})
                         (#(conj % nil)))]  ;; nil = default socket
        (doseq [socket sockets]
          (blackboard! "*agents*" content
                       (if socket (assoc opts :emacs-socket socket) opts))))
      (catch Throwable _ nil))))

;; -----------------------------------------------------------------------------
;; :processes — CYDER process registry overview
;; -----------------------------------------------------------------------------

(defn- format-state-lines
  "Format a state map as indented key-value lines for the process buffer.
   Handles vectors (one item per sub-line) and scalars."
  [state-map indent]
  (when (and state-map (map? state-map))
    (->> (dissoc state-map :phase)
         (mapcat (fn [[k v]]
                   (cond
                     (and (sequential? v) (seq v))
                     (into [(str indent (name k) ":")]
                           (map #(str indent "  " %) v))
                     (some? v)
                     [(str indent (name k) ": " v)]
                     :else nil)))
         (str/join "\n"))))

(def ^:private expected-daemons
  "Process IDs that should normally be running. Shows ✖ DOWN when absent."
  #{"fm-conductor" "tickle-watchdog"})

(defn format-process-status
  "Format CYDER process registry for blackboard display.
   Separates running daemons/peripherals (with live state) from
   infrastructure and mission docs. Shows ✖ DOWN for expected processes
   that are missing."
  [registry-entries]
  (let [now-ms (System/currentTimeMillis)
        by-type (group-by :process/type registry-entries)
        ;; Running processes: daemons, peripherals (not state-machines which are mission docs)
        running (concat (get by-type :daemon)
                        (get by-type :peripheral))
        running-ids (set (map :process/id running))
        missing (remove running-ids expected-daemons)
        infra (get (group-by :process/layer registry-entries) :infra)
        missions (get by-type :state-machine)]
    (str "Processes\n"
         (str/join (repeat 40 "─")) "\n"
         ;; Missing expected processes (red warning)
         (when (seq missing)
           (str (str/join "\n"
                  (map #(str "  ✖ " % " DOWN") (sort missing)))
                "\n\n"))
         ;; Running daemons/peripherals with live state
         (if (seq running)
           (str/join "\n\n"
             (map (fn [p]
                    (let [state (when-let [sf (:process/state-fn p)]
                                  (try (sf) (catch Exception _ nil)))
                          last-active (format-relative-time
                                        (str (:process/last-active p))
                                        now-ms)
                          phase (or (:phase state)
                                    (get-in p [:process/metadata :phase]))
                          state-text (format-state-lines state "    ")]
                      (str "  ● " (:process/id p)
                           " [" (name (:process/type p)) "]"
                           (when last-active (str " (" last-active ")"))
                           (when phase (str "\n    phase: " phase))
                           (when (and state-text (not (str/blank? state-text)))
                             (str "\n" state-text)))))
                  running))
           "  (no running daemons)")
         "\n"
         ;; Infrastructure
         (when (seq infra)
           (str "\n" (str/join (repeat 40 "─")) "\n"
                "Infrastructure (" (count infra) ")\n"
                (str/join "\n"
                  (map (fn [p]
                         (let [last-active (format-relative-time
                                            (str (:process/last-active p))
                                            now-ms)]
                           (str "  " (:process/id p)
                                " [" (name (:process/type p)) "]"
                                (when last-active (str " (" last-active ")")))))
                       infra))
                "\n"))
         ;; Missions — just a count, don't clutter
         (when (seq missions)
           (let [active (filter (fn [m]
                                  (let [phase (get-in m [:process/metadata :phase] "")]
                                    (not (re-find #"(?i)done|complete|closed" phase))))
                                missions)]
             (str "\nMissions: " (count active) " active, "
                  (- (count missions) (count active)) " done\n"))))))

(def ^:private processes-highlight-elisp
  "Elisp to apply faces to process buffer status markers after insert."
  (str "(goto-char (point-min))"
       "(while (re-search-forward \"●\" nil t)"
       "  (put-text-property (match-beginning 0) (match-end 0) 'face '(:foreground \"green\")))"
       "(goto-char (point-min))"
       "(while (re-search-forward \"✖[^\n]*\" nil t)"
       "  (put-text-property (match-beginning 0) (match-end 0) 'face '(:foreground \"red\" :weight bold)))"
       "(goto-char (point-min))"
       "(while (re-search-forward \"⚠[^\n]*\" nil t)"
       "  (put-text-property (match-beginning 0) (match-end 0) 'face '(:foreground \"orange\" :weight bold)))"
       "(goto-char (point-min))"))

(defn project-processes!
  "Project CYDER process registry to the *processes* blackboard buffer.
   Reads raw registry entries (not list-processes) so state-fn can provide live data."
  [registry-entries]
  (when *enabled*
    (try
      (let [content (format-process-status registry-entries)]
        (blackboard! "*processes*" content {:width 60 :slot 1
                                           :post-elisp processes-highlight-elisp}))
      (catch Throwable _ nil))))

;; =============================================================================
;; Evidence emission — blackboard "commits"
;; =============================================================================

(defn- emit-blackboard-evidence!
  "Record a blackboard snapshot as evidence. Fire-and-forget — never throws."
  [peripheral-id state content]
  (when-let [store (:evidence-store state)]
    (try
      (estore/append* store
                      {:evidence/id (str "e-bb-" (UUID/randomUUID))
                       :evidence/subject {:ref/type :peripheral
                                          :ref/id (name peripheral-id)}
                       :evidence/type :coordination
                       :evidence/claim-type :observation
                       :evidence/author (or (:author state) (str (name peripheral-id) "-blackboard"))
                       :evidence/at (str (java.time.Instant/now))
                       :evidence/body {:blackboard (name peripheral-id)
                                       :content content
                                       :session-id (:session-id state)
                                       :step-count (:step-count state)}
                       :evidence/tags [:blackboard (keyword (name peripheral-id))]
                       :evidence/session-id (:session-id state)})
      (catch Throwable _
        nil))))

;; =============================================================================
;; Projection helper — call from step dispatch
;; =============================================================================

(defn project!
  "Project the current peripheral state to the blackboard.

   Called after each step. Renders the peripheral's state via its adaptor,
   then writes to a buffer named after the peripheral. Also emits an
   evidence entry tagged :blackboard — the permanent record.

   No-op when *enabled* is false (tests, non-interactive contexts).
   Never throws — rendering and projection errors are swallowed.
   Returns nil (projection is fire-and-forget)."
  ([peripheral-id state]
   (project! peripheral-id state {}))
  ([peripheral-id state opts]
   (when *enabled*
     (try
       (when-let [content (render-blackboard peripheral-id state)]
         (let [buf-name (str "*" (name peripheral-id) "*")]
           (blackboard! buf-name content opts)
           (emit-blackboard-evidence! peripheral-id state content)
           nil))
       (catch Throwable _
         nil)))))
