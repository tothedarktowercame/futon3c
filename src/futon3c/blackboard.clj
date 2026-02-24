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

;; =============================================================================
;; Enable/disable — bind *enabled* to false in tests or non-interactive contexts
;; =============================================================================

(def ^:dynamic *enabled*
  "When false, project! is a no-op. Defaults to true.
   Bind to false in tests to avoid spawning emacsclient processes."
  true)

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
   is best-effort, never blocks the agent)."
  [elisp]
  (try
    (let [pb (ProcessBuilder. ["emacsclient" "--eval" elisp])
          _ (.redirectErrorStream pb true)
          proc (.start pb)
          finished? (.waitFor proc 2 java.util.concurrent.TimeUnit/SECONDS)]
      (if finished?
        {:ok (zero? (.exitValue proc))
         :output (slurp (.getInputStream proc))}
        (do (.destroyForcibly proc)
            {:ok false :output "timeout"})))
    (catch Exception e
      {:ok false :output (.getMessage e)})))

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
         elisp (str "(let ((buf (get-buffer-create \"" buf-escaped "\")))"
                    "(with-current-buffer buf"
                    "(let ((inhibit-read-only t))"
                    "(erase-buffer)"
                    "(insert \"" escaped "\")"
                    "(goto-char (point-min))))"
                    (or display-form "")
                    "nil)")]
     (run-emacsclient! elisp))))

(defn blackboard-eval!
  "Run arbitrary elisp via emacsclient. For cases where text content
   isn't enough — e.g. opening magit, finding a file, arranging windows.

   Returns {:ok bool :output str}."
  [elisp]
  (run-emacsclient! elisp))

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
;; :agents — registered agent status overview
;; -----------------------------------------------------------------------------

(defn format-agent-status
  "Format agent registry status for blackboard display.
   Takes the registry-status map from registry/registry-status."
  [registry-status]
  (let [agents (:agents registry-status)
        now-ms (System/currentTimeMillis)]
    (str "Agents (" (:count registry-status) " registered)\n"
         (str/join "\n"
                   (map (fn [[aid info]]
                          (let [status (or (:status info) :idle)
                                type-str (some-> (:type info) name)
                                elapsed (when (and (= status :invoking)
                                                   (:invoke-started-at info))
                                          (try
                                            (let [started (.toEpochMilli
                                                            (java.time.Instant/parse
                                                              (:invoke-started-at info)))
                                                  secs (quot (- now-ms started) 1000)]
                                              (if (>= secs 60)
                                                (str (quot secs 60) "m" (mod secs 60) "s")
                                                (str secs "s")))
                                            (catch Exception _ nil)))
                                status-line (case status
                                              :invoking (str "INVOKING"
                                                             (when elapsed (str " (" elapsed ")"))
                                                             "\n    "
                                                             (:invoke-prompt-preview info))
                                              :idle "idle"
                                              (name status))]
                            (str "  " (name aid) " [" type-str "] " status-line)))
                        agents))
         "\n")))

(defn project-agents!
  "Project agent registry status to the *agents* blackboard buffer.
   Call this from anywhere — it reads directly from the registry."
  [registry-status]
  (when *enabled*
    (try
      (let [content (format-agent-status registry-status)]
        ;; Keep *agents* in a stable side-window slot.
        (blackboard! "*agents*" content {:width 60 :slot 0}))
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
