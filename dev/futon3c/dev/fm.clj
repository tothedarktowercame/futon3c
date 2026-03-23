(ns futon3c.dev.fm
  "FM-001 conductor — mechanical proof-obligation dispatch.

   Extracted from futon3c.dev (Phase 2 of TN-dev-clj-decomposition).
   Manages the event-driven conductor loop that assigns FM obligations
   to idle agents via IRC. Registers with CYDER for inspectability."
  (:require [clojure.string :as str]
            [futon3c.agents.tickle-orchestrate :as orch]
            [futon3c.agents.tickle-queue :as tq]
            [futon3c.agents.mfuton-prompt-override :as mfuton-prompt-override]
            [futon3c.agency.registry :as reg]
            [futon3c.blackboard :as bb]
            [futon3c.cyder :as cyder]
            [futon3c.dev.config :as config]
            [futon3c.dev.irc :as dev-irc]
            [futon3c.logic.invariant-runner :as ir]
            [futon3c.social.whistles :as whistles])
  (:import [java.time Instant]
           [java.time.temporal ChronoUnit]))

;; ---------------------------------------------------------------------------
;; State
;; ---------------------------------------------------------------------------

(defonce !fm-conductor (atom nil))
(defonce !post-invoke-hook (atom nil))

(declare fm-dispatch-mechanical!)

(def ^:private fm-claim-prompt-re
  #"(?im)^([a-z0-9._-]+):\s+I['’]ll take\s+([A-Za-z0-9._:-]+)\s*$")

(def ^:private fm-bell-prompt-re
  #"(?im)(?:@?tickle:?[ \t]+)?BELL[ \t]+([A-Za-z0-9._:-]+)")

(def ^:private irc-surface-channel-re
  #"(?i)\[Surface:\s*IRC\s*\|\s*Channel:\s*(#[^ |\]]+)")

(def ^:private irc-surface-speaker-re
  #"(?i)\[Surface:\s*IRC\s*\|\s*Channel:\s*#[^|\]]+\|\s*Speaker:\s*([^ |\]]+)")

;; ---------------------------------------------------------------------------
;; FM-001 thin wrappers (delegates to tickle_orchestrate.clj)
;; ---------------------------------------------------------------------------

(defn fm-assignable-obligations
  "Find FM-001 ledger obligations that are assignable (open, all deps met)."
  [problem-id]
  (orch/fm-assignable-obligations problem-id))

(defn- math-irc-enabled?
  []
  (config/env-bool "MATH_IRC" false))

(defn- fm-dispatch-channel
  []
  (if (math-irc-enabled?) "#math" "#futon"))

(defn- prompt-channel
  [prompt]
  (some->> (re-find irc-surface-channel-re (str (or prompt "")))
           second))

(defn- prompt-speaker
  [prompt]
  (some->> (re-find irc-surface-speaker-re (str (or prompt "")))
           second))

(defn- current-conductor-state-atom
  []
  (some-> @!fm-conductor :conductor-state))

(defn- claimed-obligations
  [conductor-state]
  (or (:claimed-obligations @conductor-state) {}))

(defn- claimed-obligation-summary
  [conductor-state]
  (let [claimed (claimed-obligations conductor-state)]
    (into {}
          (map (fn [[obligation-id {:keys [agent-id nick]}]]
                 [obligation-id (or nick agent-id)]))
          claimed)))

(defn- claimed-obligation-ids
  [conductor-state]
  (set (keys (claimed-obligations conductor-state))))

(defn- unclaimed-assignable-obligations
  [problem-id conductor-state]
  (let [claimed-ids (if conductor-state
                      (claimed-obligation-ids conductor-state)
                      #{})]
    (remove #(contains? claimed-ids (:item/id %))
            (fm-assignable-obligations problem-id))))

(defn- configured-nick-agent-map
  []
  (into {}
        (keep (fn [entry]
                (when-let [[_ nick agent-id] (re-matches #"(?i)\s*([^:,\s]+)\s*:\s*([^:,\s]+)\s*" entry)]
                  [(str/lower-case nick) agent-id])))
        (config/env-list "NICK_AGENT_MAP" [])))

(def ^:private fm-agent-nicks
  {"claude-1" "claude" "claude-2" "claude-2" "codex-1" "codex"
   "claude-3" "claude-3" "codex-2" "codex-2" "codex-3" "codex-3"})

(def ^:private fm-nick-agents
  (into {}
        (map (fn [[agent-id nick]]
               [(str/lower-case nick) agent-id]))
        fm-agent-nicks))

(defn- claim-nick->agent-id
  [nick]
  (let [nick-lower (some-> nick str str/trim str/lower-case)
        configured-map (configured-nick-agent-map)
        configured-codex-nick (some-> (config/configured-codex-relay-nick)
                                      str/lower-case)]
    (cond
      (str/blank? nick-lower)
      nil

      (contains? configured-map nick-lower)
      (get configured-map nick-lower)

      (contains? fm-nick-agents nick-lower)
      (get fm-nick-agents nick-lower)

      (and configured-codex-nick
           (str/starts-with? nick-lower configured-codex-nick))
      (config/configured-codex-agent-id)

      (str/starts-with? nick-lower "codex")
      (config/configured-codex-agent-id)

      (str/starts-with? nick-lower "claude")
      "claude-1"

      :else
      nil)))

(defn- parse-fm-claim-prompt
  [prompt]
  (when-let [[_ nick obligation-id] (re-find fm-claim-prompt-re (str (or prompt "")))]
    {:nick (str/lower-case nick)
     :obligation-id obligation-id
     :channel (prompt-channel prompt)}))

(defn- parse-fm-bell-prompt
  [prompt]
  (when-let [[_ event] (re-find fm-bell-prompt-re (str (or prompt "")))]
    {:nick (some-> (prompt-speaker prompt) str/lower-case)
     :event event
     :channel (prompt-channel prompt)}))

(defn fm-dispatch!
  "Have Tickle assign the top FM-001 obligation on #math."
  ([] (fm-dispatch! "FM-001"))
  ([problem-id]
   (let [tasks (unclaimed-assignable-obligations problem-id (current-conductor-state-atom))]
     (if (empty? tasks)
       (do (println "[tickle-fm] No assignable obligations for " problem-id)
           nil)
       (let [{:item/keys [id label]} (first tasks)
             msg (str "TASK ASSIGNMENT [" problem-id " / " id "]: " label
                      ". Current mode: FALSIFY. "
                      "Who wants to take this? Claim with @tickle I'll take " id)]
         ((dev-irc/make-bridge-irc-send-fn) "#math" "tickle" msg)
         (println (str "[tickle-fm] Assigned " id " on #math"))
         {:assigned id :label label})))))

(defn fm-status!
  "Print FM-001 ledger status for Tickle's view."
  ([] (fm-status! "FM-001"))
  ([problem-id]
   (let [tasks (fm-assignable-obligations problem-id)]
     (println (str "FM-001 assignable obligations (" (count tasks) "):"))
     (doseq [{:item/keys [id label]} tasks]
       (println (str "  " id ": " label))))))

;; ---------------------------------------------------------------------------
;; IRC helpers
;; ---------------------------------------------------------------------------

(defn irc-recent-channel
  "Return last N messages from a specific channel."
  [channel n]
  (->> (dev-irc/irc-recent (* n 3))
       (filter #(= channel (:channel %)))
       (take-last n)
       vec))

(defn- fm-dispatch-message-original
  [nick ob-id ob-label]
  (str "@" nick " " ob-id ": " ob-label
       ". Push results to git when done."))

(defn handle-claim-prompt!
  "Honor the existing FrontierMath claim seam on the dedicated #math lane.
   Returns nil when PROMPT is not a math-lane claim and generic tickle behavior
   should continue."
  [prompt session-id]
  (when (math-irc-enabled?)
    (when-let [{:keys [nick obligation-id channel]} (parse-fm-claim-prompt prompt)]
      (when (= "#math" channel)
        (if-let [conductor-state (current-conductor-state-atom)]
          (let [agent-id (claim-nick->agent-id nick)
                claimed (get (claimed-obligations conductor-state) obligation-id)
                obligation (some #(when (= obligation-id (:item/id %)) %)
                                 (unclaimed-assignable-obligations "FM-001" conductor-state))]
            (cond
              (nil? agent-id)
              {:ok true
               :session-id session-id
               :result (str "@" nick " claim rejected: unknown agent mapping for " nick ".")}

              claimed
              {:ok true
               :session-id session-id
               :result (str "@" nick " " obligation-id " already claimed by "
                            (or (:nick claimed) (:agent-id claimed)) ".")}

              (nil? obligation)
              {:ok true
               :session-id session-id
               :result (str "@" nick " " obligation-id " is not currently assignable.")}

              :else
              (let [ob-label (:item/label obligation)
                    claim {:agent-id agent-id
                           :nick nick
                           :label ob-label
                           :claimed-at (str (Instant/now))}
                    original-msg (fm-dispatch-message-original nick obligation-id ob-label)
                    msg (if (mfuton-prompt-override/mfuton-mode?)
                          (mfuton-prompt-override/fm-dispatch-message-override original-msg)
                          original-msg)
                    now-ms (System/currentTimeMillis)]
                (swap! conductor-state
                       (fn [s]
                         (-> s
                             (assoc-in [:claimed-obligations obligation-id] claim)
                             (assoc-in [:last-paged agent-id] now-ms)
                             (update-in [:paged-obligations agent-id] (fnil conj #{}) obligation-id)
                             (assoc :last-cycle {:action :claim
                                                 :target agent-id
                                                 :obligation obligation-id
                                                 :text msg})
                             (assoc :last-cycle-ms now-ms))))
                {:ok true
                 :session-id session-id
                 :result msg})))
          {:ok true
           :session-id session-id
           :result (str "@" nick " claim rejected: FM conductor is not running.")})))))

(defn handle-bell-prompt!
  "Honor the existing FrontierMath bell phrase on the dedicated #math lane.
   Returns nil when PROMPT is not a math-lane bell and generic tickle behavior
   should continue."
  [prompt session-id]
  (when (math-irc-enabled?)
    (when-let [{:keys [nick event channel]} (parse-fm-bell-prompt prompt)]
      (when (= "#math" channel)
        (if-let [conductor-state (current-conductor-state-atom)]
          (let [agent-id (claim-nick->agent-id nick)]
            (if (nil? agent-id)
              {:ok true
               :session-id session-id
               :result (str "BELL " event
                            " rejected: unknown agent mapping for " nick ".")}
              (let [dispatch-result (fm-dispatch-mechanical!
                                     agent-id
                                     {:problem-id "FM-001"
                                      :bridge-send-fn (dev-irc/make-bridge-irc-send-fn)
                                      :conductor-state conductor-state})
                    now-ms (System/currentTimeMillis)
                    result-text (case (:action dispatch-result)
                                  :page (str "BELL " event
                                             " acknowledged for " nick
                                             ". Next work was posted to #math.")
                                  :pass (str "BELL " event
                                             " acknowledged for " nick
                                             ". No new assignable obligations right now.")
                                  :cooldown (str "BELL " event
                                                 " acknowledged for " nick
                                                 ". No new dispatch during cooldown.")
                                  :skip (str "BELL " event
                                             " acknowledged for " nick
                                             ". You're not idle for a new assignment yet.")
                                  (str "BELL " event
                                       " acknowledged for " nick "."))]
                (swap! conductor-state assoc
                       :last-cycle {:action :bell
                                    :target agent-id
                                    :event event
                                    :dispatch-action (:action dispatch-result)
                                    :text result-text}
                       :last-cycle-ms now-ms)
                {:ok true
                 :session-id session-id
                 :result result-text})))
          {:ok true
           :session-id session-id
           :result (str "BELL " event
                        " rejected: FM conductor is not running for " nick ".")})))))

;; ---------------------------------------------------------------------------
;; Display projection
;; ---------------------------------------------------------------------------

(defn- refresh-processes-buffer!
  "Re-project the *processes* buffer to all connected Emacs instances."
  []
  (try
    (let [entries (vals @cyder/!processes)]
      (bb/project-processes! entries))
    (catch Exception _ nil)))

(defn project-tickle-state!
  "Project combined tickle state (conductor + watchdog) to the *tickle* blackboard."
  [!tickle]
  (try
    (let [conductor-handle @!fm-conductor
          conductor-state (when conductor-handle
                            @(:conductor-state conductor-handle))
          watchdog-state (when @!tickle
                            (when-let [p (get @cyder/!processes "tickle-watchdog")]
                              (when-let [sf (:process/state-fn p)]
                                (try (sf) (catch Exception _ nil)))))
          now-ms (System/currentTimeMillis)
          state (cond-> {}
                  conductor-state
                  (assoc :conductor
                         (let [s conductor-state
                               rotation (or (:rotation s) ["codex-1" "claude-3" "codex-2" "codex-3"])
                               idx (or (:idx s) 0)
                               next-agent (nth rotation (mod idx (count rotation)))
                               cooldowns (:last-paged s)
                               cooldown-ms 900000
                               fmt-cd (fn [a]
                                        (let [marker (if (= a next-agent) "▶ " "  ")]
                                          (if-let [ts (get cooldowns a)]
                                            (let [ago-s (quot (- now-ms ts) 1000)
                                                  remaining (- (quot cooldown-ms 1000) ago-s)]
                                              (if (pos? remaining)
                                                (str marker a " → cooldown " remaining "s")
                                                (str marker a " → ready (paged " ago-s "s ago)")))
                                            (str marker a " → ready"))))
                               base-ms (or (:last-cycle-ms s) (:started-at-ms s))
                               step-ms-raw (or (some-> (get @cyder/!processes "fm-conductor")
                                                       :process/metadata :step-ms)
                                               300000)
                               next-at (when base-ms
                                         (str (.truncatedTo
                                                (Instant/ofEpochMilli (+ base-ms step-ms-raw))
                                                ChronoUnit/SECONDS)))]
                           (cond-> {:problem-id (or (:problem-id s) "FM-001")
                                    :cycles (:cycles-completed s 0)
                                    :step-ms (str (quot step-ms-raw 1000) "s")
                                    :rotation rotation
                                    :agents (mapv fmt-cd rotation)
                                    :idx idx
                                    :last-cycle (:last-cycle s)}
                             next-at (assoc :next-at next-at))))
                  watchdog-state (assoc :watchdog watchdog-state)
                  ;; Task queue state
                  true (assoc :queue (let [q (tq/snapshot)]
                                       {:pending (count (:pending q))
                                        :assigned (into {}
                                                        (map (fn [[aid info]]
                                                               [aid (:obligation info)]))
                                                        (:assigned q))
                                        :completed-count (count (:completed q))})))]
      (bb/project! :tickle state))
    (catch Exception _ nil)))

;; ---------------------------------------------------------------------------
;; Mechanical conductor
;; ---------------------------------------------------------------------------

(defn- agent-idle?
  "Check if an agent is idle (not currently invoking).
   Returns false for agents not in the registry."
  [agent-id]
  (let [a (get @reg/!registry agent-id)]
    (and (some? a)
         (not= :invoking (:agent/status a)))))

(defn- idle-agents
  "Return agent IDs from the rotation that are currently idle."
  [rotation]
  (filterv agent-idle? rotation))

(defn- fm-dispatch-mechanical!
  "Mechanical FM conductor dispatch. No LLM — just state + obligations.
   If agent is idle and obligations exist, page with a templated message.
   Returns {:action :page|:pass|:skip, :target str, :text str}."
  [agent-id {:keys [problem-id bridge-send-fn conductor-state cooldown-ms]
             :or {problem-id "FM-001" cooldown-ms (* 3 60 1000)}}]
  (let [conductor-state (or conductor-state (atom {}))
        nick (get fm-agent-nicks agent-id agent-id)]
    (cond
      (not (agent-idle? agent-id))
      {:action :skip :target agent-id}

      (let [last-paged (get-in @conductor-state [:last-paged agent-id])]
        (and last-paged (<= (- (System/currentTimeMillis) last-paged) cooldown-ms)))
      {:action :cooldown :target agent-id}

      :else
      (let [assignable (unclaimed-assignable-obligations problem-id conductor-state)
            already-paged (get-in @conductor-state [:paged-obligations agent-id] #{})
            fresh (remove #(contains? already-paged (:item/id %)) assignable)]
        (if (empty? fresh)
          {:action :pass :target agent-id}
          (let [ob (first fresh)
                ob-id (:item/id ob)
                ob-label (:item/label ob)
                original-msg (fm-dispatch-message-original nick ob-id ob-label)
                msg (if (mfuton-prompt-override/mfuton-mode?)
                      (mfuton-prompt-override/fm-dispatch-message-override original-msg)
                      original-msg)]
            (println (str "[conductor] " agent-id " → PAGE " ob-id))
            (swap! conductor-state
                   (fn [s]
                     (-> s
                         (assoc-in [:last-paged agent-id] (System/currentTimeMillis))
                         (update-in [:paged-obligations agent-id] (fnil conj #{}) ob-id))))
            (when (fn? bridge-send-fn)
              (bridge-send-fn "#math" "tickle" msg))
            {:action :page :target agent-id :text msg :obligation ob-id}))))))

(defn- fm-dispatch-idle-agents!
  "Scan all agents in rotation, dispatch work to any that are idle."
  [config]
  (let [rotation (or (:rotation config) ["codex-1" "claude-3" "codex-2" "codex-3"])
        idle (idle-agents rotation)]
    (when (seq idle)
      (mapv #(fm-dispatch-mechanical! % config) idle))))

;; ---------------------------------------------------------------------------
;; Config builder (needs make-claude-invoke-fn injected)
;; ---------------------------------------------------------------------------

(defn make-fm-conductor-config
  "Build the config map for FM conductor functions.
   Requires :make-claude-invoke-fn and :evidence-store to be supplied."
  [{:keys [make-claude-invoke-fn evidence-store !tickle
           invariant-aggregate-fn invariant-domains invariant-load-profile]} overrides]
  (merge {:problem-id "FM-001"
          :cooldown-ms (* 3 60 1000)
          :irc-read-fn #(irc-recent-channel "#math" 20)
          :bridge-send-fn (dev-irc/make-bridge-irc-send-fn)
          :evidence-store evidence-store
          :invariant-aggregate-fn (or invariant-aggregate-fn
                                      (when (seq invariant-domains)
                                        (fn []
                                          (ir/run-aggregate (or invariant-load-profile {})
                                                            invariant-domains))))
          :invoke-fn (when make-claude-invoke-fn
                       (make-claude-invoke-fn
                         {:claude-bin (config/env "CLAUDE_BIN" "claude")
                          :permission-mode "default"
                          :agent-id "fm-conductor"
                          :model "claude-haiku-4-5-20251001"
                          :timeout-ms 120000}))
          :whistle-fn (fn [{:keys [to] :as msg}]
                        (println (str "[fm-conductor] whistle → " to ": " (:reason msg)))
                        (try
                          (let [result (whistles/whistle!
                                         {:agent-id to
                                          :prompt (str "[whistle from tickle-1] " (:reason msg)
                                                       "\nPlease review the proof ledger for " (:problem-id msg)
                                                       " and either unblock existing obligations or create new ones.")
                                          :author "tickle-1"
                                          :timeout-ms 120000
                                          :evidence-store evidence-store})]
                            (println (str "[fm-conductor] whistle response: "
                                          (if (:whistle/ok result) "ok" (:whistle/error result))))
                            result)
                          (catch Exception e
                            (println (str "[fm-conductor] whistle delivery failed: " (.getMessage e))))))
          :on-cycle-fn (fn [_result]
                         (cyder/touch! "fm-conductor")
                         (refresh-processes-buffer!)
                         (project-tickle-state! !tickle))}
         overrides))

;; ---------------------------------------------------------------------------
;; Public conductor lifecycle
;; ---------------------------------------------------------------------------

(defn fm-conduct-targeted!
  "Run one FM-001 conductor cycle targeting a specific agent."
  ([deps target-agent] (fm-conduct-targeted! deps "FM-001" target-agent))
  ([deps problem-id target-agent]
   (fm-dispatch-mechanical! target-agent (make-fm-conductor-config deps {:problem-id problem-id}))))

(defn fm-conduct!
  "Run one FM-001 conductor cycle — dispatches to all idle agents."
  ([deps] (fm-conduct! deps "FM-001"))
  ([deps problem-id]
   (fm-dispatch-idle-agents! (make-fm-conductor-config deps {:problem-id problem-id}))))

;; ---------------------------------------------------------------------------
;; Task-pool dispatch (bell-driven)
;; ---------------------------------------------------------------------------

(defn- task->prompt-original
  "Build an invoke prompt from a task map."
  [{:keys [id label source depends-on]}]
  (str "Task assignment from the conductor queue.\n\n"
       "Task ID: " id "\n"
       "Task: " label "\n"
       (when source (str "Source: " source "\n"))
       (when (seq depends-on)
         (str "Depends on (completed): " (pr-str depends-on) "\n"))
       "\nWork this task. Push results to git when done. "
       "Keep your response concise — report what you did and what the outcome was."))

(defn task->prompt
  "Build an invoke prompt from a task map."
  [task]
  (let [original-prompt (task->prompt-original task)]
    (if (mfuton-prompt-override/mfuton-mode?)
      (mfuton-prompt-override/task-prompt-override original-prompt)
      original-prompt)))

(defn structural-law-task?
  [task]
  (let [source (:source task)]
    (and (string? source)
         (.startsWith source "structural-law/"))))

(defn sync-structural-law-tasks!
  "Upsert dispatchable invariant tasks into the queue.

   Pending structural-law tasks that disappear from the latest aggregate are
   removed. Assigned tasks remain assigned while the agent is working."
  [aggregate]
  (let [desired (vec (:dispatchable-tasks aggregate))
        desired-by-id (into {} (map (juxt :id identity)) desired)
        desired-ids (set (keys desired-by-id))
        existing (into {} (map (juxt :id identity)) (tq/all-tasks))
        now (Instant/now)]
    (doseq [[task-id task] desired-by-id]
      (let [existing-task (get existing task-id)
            assigned? (= :assigned (:status existing-task))
            upserted (merge existing-task
                            task
                            {:status (if assigned? :assigned :pending)
                             :assignee (when assigned? (:assignee existing-task))
                             :created-at (or (:created-at existing-task) now)
                             :completed-at nil})]
        (tq/add-task! upserted)))
    (doseq [{:keys [id status] :as task} (tq/all-tasks)
            :when (and (structural-law-task? task)
                       (= :pending status)
                       (not (contains? desired-ids id)))]
      (tq/remove-task! id))
    {:desired-count (count desired)
     :task-ids (sort desired-ids)}))

(defn refresh-structural-law-tasks!
  "Run the configured invariant aggregate and sync its dispatchable tasks into
   the conductor queue."
  [{:keys [invariant-aggregate-fn conductor-state]}]
  (when (fn? invariant-aggregate-fn)
    (let [aggregate (invariant-aggregate-fn)
          sync-result (sync-structural-law-tasks! aggregate)
          summary (:summary aggregate)
          result {:summary summary
                  :sync sync-result
                  :dispatchable-count (count (:dispatchable-tasks aggregate))
                  :needs-review-count (count (get-in aggregate [:obligations-by-actionability :needs-review]))
                  :informational-count (count (get-in aggregate [:obligations-by-actionability :informational]))}]
      (when conductor-state
        (swap! conductor-state assoc
               :last-invariant-sync result
               :last-invariant-sync-ms (System/currentTimeMillis)))
      result)))

(defn dispatch-task!
  "Pick the highest-priority task from the pool and invoke the agent with it.
   Returns {:action :dispatch|:pass|:error, :task-id str, :agent-id str}."
  [agent-id {:keys [bridge-send-fn conductor-state] :as config}]
  (refresh-structural-law-tasks! config)
  (if-let [task (tq/pick-task! agent-id)]
    (let [tid (:id task)
          nick (get fm-agent-nicks agent-id agent-id)
          dispatch-channel (fm-dispatch-channel)]
      (println (str "[conductor] " agent-id " → DISPATCH " tid ": " (:label task)))
      (when (fn? bridge-send-fn)
        (bridge-send-fn dispatch-channel "tickle"
                        (str "@" nick " dispatched: " tid " — " (:label task))))
      (when conductor-state
        (swap! conductor-state assoc-in [:last-paged agent-id] (System/currentTimeMillis)))
      ;; Invoke in a future — the agent will bell idle when done,
      ;; which re-enters this dispatch loop
      (future
        (try
          (let [prompt (task->prompt task)
                result (reg/invoke-agent! agent-id prompt 600000)]
            (if (:ok result)
              (do
                (println (str "[conductor] " agent-id " completed " tid))
                (tq/complete-task! agent-id))
              (do
                (println (str "[conductor] " agent-id " failed " tid ": " (:error result)))
                (tq/fail-task! agent-id))))
          (catch Exception e
            (println (str "[conductor] " agent-id " exception on " tid ": " (.getMessage e)))
            (tq/fail-task! agent-id))))
      {:action :dispatch :task-id tid :agent-id agent-id :label (:label task)})
    (do
      (println (str "[conductor] " agent-id " → no tasks available"))
      {:action :pass :agent-id agent-id})))

(defn- dispatch-from-queue!
  "Consume pending bells from the task queue, dispatch tasks to idle agents.
   Safety net for bells not consumed by the immediate on-idle handler.
   Returns seq of dispatch results."
  [config conductor-state]
  (let [bells (tq/drain-pending!)]
    (when (seq bells)
      (->> bells
           (filter :ok?)
           (group-by :agent-id)
           vals
           (map last)
           (mapv (fn [{:keys [agent-id]}]
                   (let [result (dispatch-task! agent-id config)]
                     (when result
                       (swap! conductor-state assoc
                              :last-cycle result
                              :last-cycle-ms (System/currentTimeMillis)))
                     result)))))))

(defn start-fm-conductor!
  "Start the mechanical FM conductor loop.
   Bell-driven: agents idle → queue bell → dispatch obligation.
   Periodic loop drains any un-consumed bells as safety net.
   Registers with CYDER for inspectability."
  ([deps] (start-fm-conductor! deps {}))
  ([deps {:keys [step-ms] :or {step-ms (* 20 60 1000)}}]
   (when-let [old @!fm-conductor]
     ((:stop-fn old))
     (cyder/deregister! "fm-conductor")
     (println "[fm-conductor] Stopped previous conductor."))
   (let [config (make-fm-conductor-config deps {:step-ms step-ms})
         conductor-state (atom {:cycles-completed 0
                                :last-cycle nil
                                :started-at-ms (System/currentTimeMillis)})
         config (assoc config :conductor-state conductor-state)
         running (atom true)
         rotation (or (:rotation config) ["codex-1" "claude-3" "codex-2" "codex-3"])
         cooldown-ms (or (:cooldown-ms config) (* 3 60 1000))
         !tickle (:!tickle deps)
         handle {:stop-fn #(reset! running false)
                 :conductor-state conductor-state
                 :running running}]
     ;; Wire bell-driven dispatch: agents idle → enqueue + dispatch from task pool
     ;; on-idle signature: (fn [agent-id outcome]) where outcome is
     ;; {:ok bool :error str-or-nil :session-id str-or-nil}
     (reg/set-on-idle!
       (fn [agent-id outcome]
         (tq/enqueue! agent-id outcome)
         (when @running
           (let [ok? (:ok outcome true)]
             (if ok?
               (do
                 (println (str "[bell→queue] " agent-id " idle (ok) → checking task pool"))
                 (let [result (dispatch-task! agent-id config)]
                   (when result
                     (swap! conductor-state assoc
                            :last-cycle result
                            :last-cycle-ms (System/currentTimeMillis))
                     (cyder/touch! "fm-conductor")
                     (refresh-processes-buffer!)
                     (project-tickle-state! !tickle))))
               ;; Error: don't dispatch new work, just log and project
               (do
                 (println (str "[bell→queue] " agent-id " idle (ERROR) — "
                               (when-let [f (tq/agent-failures agent-id)]
                                 (str (:consecutive f) " consecutive failures"))))
                 (project-tickle-state! !tickle)))))))
     ;; Periodic safety-net (20 min default): drain unconsumed bells.
     ;; TODO: make this do something useful — e.g. detect stuck agents,
     ;; escalate to mentor, or reprioritize stale tasks.
     (future
       (while @running
         (try
           (Thread/sleep (long step-ms))
           (when @running
             (refresh-structural-law-tasks! config)
             (let [queue-results (dispatch-from-queue! config conductor-state)
                   results queue-results]
               (swap! conductor-state assoc
                      :cycles-completed (inc (or (:cycles-completed @conductor-state) 0))
                      :last-cycle (when (seq results) (last results))
                      :last-cycle-ms (System/currentTimeMillis))
               (cyder/touch! "fm-conductor")
               (refresh-processes-buffer!)
               (project-tickle-state! !tickle)))
           (catch Exception e
             (println (str "[fm-conductor] Error: " (.getMessage e)))))))
     (reset! !fm-conductor handle)
     (cyder/deregister! "fm-conductor")
     (cyder/register!
      {:id "fm-conductor"
       :type :daemon
       :layer :repl
       :stop-fn (fn []
                  (reset! running false)
                  (reg/set-on-idle! nil)
                  (reset! !fm-conductor nil))
       :state-fn (fn []
                   (let [state @conductor-state
                         now-ms (System/currentTimeMillis)
                         cooldowns (:last-paged state)
                         cycles (or (:cycles-completed state) 0)
                         last-cycle (:last-cycle state)
                         invariant-sync (:last-invariant-sync state)
                         queue (tq/snapshot)
                         fmt-agent (fn [agent-id]
                                     (let [a (get @reg/!registry agent-id)
                                           status (or (:agent/status a) :unknown)
                                           idle? (not= :invoking status)
                                           assignment (get-in queue [:assigned agent-id])
                                           cd-ts (get cooldowns agent-id)
                                           cd-remaining (when cd-ts
                                                          (let [r (- cooldown-ms (- now-ms cd-ts))]
                                                            (when (pos? r) r)))]
                                       (str "  " agent-id
                                            " [" (name status) "]"
                                            (if idle?
                                              (if cd-remaining
                                                (str " cooldown " (quot cd-remaining 1000) "s")
                                                " ready")
                                              (when-let [t (:agent/invoke-started-at a)]
                                                (str " for " (quot (- now-ms (.toEpochMilli t)) 1000) "s")))
                                            (when assignment
                                              (str " → " (:obligation assignment))))))]
                     (cond-> {:problem-id (or (:problem-id config) "FM-001")
                              :step-ms (str (quot step-ms 1000) "s")
                              :cooldown (str (quot cooldown-ms 1000) "s")
                              :cycles cycles
                              :invariants invariant-sync
                              :queue {:pending (tq/pending-count)
                                      :assigned (tq/assigned-count)}
                              :agents (mapv fmt-agent rotation)}
                       last-cycle (assoc :last (str (:target last-cycle) " "
                                                    (name (:action last-cycle))
                                                    (when (:text last-cycle)
                                                      (str ": " (subs (:text last-cycle)
                                                                       0 (min 50 (count (:text last-cycle)))))))))))
       :step-fn (fn []
                  (dispatch-from-queue! config conductor-state))
       :metadata {:step-ms step-ms
                  :problem-id (or (:problem-id config) "FM-001")}})
     handle)))

(defn stop-fm-conductor!
  "Stop the FM-001 conductor loop."
  []
  (when-let [h @!fm-conductor]
    ((:stop-fn h))
    (reg/set-on-idle! nil)
    (reset! !fm-conductor nil)
    (cyder/deregister! "fm-conductor")))
