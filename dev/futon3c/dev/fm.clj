(ns futon3c.dev.fm
  "FM-001 conductor — mechanical proof-obligation dispatch.

   Extracted from futon3c.dev (Phase 2 of TN-dev-clj-decomposition).
   Manages the event-driven conductor loop that assigns FM obligations
   to idle agents via IRC. Registers with CYDER for inspectability."
  (:require [futon3c.agents.tickle-orchestrate :as orch]
            [futon3c.agents.tickle-queue :as tq]
            [futon3c.agency.registry :as reg]
            [futon3c.blackboard :as bb]
            [futon3c.cyder :as cyder]
            [futon3c.dev.config :as config]
            [futon3c.dev.irc :as dev-irc]
            [futon3c.social.whistles :as whistles])
  (:import [java.time Instant]
           [java.time.temporal ChronoUnit]))

;; ---------------------------------------------------------------------------
;; State
;; ---------------------------------------------------------------------------

(defonce !fm-conductor (atom nil))
(defonce !post-invoke-hook (atom nil))

;; ---------------------------------------------------------------------------
;; FM-001 thin wrappers (delegates to tickle_orchestrate.clj)
;; ---------------------------------------------------------------------------

(defn fm-assignable-obligations
  "Find FM-001 ledger obligations that are assignable (open, all deps met)."
  [problem-id]
  (orch/fm-assignable-obligations problem-id))

(defn fm-dispatch!
  "Have Tickle assign the top FM-001 obligation on #math."
  ([] (fm-dispatch! "FM-001"))
  ([problem-id]
   (let [tasks (fm-assignable-obligations problem-id)]
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

(def ^:private fm-agent-nicks
  {"claude-1" "claude" "claude-2" "claude-2" "codex-1" "codex"
   "claude-3" "claude-3" "codex-2" "codex-2" "codex-3" "codex-3"})

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
      (let [assignable (orch/fm-assignable-obligations problem-id)
            already-paged (get-in @conductor-state [:paged-obligations agent-id] #{})
            fresh (remove #(contains? already-paged (:item/id %)) assignable)]
        (if (empty? fresh)
          {:action :pass :target agent-id}
          (let [ob (first fresh)
                ob-id (:item/id ob)
                ob-label (:item/label ob)
                msg (str "@" nick " " ob-id ": " ob-label
                         ". Push results to git when done.")]
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
  [{:keys [make-claude-invoke-fn evidence-store !tickle]} overrides]
  (merge {:problem-id "FM-001"
          :cooldown-ms (* 3 60 1000)
          :irc-read-fn #(irc-recent-channel "#math" 20)
          :bridge-send-fn (dev-irc/make-bridge-irc-send-fn)
          :evidence-store evidence-store
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

(defn- dispatch-from-queue!
  "Consume pending bells from the task queue, dispatch work to each idle agent.
   Dispatches to ANY registered agent that bells, not just the rotation.
   Returns seq of dispatch results."
  [config conductor-state]
  (let [bells (tq/drain-pending!)]
    (when (seq bells)
      (->> bells
           ;; Deduplicate: if same agent belled multiple times, keep latest
           (group-by :agent-id)
           vals
           (map last)
           (mapv (fn [{:keys [agent-id]}]
                   ;; Complete any previous assignment for this agent
                   (tq/complete! agent-id)
                   (let [result (fm-dispatch-mechanical! agent-id config)]
                     ;; Track assignment in queue
                     (when (= :page (:action result))
                       (tq/assign! agent-id (:obligation result)
                                   (:text result)
                                   (or (:problem-id config) "FM-001")))
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
  ([deps {:keys [step-ms] :or {step-ms 60000}}]
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
     ;; Wire bell-driven dispatch: agents idle → enqueue + immediate dispatch
     ;; on-idle signature: (fn [agent-id outcome]) where outcome is
     ;; {:ok bool :error str-or-nil :session-id str-or-nil}
     (reg/set-on-idle!
       (fn [agent-id outcome]
         (tq/enqueue! agent-id outcome)
         (when @running
           (let [ok? (:ok outcome true)]
             (if ok?
               (do
                 (println (str "[bell→queue] " agent-id " idle (ok) → dispatching"))
                 (tq/complete! agent-id)
                 (let [result (fm-dispatch-mechanical! agent-id config)]
                   (when (= :page (:action result))
                     (tq/assign! agent-id (:obligation result)
                                 (:text result)
                                 (or (:problem-id config) "FM-001")))
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
     ;; Periodic safety-net loop: drain any un-consumed bells + scan for idle agents
     (future
       (while @running
         (try
           (Thread/sleep (long step-ms))
           (when @running
             ;; Drain any bells that weren't consumed by the immediate dispatch
             (let [queue-results (dispatch-from-queue! config conductor-state)
                   ;; Also scan rotation for idle agents (catches agents that
                   ;; went idle before the watcher was installed)
                   scan-results (fm-dispatch-idle-agents! config)
                   results (concat queue-results scan-results)]
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
                              :queue {:pending (tq/pending-count)
                                      :assigned (tq/assigned-count)}
                              :agents (mapv fmt-agent rotation)}
                       last-cycle (assoc :last (str (:target last-cycle) " "
                                                    (name (:action last-cycle))
                                                    (when (:text last-cycle)
                                                      (str ": " (subs (:text last-cycle)
                                                                       0 (min 50 (count (:text last-cycle)))))))))))
       :step-fn (fn []
                  (fm-dispatch-idle-agents! (assoc config :cooldown-ms 0)))
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
