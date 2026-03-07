(ns futon3c.peripheral.mentor
  "Mentor peripheral — observation-driven intervention for proof sessions.

   Unlike other peripherals which are request-response (agent takes an action,
   peripheral returns a result), the mentor peripheral is *observation-driven*:

   1. INGEST: read recent IRC messages into state
   2. ACCUMULATE: update conversation observations
   3. EVALUATE: check QP-1..QP-8 triggers against accumulated state
   4. INTERVENE: post to IRC only when a trigger fires (rare)

   The mentor pulls messages (via irc-read-fn) rather than having them pushed.
   The step function is called on a timer (via CYDER) or manually. Most steps
   produce no output — the mentor stays silent until a trigger fires.

   Tools:
   - :mentor-observe  — ingest new IRC messages, return observations
   - :mentor-evaluate — check triggers, return any interventions
   - :mentor-intervene — post an intervention to IRC
   - :mentor-status   — current state summary
   - :mentor-ledger   — read proof ledger for problem context

   Fruit: {:observations-count int :interventions-count int :triggers-fired [kw ...]}
   Exit context: {:session-id str :problem-id str}"
  (:require [futon3c.peripheral.common :as common]
            [futon3c.peripheral.evidence :as evidence]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]
            [futon3c.blackboard :as bb]
            [clojure.string :as str])
  (:import [java.time Instant]))

;; =============================================================================
;; Trigger definitions (QP-1 through QP-8)
;; =============================================================================

(def trigger-definitions
  "The 8 question-asking pattern triggers. Each has:
   :id       — trigger keyword
   :name     — human name
   :question — the question the trigger asks
   :signal   — what to look for in conversation"
  [{:id :QP-1
    :name "LANDSCAPE SCOUT missing"
    :question "What does the territory look like before committing to a route?"
    :signal "Agent jumps to an approach without computational exploration"}
   {:id :QP-2
    :name "TECHNIQUE LANDSCAPE missing"
    :question "What methods get close, and where does each one break?"
    :signal "Agent commits to one method without surveying alternatives"}
   {:id :QP-3
    :name "STRUCTURAL PROBE missing"
    :question "Does this mechanism actually work, or is there a structural obstruction?"
    :signal "Agent invests in approach without checking for obstructions"}
   {:id :QP-4
    :name "FAILURE not converted to theorem"
    :question "This failed — is the failure a theorem about the problem?"
    :signal "Agent abandons failed approach without characterizing the failure"}
   {:id :QP-5
    :name "THEOREM APPLICABILITY unchecked"
    :question "Do the hypotheses of theorem T actually match my situation?"
    :signal "Agent imports theorem without verifying hypothesis fit"}
   {:id :QP-6
    :name "TENSION unrecognized"
    :question "Two parts of the proof have conflicting requirements — can they be resolved?"
    :signal "Agent works on parts with conflicting constraints without noticing"}
   {:id :QP-7
    :name "KERNEL not isolated"
    :question "What's the one remaining lemma — stated with exact hypotheses?"
    :signal "Proof mostly done but remaining gap is vaguely stated"}
   {:id :QP-8
    :name "CONFIDENCE anticorrelation"
    :question "Am I confident about the wrong things?"
    :signal "Agent expresses high confidence in untested approach"}])

;; =============================================================================
;; State shape
;; =============================================================================

(defn- initial-state
  "Create initial mentor state from context."
  [context]
  {:session-id (:session-id context)
   :author (common/resolve-author context)
   :problem-id (:problem-id context)
   :channel (or (:channel context) "#math")
   ;; Conversation tracking
   :messages-seen 0
   :last-seen-at nil
   :conversation []          ;; ring buffer of recent messages
   :conversation-max 100     ;; keep last 100 messages
   ;; Trigger tracking
   :observations []          ;; [{:at Instant :trigger-id kw :summary str}]
   :interventions []         ;; [{:at Instant :trigger-id kw :message str}]
   :triggers-fired #{}       ;; set of trigger IDs that have fired
   ;; Injected functions (set by factory)
   :irc-read-fn nil
   :irc-send-fn nil
   ;; Evidence
   :evidence-store (:evidence-store context)
   :last-evidence-id nil})

;; =============================================================================
;; Tool implementations
;; =============================================================================

(defn- tool-observe
  "Read new IRC messages into state. Returns updated state + observation summary.

   This is the ingest step: pull messages from the IRC feed, diff against
   what we've already seen, add new ones to the conversation buffer."
  [state _args]
  (let [read-fn (:irc-read-fn state)
        messages (if (fn? read-fn) (read-fn) [])
        seen (:messages-seen state 0)
        new-msgs (drop seen messages)
        new-count (count new-msgs)
        updated-conv (vec (take-last
                           (:conversation-max state 100)
                           (into (:conversation state) new-msgs)))]
    {:result {:new-messages new-count
              :total-seen (+ seen new-count)
              :latest (when (seq new-msgs)
                        (mapv (fn [m]
                                {:nick (:nick m)
                                 :text (subs (:text m "") 0
                                             (min 80 (count (or (:text m) ""))))})
                              (take-last 5 new-msgs)))}
     :state-updates {:messages-seen (+ seen new-count)
                     :last-seen-at (str (Instant/now))
                     :conversation updated-conv}}))

(defn- format-conversation-for-eval
  "Format recent conversation as a string for trigger evaluation."
  [messages]
  (str/join "\n"
    (map (fn [{:keys [nick text at]}]
           (str (when at (subs (str at) 11 19)) " <" nick "> " text))
         messages)))

(defn- tool-evaluate
  "Check triggers against accumulated conversation. Returns any observations.

   This is the judgment step — the LLM reviews the conversation and checks
   each trigger. Returns a list of observations (triggers that appear to fire)
   but does NOT post anything yet. Mentor reviews observations before acting.

   The actual evaluation is done by the agent inhabiting this peripheral —
   this tool provides the conversation context and trigger checklist.
   The agent decides which triggers fire."
  [state _args]
  (let [conv (:conversation state)
        recent (take-last 30 conv)
        formatted (format-conversation-for-eval recent)]
    {:result {:conversation-window (count recent)
              :conversation-text formatted
              :triggers (mapv (fn [{:keys [id name signal]}]
                                {:id id :name name :signal signal})
                              trigger-definitions)
              :previously-fired (:triggers-fired state)
              :instruction "Review the conversation against each trigger. Return which triggers fire and why. Do NOT intervene yet — just observe."}}))

(defn- tool-intervene
  "Post an intervention message to IRC.

   args: [trigger-id message]
   The mentor has decided a trigger warrants intervention. This posts
   the message to the channel and records it in state."
  [state args]
  (let [[trigger-id message] args
        trigger-kw (if (keyword? trigger-id) trigger-id (keyword trigger-id))
        send-fn (:irc-send-fn state)
        channel (:channel state "#math")]
    (cond
      (str/blank? message)
      {:result {:error "Intervention message cannot be blank"}}

      (not (fn? send-fn))
      {:result {:error "No irc-send-fn configured — cannot post to IRC"}}

      :else
      (do
        (send-fn channel "claude-2" message)
        {:result {:posted true
                  :trigger trigger-kw
                  :channel channel
                  :message message}
         :state-updates
         {:interventions (conj (:interventions state)
                               {:at (str (Instant/now))
                                :trigger-id trigger-kw
                                :message message})
          :triggers-fired (conj (:triggers-fired state) trigger-kw)}}))))

(defn- tool-status
  "Return current mentor state summary."
  [state _args]
  {:result {:messages-seen (:messages-seen state)
            :conversation-size (count (:conversation state))
            :observations-count (count (:observations state))
            :interventions-count (count (:interventions state))
            :triggers-fired (:triggers-fired state)
            :last-seen-at (:last-seen-at state)
            :problem-id (:problem-id state)
            :channel (:channel state)}})

(defn- dispatch-tool
  "Dispatch a tool action. Returns {:result ... :state-updates ...}."
  [state {:keys [tool args]}]
  (case tool
    :mentor-observe   (tool-observe state args)
    :mentor-evaluate  (tool-evaluate state args)
    :mentor-intervene (tool-intervene state args)
    :mentor-status    (tool-status state args)
    {:result {:error (str "Unknown mentor tool: " tool)}}))

;; =============================================================================
;; PeripheralRunner
;; =============================================================================

(def mentor-tools
  "Tools available in the mentor peripheral."
  #{:mentor-observe :mentor-evaluate :mentor-intervene :mentor-status})

(defrecord MentorPeripheral [spec backend irc-read-fn irc-send-fn]
  runner/PeripheralRunner

  (start [_ context]
    (if-let [err (runner/validate-context :mentor context #{:session-id})]
      err
      (let [sid (:session-id context)
            state (-> (initial-state context)
                      (assoc :irc-read-fn irc-read-fn
                             :irc-send-fn irc-send-fn))
            author (common/resolve-author context)
            ev (evidence/make-start-evidence :mentor sid author)
            append-err (common/maybe-append-evidence! state ev)]
        (if append-err
          append-err
          {:ok true
           :state (assoc state :last-evidence-id (:evidence/id ev))
           :evidence ev}))))

  (step [_ state action]
    (if-let [err (common/validate-action :mentor action)]
      err
      (let [tool (:tool action)
            args (or (:args action) [])]
        (if-not (contains? mentor-tools tool)
          (runner/runner-error :mentor :tool-not-allowed
                               (str "Tool not available in mentor: " tool)
                               :tool tool
                               :available mentor-tools)
          (let [{:keys [result state-updates]} (dispatch-tool state {:tool tool :args args})
                new-state (merge state state-updates)
                ev (evidence/make-step-evidence
                    :mentor
                    (:session-id state)
                    (:author state)
                    tool
                    args
                    result
                    (:last-evidence-id state))
                new-state (assoc new-state :last-evidence-id (:evidence/id ev))
                ;; Record step for history
                new-state (update new-state :steps
                                  (fnil conj [])
                                  {:tool tool :result result :at (str (Instant/now))})]
            ;; Project to blackboard
            (bb/project! :mentor new-state)
            (common/maybe-append-evidence! new-state ev)
            {:ok true :state new-state :result result :evidence ev})))))

  (stop [_ state reason]
    (let [fruit {:observations-count (count (:observations state))
                 :interventions-count (count (:interventions state))
                 :triggers-fired (vec (:triggers-fired state))
                 :messages-seen (:messages-seen state)}
          ev (evidence/make-stop-evidence
              :mentor
              (:session-id state)
              (:author state)
              fruit
              reason
              (:last-evidence-id state))
          append-err (common/maybe-append-evidence! state ev)]
      (if append-err
        append-err
        {:ok true
         :context {:session-id (:session-id state)
                   :problem-id (:problem-id state)}
         :fruit fruit
         :evidence ev}))))

;; =============================================================================
;; Factory
;; =============================================================================

(defn make-mentor
  "Create a mentor peripheral.

   Required opts in backend or kwargs:
   - :irc-read-fn  (fn [] -> [{:nick str :text str :at str} ...])
   - :irc-send-fn  (fn [channel from-nick message] -> bool)

   These are injected because the mentor needs to observe IRC without
   the bridge pushing messages to it."
  ([backend]
   (make-mentor backend {}))
  ([backend {:keys [irc-read-fn irc-send-fn]}]
   (->MentorPeripheral
    {:peripheral/id :mentor
     :peripheral/tools mentor-tools
     :peripheral/scope :observation
     :peripheral/entry #{:user-request :from-any}
     :peripheral/exit #{:user-request :session-end}
     :peripheral/context {:session-id :inherit :problem-id :optional}}
    backend
    irc-read-fn
    irc-send-fn)))
