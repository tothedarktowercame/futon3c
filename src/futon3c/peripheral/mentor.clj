(ns futon3c.peripheral.mentor
  "Mentor peripheral — space-like observation for proof sessions.

   The Mentor is the space-like complement of Tickle (time-like):
   - Tickle pushes obligations through the DAG (what happens next)
   - Mentor maps what territory has been covered (where are the gaps)

   The conversation map tracks topics, patterns, tensions, confidence claims,
   and effort distribution. QP triggers fire when gaps appear in the map.

   Handles: each mentor instance has a handle like 'mentor:FM-001' that is
   independent of which agent inhabits it. Any agent can be mentor; the handle
   owns the map, not the agent. Multiple mentors can run for different problems.

   Persistence: the map checkpoints to futon1a on each observe cycle.
   On restart, the latest checkpoint is restored so nothing is lost.

   Tools:
   - :mentor-observe   — ingest new IRC messages, enrich the map
   - :mentor-evaluate  — check triggers against the map, return gap analysis
   - :mentor-intervene — post an intervention to IRC when a trigger fires
   - :mentor-status    — current map summary + trigger state
   - :mentor-map       — full conversation map dump"
  (:require [futon3c.peripheral.common :as common]
            [futon3c.peripheral.evidence :as evidence]
            [futon3c.peripheral.mentor-map :as mmap]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]
            [futon3c.blackboard :as bb]
            [futon3c.evidence.store :as estore]
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
;; State shape — now centered on the conversation map
;; =============================================================================

(defn- initial-state
  "Create initial mentor state from context.
   If a checkpoint exists in the evidence store, restore from it."
  [context handle]
  (let [base {:session-id (:session-id context)
              :author (common/resolve-author context)
              :handle handle
              :problem-id (:problem-id context)
              :channel (or (:channel context) "#math")
              ;; The space-like map (core state)
              :cmap (mmap/empty-map handle
                                    (:problem-id context)
                                    (or (:channel context) "#math"))
              ;; Injected functions (runtime, not persisted)
              :irc-read-fn nil
              :irc-send-fn nil
              ;; Evidence
              :evidence-store (:evidence-store context)
              :last-evidence-id nil}]
    ;; Try to restore from checkpoint
    (if-let [store (:evidence-store context)]
      (let [checkpoints (estore/query* store
                          {:evidence/tags [:mentor :checkpoint handle]})
            latest (last (sort-by :evidence/at checkpoints))]
        (if latest
          (let [restored-map (mmap/restore-from-checkpoint
                               (:evidence/body latest))]
            (println (str "[mentor] Restored checkpoint for " handle
                          " (v" (:mentor/version restored-map)
                          ", " (:map/messages-seen restored-map) " msgs)"))
            (assoc base :cmap restored-map))
          base))
      base)))

;; =============================================================================
;; Checkpoint persistence
;; =============================================================================

(defn- checkpoint!
  "Save the current conversation map to the evidence store.
   Called after each observe cycle."
  [state]
  (when-let [store (:evidence-store state)]
    (let [body (mmap/checkpoint-body (:cmap state))
          entry {:evidence/id (str "e-mentor-ckpt-" (java.util.UUID/randomUUID))
                 :evidence/subject {:ref/type :mentor-session
                                    :ref/id (:handle state)}
                 :evidence/type :coordination
                 :evidence/claim-type :observation
                 :evidence/author (:author state)
                 :evidence/at (str (Instant/now))
                 :evidence/body body
                 :evidence/tags [:mentor :checkpoint (:handle state)]}]
      (try
        (estore/append* store entry)
        nil ;; success
        (catch Exception e
          (println (str "[mentor] Checkpoint failed: " (.getMessage e))))))))

;; =============================================================================
;; Tool implementations
;; =============================================================================

(defn- tool-observe
  "Read new IRC messages, enrich the conversation map.
   Checkpoints the map to futon1a after enrichment."
  [state _args]
  (let [read-fn (:irc-read-fn state)
        messages (if (fn? read-fn) (read-fn) [])
        seen (get-in state [:cmap :map/messages-seen] 0)
        new-msgs (vec (drop seen messages))
        new-count (count new-msgs)
        updated-map (mmap/enrich (:cmap state) new-msgs)]
    {:result {:new-messages new-count
              :total-seen (:map/messages-seen updated-map)
              :map-version (:mentor/version updated-map)
              :topics-found (count (:map/topics updated-map))
              :latest (when (seq new-msgs)
                        (mapv (fn [m]
                                {:nick (:nick m)
                                 :text (subs (:text m "") 0
                                             (min 80 (count (or (:text m) ""))))})
                              (take-last 5 new-msgs)))}
     :state-updates {:cmap updated-map}
     :checkpoint? true}))

(defn- format-digest-for-eval
  "Format recent digest entries for trigger evaluation context."
  [digest n]
  (str/join "\n"
    (map (fn [{:keys [at speaker summary]}]
           (str (when at (subs (str at) 11 (min 19 (count (str at)))))
                " <" speaker "> " summary))
         (take-last n digest))))

(defn- tool-evaluate
  "Check triggers against the conversation map. Returns gap analysis.

   Two layers:
   1. Structural: the map's own gap detection (topic gaps, pattern gaps, etc.)
   2. Semantic: conversation digest for the inhabiting agent to review

   The agent gets both and decides which triggers actually fire."
  [state _args]
  (let [cmap (:cmap state)
        structural-triggers (mmap/evaluate-triggers cmap)
        firing (when structural-triggers
                 (filterv :fires? structural-triggers))
        digest-text (format-digest-for-eval (:map/digest cmap) 30)]
    {:result {:map-version (:mentor/version cmap)
              :messages-seen (:map/messages-seen cmap)
              ;; Structural gap analysis
              :topic-gaps (vec (mmap/topic-gaps cmap))
              :pattern-gaps (vec (mmap/pattern-gaps cmap))
              :effort-concentration (mmap/effort-concentration cmap)
              :untested-confidence (vec (mmap/untested-high-confidence cmap))
              :unresolved-tensions (vec (mmap/unresolved-tensions cmap))
              ;; Trigger evaluation
              :structural-fires (vec (map :trigger-id (or firing [])))
              :all-triggers (mapv (fn [{:keys [id name signal]}]
                                    {:id id :name name :signal signal})
                                  trigger-definitions)
              :previously-fired (:triggers/fired cmap)
              ;; Conversation context for semantic review
              :conversation-digest digest-text
              :instruction "Review the structural gap analysis AND the conversation digest. Confirm or override which triggers fire. Some triggers need semantic judgment beyond structural detection."}
     :state-updates {:cmap (assoc cmap :triggers/last-check (str (Instant/now)))}}))

(defn- tool-intervene
  "Post an intervention message to IRC.
   Records the intervention in the map."
  [state args]
  (let [[trigger-id message] args
        trigger-kw (if (keyword? trigger-id) trigger-id (keyword trigger-id))
        send-fn (:irc-send-fn state)
        channel (:channel state "#math")
        handle (:handle state)
        author (:author state "claude-2")]
    (cond
      (str/blank? message)
      {:result {:error "Intervention message cannot be blank"}}

      (not (fn? send-fn))
      {:result {:error "No irc-send-fn configured — cannot post to IRC"}}

      :else
      (do
        (send-fn channel author message)
        {:result {:posted true
                  :trigger trigger-kw
                  :channel channel
                  :handle handle
                  :message message}
         :state-updates
         {:cmap (-> (:cmap state)
                    (update :interventions conj
                            {:at (str (Instant/now))
                             :trigger-id trigger-kw
                             :message message})
                    (update :triggers/fired conj trigger-kw))}
         :checkpoint? true}))))

(defn- tool-status
  "Return mentor map summary."
  [state _args]
  (let [cmap (:cmap state)]
    {:result {:handle (:handle state)
              :problem-id (:mentor/problem cmap)
              :channel (:mentor/channel cmap)
              :map-version (:mentor/version cmap)
              :messages-seen (:map/messages-seen cmap)
              :last-seen-at (:map/last-seen-at cmap)
              :topics (mapv (fn [{:keys [id status by]}]
                              {:id id :status status :by by})
                            (:map/topics cmap))
              :patterns-applied (vec (get-in cmap [:map/patterns :applied]))
              :patterns-relevant-unapplied
              (vec (remove (get-in cmap [:map/patterns :applied])
                           (get-in cmap [:map/patterns :relevant])))
              :effort (into {} (map (fn [[k v]] [k (vec v)])
                                    (:map/effort cmap)))
              :triggers-fired (:triggers/fired cmap)
              :interventions-count (count (:interventions cmap))
              :last-trigger-check (:triggers/last-check cmap)}}))

(defn- tool-map
  "Return full conversation map. For debugging and deep inspection."
  [state _args]
  {:result (mmap/checkpoint-body (:cmap state))})

(defn- dispatch-tool
  "Dispatch a tool action. Returns {:result ... :state-updates ... :checkpoint? ...}."
  [state {:keys [tool args]}]
  (case tool
    :mentor-observe   (tool-observe state args)
    :mentor-evaluate  (tool-evaluate state args)
    :mentor-intervene (tool-intervene state args)
    :mentor-status    (tool-status state args)
    :mentor-map       (tool-map state args)
    {:result {:error (str "Unknown mentor tool: " tool)}}))

;; =============================================================================
;; PeripheralRunner
;; =============================================================================

(def mentor-tools
  "Tools available in the mentor peripheral."
  #{:mentor-observe :mentor-evaluate :mentor-intervene :mentor-status :mentor-map})

(defrecord MentorPeripheral [spec backend irc-read-fn irc-send-fn handle]
  runner/PeripheralRunner

  (start [_ context]
    (if-let [err (runner/validate-context :mentor context #{:session-id})]
      err
      (let [sid (:session-id context)
            state (-> (initial-state context handle)
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
          (let [{:keys [result state-updates checkpoint?]}
                (dispatch-tool state {:tool tool :args args})
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
                new-state (update new-state :steps
                                  (fnil conj [])
                                  {:tool tool :result result :at (str (Instant/now))})]
            ;; Checkpoint to futon1a if requested
            (when checkpoint? (checkpoint! new-state))
            ;; Project to blackboard (stable slot 2)
            (bb/project-mentor! new-state)
            (common/maybe-append-evidence! new-state ev)
            {:ok true :state new-state :result result :evidence ev})))))

  (stop [_ state reason]
    ;; Final checkpoint before stopping
    (checkpoint! state)
    (let [cmap (:cmap state)
          fruit {:handle (:handle state)
                 :map-version (:mentor/version cmap)
                 :messages-seen (:map/messages-seen cmap)
                 :topics-tracked (count (:map/topics cmap))
                 :interventions-count (count (:interventions cmap))
                 :triggers-fired (vec (:triggers/fired cmap))}
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

(def default-spec
  {:peripheral/id :mentor
   :peripheral/tools mentor-tools
   :peripheral/scope :observation
   :peripheral/entry #{:user-request :from-any}
   :peripheral/exit #{:user-request :session-end}
   :peripheral/context {:session-id :inherit :problem-id :optional}})

(defn make-mentor
  "Create a mentor peripheral for a specific handle.

   handle: string like 'mentor:FM-001' — identifies this mentor instance.
           The handle owns the map; any agent can inhabit it.

   Required opts (for real use):
   - :irc-read-fn  (fn [] -> [{:nick str :text str :at str} ...])
   - :irc-send-fn  (fn [channel from-nick message] -> bool)"
  ([backend]
   (make-mentor backend {} "mentor:default"))
  ([backend opts]
   (make-mentor backend opts (or (:handle opts) "mentor:default")))
  ([backend {:keys [irc-read-fn irc-send-fn]} handle]
   (->MentorPeripheral
    default-spec
    backend
    irc-read-fn
    irc-send-fn
    handle)))
