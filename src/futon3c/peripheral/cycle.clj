(ns futon3c.peripheral.cycle
  "Generic cycle machine — configuration-driven phase-gated peripheral.

   Extracted from proof.clj: the proof peripheral is a cycle machine with
   proof-domain configuration. This namespace is the generic engine that
   both proof and code mission peripherals instantiate.

   A CycleDomainConfig provides:
   - :domain-id        keyword identifying the domain (:proof, :mission, etc.)
   - :phase-order      vector of phase keywords (linear, no skipping)
   - :phase-tools      {phase -> #{tools}} tool restrictions per phase
   - :setup-tools      #{tools} available when no cycle is active
   - :tool-ops         {tool -> :observe|:action} operation classification
   - :required-outputs {phase -> #{keys}} mandatory outputs before advancing
   - :cycle-begin-tool keyword for the tool that starts a cycle
   - :cycle-advance-tool keyword for the tool that advances phases
   - :state-init-fn    (fn [context] -> domain-state-map) additional state at start
   - :fruit-fn         (fn [state] -> fruit-map) extract fruit at stop
   - :exit-context-fn  (fn [state] -> context-map) exit context at stop
   - :phase-tags-fn    (fn [phase tool] -> tags) optional Table 25 auto-tags
   - :autoconf-fn      (fn [context config] -> CycleDomainConfig) optional autoconf

   The futonic loop maps onto the cycle machine:
     象 = CycleDomainConfig (the configuration entering the cycle)
     部 = phase-order (the decomposition regime)
     咅 = each phase transition (articulation of progress)
     鹽 = the moment domain tools + evidence protocol compose
     香 = phase-tags-fn (perception of what's happening)
     味 = required-outputs (evaluation: did this phase produce enough?)
     🔮 = phase gating (regulation: constrain tools to prevent harm)
     捨 = stop with reason (set-down when boundary is reached)"
  (:require [futon3c.blackboard :as bb]
            [futon3c.peripheral.common :as common]
            [futon3c.peripheral.evidence :as evidence]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]))

;; =============================================================================
;; Configuration validation
;; =============================================================================

(defn valid-domain-config?
  "Check that a domain config has the required keys."
  [config]
  (and (keyword? (:domain-id config))
       (vector? (:phase-order config))
       (seq (:phase-order config))
       (map? (:phase-tools config))
       (set? (:setup-tools config))
       (map? (:tool-ops config))
       (map? (:required-outputs config))
       (keyword? (:cycle-begin-tool config))
       (keyword? (:cycle-advance-tool config))
       (fn? (:fruit-fn config))
       (fn? (:exit-context-fn config))))

;; =============================================================================
;; Phase logic (generic)
;; =============================================================================

(defn- phase-transitions
  "Build phase transition map from phase order vector."
  [phase-order]
  (into {} (map vector phase-order (rest phase-order))))

(defn- current-phase-tools
  "Get the set of tools allowed in the current cycle phase."
  [{:keys [phase-tools setup-tools]} state]
  (if-let [phase (:current-phase state)]
    (get phase-tools phase #{})
    setup-tools))

(defn- phase-allows-tool?
  "Check if the current phase allows the given tool."
  [config state tool]
  (contains? (current-phase-tools config state) tool))

(defn- tool-operation-kind
  "Return :observe or :action for a tool, or nil."
  [{:keys [tool-ops]} tool]
  (get tool-ops tool))

;; =============================================================================
;; Evidence enrichment
;; =============================================================================

(defn- enrich-evidence
  "Add domain-specific and Table 25 tags to step evidence."
  [config state ev tool]
  (let [domain-id (:domain-id config)
        op-kind (tool-operation-kind config tool)
        phase (:current-phase state)
        phase-tags (when-let [f (:phase-tags-fn config)]
                     (f phase tool))
        ev (assoc-in ev [:evidence/body (keyword (name domain-id) "operation-kind")]
                     op-kind)]
    (if phase-tags
      (update ev :evidence/tags into phase-tags)
      ev)))

;; =============================================================================
;; Step dispatch (generic cycle machine)
;; =============================================================================

(defn- dispatch-step
  "Dispatch a single action through phase gating and the backend."
  [config spec backend state action]
  (if-let [err (common/validate-action (:domain-id config) action)]
    err
    (let [{:keys [tool args]} (common/normalize-action action)
          cycle-begin (:cycle-begin-tool config)
          cycle-advance (:cycle-advance-tool config)]
      (cond
        ;; Phase gating
        (not (phase-allows-tool? config state tool))
        (runner/runner-error (:domain-id config) :phase-tool-not-allowed
                             (str "Tool " tool " is not allowed in phase "
                                  (or (:current-phase state) :setup))
                             :tool tool
                             :phase (or (:current-phase state) :setup)
                             :allowed (vec (current-phase-tools config state)))

        ;; Operation classification must be total
        (nil? (tool-operation-kind config tool))
        (runner/runner-error (:domain-id config) :unclassified-tool
                             (str "Tool " tool " has no observe/action classification")
                             :tool tool)

        :else
        (let [dispatch-result (tools/dispatch-tool tool args spec backend)]
          (cond
            (common/social-error? dispatch-result)
            dispatch-result

            (not (:ok dispatch-result))
            (runner/runner-error (:domain-id config) :tool-execution-failed
                                 "Tool execution failed"
                                 :tool tool :args args :result dispatch-result)

            :else
            (let [result (:result dispatch-result)
                  ev (evidence/make-step-evidence
                      (:domain-id config) (:session-id state) (:author state)
                      tool args result (:last-evidence-id state))
                  ev (enrich-evidence config state ev tool)
                  ;; Track phase transitions
                  new-phase (when (= tool cycle-advance)
                              (:cycle/phase result))
                  new-cycle-id (when (= tool cycle-begin)
                                 (:cycle/id result))
                  last-phase (last (:phase-order config))
                  new-state (cond-> state
                              true (assoc :last-evidence-id (:evidence/id ev))
                              true (update :steps conj {:tool tool :args args :result result})
                              new-phase (assoc :current-phase new-phase)
                              new-cycle-id (assoc :current-cycle-id (:cycle/id result)
                                                  :current-phase (first (:phase-order config)))
                              ;; When cycle completes, clear active cycle
                              (= new-phase last-phase)
                              (-> (dissoc :current-phase :current-cycle-id)
                                  (update :cycles-completed inc)))
                  append-err (common/maybe-append-evidence! new-state ev)]
              (if append-err
                append-err
                ;; Emit state snapshot if snapshot-fn returns non-nil
                (let [snapshot-fn (:state-snapshot-fn config)
                      snapshot (when snapshot-fn
                                 (snapshot-fn new-state tool result))
                      snap-ev (when snapshot
                                (evidence/make-snapshot-evidence
                                 (:domain-id config)
                                 (:session-id new-state)
                                 (:author new-state)
                                 (:snapshot/subject snapshot)
                                 (:snapshot/body snapshot)
                                 (:snapshot/tags snapshot)
                                 (:evidence/id ev)))
                      snap-err (when snap-ev
                                 (common/maybe-append-evidence! new-state snap-ev))
                      new-state (if snap-ev
                                  (assoc new-state :last-evidence-id (:evidence/id snap-ev))
                                  new-state)]
                  (if snap-err
                    snap-err
                    {:ok true :state new-state :result result
                     :evidence ev
                     :snapshot-evidence snap-ev}))))))))))

;; =============================================================================
;; CyclePeripheral record (generic)
;; =============================================================================

(defrecord CyclePeripheral [config spec backend]
  runner/PeripheralRunner
  (start [_ context]
    (let [domain-id (:domain-id config)]
      (if-let [err (runner/validate-context domain-id context #{:session-id})]
        err
        (let [;; Autoconf: allow domain config to be refined from context.
              ;; Store refined config in state so step/stop use it too.
              effective-config (if-let [autoconf (:autoconf-fn config)]
                                 (autoconf context config)
                                 config)
              sid (:session-id context)
              author (common/resolve-author context)
              ev (evidence/make-start-evidence domain-id sid author)
              base-state {:session-id sid
                          :author author
                          :last-evidence-id (:evidence/id ev)
                          :steps []
                          :current-phase nil
                          :current-cycle-id nil
                          :cycles-completed 0
                          :evidence-store (:evidence-store context)
                          :cycle-config effective-config}
              ;; Domain-specific state initialization
              domain-state (when-let [f (:state-init-fn effective-config)]
                             (f context))
              state (merge base-state domain-state)
              append-err (common/maybe-append-evidence! state ev)]
          (if append-err
            append-err
            {:ok true :state state :evidence ev})))))

  (step [_ state action]
    (let [effective-config (or (:cycle-config state) config)
          result (dispatch-step effective-config spec backend state action)]
      (when (:ok result)
        (bb/project! (:domain-id effective-config) (:state result)))
      result))

  (stop [_ state reason]
    (let [effective-config (or (:cycle-config state) config)
          domain-id (:domain-id effective-config)
          fruit ((:fruit-fn effective-config) state)
          exit-ctx ((:exit-context-fn effective-config) state)
          ev (evidence/make-stop-evidence
              domain-id (:session-id state) (:author state)
              fruit reason (:last-evidence-id state))
          append-err (common/maybe-append-evidence! state ev)]
      (if append-err
        append-err
        {:ok true :context exit-ctx :fruit fruit :evidence ev}))))

;; =============================================================================
;; Factory
;; =============================================================================

(defn make-cycle-peripheral
  "Create a cycle peripheral from a domain config and backend.

   The domain config specifies the phase structure, tool gates, and
   domain-specific behavior. The backend provides tool execution.

   This is the generic factory — domain-specific factories
   (make-proof, make-mission) wrap this with their domain config."
  ([config] (make-cycle-peripheral config (tools/make-mock-backend)))
  ([config backend]
   (when-not (valid-domain-config? config)
     (throw (ex-info "Invalid CycleDomainConfig" {:config config})))
   (->CyclePeripheral config
                       (common/load-spec (:domain-id config))
                       backend))
  ([config spec backend]
   (when-not (valid-domain-config? config)
     (throw (ex-info "Invalid CycleDomainConfig" {:config config})))
   (->CyclePeripheral config spec backend)))
