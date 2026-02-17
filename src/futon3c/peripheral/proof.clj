(ns futon3c.peripheral.proof
  "Proof peripheral implementation — structurally-enforced proof cycles.

   The proof peripheral manages the 9-phase cycle state machine:
   observe → propose → execute → validate → classify → integrate →
   commit → gate-review → completed

   Phase gating restricts which tools are available in each phase.
   The agent enters :proof mode, works with proof-specific tools,
   and cannot advance a cycle without satisfying gate criteria.

   Key insight from sexpr-peripheral-design-note:
   'the paren IS the gate' — generation and checking are the same act.

   Fruit: {:problem-id str :cycles-completed int :ledger-summary {...}}
   Exit context: {:session-id str :problem-id str}"
  (:require [futon3c.peripheral.common :as common]
            [futon3c.peripheral.evidence :as evidence]
            [futon3c.peripheral.proof-shapes :as ps]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]))

;; =============================================================================
;; Phase-gated tool dispatch
;; =============================================================================

(defn- current-phase-tools
  "Get the set of tools allowed in the current cycle phase.
   When no cycle is active, a base set is available for setup."
  [state]
  (if-let [phase (:current-phase state)]
    (get ps/phase-allowed-tools phase #{})
    ;; No active cycle — allow setup tools
    #{:proof-load :proof-save :ledger-query :ledger-upsert
      :dag-check :dag-impact :canonical-get :canonical-update
      :cycle-begin :cycle-list :cycle-get :failed-route-add
      :read :glob :grep :bash-readonly}))

(defn- phase-allows-tool?
  "Check if the current phase allows the given tool."
  [state tool]
  (contains? (current-phase-tools state) tool))

(defn- dispatch-step
  "Dispatch a single action through phase gating and the backend."
  [spec backend state action]
  (if-let [err (common/validate-action :proof action)]
    err
    (let [{:keys [tool args]} (common/normalize-action action)]
      (cond
        ;; Phase gating: reject tools not allowed in current phase
        (not (phase-allows-tool? state tool))
        (runner/runner-error :proof :phase-tool-not-allowed
                             (str "Tool " tool " is not allowed in phase "
                                  (or (:current-phase state) :setup))
                             :tool tool
                             :phase (or (:current-phase state) :setup)
                             :allowed (vec (current-phase-tools state)))

        ;; Observe/action classification must be total for allowed proof tools.
        (nil? (ps/tool-operation-kind tool))
        (runner/runner-error :proof :unclassified-proof-tool
                             (str "Tool " tool " has no observe/action classification")
                             :tool tool)

        :else
        (let [operation-kind (ps/tool-operation-kind tool)
              dispatch-result (tools/dispatch-tool tool args spec backend)]
          (cond
            (common/social-error? dispatch-result)
            dispatch-result

            (not (:ok dispatch-result))
            (runner/runner-error :proof :tool-execution-failed
                                 "Proof tool execution failed"
                                 :tool tool
                                 :args args
                                 :result dispatch-result)

            :else
            (let [result (:result dispatch-result)
                  ev (assoc-in
                      (evidence/make-step-evidence
                       :proof (:session-id state) (:author state)
                       tool args result (:last-evidence-id state))
                      [:evidence/body :proof/operation-kind]
                      operation-kind)
                  ;; Track phase transitions from cycle-advance results
                  new-phase (when (= tool :cycle-advance)
                              (:cycle/phase result))
                  new-cycle-id (when (= tool :cycle-begin)
                                 (:cycle/id result))
                  new-state (cond-> state
                              true (assoc :last-evidence-id (:evidence/id ev))
                              true (update :steps conj {:tool tool :args args :result result})
                              new-phase (assoc :current-phase new-phase)
                              new-cycle-id (assoc :current-cycle-id (:cycle/id result)
                                                  :current-phase :observe)
                              ;; When cycle completes, clear active cycle
                              (= new-phase :completed)
                              (-> (dissoc :current-phase :current-cycle-id)
                                  (update :cycles-completed inc)))
                  append-err (common/maybe-append-evidence! new-state ev)]
              (if append-err
                append-err
                {:ok true
                 :state new-state
                 :result result
                 :evidence ev}))))))))

;; =============================================================================
;; ProofPeripheral record
;; =============================================================================

(defrecord ProofPeripheral [spec backend]
  runner/PeripheralRunner
  (start [_ context]
    (if-let [err (runner/validate-context :proof context #{:session-id})]
      err
      (let [sid (:session-id context)
            author (common/resolve-author context)
            ev (evidence/make-start-evidence :proof sid author)
            state {:session-id sid
                   :author author
                   :last-evidence-id (:evidence/id ev)
                   :steps []
                   :problem-id (:problem-id context)
                   :current-phase nil       ;; No active cycle yet
                   :current-cycle-id nil
                   :cycles-completed 0
                   :evidence-store (:evidence-store context)}
            append-err (common/maybe-append-evidence! state ev)]
        (if append-err
          append-err
          {:ok true :state state :evidence ev}))))

  (step [_ state action]
    (dispatch-step spec backend state action))

  (stop [_ state reason]
    (let [fruit {:problem-id (:problem-id state)
                 :cycles-completed (:cycles-completed state)
                 :steps-taken (count (:steps state))
                 :final-phase (:current-phase state)}
          ev (evidence/make-stop-evidence
              :proof
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

(defn make-proof
  "Create a proof peripheral with injected backend.
   Backend should be a ProofBackend (or MockBackend for tests)."
  ([] (make-proof (tools/make-mock-backend)))
  ([backend]
   (->ProofPeripheral (common/load-spec :proof) backend))
  ([spec backend]
   (->ProofPeripheral spec backend)))
