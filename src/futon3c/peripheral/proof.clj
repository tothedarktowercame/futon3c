(ns futon3c.peripheral.proof
  "Proof peripheral — structurally-enforced proof cycles.

   Instantiates the generic cycle machine (cycle.clj) with proof-domain
   configuration from proof_shapes.clj.

   The proof peripheral manages the 9-phase cycle state machine:
   observe → propose → execute → validate → classify → integrate →
   commit → gate-review → completed

   Phase gating restricts which tools are available in each phase.
   The agent enters :proof mode, works with proof-specific tools,
   and cannot advance a cycle without satisfying gate criteria.

   Key insight from sexpr-peripheral-design-note:
   'the paren IS the gate' — generation and checking are the same act.

   Fruit: {:problem-id str :cycles-completed int :steps-taken int :final-phase kw}
   Exit context: {:session-id str :problem-id str}

   INSTANTIATE: This file was refactored from a standalone ProofPeripheral
   record to a thin wrapper around the generic cycle machine. The original
   dispatch-step, phase gating, evidence enrichment, and state tracking
   logic now live in cycle.clj and are parameterized by proof-domain-config."
  (:require [futon3c.peripheral.cycle :as cycle]
            [futon3c.peripheral.proof-shapes :as ps]
            [futon3c.peripheral.tools :as tools]))

;; =============================================================================
;; Setup tools — available when no cycle is active
;; =============================================================================

(def setup-tools
  "Tools available when no cycle is active (setup/between cycles)."
  #{:proof-load :proof-save :ledger-query :ledger-upsert
    :dag-check :dag-impact :canonical-get :canonical-update
    :cycle-begin :cycle-list :cycle-get :failed-route-add
    :read :glob :grep :bash-readonly})

;; =============================================================================
;; Domain state initialization
;; =============================================================================

(defn- state-init
  "Initialize proof-specific state fields from context.
   Adds :problem-id to the base cycle state."
  [context]
  {:problem-id (:problem-id context)})

;; =============================================================================
;; Fruit and exit context
;; =============================================================================

(defn- fruit
  "Extract fruit from proof session state."
  [state]
  {:problem-id (:problem-id state)
   :cycles-completed (:cycles-completed state)
   :steps-taken (count (:steps state))
   :final-phase (:current-phase state)})

(defn- exit-context
  "Extract exit context for hop/resume."
  [state]
  {:session-id (:session-id state)
   :problem-id (:problem-id state)})

;; =============================================================================
;; Domain config
;; =============================================================================

(def proof-domain-config
  "CycleDomainConfig for mathematical proof development.
   This is the 象 (configuration) that enters the cycle machine."
  {:domain-id :proof
   :phase-order ps/phase-order
   :phase-tools ps/phase-allowed-tools
   :setup-tools setup-tools
   :tool-ops ps/proof-tool-operation-kinds
   :required-outputs ps/phase-required-outputs
   :cycle-begin-tool :cycle-begin
   :cycle-advance-tool :cycle-advance
   :state-init-fn state-init
   :fruit-fn fruit
   :exit-context-fn exit-context})

;; =============================================================================
;; Factory
;; =============================================================================

(defn make-proof
  "Create a proof peripheral with injected backend.
   Uses the generic cycle machine with proof-domain configuration.

   Backend should be a ProofBackend (or MockBackend for tests)."
  ([] (make-proof (tools/make-mock-backend)))
  ([backend]
   (cycle/make-cycle-peripheral proof-domain-config backend))
  ([spec backend]
   (cycle/make-cycle-peripheral proof-domain-config spec backend)))
