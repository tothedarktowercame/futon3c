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
            [futon3c.peripheral.proof-backend :as pb]
            [futon3c.peripheral.proof-shapes :as ps]
            [futon3c.peripheral.real-backend]
            [futon3c.peripheral.tools :as tools])
  (:import [futon3c.peripheral.proof_backend ProofBackend]
           [futon3c.peripheral.real_backend RealBackend]))

;; =============================================================================
;; Setup tools — available when no cycle is active
;; =============================================================================

(def setup-tools
  "Tools available when no cycle is active (setup/between cycles)."
  #{:proof-load :proof-save :ledger-query :ledger-upsert
    :dag-check :dag-impact :canonical-get :canonical-update
    :cycle-begin :cycle-list :cycle-get :failed-route-add
    :tryharder-license :proof-mode-get :proof-mode-set
    :read :glob :grep :bash-readonly})

;; =============================================================================
;; Domain state initialization
;; =============================================================================

(defn- state-init
  "Initialize proof-specific state fields from context.
   Adds :problem-id and loads proof mode/license state from persisted EDN."
  [context]
  (let [problem-id (:problem-id context)
        base {:problem-id problem-id}]
    (if problem-id
      (let [state-file (java.io.File.
                         (str "data/proof-state/" problem-id ".edn"))]
        (if (.exists state-file)
          (try
            (let [pstate (read-string (slurp state-file))]
              (merge base
                     {:proof/current-mode (:proof/current-mode pstate :SPEC)
                      :proof/falsify-completed? (:proof/falsify-completed? pstate false)
                      :proof/tryharder-log (:proof/tryharder-log pstate [])
                      :proof/active-license (:proof/active-license pstate)}))
            (catch Throwable _ base))
          base))
      base)))

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
;; State snapshot — return snapshot map on :proof-save
;; =============================================================================

(defn- state-snapshot-fn
  "Return a snapshot map when proof state is saved.
   The cycle machine handles emitting this as evidence.
   Returns {:snapshot/subject ... :snapshot/body ... :snapshot/tags ...} or nil."
  [state tool result]
  (when (and (= tool :proof-save) (:ok result))
    {:snapshot/subject {:ref/type :peripheral :ref/id "proof"}
     :snapshot/body {:snapshot :proof-save
                     :problem-id (:problem-id state)
                     :phase (:current-phase state)
                     :cycles-completed (:cycles-completed state)
                     :step-count (count (:steps state))
                     :mode (:proof/current-mode state)}
     :snapshot/tags [:proof :snapshot]}))

;; =============================================================================
;; Autoconf — refine domain config from problem context
;; =============================================================================

(defn- autoconf-fn
  "Refine proof domain config from context.
   Autoconf takes (context, config) and returns a refined config."
  [context config]
  config)

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
   :state-snapshot-fn state-snapshot-fn
   :autoconf-fn autoconf-fn
   :fruit-fn fruit
   :exit-context-fn exit-context})

;; =============================================================================
;; Factory
;; =============================================================================

(defn- ensure-proof-backend
  "Wrap a RealBackend in a ProofBackend so proof-domain tools are available.
   ProofBackend and MockBackend pass through unchanged."
  [backend]
  (if (instance? RealBackend backend)
    (let [home (System/getProperty "user.home")
          venv-python (str home "/code/futon3a/.venv/bin/python3")]
      (pb/make-proof-backend
        {:cwd (System/getProperty "user.dir")
         :futon3a-python (if (.exists (java.io.File. venv-python))
                           venv-python
                           "python3")}
        backend))
    backend))

(defn make-proof
  "Create a proof peripheral with injected backend.
   Uses the generic cycle machine with proof-domain configuration.

   If a RealBackend is passed (e.g. from the dispatch router), it is
   automatically wrapped in a ProofBackend so that proof-domain
   tools (:ledger-load, :cycle-begin, etc.) are available."
  ([] (make-proof (tools/make-mock-backend)))
  ([backend]
   (cycle/make-cycle-peripheral proof-domain-config (ensure-proof-backend backend)))
  ([spec backend]
   (cycle/make-cycle-peripheral proof-domain-config spec (ensure-proof-backend backend))))
