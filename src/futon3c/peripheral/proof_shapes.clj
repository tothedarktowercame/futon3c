(ns futon3c.peripheral.proof-shapes
  "Malli shapes for the proof domain.

   These shapes define the data contracts for proof state persistence:
   ledger items, cycle records, failed routes, canonical statements,
   and the composite ProofState.

   Status policy is structural: only the five allowed statuses pass
   validation. Evidence class constrains what status transitions are
   legal (e.g. numerical evidence cannot yield :proved).

   Derived from: futon6/data/first-proof/proof-strategy-cycle-requirements.md"
  (:require [malli.core :as m]
            [malli.error :as me]))

;; =============================================================================
;; Enumerations
;; =============================================================================

(def ItemStatus
  "Ledger item status — enforced, not conventional (SR-2).
   Nothing else is accepted. 'basically solved' → rejected."
  [:enum :proved :partial :open :false :numerically-verified])

(def EvidenceClass
  "Evidence classification for a ledger item (SR-5).
   Constrains valid status transitions: :numerical evidence
   cannot produce :proved status."
  [:enum :analytical :numerical :mixed])

(def CyclePhase
  "Cycle phase in the proof development state machine (CR-1).
   Phases must be traversed in order; no skipping."
  [:enum :observe :propose :execute :validate
   :classify :integrate :commit :gate-review :completed])

(def CycleResultStatus
  "Result status for a completed cycle."
  [:enum :proved :partial :numerically-verified :falsified :inconclusive])

;; =============================================================================
;; Ledger item — a single proof obligation (SR-2, SR-3, SR-5, SR-8)
;; =============================================================================

(def LedgerItem
  "A single entry in the proof ledger — one obligation or sub-goal.
   SR-2: tracked with enforced status policy.
   SR-3: DAG edges via depends-on/unlocks.
   SR-5: evidence-type determines allowed transitions.
   SR-8: failure-point is append-only (honesty)."
  [:map
   [:item/id :string]
   [:item/label :string]
   [:item/status ItemStatus]
   [:item/depends-on [:set :string]]
   [:item/unlocks [:set :string]]
   [:item/evidence-type {:optional true} EvidenceClass]
   [:item/artifact-paths [:vector :string]]
   [:item/failure-point {:optional true} [:maybe :string]]])

;; =============================================================================
;; Canonical statement — the problem being proved (SR-1)
;; =============================================================================

(def CanonicalStatement
  "The canonical problem statement with version tracking (SR-1).
   Every change records a version bump and hash for traceability."
  [:map
   [:statement :string]
   [:closure-criterion :string]
   [:statement-hash :string]
   [:version-history [:vector [:map
                               [:version :int]
                               [:statement :string]
                               [:hash :string]
                               [:changed-at :string]
                               [:reason :string]]]]])

;; =============================================================================
;; Cycle record — one pass through the 8-phase cycle (CR-1..8)
;; =============================================================================

(def CycleRecord
  "Record of a single proof cycle traversal.
   CR-1: must begin with observe, proceed in order.
   CR-8: gate-review checks G5-G0 before completing."
  [:map
   [:cycle/id :string]
   [:cycle/blocker-id :string]
   [:cycle/phase CyclePhase]
   [:cycle/result-status {:optional true} CycleResultStatus]
   [:cycle/phases-completed [:vector CyclePhase]]
   [:cycle/phase-data [:map-of CyclePhase :any]]
   [:cycle/started-at :string]
   [:cycle/updated-at :string]])

;; =============================================================================
;; Failed route — append-only record of dead ends (SR-8)
;; =============================================================================

(def FailedRoute
  "A failed proof attempt — recorded for honesty (SR-8).
   Append-only: cannot be deleted or modified once recorded."
  [:map
   [:route/id :string]
   [:route/blocker-id :string]
   [:route/approach :string]
   [:route/failure-reason :string]
   [:route/evidence-refs [:vector :string]]
   [:route/recorded-at :string]])

;; =============================================================================
;; Composite proof state — persisted per problem
;; =============================================================================

(def ProofState
  "Complete proof state for a single problem.
   Persisted as EDN at data/proof-state/{problem-id}.edn."
  [:map
   [:proof/problem-id :string]
   [:proof/version :int]
   [:proof/canonical CanonicalStatement]
   [:proof/ledger [:map-of :string LedgerItem]]
   [:proof/cycles [:vector CycleRecord]]
   [:proof/failed-routes [:vector FailedRoute]]
   [:proof/updated-at :string]])

;; =============================================================================
;; Phase tool restrictions
;; =============================================================================

(def phase-allowed-tools
  "Tools available in each cycle phase.
   Enforcement is structural: tools outside this set are rejected.
   :cycle-advance and :cycle-get are available in all active phases
   (they are the mechanism for phase transitions and inspection)."
  {:observe   #{:ledger-query :dag-impact :read :grep :glob :bash-readonly
                :cycle-advance :cycle-get}
   :propose   #{:ledger-query :dag-impact :read :grep :glob :bash-readonly
                :cycle-advance :cycle-get}
   :execute   #{:read :write :bash :glob :grep
                :cycle-advance :cycle-get}
   :validate  #{:read :bash :bash-readonly :glob :grep
                :cycle-advance :cycle-get}
   :classify  #{:status-validate :ledger-query :read
                :cycle-advance :cycle-get}
   :integrate #{:ledger-upsert :canonical-update :write :dag-check :failed-route-add
                :cycle-advance :cycle-get}
   :commit    #{:proof-save :read
                :cycle-advance :cycle-get}
   :gate-review #{:gate-check :ledger-query :dag-check :read
                  :cycle-advance :cycle-get}
   :completed #{}})

;; =============================================================================
;; Phase transition requirements
;; =============================================================================

(def phase-order
  "The canonical order of cycle phases."
  [:observe :propose :execute :validate :classify :integrate :commit :gate-review :completed])

(def phase-transitions
  "Valid phase transitions — each phase can only advance to the next."
  (into {} (map vector phase-order (rest phase-order))))

(def phase-required-outputs
  "Required outputs before a phase can advance (CR-1..8).
   Keys are the phase being left; values are required keys in phase-data."
  {:observe   #{:blocker-id}
   :propose   #{:approach}
   :execute   #{:artifacts}
   :validate  #{:validation-artifacts}
   :classify  #{:classification}
   :integrate #{:rationale :ledger-changes}
   :commit    #{:saved?}
   :gate-review #{:gates-passed}})

;; =============================================================================
;; Gate checklist (CR-8: G5-G0)
;; =============================================================================

(def mandatory-gates
  "Gates that must pass before a cycle can complete (CR-8).
   Mapped from the G5-G0 gate pipeline."
  #{:G5-scope     ;; blocker matches canonical
    :G4-evidence  ;; artifacts exist and are referenceable
    :G3-status    ;; status transition is valid
    :G2-dag       ;; DAG is acyclic after changes
    :G1-ledger    ;; ledger is consistent
    :G0-commit})  ;; state was persisted

;; =============================================================================
;; Validation helpers
;; =============================================================================

(defn validate
  "Validate data against a proof shape. Returns nil on success, error map on failure."
  [shape data]
  (when-not (m/validate shape data)
    {:error (me/humanize (m/explain shape data))
     :shape (m/form shape)}))

(defn valid?
  "Returns true if data conforms to proof shape."
  [shape data]
  (m/validate shape data))

(defn valid-status?
  "Check if a status keyword is in the allowed set."
  [status]
  (contains? #{:proved :partial :open :false :numerically-verified} status))

(defn valid-status-transition?
  "Check if a status transition is valid given the evidence class (SR-5).
   Key rule: :numerical evidence cannot yield :proved."
  [from to evidence-type]
  (cond
    ;; Can't transition to :proved with only numerical evidence
    (and (= to :proved) (= evidence-type :numerical))
    false

    ;; Both statuses must be valid
    (not (valid-status? from))
    false

    (not (valid-status? to))
    false

    ;; :false is terminal — can't transition away from it
    ;; (but can transition TO it)
    (= from :false)
    false

    :else true))
