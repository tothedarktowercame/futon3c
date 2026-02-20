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
;; Tool operation kinds (observe vs action)
;; =============================================================================

(def OperationKind
  "Proof tool operation category.
   :observe tools inspect state/evidence without changing proof state.
   :action tools mutate proof state or execute side-effecting commands."
  [:enum :observe :action])

(def proof-tool-operation-kinds
  "Classification for tools used by the proof peripheral.
   This is used for evidence tagging and invariant checks."
  {:proof-load :observe
   :proof-save :action
   :ledger-query :observe
   :ledger-upsert :action
   :dag-check :observe
   :dag-impact :observe
   :canonical-get :observe
   :canonical-update :action
   :cycle-begin :action
   :cycle-advance :action
   :cycle-get :observe
   :cycle-list :observe
   :failed-route-add :action
   :status-validate :observe
   :gate-check :observe
   :tryharder-license :action
   :proof-mode-get :observe
   :proof-mode-set :action
   :corpus-check :observe
   :read :observe
   :glob :observe
   :grep :observe
   :bash :action
   :bash-readonly :observe
   :write :action})

(defn tool-operation-kind
  "Return the operation kind (:observe | :action) for TOOL, or nil if unknown."
  [tool]
  (get proof-tool-operation-kinds tool))

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
   [:route/structural-obstruction :string]
   [:route/failure-reason :string]
   [:route/evidence-refs [:vector :string]]
   [:route/recorded-at :string]])

;; =============================================================================
;; Proof mode — high-level protocol mode (FrontierMath 5-mode workflow)
;; =============================================================================

(def ProofMode
  "High-level proof mode from the FrontierMath protocol.
   SPEC: lock problem statement. FALSIFY: mandatory opposite-answer cycle.
   CONSTRUCT: build proof routes. VERIFY: validate fragments. MAP: survey landscape."
  [:enum :SPEC :FALSIFY :CONSTRUCT :VERIFY :MAP])

(def BottleneckType
  "Classification of why a TryHarder license is needed."
  [:enum :missing-lemma :obstruction-search :spec-ambiguity
   :dependency-gap :computation-gap])

;; =============================================================================
;; TryHarder license — gates persistence loops
;; =============================================================================

(def TryHarderLicense
  "A signed license permitting one more persistence cycle.
   Required when re-attempting a blocker. New lever must be materially different.
   Kill condition forces mode switch when met."
  [:map
   [:license/id :string]
   [:license/problem-id :string]
   [:license/target-claim :string]
   [:license/bottleneck-type BottleneckType]
   [:license/new-lever :string]
   [:license/witness :string]
   [:license/kill-condition :string]
   [:license/timebox-minutes :int]
   [:license/issued-at :string]
   [:license/outcome {:optional true}
    [:map
     [:witness-met :boolean]
     [:mode-after [:maybe ProofMode]]
     [:notes :string]]]])

;; =============================================================================
;; Valid proof mode transitions
;; =============================================================================

(def proof-mode-transitions
  "Valid mode transitions. FALSIFY is mandatory before CONSTRUCT."
  {:SPEC      #{:FALSIFY}
   :FALSIFY   #{:CONSTRUCT}
   :CONSTRUCT #{:VERIFY}
   :VERIFY    #{:MAP}
   :MAP       #{:SPEC}})

(defn valid-mode-transition?
  "Check if a mode transition is valid."
  [from to]
  (contains? (get proof-mode-transitions from) to))

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
   [:proof/updated-at :string]
   [:proof/current-mode {:optional true} ProofMode]
   [:proof/falsify-completed? {:optional true} :boolean]
   [:proof/tryharder-log {:optional true} [:vector TryHarderLicense]]
   [:proof/active-license {:optional true} [:maybe TryHarderLicense]]])

;; =============================================================================
;; Phase tool restrictions
;; =============================================================================

(def phase-allowed-tools
  "Tools available in each cycle phase.
   Enforcement is structural: tools outside this set are rejected.
   :cycle-advance and :cycle-get are available in all active phases
   (they are the mechanism for phase transitions and inspection)."
  {:observe   #{:ledger-query :dag-impact :corpus-check :read :grep :glob
                :bash-readonly :cycle-advance :cycle-get
                :tryharder-license :proof-mode-get}
   :propose   #{:ledger-query :dag-impact :corpus-check :read :grep :glob
                :bash-readonly :cycle-advance :cycle-get :proof-mode-get}
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
