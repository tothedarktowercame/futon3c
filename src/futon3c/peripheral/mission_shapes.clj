(ns futon3c.peripheral.mission-shapes
  "Malli shapes for the code mission domain.

   These shapes define the data contracts for mission state: obligations
   (DAG-tracked tasks), cycle records, failed approaches, and the
   composite MissionState.

   Derived from proof-shapes.clj by generalization: the proof domain's
   obligations, cycles, and failed routes map onto code mission equivalents.

   Theoretical anchoring:
   - Table 24 (Corneli 2014): X=project, P=problem, S=solution, H=heuristic
   - Futonic logic: 象=MissionState, 部=phase-order, 味=required-outputs
   - mission-lifecycle.flexiarg: :greenfield → :scoped → :active → :done"
  (:require [clojure.set]
            [malli.core :as m]
            [malli.error :as me]))

;; =============================================================================
;; Enumerations
;; =============================================================================

(def ObligationStatus
  "Mission obligation status — tracked and enforced.
   Maps to mission-lifecycle states at the obligation level."
  [:enum :done :partial :open :blocked :abandoned])

(def EvidenceClass
  "How the evidence for this obligation was obtained.
   Constrains what status transitions are credible."
  [:enum :test :review :assertion :mixed])

(def CyclePhase
  "Cycle phase in the code mission state machine.
   Same 9 phases as proof, adapted for code development.
   Traversed in order; no skipping."
  [:enum :observe :propose :execute :validate
   :classify :integrate :commit :gate-review :completed])

(def CycleResultStatus
  "Result status for a completed mission cycle."
  [:enum :done :partial :blocked :abandoned :inconclusive])

;; =============================================================================
;; Tool operation kinds
;; =============================================================================

(def OperationKind
  [:enum :observe :action])

(def mission-tool-operation-kinds
  "Classification for tools used by the mission peripheral.
   Extends the proof tool set with mission-specific tools."
  {;; Mission-specific tools (observe)
   :mission-load :observe
   :mission-wiring :observe
   :mission-doc-audit :observe
   :obligation-query :observe
   :dag-check :observe
   :dag-impact :observe
   :mission-spec-get :observe
   :cycle-get :observe
   :cycle-list :observe
   :status-validate :observe
   :gate-check :observe
   :corpus-check :observe
   :evidence-query :observe
   ;; Mission-specific tools (action)
   :mission-save :action
   :obligation-upsert :action
   :mission-spec-update :action
   :cycle-begin :action
   :cycle-advance :action
   :failed-approach-add :action
   ;; Delegated tools (same as proof)
   :read :observe
   :glob :observe
   :grep :observe
   :bash-readonly :observe
   :bash :action
   :write :action})

(defn tool-operation-kind [tool]
  (get mission-tool-operation-kinds tool))

;; =============================================================================
;; Obligation — a single mission task (DAG node)
;; =============================================================================

(def Obligation
  "A single obligation in the mission ledger.
   Uses the same DAG structure as proof LedgerItem (proof_dag.clj
   algorithms work unchanged).

   Table 24 mapping: P (problem) or S (solution) depending on status."
  [:map
   [:item/id :string]
   [:item/label :string]
   [:item/status ObligationStatus]
   [:item/depends-on [:set :string]]
   [:item/unlocks [:set :string]]
   [:item/evidence-type {:optional true} EvidenceClass]
   [:item/artifact-paths [:vector :string]]
   [:item/owner {:optional true} :string]
   [:item/tags {:optional true} [:vector :keyword]]
   [:item/failure-reason {:optional true} [:maybe :string]]])

;; =============================================================================
;; Mission spec — what the mission is trying to achieve
;; =============================================================================

(def MissionSpec
  "The mission specification with version tracking.
   Analogous to proof's CanonicalStatement.
   Maps to mission-scoping.flexiarg requirements."
  [:map
   [:title :string]
   [:success-criteria [:vector :string]]
   [:scope-in [:vector :string]]
   [:scope-out [:vector :string]]
   [:owner {:optional true} :string]
   [:version :int]
   [:version-history [:vector [:map
                                [:version :int]
                                [:title :string]
                                [:changed-at :string]
                                [:reason :string]]]]])

;; =============================================================================
;; Cycle record — one pass through the phase machine
;; =============================================================================

(def CycleRecord
  "Record of a single mission cycle traversal.
   Each cycle targets one obligation (the blocker)."
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
;; Failed approach — append-only record of dead ends
;; =============================================================================

(def FailedApproach
  "A failed approach to an obligation — recorded for learning.
   Append-only: cannot be deleted or modified.
   Table 25 mapping: ♔ constructive feedback, 🧠 comprehension."
  [:map
   [:approach/id :string]
   [:approach/obligation-id :string]
   [:approach/strategy :string]
   [:approach/failure-reason :string]
   [:approach/rationale :string]
   [:approach/evidence-refs [:vector :string]]
   [:approach/recorded-at :string]])

;; =============================================================================
;; Composite mission state
;; =============================================================================

(def MissionState
  "Complete mission state — persisted per mission.
   象 in futonic logic: a structured whole that can be referenced
   without full expansion."
  [:map
   [:mission/id :string]
   [:mission/version :int]
   [:mission/spec MissionSpec]
   [:mission/obligations [:map-of :string Obligation]]
   [:mission/cycles [:vector CycleRecord]]
   [:mission/failed-approaches [:vector FailedApproach]]
   [:mission/updated-at :string]])

;; =============================================================================
;; Phase configuration — tool restrictions and requirements
;; =============================================================================

(def phase-order
  [:observe :propose :execute :validate :classify :integrate :commit :gate-review :completed])

(def phase-allowed-tools
  "Tools available in each cycle phase for code missions.
   Key differences from proof:
   - :observe adds :evidence-query (query evidence landscape)
   - :execute adds :bash (run tests, build, etc.)
   - :classify adds :obligation-query (check obligation state)
   - :integrate adds :obligation-upsert (update DAG)
   - :commit uses :mission-save instead of :proof-save"
  {:observe    #{:obligation-query :dag-impact :corpus-check :evidence-query
                 :mission-wiring :mission-doc-audit
                 :read :grep :glob :bash-readonly
                 :cycle-advance :cycle-get}
   :propose    #{:obligation-query :dag-impact :corpus-check :evidence-query
                 :mission-doc-audit
                 :read :grep :glob :bash-readonly
                 :cycle-advance :cycle-get}
   :execute    #{:read :write :bash :glob :grep
                 :cycle-advance :cycle-get}
   :validate   #{:read :bash :bash-readonly :glob :grep
                 :cycle-advance :cycle-get}
   :classify   #{:status-validate :obligation-query :read
                 :cycle-advance :cycle-get}
   :integrate  #{:obligation-upsert :mission-spec-update :write
                 :dag-check :failed-approach-add
                 :cycle-advance :cycle-get}
   :commit     #{:mission-save :read
                 :cycle-advance :cycle-get}
   :gate-review #{:gate-check :mission-doc-audit
                  :obligation-query :dag-check :read
                  :cycle-advance :cycle-get}
   :completed  #{}})

(def phase-required-outputs
  "Required outputs before a phase can advance.
   Same structure as proof, adapted for code missions."
  {:observe    #{:blocker-id}
   :propose    #{:approach}
   :execute    #{:artifacts}
   :validate   #{:validation-artifacts}
   :classify   #{:classification}
   ;; Post-INSTANTIATE DOCUMENT checkpoint:
   ;; every cycle must record documentation artifacts and a hypergraph plan.
   :integrate  #{:rationale :obligation-changes :doc-artifacts :hypergraph-plan}
   :commit     #{:saved?}
   :gate-review #{:gates-passed}})

;; =============================================================================
;; Table 25 auto-tags per phase
;; =============================================================================

(def phase-sigil-tags
  "Table 25 (Corneli 2014) sigils auto-applied per phase.
   These tags are added to evidence entries emitted during each phase,
   making the para-development dimensions queryable without manual
   annotation."
  {:observe    [:sigil/getting-information :sigil/perception]
   :propose    [:sigil/argumentation :sigil/intuition]
   :execute    [:sigil/software :sigil/written-language]
   :validate   [:sigil/logic-deduction :sigil/concrete-applications]
   :classify   [:sigil/personal-comprehension :sigil/self-discovery]
   :integrate  [:sigil/collaborative-knowledge :sigil/organization]
   :commit     [:sigil/consistency :sigil/gradual-accumulation]
   :gate-review [:sigil/quality :sigil/constructive-feedback]})

;; =============================================================================
;; Validation helpers
;; =============================================================================

(defn validate [shape data]
  (when-not (m/validate shape data)
    {:error (me/humanize (m/explain shape data))
     :shape (m/form shape)}))

(defn valid? [shape data]
  (m/validate shape data))

(defn valid-status? [status]
  (contains? #{:done :partial :open :blocked :abandoned} status))

;; =============================================================================
;; Prediction enrichment (D-3 in M-aif-head)
;; =============================================================================

(defn compute-prediction-divergence
  "Compare :propose prediction against :execute actuals.
   Returns [0,1] divergence score.

   Structured predictions make cross-phase learning computable
   (agent/state-is-hypothesis, aif/belief-state-operational-hypotheses).
   Without structured predictions, returns nil (no divergence signal)."
  [propose-data execute-data]
  (when (and (map? propose-data) (map? execute-data))
    (let [pred-artifacts (set (:predicted-artifacts propose-data))
          actual-artifacts (set (:artifacts execute-data))
          all-artifacts (clojure.set/union pred-artifacts actual-artifacts)
          shared (clojure.set/intersection pred-artifacts actual-artifacts)
          artifact-div (if (empty? pred-artifacts) nil
                         (/ (double (- (count all-artifacts) (count shared)))
                            (max 1.0 (double (count all-artifacts)))))
          criteria (vec (:success-criteria propose-data))
          criteria-met (when (seq criteria)
                         (count (filter (fn [c]
                                          ;; Check if criterion text appears in evidence
                                          (some #(and (string? %)
                                                      (.contains ^String % (str c)))
                                                (:validation-artifacts execute-data)))
                                        criteria)))
          criteria-total (count criteria)
          criteria-div (when (and criteria-met (pos? criteria-total))
                         (- 1.0 (/ (double criteria-met) (double criteria-total))))]
      ;; Return weighted average of available signals, or nil if no signals
      (cond
        (and artifact-div criteria-div)
        (* 0.5 (+ artifact-div criteria-div))

        artifact-div artifact-div
        criteria-div criteria-div
        :else nil))))

(defn valid-status-transition?
  "Check if a status transition is valid given the evidence class.
   Key rule: :assertion evidence alone cannot yield :done."
  [from to evidence-type]
  (cond
    (and (= to :done) (= evidence-type :assertion)) false
    (not (valid-status? from)) false
    (not (valid-status? to)) false
    (= from :abandoned) false   ;; terminal
    :else true))
