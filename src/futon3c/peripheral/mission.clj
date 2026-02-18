(ns futon3c.peripheral.mission
  "Mission peripheral — configuration-driven code development cycles.

   Instantiates the generic cycle machine (cycle.clj) with mission-domain
   configuration from mission_shapes.clj.

   The mission peripheral manages the same 9-phase cycle as proof, adapted
   for code development:
   - Obligations instead of ledger items (same DAG, different semantics)
   - Mission spec instead of canonical statement (same versioning)
   - Failed approaches instead of failed routes
   - Table 25 auto-tags per phase (phase-sigil-tags from mission_shapes)

   Futonic logic mapping:
     象 = MissionDomainConfig (enters the cycle machine)
     部 = phase-order (linear phase sequence)
     味 = phase-required-outputs (what each phase must produce)
     🔮 = phase-allowed-tools (tool gating per phase)
     香 = phase-sigil-tags (Table 25 perception tags)
     捨 = stop with reason (set-down when boundary is reached)

   Autoconf: when context includes :mission-spec-path, the autoconf-fn
   reads the spec document and adjusts the config. When no mission is
   specified, the peripheral operates in exploration mode with wider
   tool access."
  (:require [futon3c.peripheral.cycle :as cycle]
            [futon3c.peripheral.mission-backend :as mb]
            [futon3c.peripheral.mission-shapes :as ms]
            [futon3c.peripheral.tools :as tools]))

;; =============================================================================
;; Setup tools — available when no cycle is active
;; =============================================================================

(def setup-tools
  "Tools available when no cycle is active (setup/between cycles).
   Wider access than any single phase — can load, query, and begin cycles."
  #{:mission-load :mission-save :obligation-query :obligation-upsert
    :dag-check :dag-impact :mission-spec-get :mission-spec-update
    :cycle-begin :cycle-list :cycle-get :failed-approach-add
    :evidence-query :corpus-check
    :read :glob :grep :bash-readonly})

;; =============================================================================
;; Autoconf — refine config from mission context
;; =============================================================================

(defn- autoconf
  "Refine domain config from context.
   If context includes :mission-id, the config is left unchanged
   (the agent will load mission state via :mission-load tool).

   Future: when :mission-spec-path is present, parse the spec document
   and derive obligations/scope automatically."
  [context config]
  ;; Currently a pass-through — the mission agent loads state manually.
  ;; The autoconf hook exists so that future refinements (reading
  ;; mission spec documents, deriving scope from file paths, etc.)
  ;; slot in without changing the factory or cycle machine.
  config)

;; =============================================================================
;; Domain state initialization
;; =============================================================================

(defn- state-init
  "Initialize domain-specific state fields from context.
   Adds :mission-id to the base cycle state."
  [context]
  {:mission-id (:mission-id context)})

;; =============================================================================
;; Fruit and exit context
;; =============================================================================

(defn- fruit
  "Extract fruit from mission session state.
   Fruit is the structured output of the constrained session."
  [state]
  {:mission-id (:mission-id state)
   :cycles-completed (:cycles-completed state)
   :steps-taken (count (:steps state))
   :final-phase (:current-phase state)})

(defn- exit-context
  "Extract exit context for hop/resume.
   Minimal context needed to resume or hand off."
  [state]
  {:session-id (:session-id state)
   :mission-id (:mission-id state)})

;; =============================================================================
;; State snapshots — evidence landscape integration
;; =============================================================================

(defn- summarize-obligations
  "Summarize obligations by status for snapshot body."
  [obligations]
  (let [items (vals obligations)]
    {:total (count items)
     :by-status (frequencies (map :item/status items))}))

(defn- state-snapshot
  "Produce an evidence-landscape snapshot when mission state is saved.
   Fires on :mission-save only — the moment state hits disk is when a
   snapshot becomes meaningful (the operational state is now durable).

   Returns nil for other tools (no snapshot emitted)."
  [state tool result]
  (when (= tool :mission-save)
    (let [mission-id (or (:mission/id result) (:mission-id state))
          obligations (:mission/obligations result)
          cycles (:mission/cycles result)
          failed (:mission/failed-approaches result)]
      {:snapshot/subject {:ref/type :mission :ref/id mission-id}
       :snapshot/body {:mission/id mission-id
                       :mission/version (:mission/version result)
                       :obligations (when obligations (summarize-obligations obligations))
                       :cycles-count (count (or cycles []))
                       :failed-approaches-count (count (or failed []))
                       :current-phase (:current-phase state)
                       :current-cycle-id (:current-cycle-id state)
                       :cycles-completed (:cycles-completed state)}
       :snapshot/tags [(keyword "mission" (str mission-id)) :snapshot]})))

;; =============================================================================
;; Table 25 phase tags
;; =============================================================================

(defn- phase-tags
  "Return Table 25 sigil tags for a phase + tool combination.
   These tags are auto-applied to evidence entries during each phase,
   making para-development dimensions queryable."
  [phase _tool]
  (get ms/phase-sigil-tags phase))

;; =============================================================================
;; Domain config
;; =============================================================================

(def mission-domain-config
  "CycleDomainConfig for code development missions.
   This is the 象 (configuration) that enters the cycle machine."
  {:domain-id :mission
   :phase-order ms/phase-order
   :phase-tools ms/phase-allowed-tools
   :setup-tools setup-tools
   :tool-ops ms/mission-tool-operation-kinds
   :required-outputs ms/phase-required-outputs
   :cycle-begin-tool :cycle-begin
   :cycle-advance-tool :cycle-advance
   :state-init-fn state-init
   :fruit-fn fruit
   :exit-context-fn exit-context
   :phase-tags-fn phase-tags
   :autoconf-fn autoconf
   :state-snapshot-fn state-snapshot})

;; =============================================================================
;; Factory
;; =============================================================================

(defn make-mission
  "Create a mission peripheral from optional backend.
   Uses the generic cycle machine with mission-domain configuration.

   Backend should be a MissionBackend (or MockBackend for tests)."
  ([] (make-mission (tools/make-mock-backend)))
  ([backend]
   (cycle/make-cycle-peripheral mission-domain-config backend))
  ([spec backend]
   (cycle/make-cycle-peripheral mission-domain-config spec backend)))
