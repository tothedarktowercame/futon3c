(ns futon3c.peripheral.mission-control-shapes
  "Domain shapes for mission control peripheral.

   Mission control operates at the portfolio level — reading mission
   inventories, devmap coverage, and mana state to produce portfolio
   reviews as evidence entries.

   Key shapes:
   - MissionStatus — classified status of a mission file
   - MissionEntry — parsed mission from .md file or devmap EDN
   - DevmapSummary — summary of a devmap with component coverage
   - PortfolioReview — the main output shape: a full portfolio assessment")

;; =============================================================================
;; Mission status classification
;; =============================================================================

(def MissionStatus
  "Classified status of a mission. Derived from parsing **Status:** headers
   or :mission/state EDN keywords."
  [:enum :complete :blocked :ready :in-progress :unknown])

(def MissionSource
  "Where the mission data came from."
  [:enum :md-file :devmap-edn :evidence-snapshot])

;; =============================================================================
;; Mission inventory entry
;; =============================================================================

(def MissionEntry
  "A single mission as seen by the inventory scanner."
  [:map
   [:mission/id :string]
   [:mission/status MissionStatus]
   [:mission/source MissionSource]
   [:mission/repo {:optional true} :string]
   [:mission/path {:optional true} :string]
   [:mission/date {:optional true} :string]
   [:mission/blocked-by {:optional true} :string]
   [:mission/raw-status {:optional true} :string]
   [:mission/devmap-id {:optional true} :keyword]])

;; =============================================================================
;; Devmap coverage
;; =============================================================================

(def DevmapComponent
  "A component from a wiring diagram."
  [:map
   [:component/id :keyword]
   [:component/name :string]])

(def DevmapSummary
  "Summary of a wiring diagram with validation and coverage info."
  [:map
   [:devmap/id :keyword]
   [:devmap/state :keyword]
   [:devmap/input-count :int]
   [:devmap/output-count :int]
   [:devmap/component-count :int]
   [:devmap/edge-count :int]
   [:devmap/all-valid :boolean]
   [:devmap/failed-checks [:vector :keyword]]
   [:devmap/components [:vector DevmapComponent]]])

(def CoverageEntry
  "One devmap's coverage: which components have missions."
  [:map
   [:coverage/devmap-id :keyword]
   [:coverage/total-components :int]
   [:coverage/covered-components :int]
   [:coverage/uncovered [:vector :keyword]]
   [:coverage/coverage-pct :double]])

;; =============================================================================
;; Mana state (read-only view)
;; =============================================================================

(def ManaSnapshot
  "Read-only snapshot of the mana pool state."
  [:map
   [:mana/available :boolean]
   [:mana/pool-balance {:optional true} :double]
   [:mana/total-donated {:optional true} :double]
   [:mana/total-funded {:optional true} :double]
   [:mana/active-proposals {:optional true} :int]])

;; =============================================================================
;; Portfolio review (the main output)
;; =============================================================================

(def PortfolioReview
  "A complete portfolio review — the fruit of a mission control session."
  [:map
   [:portfolio/missions [:vector MissionEntry]]
   [:portfolio/devmap-summaries [:vector DevmapSummary]]
   [:portfolio/coverage [:vector CoverageEntry]]
   [:portfolio/mana ManaSnapshot]
   [:portfolio/summary :string]
   [:portfolio/gaps [:vector :string]]
   [:portfolio/actionable [:vector :string]]])

;; =============================================================================
;; Tension export (structured gaps for hyperedge creation)
;; =============================================================================

(def TensionType
  [:enum :uncovered-component :blocked-mission :structural-invalid])

(def TensionEntry
  "A single tension — a typed discrepancy between ideal and actual.
   Pre-shaped for Arxana hyperedge creation."
  [:map
   [:tension/type TensionType]
   [:tension/devmap {:optional true} :keyword]
   [:tension/component {:optional true} :keyword]
   [:tension/mission {:optional true} :string]
   [:tension/blocked-by {:optional true} :string]
   [:tension/coverage-pct {:optional true} :double]
   [:tension/detected-at :string]
   [:tension/summary :string]])
