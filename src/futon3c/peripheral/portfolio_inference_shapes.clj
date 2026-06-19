(ns futon3c.peripheral.portfolio-inference-shapes
  "Domain shapes for the portfolio-inference peripheral.

   Portfolio Inference operates on the same substrate as Mission Control
   (cross-repo missions, devmaps, mana) but adds three layers:

   - **Mission-feature scoring** — per-mission closure criteria classified
     by recurrence-shape (one-shot / structural-recurring /
     process-recurring / ambiguous), and mapped to invariant families.

   - **Per-family aggregation** — precision-proxy signals (expectation
     drift, loss severity, witness quality, actionability gradient)
     aggregated from mission evidence per invariant family.

   - **AIF loop wrappers** — re-export of futon3c.portfolio.core
     entry points (aif-step / portfolio-step! / portfolio-heartbeat!) as
     peripheral tools.

   Companion to: futon5a/scripts/run_mission_feature_loop.clj (babashka
   regression baseline for VERIFY).")

;; =============================================================================
;; Criterion shape classification
;; =============================================================================

(def CriterionShape
  "Recurrence shape of a mission closure criterion."
  [:enum :one-shot :structural-recurring :process-recurring :ambiguous])

(def ScoredCriterion
  "A single criterion line with its scored shape."
  [:map
   [:text :string]
   [:shape CriterionShape]])

;; =============================================================================
;; Mission feature entry
;; =============================================================================

(def FamilyMatch
  "[family-id hit-count] pair from word-bounded phrase match."
  [:tuple :keyword :int])

(def MissionFeatureEntry
  "A mission with criterion-shape and family-mapping annotation.
   Extends MissionEntry from mission-control-shapes."
  [:map
   [:mission/id :string]
   [:mission/repo :string]
   [:mission/path :string]
   [:mission/title {:optional true} :string]
   [:mission/status :keyword]
   [:mission/raw-status {:optional true} :string]
   [:mission/criteria-section-count :int]
   [:mission/criteria [:vector ScoredCriterion]]
   [:mission/criteria-shape-counts [:map-of CriterionShape :int]]
   [:mission/families [:vector FamilyMatch]]])

;; =============================================================================
;; Per-family aggregation
;; =============================================================================

(def FamilyAggregation
  "Per-family rollup from the mission portfolio with precision-proxy
   signals. Used as the precision-table input."
  [:map
   [:family/id :keyword]
   [:family/name :string]
   [:family/layer :keyword]
   [:family/status :keyword]
   [:family/firm-exemplar {:optional true} [:maybe :keyword]]
   [:missions/total :int]
   [:missions/complete :int]
   [:missions/in-progress :int]
   [:missions/blocked :int]
   [:missions/total-score :int]
   [:criteria/structural-recurring :int]
   [:criteria/process-recurring :int]
   [:criteria/one-shot :int]
   [:criteria/ambiguous :int]
   [:precision/expectation-drift :int]
   [:precision/loss-severity :int]
   [:precision/witness-quality :int]
   [:precision/actionability-gradient :int]
   [:precision/raw-score :double]
   [:precision/proxy :int]
   [:missions/ids [:vector :string]]])

;; =============================================================================
;; Promotion candidate
;; =============================================================================

(def PromotionCandidate
  "A structural-recurring criterion paired with a candidate family that
   the host mission touches. Direct-promotion candidate for
   structural-law-inventory.sexp."
  [:map
   [:mission/id :string]
   [:mission/repo :string]
   [:mission/status :keyword]
   [:family/id :keyword]
   [:family/name :string]
   [:family/layer :keyword]
   [:family/match-score :int]
   [:criterion :string]])
