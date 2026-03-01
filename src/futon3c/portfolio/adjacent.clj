(ns futon3c.portfolio.adjacent
  "Adjacent-possible computation — the computable boundary between
   what has been realized and what is structurally enabled.

   Delegates to portfolio.logic for relational adjacency queries.
   Preserves the original API (adjacent?, compute-adjacent-set) while
   adding logic-powered structural queries (what-if, critical path).

   Source: D-7 in M-portfolio-inference.md"
  (:require [futon3c.portfolio.logic :as logic]))

;; =============================================================================
;; Adjacent-possible (logic-backed)
;; =============================================================================

(defn adjacent?
  "Determine if a mission is adjacent-possible (structurally enabled).

   Takes:
   - mission: a MissionEntry map
   - db: core.logic fact database (from logic/build-db)

   Returns:
   - {:adjacent? bool, :reasons [keyword...]}
     Reasons explain why a mission is NOT adjacent."
  [mission db]
  (let [mid (:mission/id mission)
        adj-set (set (logic/query-adjacent db))]
    (if (contains? adj-set mid)
      {:adjacent? true :reasons []}
      ;; Diagnose why not adjacent
      (let [blocked-pairs (logic/query-blocked-missions db)
            has-unresolved-deps (some (fn [[m _]] (= m mid)) blocked-pairs)
            ;; Check each condition for diagnostic
            reasons (cond-> []
                      has-unresolved-deps (conj :blocked-by-dependency))]
        {:adjacent? false :reasons reasons}))))

(defn compute-adjacent-set
  "Compute the full adjacent-possible set from a portfolio.

   Takes:
   - missions: vector of MissionEntry maps
   - mana-state: ManaSnapshot
   - opts: optional {:evidence-counts, :patterns-used} for logic DB

   Returns:
   - vector of {:mission MissionEntry, :adjacent? bool, :reasons [...]}
     Only includes non-complete missions."
  ([missions mana-state] (compute-adjacent-set missions mana-state {}))
  ([missions mana-state opts]
   (let [db (logic/build-db missions mana-state opts)
         adj-set (set (logic/query-adjacent db))
         active (remove #(= :complete (:mission/status %)) missions)]
     (mapv (fn [m]
             (let [mid (:mission/id m)]
               {:mission m
                :adjacent? (contains? adj-set mid)
                :reasons (if (contains? adj-set mid)
                           []
                           (let [blocked-pairs (logic/query-blocked-missions db)]
                             (cond-> []
                               (some (fn [[bm _]] (= bm mid)) blocked-pairs)
                               (conj :blocked-by-dependency))))}))
           active))))

;; =============================================================================
;; Structural queries (delegated to logic layer)
;; =============================================================================

(defn what-if-complete
  "What missions become adjacent if target-mid is completed?
   Returns vector of mission IDs."
  [missions mana-state target-mid]
  (let [db (logic/build-db missions mana-state {})]
    (logic/query-what-if-complete db target-mid)))

(defn critical-path
  "Find missions with the deepest dependency chains.
   Returns [{:mission mid :depth n}...] sorted by depth descending."
  [missions mana-state]
  (let [db (logic/build-db missions mana-state {})]
    (logic/query-critical-path db)))

(defn structural-summary
  "Produce a structural summary of the portfolio knowledge base."
  [missions mana-state opts]
  (let [db (logic/build-db missions mana-state (or opts {}))]
    (logic/structural-summary db)))
