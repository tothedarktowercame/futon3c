(ns futon3c.portfolio.adjacent
  "Adjacent-possible computation — the computable boundary between
   what has been realized and what is structurally enabled.

   A mission is adjacent-possible when 5 conditions hold:
   1. Dependencies cleared
   2. Shapes exist
   3. Mana available
   4. Patterns exist (soft: true if not specifically required)
   5. No higher-EFE competitor (deferred to policy layer)

   Source: D-7 in M-portfolio-inference.md")

;; =============================================================================
;; Condition checks
;; =============================================================================

(defn- dependencies-cleared?
  "Check if all blocked-by dependencies are resolved.
   A mission is unblocked if:
   - it has no blocked-by field, or
   - its blocked-by references are all :complete in the inventory."
  [mission inventory-by-id]
  (let [blocked-by (:mission/blocked-by mission)]
    (or (nil? blocked-by)
        (= "None" blocked-by)
        ;; Parse blocked-by: could be a string like "M-foo (predecessor)"
        ;; or "M-foo complete; M-bar"
        (let [refs (re-seq #"M-[\w-]+" blocked-by)]
          (or (empty? refs)
              (every? (fn [ref]
                        (let [mid (clojure.string/replace ref #"^M-" "")]
                          (= :complete (:mission/status (get inventory-by-id mid)))))
                      refs))))))

(defn- shapes-exist?
  "Check if required data shapes are defined.
   Soft check: returns true unless mission explicitly declares missing shapes.
   In practice, most missions don't require specific shapes."
  [_mission]
  ;; Soft pass — future: check if mission references shapes that don't exist
  true)

(defn- mana-available?
  "Check if sufficient mana exists for this mission.
   If the mana system isn't initialized, defaults to true (don't block
   on infrastructure that isn't set up yet)."
  [_mission mana-state]
  (or (not (:mana/available mana-state))
      ;; When mana is available, check pool balance > 0
      ;; Future: check against mission-specific mana cost estimate
      (pos? (or (:mana/pool-balance mana-state) 0.0))))

(defn- patterns-exist?
  "Check if required patterns are available in the library.
   Soft pass: true if mission doesn't specifically require patterns."
  [_mission]
  ;; Soft pass — future: check pattern catalog
  true)

;; =============================================================================
;; Adjacent-possible predicate
;; =============================================================================

(defn adjacent?
  "Determine if a mission is adjacent-possible (structurally enabled).

   Takes:
   - mission: a MissionEntry map
   - inventory-by-id: {mission-id → MissionEntry} for dependency checking
   - mana-state: ManaSnapshot from mc-backend

   Returns:
   - {:adjacent? bool, :reasons [keyword...]}
     Reasons explain why a mission is NOT adjacent."
  [mission inventory-by-id mana-state]
  (let [c1 (dependencies-cleared? mission inventory-by-id)
        c2 (shapes-exist? mission)
        c3 (mana-available? mission mana-state)
        c4 (patterns-exist? mission)]
    {:adjacent? (and c1 c2 c3 c4)
     :reasons (cond-> []
                (not c1) (conj :blocked-by-dependency)
                (not c2) (conj :missing-shapes)
                (not c3) (conj :insufficient-mana)
                (not c4) (conj :missing-patterns))}))

(defn compute-adjacent-set
  "Compute the full adjacent-possible set from a portfolio.

   Takes:
   - missions: vector of MissionEntry maps
   - mana-state: ManaSnapshot

   Returns:
   - vector of {:mission MissionEntry, :adjacent? bool, :reasons [...]}
     Only includes non-complete missions."
  [missions mana-state]
  (let [inventory-by-id (into {} (map (juxt :mission/id identity)) missions)
        active (remove #(= :complete (:mission/status %)) missions)]
    (mapv (fn [m]
            (let [result (adjacent? m inventory-by-id mana-state)]
              (assoc result :mission m)))
          active)))
