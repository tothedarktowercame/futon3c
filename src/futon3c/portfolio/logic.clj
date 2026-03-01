(ns futon3c.portfolio.logic
  "Portfolio knowledge base — core.logic relational layer.

   Mission facts as relations, adjacency as a logic goal.
   Provides structural queries: what-if, critical path, pattern
   co-occurrence, dependency clustering.

   The fact database is rebuilt each aif-step from mc-backend data.
   core.logic produces the candidate set (what's structurally valid);
   AIF evaluates the candidates (what's best among them).

   Design pattern: pldb fact DB (from futon3/hx/logic.clj)"
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.pldb :as pldb]
            [clojure.string :as str]))

;; =============================================================================
;; Relations (fact schema)
;; =============================================================================

(pldb/db-rel missiono mid)
(pldb/db-rel statuso mid status)
(pldb/db-rel blocked-byo mid blocker-mid)
(pldb/db-rel repo-ofo mid repo)
(pldb/db-rel evidenceo mid count)
(pldb/db-rel patterno-used mid pattern-id)
(pldb/db-rel shapeso-defined mid)
(pldb/db-rel mana-fundedo mid)

;; =============================================================================
;; Fact database construction
;; =============================================================================

(defn- parse-blocked-by
  "Extract mission IDs from a blocked-by string.
   'M-foo (predecessor)' → ['foo']
   'M-foo complete; M-bar' → ['foo' 'bar']
   nil or 'None' → []"
  [blocked-by-str]
  (if (or (nil? blocked-by-str)
          (= "None" blocked-by-str))
    []
    (let [refs (re-seq #"M-([\w-]+)" blocked-by-str)]
      (mapv second refs))))

(defn build-db
  "Build a core.logic fact database from mission inventory + mana state.

   Takes:
   - missions: vector of MissionEntry maps (from mc/build-inventory)
   - mana-state: ManaSnapshot map
   - opts: {:evidence-counts {mid → count}, :patterns-used {mid → [pattern-id...]}}

   Returns a pldb database."
  [missions mana-state opts]
  (let [evidence-counts (or (:evidence-counts opts) {})
        patterns-used (or (:patterns-used opts) {})
        mana-available? (or (not (:mana/available mana-state))
                            (pos? (or (:mana/pool-balance mana-state) 0.0)))
        base (pldb/db)]
    (reduce
     (fn [db mission]
       (let [mid (:mission/id mission)
             status (:mission/status mission)
             repo (or (:mission/repo mission) "unknown")
             blockers (parse-blocked-by (:mission/blocked-by mission))
             ev-count (get evidence-counts mid 0)
             patterns (get patterns-used mid [])]
         (as-> db d
           ;; Core facts
           (pldb/db-fact d missiono mid)
           (pldb/db-fact d statuso mid status)
           (pldb/db-fact d repo-ofo mid repo)
           (pldb/db-fact d evidenceo mid ev-count)
           ;; Dependency edges
           (reduce (fn [d2 blocker]
                     (pldb/db-fact d2 blocked-byo mid blocker))
                   d blockers)
           ;; Pattern usage
           (reduce (fn [d2 pat]
                     (pldb/db-fact d2 patterno-used mid pat))
                   d patterns)
           ;; Shapes — soft pass (all missions have shapes for now)
           (pldb/db-fact d shapeso-defined mid)
           ;; Mana — global gate for now
           (if mana-available?
             (pldb/db-fact d mana-fundedo mid)
             d))))
     base
     missions)))

;; =============================================================================
;; Logic goals
;; =============================================================================

(defn completeo
  "Goal: mission mid has status :complete."
  [mid]
  (statuso mid :complete))

(defn not-completeo
  "Goal: mission mid exists and is not :complete."
  [mid]
  (l/fresh [status]
    (missiono mid)
    (statuso mid status)
    (l/!= status :complete)))

(defn- deps-complete?
  "Imperative check: are all blockers of mid :complete in db?
   mid must be a ground value (string), not a logic variable."
  [db mid]
  (let [blockers (pldb/with-db db
                   (l/run* [b]
                     (blocked-byo mid b)))]
    (or (empty? blockers)
        (every? (fn [b]
                  (seq (pldb/with-db db
                         (l/run 1 [q]
                           (completeo b)
                           (l/== q true)))))
                blockers))))

(defn adjacento
  "Goal: mission mid is adjacent-possible.
   Requires: not complete, all deps complete, shapes exist, mana funded.
   Uses l/project to ground mid before checking dependency chains."
  [mid db]
  (l/all
   (not-completeo mid)
   (shapeso-defined mid)
   (mana-fundedo mid)
   (l/project [mid]
     (if (deps-complete? db mid)
       l/succeed
       l/fail))))

;; =============================================================================
;; Structural queries
;; =============================================================================

(defn query-adjacent
  "Return all adjacent-possible mission IDs."
  [db]
  (pldb/with-db db
    (l/run* [m]
      (adjacento m db))))

(defn query-blocked-missions
  "Return all missions that are blocked and what blocks them."
  [db]
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [mid blocker]
        (blocked-byo mid blocker)
        (l/== q [mid blocker])))))

(defn- adjacent-if-complete?
  "Imperative check: would mid become adjacent if target-mid were complete?
   Checks: not complete, shapes exist, mana funded, all deps complete
   (treating target-mid as if it were complete)."
  [db mid target-mid]
  (let [has-shapes (seq (pldb/with-db db
                          (l/run 1 [q] (shapeso-defined mid) (l/== q true))))
        has-mana (seq (pldb/with-db db
                        (l/run 1 [q] (mana-fundedo mid) (l/== q true))))
        not-done (seq (pldb/with-db db
                        (l/run 1 [q] (not-completeo mid) (l/== q true))))
        blockers (pldb/with-db db (l/run* [b] (blocked-byo mid b)))
        others-complete (every?
                         (fn [b]
                           (or (= b target-mid)
                               (seq (pldb/with-db db
                                      (l/run 1 [q]
                                        (completeo b)
                                        (l/== q true))))))
                         blockers)]
    (and has-shapes has-mana not-done others-complete)))

(defn query-what-if-complete
  "What missions become adjacent if `target-mid` were completed?
   Returns mission IDs that are currently blocked by target-mid
   and would become adjacent once it's complete."
  [db target-mid]
  (let [blocked-by-target (pldb/with-db db
                            (l/run* [m] (blocked-byo m target-mid)))]
    (filterv #(adjacent-if-complete? db % target-mid) blocked-by-target)))

(defn query-dependency-clusters
  "Find missions blocked by the same dependency (co-blocked clusters).
   Returns [m1 m2 shared-blocker] triples."
  [db]
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [m1 m2 blocker]
        (blocked-byo m1 blocker)
        (blocked-byo m2 blocker)
        (l/!= m1 m2)
        ;; Avoid duplicate pairs (m1 < m2 lexicographically)
        (l/project [m1 m2]
          (l/== true (neg? (compare m1 m2))))
        (l/== q [m1 m2 blocker])))))

(defn query-unblocked-by
  "What does completing `target-mid` directly unblock?
   Returns mission IDs that have target-mid as a blocker."
  [db target-mid]
  (pldb/with-db db
    (l/run* [m]
      (blocked-byo m target-mid))))

(defn query-chain-depth
  "Compute maximum dependency chain depth from a mission.
   Returns the depth (0 = no deps, 1 = direct deps, etc.)."
  [db mid]
  (let [blockers (pldb/with-db db
                   (l/run* [b]
                     (blocked-byo mid b)))]
    (if (empty? blockers)
      0
      (inc (apply max
                  (map #(query-chain-depth db %) blockers))))))

(defn query-critical-path
  "Find the mission(s) with the deepest dependency chain.
   Returns [{:mission mid :depth n}...] sorted by depth descending."
  [db]
  (let [all-missions (pldb/with-db db
                       (l/run* [m] (missiono m)))
        depths (mapv (fn [m]
                       {:mission m :depth (query-chain-depth db m)})
                     all-missions)]
    (->> depths
         (sort-by :depth >)
         (take-while #(pos? (:depth %))))))

(defn query-pattern-co-occurrence
  "Which patterns appear in multiple active (non-complete) missions?
   Returns [{:pattern pat :missions [mid...] :count n}...]."
  [db]
  (let [active (pldb/with-db db
                 (l/run* [m] (not-completeo m)))
        usage (pldb/with-db db
                (l/run* [q]
                  (l/fresh [mid pat]
                    (patterno-used mid pat)
                    (l/membero mid active)
                    (l/== q [mid pat]))))
        by-pattern (group-by second usage)]
    (->> by-pattern
         (map (fn [[pat pairs]]
                {:pattern pat
                 :missions (mapv first pairs)
                 :count (count pairs)}))
         (filter #(> (:count %) 1))
         (sort-by :count >))))

(defn query-all-missions
  "Return all mission IDs with their status."
  [db]
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [mid status]
        (missiono mid)
        (statuso mid status)
        (l/== q [mid status])))))

;; =============================================================================
;; Summary — human-readable structural overview
;; =============================================================================

(defn structural-summary
  "Produce a human-readable structural summary of the knowledge base."
  [db]
  (let [all (query-all-missions db)
        adjacent (query-adjacent db)
        blocked (query-blocked-missions db)
        critical (query-critical-path db)
        clusters (query-dependency-clusters db)]
    {:total (count all)
     :by-status (frequencies (map second all))
     :adjacent adjacent
     :adjacent-count (count adjacent)
     :blocked-pairs blocked
     :critical-path (take 5 critical)
     :dependency-clusters clusters}))

;; =============================================================================
;; Coverage & tension relations
;; =============================================================================
;;
;; These relations extend the portfolio knowledge base with devmap structure
;; and coverage facts. The key insight: coverage is currently computed
;; procedurally in mc-backend/compute-coverage (three-tier heuristic matching).
;; Expressing it relationally makes tension discovery compositional:
;; a tension is simply a structural query that fails to find a covering fact.
;;
;; Anti-drift property: any tension hyperedge in XTDB should be re-derivable
;; by running these queries against the current state. When re-derivation
;; fails (the tension was resolved) or succeeds where no hyperedge exists
;; (new tension), that's a consistency signal.

;; -- Devmap structure facts --
(pldb/db-rel devmapo dm-id)                     ; devmap exists
(pldb/db-rel devmap-stateo dm-id state)          ; devmap state (:active, :complete, etc.)
(pldb/db-rel componento dm-id comp-id)           ; component belongs to devmap
(pldb/db-rel devmap-edgeo dm-id from to)         ; edge in devmap wiring

;; -- Coverage facts (derived from compute-coverage or annotation) --
(pldb/db-rel coverso mid comp-id)                ; mission covers component
(pldb/db-rel annotatedo comp-id mid)             ; explicit annotation: comp → mission

;; -- Hyperedge facts (from XTDB, for consistency checking) --
(pldb/db-rel hyperedgeo hx-id hx-type)           ; hyperedge exists with type
(pldb/db-rel hx-endpointo hx-id endpoint)        ; hyperedge has endpoint

;; -- Invariant facts (for porting audit) --
(pldb/db-rel invarianto inv-id source-repo)      ; invariant defined in source repo
(pldb/db-rel implementedo inv-id target-repo)    ; invariant implemented in target

;; =============================================================================
;; Extended fact database construction
;; =============================================================================

(defn build-coverage-db
  "Extend a portfolio logic DB with devmap structure and coverage facts.

   Takes:
   - db: existing portfolio logic DB (from build-db)
   - devmaps: vector of devmap summaries (from read-all-devmaps)
   - coverage: vector of coverage results (from compute-coverage)
   - opts: {:hyperedges [{:hx/id _ :hx/type _ :hx/endpoints [...]}...]}

   Returns extended pldb database."
  [db devmaps coverage opts]
  (let [hyperedges (or (:hyperedges opts) [])]
    (as-> db d
      ;; Devmap structure
      (reduce (fn [d2 dm]
                (let [dm-id (:devmap/id dm)
                      state (:devmap/state dm)
                      components (:devmap/components dm)]
                  (as-> d2 d3
                    (pldb/db-fact d3 devmapo dm-id)
                    (pldb/db-fact d3 devmap-stateo dm-id (or state :unknown))
                    (reduce (fn [d4 c]
                              (pldb/db-fact d4 componento dm-id (:component/id c)))
                            d3 components))))
              d devmaps)
      ;; Coverage
      (reduce (fn [d2 cov]
                (let [dm-id (:coverage/devmap-id cov)
                      covered (:coverage/covered cov)]
                  (reduce (fn [d3 comp-id]
                            (if-let [mids (get (:coverage/by-component cov) comp-id)]
                              (reduce (fn [d4 mid]
                                        (pldb/db-fact d4 coverso mid comp-id))
                                      d3 mids)
                              d3))
                          d2 covered)))
              d coverage)
      ;; Hyperedges from XTDB (for consistency checking)
      (reduce (fn [d2 hx]
                (let [hx-id (:hx/id hx)
                      hx-type (:hx/type hx)
                      endpoints (:hx/endpoints hx)]
                  (as-> d2 d3
                    (pldb/db-fact d3 hyperedgeo hx-id hx-type)
                    (reduce (fn [d4 ep]
                              (pldb/db-fact d4 hx-endpointo hx-id ep))
                            d3 (or endpoints [])))))
              d hyperedges))))

;; =============================================================================
;; Coverage goals
;; =============================================================================

(defn uncoveredo
  "Goal: component comp-id in devmap dm-id has no covering mission."
  [dm-id comp-id]
  (l/all
   (componento dm-id comp-id)
   (l/project [comp-id]
     ;; No mission covers this component
     (l/== true true)  ; placeholder — negation-as-failure below
     )))

(defn query-uncovered-components
  "Find all (devmap, component) pairs with no covering mission.
   This is the relational equivalent of mc-backend's 'uncovered' computation."
  [db]
  (let [all-comps (pldb/with-db db
                    (l/run* [q]
                      (l/fresh [dm comp]
                        (componento dm comp)
                        (l/== q [dm comp]))))
        covered? (fn [[_dm comp]]
                   (seq (pldb/with-db db
                          (l/run 1 [mid]
                            (coverso mid comp)))))]
    (vec (remove covered? all-comps))))

(defn query-derived-tensions
  "Derive tension set from current state. Each tension is a map matching
   the TensionEntry schema. Compare against stored hyperedges to find:
   - :new — tension derivable but no hyperedge exists
   - :resolved — hyperedge exists but tension no longer derivable
   - :consistent — both agree"
  [db]
  (let [uncovered (query-uncovered-components db)
        ;; Check which have corresponding hyperedges
        stored-tensions (pldb/with-db db
                          (l/run* [hx-id]
                            (hyperedgeo hx-id :tension/uncovered-component)))]
    {:derived (mapv (fn [[dm comp]]
                      {:tension/type :uncovered-component
                       :tension/devmap dm
                       :tension/component comp})
                    uncovered)
     :derived-count (count uncovered)
     :stored-count (count stored-tensions)}))

;; =============================================================================
;; Invariant porting audit
;; =============================================================================

(defn query-unported-invariants
  "Find invariants defined in source-repo with no implementation in target-repo.
   This is exactly the query that would have caught the futon1→futon1a drift."
  [db source-repo target-repo]
  (let [all-invariants (pldb/with-db db
                         (l/run* [inv]
                           (invarianto inv source-repo)))
        ported? (fn [inv]
                  (seq (pldb/with-db db
                         (l/run 1 [q]
                           (implementedo inv target-repo)
                           (l/== q true)))))]
    (vec (remove ported? all-invariants))))

(defn query-consistency
  "Check consistency between derived tensions and stored hyperedges.
   Returns :ok if they agree, or a drift report if they don't."
  [db]
  (let [{:keys [derived stored-count]} (query-derived-tensions db)
        unported (query-unported-invariants db "futon1" "futon1a")]
    (cond-> {:tension-drift (if (= (count derived) stored-count)
                              :consistent
                              :drifted)
             :derived-tension-count (count derived)
             :stored-tension-count stored-count}
      (seq unported)
      (assoc :unported-invariants unported
             :invariant-drift :drifted))))
