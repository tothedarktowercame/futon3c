(ns futon3c.peripheral.proof-dag
  "DAG operations for the proof ledger.

   The proof ledger forms a directed acyclic graph via :item/depends-on
   and :item/unlocks edges. This namespace provides:

   1. Acyclicity checking (SR-3) via Kahn's algorithm
   2. Impact scoring (SR-6) — rank blockers by transitive unlock count
   3. Reachability — which items are reachable from a given item

   All functions are pure — they take ledger maps and return results."
  (:require [clojure.set]))

;; =============================================================================
;; Graph extraction
;; =============================================================================

(defn- build-adjacency
  "Build an adjacency map from ledger items using :item/unlocks edges.
   Returns {item-id -> #{unlocked-item-ids ...}}."
  [ledger]
  (reduce-kv
   (fn [acc item-id item]
     (assoc acc item-id (or (:item/unlocks item) #{})))
   {}
   ledger))

(defn- build-reverse-adjacency
  "Build reverse adjacency: {item-id -> #{items-that-depend-on-it}}.
   Uses :item/depends-on edges."
  [ledger]
  (reduce-kv
   (fn [acc item-id item]
     (reduce (fn [a dep-id]
               (update a dep-id (fnil conj #{}) item-id))
             acc
             (or (:item/depends-on item) #{})))
   {}
   ledger))

;; =============================================================================
;; Acyclicity check — Kahn's algorithm (SR-3)
;; =============================================================================

(defn acyclic?
  "Check if the ledger DAG is acyclic using Kahn's algorithm.
   Returns {:acyclic? bool :cycle-nodes [...] (when cyclic)}.

   Uses :item/depends-on for in-edges and :item/unlocks for out-edges."
  [ledger]
  (let [all-ids (set (keys ledger))
        ;; in-degree: count of depends-on edges pointing to each node
        in-degree (reduce-kv
                   (fn [acc item-id item]
                     (assoc acc item-id
                            (count (clojure.set/intersection
                                    all-ids
                                    (or (:item/depends-on item) #{})))))
                   {}
                   ledger)
        ;; Start with nodes that have no dependencies
        initial-queue (into clojure.lang.PersistentQueue/EMPTY
                            (filter #(zero? (get in-degree % 0)) all-ids))]
    (loop [queue initial-queue
           visited #{}
           degrees in-degree]
      (if (empty? queue)
        (let [remaining (clojure.set/difference all-ids visited)]
          (if (empty? remaining)
            {:acyclic? true}
            {:acyclic? false :cycle-nodes (vec remaining)}))
        (let [node (peek queue)
              queue' (pop queue)
              visited' (conj visited node)
              unlocks (or (:item/unlocks (get ledger node)) #{})
              ;; Decrease in-degree for unlocked nodes
              {:keys [queue'' degrees']}
              (reduce (fn [{:keys [queue'' degrees']} target]
                        (let [new-deg (dec (get degrees' target 0))]
                          {:queue'' (if (and (zero? new-deg)
                                            (not (visited' target)))
                                     (conj queue'' target)
                                     queue'')
                           :degrees' (assoc degrees' target new-deg)}))
                      {:queue'' queue' :degrees' degrees}
                      (filter all-ids unlocks))]
          (recur queue'' visited' degrees'))))))

;; =============================================================================
;; Impact scoring — transitive unlock count (SR-6)
;; =============================================================================

(defn- transitive-unlocks
  "Compute the set of all items transitively unlocked by resolving item-id."
  [ledger item-id]
  (loop [frontier #{item-id}
         visited #{}]
    (if (empty? frontier)
      (disj visited item-id)  ;; Don't count self
      (let [current (first frontier)
            frontier' (disj frontier current)
            visited' (conj visited current)
            unlocks (or (:item/unlocks (get ledger current)) #{})
            new-nodes (clojure.set/difference unlocks visited')]
        (recur (into frontier' new-nodes) visited')))))

(defn impact-scores
  "Rank all items by transitive unlock count (SR-6).
   Returns [{:item-id str :score int :unlocks #{...}} ...]
   sorted by score descending. Higher score = resolving this item
   unblocks more downstream work."
  [ledger]
  (->> (keys ledger)
       (map (fn [item-id]
              (let [unlocks (transitive-unlocks ledger item-id)]
                {:item-id item-id
                 :score (count unlocks)
                 :unlocks unlocks})))
       (sort-by :score >)
       vec))

(defn impact-score
  "Get the impact score for a single item."
  [ledger item-id]
  (count (transitive-unlocks ledger item-id)))

;; =============================================================================
;; Reachability
;; =============================================================================

(defn reachable-from
  "Return the set of item-ids reachable from start-id via :item/unlocks edges."
  [ledger start-id]
  (transitive-unlocks ledger start-id))

(defn depends-chain
  "Return the set of item-ids that start-id transitively depends on
   (via :item/depends-on edges)."
  [ledger start-id]
  (loop [frontier #{start-id}
         visited #{}]
    (if (empty? frontier)
      (disj visited start-id)
      (let [current (first frontier)
            frontier' (disj frontier current)
            visited' (conj visited current)
            deps (or (:item/depends-on (get ledger current)) #{})
            new-nodes (clojure.set/difference deps visited')]
        (recur (into frontier' new-nodes) visited')))))

;; =============================================================================
;; Consistency checks
;; =============================================================================

(defn dangling-refs
  "Find item IDs referenced in depends-on/unlocks that don't exist in the ledger.
   Returns #{missing-id ...} or empty set."
  [ledger]
  (let [all-ids (set (keys ledger))]
    (reduce-kv
     (fn [acc _item-id item]
       (let [deps (or (:item/depends-on item) #{})
             unlocks (or (:item/unlocks item) #{})]
         (into acc (clojure.set/difference
                    (clojure.set/union deps unlocks)
                    all-ids))))
     #{}
     ledger)))

(defn edge-consistency?
  "Check that depends-on/unlocks edges are symmetric:
   if A unlocks B, then B depends-on A (and vice versa).
   Returns {:consistent? bool :mismatches [...]}."
  [ledger]
  (let [mismatches
        (reduce-kv
         (fn [acc item-id item]
           (let [unlocks (or (:item/unlocks item) #{})
                 ;; For each item this unlocks, check reverse edge
                 forward-issues
                 (keep (fn [target-id]
                         (when-let [target (get ledger target-id)]
                           (when-not (contains? (or (:item/depends-on target) #{}) item-id)
                             {:type :missing-depends-on
                              :from item-id :to target-id})))
                       unlocks)
                 deps (or (:item/depends-on item) #{})
                 reverse-issues
                 (keep (fn [dep-id]
                         (when-let [dep (get ledger dep-id)]
                           (when-not (contains? (or (:item/unlocks dep) #{}) item-id)
                             {:type :missing-unlocks
                              :from dep-id :to item-id})))
                       deps)]
             (into acc (concat forward-issues reverse-issues))))
         []
         ledger)]
    (if (empty? mismatches)
      {:consistent? true}
      {:consistent? false :mismatches (vec (distinct mismatches))})))
