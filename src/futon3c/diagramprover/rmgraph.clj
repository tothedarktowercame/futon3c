(ns futon3c.diagramprover.rmgraph
  "Pure immutable right-monogamous acyclic open hypergraphs.

  This is the MPZ comonoid representation: sharing and discard are properties
  of nodes, so output use is amonogamous and unordered. Input monogamy remains:
  a node has at most one producer, counting both target tentacles and boundary
  inputs. No explicit copy or discard edges occur in this representation.")

(defn make-graph []
  {:vdata {} :edata {} :inputs [] :outputs [] :vindex 0 :eindex 0})

(defn vertices [graph] (keys (:vdata graph)))
(defn edges [graph] (keys (:edata graph)))
(defn vertex-data [graph vertex] (get-in graph [:vdata vertex]))
(defn edge-data [graph edge] (get-in graph [:edata edge]))
(defn source [graph edge] (:source (edge-data graph edge)))
(defn target [graph edge] (:target (edge-data graph edge)))
(defn in-edges [graph vertex] (get-in graph [:vdata vertex :in-edges] #{}))
(defn out-edges [graph vertex] (get-in graph [:vdata vertex :out-edges] #{}))

(defn- reindex-boundary [graph boundary-key index-key boundary]
  (let [cleared (reduce #(assoc-in %1 [:vdata %2 index-key] #{})
                        graph (vertices graph))]
    (reduce-kv
     (fn [result index vertex]
       (when-not (contains? (:vdata result) vertex)
         (throw (ex-info "RM boundary refers to unknown node"
                         {:node vertex :boundary boundary-key})))
       (update-in result [:vdata vertex index-key] conj index))
     (assoc cleared boundary-key (vec boundary))
     (vec boundary))))

(defn set-inputs [graph inputs]
  (reindex-boundary graph :inputs :in-indices inputs))

(defn set-outputs [graph outputs]
  (reindex-boundary graph :outputs :out-indices outputs))

(defn add-vertex
  "Add a node and return `[new-graph node-id]`."
  [graph payload]
  (let [vertex (:vindex graph)
        data (-> (merge {:vtype nil :size 1 :value nil} payload)
                 (assoc :in-edges #{} :out-edges #{}
                        :in-indices #{} :out-indices #{}))]
    [(-> graph
         (assoc-in [:vdata vertex] data)
         (update :vindex inc))
     vertex]))

(defn add-edge
  "Add an RM hyperedge. Tentacle vectors are accepted in any order."
  [graph source-vertices target-vertices payload]
  (let [sources (vec source-vertices)
        targets (vec target-vertices)]
    (doseq [vertex (concat sources targets)]
      (when-not (contains? (:vdata graph) vertex)
        (throw (ex-info "RM edge refers to unknown node" {:node vertex}))))
    (let [edge (:eindex graph)
          data (-> (merge {:value nil} payload)
                   (assoc :source sources :target targets))
          with-edge (-> graph
                        (assoc-in [:edata edge] data)
                        (update :eindex inc))
          with-sources (reduce #(update-in %1 [:vdata %2 :out-edges] conj edge)
                               with-edge (set sources))
          with-targets (reduce #(update-in %1 [:vdata %2 :in-edges] conj edge)
                               with-sources (set targets))]
      [with-targets edge])))

(defn- occurrence-count [items item]
  (count (filter #{item} items)))

(defn- right-monogamous? [graph]
  (every?
   (fn [vertex]
     (<= (+ (occurrence-count (:inputs graph) vertex)
            (reduce + (map #(occurrence-count (target graph %) vertex)
                           (edges graph))))
         1))
   (vertices graph)))

(defn- dependency-adjacency [graph]
  (reduce
   (fn [adjacency edge]
     (reduce (fn [result source-node]
               (update result source-node into (set (target graph edge))))
             adjacency (set (source graph edge))))
   (zipmap (vertices graph) (repeat #{}))
   (edges graph)))

(defn- acyclic? [graph]
  (let [adjacency (dependency-adjacency graph)
        nodes (set (vertices graph))
        indegrees
        (reduce-kv (fn [result _ children]
                     (reduce #(update %1 %2 inc) result children))
                   (zipmap nodes (repeat 0)) adjacency)]
    (loop [ready (into (sorted-set)
                       (keep #(when (zero? (indegrees %)) %)) nodes)
           degrees indegrees
           visited 0]
      (if-let [node (first ready)]
        (let [[next-degrees next-ready]
              (reduce (fn [[ds rs] child]
                        (let [degree (dec (ds child))]
                          [(assoc ds child degree)
                           (cond-> rs (zero? degree) (conj child))]))
                      [degrees (disj ready node)] (adjacency node))]
          (recur next-ready next-degrees (inc visited)))
        (= visited (count nodes))))))

(defn rm-valid?
  "True for a reference-safe, right-monogamous, acyclic open hypergraph."
  [graph]
  (try
    (let [known (set (vertices graph))
          mentioned (concat (:inputs graph) (:outputs graph)
                            (mapcat #(concat (source graph %) (target graph %))
                                    (edges graph)))]
      (and (every? known mentioned)
           (right-monogamous? graph)
           (acyclic? graph)))
    (catch Exception _ false)))

(def ^:private kernel-vertex-keys
  #{:in-edges :out-edges :in-indices :out-indices})

(defn- vertex-payload [graph vertex]
  (apply dissoc (vertex-data graph vertex) kernel-vertex-keys))

(defn- edge-payload [graph edge]
  (dissoc (edge-data graph edge) :source :target))

(defn- stable-form [value]
  (cond
    (map? value) (->> value
                      (map (fn [[key item]] [(stable-form key)
                                             (stable-form item)]))
                      (sort-by pr-str)
                      vec)
    (set? value) (->> value (map stable-form) (sort-by pr-str) vec)
    (sequential? value) (mapv stable-form value)
    :else value))

(defn- structural-key [value]
  (pr-str (stable-form value)))

(defn- permutations [items]
  (if (empty? items)
    [[]]
    (mapcat (fn [item]
              (map #(into [item] %)
                   (permutations (remove #{item} items))))
            items)))

(defn- orderings [groups]
  (reduce (fn [prefixes group]
            (for [prefix prefixes permutation (permutations group)]
              (into (vec prefix) permutation)))
          [[]] groups))

(defn- graph-extras [graph]
  (dissoc graph :vdata :edata :inputs :outputs :vindex :eindex))

(defn- candidate [graph old-order]
  (let [mapping (zipmap old-order (range))
        [base _]
        (reduce (fn [[result ids] old]
                  (let [[next-result fresh]
                        (add-vertex result (vertex-payload graph old))]
                    [next-result (conj ids fresh)]))
                [(make-graph) []] old-order)
        descriptors
        (->> (edges graph)
             (map (fn [edge]
                    {:source (vec (sort (map mapping (source graph edge))))
                     :target (vec (sort (map mapping (target graph edge))))
                     :payload (edge-payload graph edge)}))
             (sort-by structural-key))
        with-edges
        (reduce (fn [result {:keys [source target payload]}]
                  (first (add-edge result source target payload)))
                base descriptors)]
    (-> with-edges
        (set-inputs (mapv mapping (:inputs graph)))
        (set-outputs (mapv mapping (:outputs graph)))
        (merge (graph-extras graph)))))

(defn canonicalize
  "Return an exact payload-preserving canonical representative.

  Tentacle multisets and edge insertion order are erased. Vertices are first
  partitioned by immutable payload, then every permutation inside a tied
  partition is considered and the least stable serialization is selected.
  This is iso-complete because any payload-preserving isomorphism lies inside
  exactly those partitions. The exhaustive fallback can be factorial for a
  highly symmetric unlabeled graph; causal RM diagrams have a unique semantic
  producer in every node payload, so their partitions are singletons."
  [graph]
  (when-not (rm-valid? graph)
    (throw (ex-info "Cannot canonicalize invalid RM graph" {})))
  (let [groups (->> (vertices graph)
                    (group-by #(structural-key (vertex-payload graph %)))
                    (sort-by key)
                    (mapv (comp vec val)))
        candidates (map #(candidate graph %) (orderings groups))]
    (first (sort-by structural-key candidates))))

(defn canonical?
  "True when the graph is already its canonical RM representative."
  [graph]
  (and (rm-valid? graph) (= graph (canonicalize graph))))
