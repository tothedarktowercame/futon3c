(ns futon3c.diagramprover.graph
  "Pure immutable typed open hypergraphs for string-diagram rewriting.")

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
(defn num-vertices [graph] (count (:vdata graph)))
(defn num-edges [graph] (count (:edata graph)))

(defn- vertex-type [data]
  [(:vtype data) (:size data 1)])

(defn domain [graph]
  (mapv #(vertex-type (vertex-data graph %)) (:inputs graph)))

(defn codomain [graph]
  (mapv #(vertex-type (vertex-data graph %)) (:outputs graph)))

(defn edge-domain [graph edge]
  (mapv #(vertex-type (vertex-data graph %)) (source graph edge)))

(defn edge-codomain [graph edge]
  (mapv #(vertex-type (vertex-data graph %)) (target graph edge)))

(defn is-input? [graph vertex]
  (seq (get-in graph [:vdata vertex :in-indices])))

(defn is-output? [graph vertex]
  (seq (get-in graph [:vdata vertex :out-indices])))

(defn is-boundary? [graph vertex]
  (boolean (or (is-input? graph vertex) (is-output? graph vertex))))

(defn- reindex-boundary [graph boundary-key index-key boundary]
  (let [cleared (reduce (fn [g vertex]
                          (assoc-in g [:vdata vertex index-key] #{}))
                        graph (vertices graph))]
    (reduce-kv (fn [g index vertex]
                 (when-not (contains? (:vdata g) vertex)
                   (throw (ex-info "Boundary refers to unknown vertex"
                                   {:vertex vertex :boundary boundary-key})))
                 (update-in g [:vdata vertex index-key] conj index))
               (assoc cleared boundary-key (vec boundary))
               (vec boundary))))

(defn set-inputs [graph inputs]
  (reindex-boundary graph :inputs :in-indices inputs))

(defn set-outputs [graph outputs]
  (reindex-boundary graph :outputs :out-indices outputs))

(defn add-inputs [graph inputs]
  (set-inputs graph (into (:inputs graph) inputs)))

(defn add-outputs [graph outputs]
  (set-outputs graph (into (:outputs graph) outputs)))

(defn add-vertex
  "Add a vertex and return `[new-graph vertex-id]`.

  Extra payload keys are preserved. Adjacency and boundary-index keys are
  kernel-owned and initialized empty."
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
  "Add an ordered hyperedge and return `[new-graph edge-id]`."
  [graph source-vertices target-vertices payload]
  (let [source-vertices (vec source-vertices)
        target-vertices (vec target-vertices)
        mentioned (concat source-vertices target-vertices)]
    (doseq [vertex mentioned]
      (when-not (contains? (:vdata graph) vertex)
        (throw (ex-info "Edge refers to unknown vertex" {:vertex vertex}))))
    (let [edge (:eindex graph)
          data (-> (merge {:value nil} payload)
                   (assoc :source source-vertices :target target-vertices))
          graph' (-> graph
                     (assoc-in [:edata edge] data)
                     (update :eindex inc))
          graph' (reduce #(update-in %1 [:vdata %2 :out-edges] conj edge)
                         graph' (set source-vertices))
          graph' (reduce #(update-in %1 [:vdata %2 :in-edges] conj edge)
                         graph' (set target-vertices))]
      [graph' edge])))

(defn remove-edge [graph edge]
  (when-not (contains? (:edata graph) edge)
    (throw (ex-info "Unknown edge" {:edge edge})))
  (let [data (edge-data graph edge)]
    (-> (reduce #(update-in %1 [:vdata %2 :out-edges] disj edge)
                graph (set (:source data)))
        (#(reduce (fn [g vertex]
                    (update-in g [:vdata vertex :in-edges] disj edge))
                  % (set (:target data))))
        (update :edata dissoc edge))))

(defn remove-vertex
  ([graph vertex] (remove-vertex graph vertex false))
  ([graph vertex strict?]
   (let [data (vertex-data graph vertex)]
     (when-not data
       (throw (ex-info "Unknown vertex" {:vertex vertex})))
     (when (and strict?
                (or (seq (:in-edges data)) (seq (:out-edges data))
                    (is-boundary? graph vertex)))
       (throw (ex-info "Cannot strictly remove attached/boundary vertex"
                       {:vertex vertex})))
     (let [incident (into (:in-edges data) (:out-edges data))
           detached (reduce
                     (fn [g edge]
                       (-> g
                           (update-in [:edata edge :source]
                                      #(vec (remove #{vertex} %)))
                           (update-in [:edata edge :target]
                                      #(vec (remove #{vertex} %)))))
                     graph incident)
           detached (update detached :vdata dissoc vertex)
           detached (set-inputs detached (remove #{vertex} (:inputs detached)))]
       (set-outputs detached (remove #{vertex} (:outputs detached)))))))

(defn merge-vertices
  "Merge `removed` into `kept`, preserving boundary positions."
  [graph kept removed]
  (if (= kept removed)
    graph
    (let [removed-data (vertex-data graph removed)]
      (when-not (and (vertex-data graph kept) removed-data)
        (throw (ex-info "Cannot merge unknown vertex"
                        {:kept kept :removed removed})))
      (let [rewired
            (reduce
             (fn [g edge]
               (-> g
                   (update-in [:edata edge :source]
                              #(mapv (fn [v] (if (= v removed) kept v)) %))
                   (update-in [:edata edge :target]
                              #(mapv (fn [v] (if (= v removed) kept v)) %))))
             graph (into (:in-edges removed-data) (:out-edges removed-data)))
            rewired (-> rewired
                        (update-in [:vdata kept :in-edges]
                                   into (:in-edges removed-data))
                        (update-in [:vdata kept :out-edges]
                                   into (:out-edges removed-data))
                        (update :vdata dissoc removed))
            rewired (set-inputs
                     rewired
                     (mapv #(if (= % removed) kept %) (:inputs rewired)))]
        (set-outputs rewired
                     (mapv #(if (= % removed) kept %) (:outputs rewired)))))))

(defn- fresh-like [graph vertex]
  (add-vertex graph
              (dissoc (vertex-data graph vertex)
                      :in-edges :out-edges :in-indices :out-indices)))

(defn explode-vertex
  "Split a vertex into distinct input-like and output-like tentacles.

  Returns `[new-graph input-like-ids output-like-ids]`."
  [graph vertex]
  (let [original (vertex-data graph vertex)
        fresh (fn [[g ids] _]
                (let [[g' id] (fresh-like g vertex)] [g' (conj ids id)]))
        replace-occurrences
        (fn [items replacements]
          (reduce (fn [[result remaining] item]
                    (if (= item vertex)
                      [(conj result (first remaining)) (subvec remaining 1)]
                      [(conj result item) remaining]))
                  [[] (vec replacements)] items))
        input-count (+ (count (:in-indices original))
                       (reduce + (map #(count (filter #{vertex} (target graph %)))
                                      (:in-edges original))))
        output-count (+ (count (:out-indices original))
                        (reduce + (map #(count (filter #{vertex} (source graph %)))
                                       (:out-edges original))))
        [graph in-ids] (reduce fresh [graph []] (range input-count))
        [graph out-ids] (reduce fresh [graph []] (range output-count))
        [new-inputs remaining-in] (replace-occurrences (:inputs graph) in-ids)
        graph-with-inputs (set-inputs graph new-inputs)
        [graph-after-inputs remaining-in-after]
        (reduce
         (fn [[g replacements] edge]
           (let [[replacement remaining] (replace-occurrences
                                          (target g edge) replacements)
                 new-vertices (filter (set in-ids) replacement)]
             [(reduce #(update-in %1 [:vdata %2 :in-edges] conj edge)
                      (assoc-in g [:edata edge :target] replacement)
                      (set new-vertices))
              remaining]))
         [graph-with-inputs remaining-in] (:in-edges original))
        [new-outputs remaining-out]
        (replace-occurrences (:outputs graph-after-inputs) out-ids)
        graph-with-outputs (set-outputs graph-after-inputs new-outputs)
        [graph-after-outputs remaining-out-after]
        (reduce
         (fn [[g replacements] edge]
           (let [[replacement remaining] (replace-occurrences
                                          (source g edge) replacements)
                 new-vertices (filter (set out-ids) replacement)]
             [(reduce #(update-in %1 [:vdata %2 :out-edges] conj edge)
                      (assoc-in g [:edata edge :source] replacement)
                      (set new-vertices))
              remaining]))
         [graph-with-outputs remaining-out] (:out-edges original))
        _ (when (or (seq remaining-in-after) (seq remaining-out-after))
            (throw (ex-info "Vertex explosion did not consume all copies"
                            {:vertex vertex})))
        detached (-> graph-after-outputs
                     (assoc-in [:vdata vertex :in-edges] #{})
                     (assoc-in [:vdata vertex :out-edges] #{}))]
    [(remove-vertex detached vertex true) in-ids out-ids]))

(defn successors [graph start-vertices]
  (loop [frontier (vec start-vertices) seen #{}]
    (if-let [vertex (peek frontier)]
      (let [next-vertices (for [edge (out-edges graph vertex)
                                target-vertex (target graph edge)
                                :when (not (contains? seen target-vertex))]
                            target-vertex)]
        (recur (into (pop frontier) next-vertices) (into seen next-vertices)))
      seen)))

(defn generator
  "Convenience constructor for one generator with typed boundary wires."
  [value domain-types codomain-types]
  (letfn [(add-typed [[graph ids] type]
            (let [[vtype size] (if (vector? type) type [type 1])
                  [graph' id] (add-vertex graph {:vtype vtype :size size})]
              [graph' (conj ids id)]))]
    (let [[graph inputs] (reduce add-typed [(make-graph) []] domain-types)
          [graph outputs] (reduce add-typed [graph []] codomain-types)
          [graph _] (add-edge graph inputs outputs {:value value})]
      (-> graph (set-inputs inputs) (set-outputs outputs)))))

(defn compose
  "Sequential composition, returning a new graph."
  [left right]
  (when-not (= (codomain left) (domain right))
    (throw (ex-info "Graph boundaries do not compose"
                    {:left-codomain (codomain left)
                     :right-domain (domain right)})))
  (let [[graph vertex-map]
        (reduce (fn [[g mapping] vertex]
                  (let [[g' fresh] (add-vertex
                                    g (dissoc (vertex-data right vertex)
                                              :in-edges :out-edges
                                              :in-indices :out-indices))]
                    [g' (assoc mapping vertex fresh)]))
                [left {}] (vertices right))
        graph (reduce (fn [g edge]
                        (first (add-edge g
                                         (mapv vertex-map (source right edge))
                                         (mapv vertex-map (target right edge))
                                         (dissoc (edge-data right edge)
                                                 :source :target))))
                      graph (edges right))
        right-inputs (mapv vertex-map (:inputs right))
        right-outputs (mapv vertex-map (:outputs right))
        graph (set-outputs graph right-outputs)]
    (reduce (fn [g [kept removed]] (merge-vertices g kept removed))
            graph (map vector (:outputs left) right-inputs))))
