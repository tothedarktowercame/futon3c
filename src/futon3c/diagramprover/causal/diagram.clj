(ns futon3c.diagramprover.causal.diagram
  "Canonical causal DAG/string-diagram functor for the plain SMC kernel."
  (:require [futon3c.diagramprover.causal.dag :as dag]
            [futon3c.diagramprover.graph :as graph]))

(defn- outcome? [variable]
  (contains? #{:outcome :outcome-witness
               "outcome" "outcome_witness"}
             (:kind variable)))

(defn- add-wire [g payload]
  (graph/add-vertex g (merge {:vtype :causal-wire :size 1} payload)))

(defn- mark-leaf [g vertex arrow index]
  (-> g
      (assoc-in [:vdata vertex :causal/consumer] (:to arrow))
      (assoc-in [:vdata vertex :causal/arrow] arrow)
      (assoc-in [:vdata vertex :causal/arrow-index] index)))

(defn- add-copy-comb
  [g owner source ordered-arrows arrow-index]
  (let [first-arrow (first ordered-arrows)
        remaining (next ordered-arrows)
        [g branch] (add-wire g {:causal/producer owner})
        g (mark-leaf g branch first-arrow (arrow-index first-arrow))]
    (if (= 1 (count remaining))
      (let [last-arrow (first remaining)
            [g last-branch] (add-wire g {:causal/producer owner})
            g (mark-leaf g last-branch last-arrow (arrow-index last-arrow))]
        (first (graph/add-edge g [source] [branch last-branch]
                               {:value :copy :causal/owner owner})))
      (let [[g rest-wire] (add-wire g {:causal/producer owner
                                       :causal/role :copy-spine})
            g (first (graph/add-edge g [source] [branch rest-wire]
                                     {:value :copy :causal/owner owner}))]
        (add-copy-comb g owner rest-wire (vec remaining) arrow-index)))))

(defn dag->diagram
  "Render a DAG as the unique right-nested, consumer-sorted copy-comb form."
  [causal-dag]
  (dag/validate causal-dag)
  (let [variables (sort (keys (:variables causal-dag)))
        arrow-index (into {} (map-indexed (fn [index arrow] [arrow index])
                                          (:arrows causal-dag)))
        [g roots]
        (reduce
         (fn [[g roots] variable-id]
           (let [[g root] (add-wire g {:causal/producer variable-id
                                       :causal/role :generator-output})
                 outgoing (->> (:arrows causal-dag)
                               (filter #(= variable-id (:from %)))
                               (sort-by (juxt :to arrow-index))
                               vec)
                 g (case (count outgoing)
                     0 (if (outcome? (get-in causal-dag [:variables variable-id]))
                         g
                         (first (graph/add-edge g [root] []
                                                {:value :discard
                                                 :causal/owner variable-id})))
                     1 (mark-leaf g root (first outgoing)
                                  (arrow-index (first outgoing)))
                     (add-copy-comb g variable-id root outgoing arrow-index))]
             [g (assoc roots variable-id root)]))
         [(graph/make-graph) {}] variables)
        g (reduce
           (fn [g variable-id]
             (let [inputs (->> (:vdata g)
                               (keep (fn [[vertex data]]
                                       (when (= variable-id
                                                (:causal/consumer data))
                                         [vertex data])))
                               (sort-by (fn [[_ data]]
                                          [(:causal/producer data)
                                           (:causal/arrow-index data)]))
                               (mapv first))]
               (first (graph/add-edge
                       g inputs [(roots variable-id)]
                       {:value variable-id
                        :causal/variable (get-in causal-dag
                                                 [:variables variable-id])}))))
           g variables)
        outputs (->> variables
                     (filter #(and (empty? (dag/children causal-dag %))
                                   (outcome? (get-in causal-dag [:variables %]))))
                     (mapv roots))]
    (-> g
        (graph/set-outputs outputs)
        (assoc :causal/metadata
               (select-keys causal-dag
                            [:leak-edges :interventions :sensors :metadata])))))

(defn- generator-edges [g]
  (remove #(contains? #{:copy :discard} (:value (graph/edge-data g %)))
          (graph/edges g)))

(defn diagram->dag
  "Recover the complete normalized DAG carried by a causal diagram."
  [g]
  (let [generators (generator-edges g)
        variables (into (sorted-map)
                        (map (fn [edge]
                               (let [data (graph/edge-data g edge)]
                                 [(:value data) (:causal/variable data)])))
                        generators)
        indexed-arrows
        (for [edge generators
              vertex (graph/source g edge)
              :let [data (graph/vertex-data g vertex)]
              :when (:causal/arrow data)]
          [(:causal/arrow-index data) (:causal/arrow data)])
        metadata (:causal/metadata g)]
    (dag/validate
     (merge {:variables variables
             :arrows (mapv second (sort-by first indexed-arrows))
             :leak-edges [] :interventions [] :sensors [] :metadata {}}
            metadata))))

(defn canonical?
  "True exactly when re-rendering recovered adjacency is structurally fixed."
  [g]
  (try
    (= g (dag->diagram (diagram->dag g)))
    (catch Exception _ false)))
