(ns futon3c.diagramprover.rmdiagram
  "Causal DAG/right-monogamous diagram functor for the MPZ regime.

  Each variable has one generator-output node. Every consumer generator names
  that same node as a source tentacle, so copying is node sharing rather than
  an explicit ordered copy comb. A zero-consumer node is legal discard."
  (:require [futon3c.diagramprover.causal.dag :as dag]
            [futon3c.diagramprover.rmgraph :as rm]))

(defn- outcome? [variable]
  (contains? #{:outcome :outcome-witness
               "outcome" "outcome_witness"}
             (:kind variable)))

(defn dag->rmdiagram
  "Render a causal DAG as a canonical right-monogamous open hypergraph."
  [causal-dag]
  (dag/validate causal-dag)
  (let [variables (sort (keys (:variables causal-dag)))
        arrow-index (into {} (map-indexed (fn [index arrow] [arrow index])
                                          (:arrows causal-dag)))
        [graph wires]
        (reduce
         (fn [[result mapping] variable-id]
           (let [[next-result wire]
                 (rm/add-vertex result
                                {:vtype :causal-wire :size 1
                                 :causal/producer variable-id
                                 :causal/role :generator-output})]
             [next-result (assoc mapping variable-id wire)]))
         [(rm/make-graph) {}] variables)
        with-generators
        (reduce
         (fn [result variable-id]
           (let [indexed-incoming
                 (->> (:arrows causal-dag)
                      (filter #(= variable-id (:to %)))
                      (map (fn [arrow] [(arrow-index arrow) arrow]))
                      (sort-by (juxt (comp :from second) first))
                      vec)
                 sources (mapv #(wires (:from (second %))) indexed-incoming)]
             (first
              (rm/add-edge
               result sources [(wires variable-id)]
               {:value variable-id
                :causal/variable (get-in causal-dag [:variables variable-id])
                :causal/indexed-incoming indexed-incoming}))))
         graph variables)
        outputs (->> variables
                     (filter #(and (empty? (dag/children causal-dag %))
                                   (outcome? (get-in causal-dag [:variables %]))))
                     (mapv wires))]
    (rm/canonicalize
     (-> with-generators
         (rm/set-outputs outputs)
         (assoc :causal/metadata
                (select-keys causal-dag
                             [:leak-edges :interventions :sensors :metadata]))))))

(defn rmdiagram->dag
  "Recover the complete normalized causal DAG carried by an RM diagram."
  [graph]
  (when-not (rm/rm-valid? graph)
    (throw (ex-info "Cannot decode invalid RM diagram" {})))
  (let [generators (rm/edges graph)
        variables
        (into (sorted-map)
              (map (fn [edge]
                     (let [data (rm/edge-data graph edge)]
                       [(:value data) (:causal/variable data)])))
              generators)
        indexed-arrows
        (mapcat #(get-in graph [:edata % :causal/indexed-incoming]) generators)
        metadata (:causal/metadata graph)]
    (dag/validate
     (merge {:variables variables
             :arrows (mapv second (sort-by first indexed-arrows))
             :leak-edges [] :interventions [] :sensors [] :metadata {}}
            metadata))))

(defn canonical?
  "True when the diagram is the canonical RM representative."
  [graph]
  (rm/canonical? graph))
