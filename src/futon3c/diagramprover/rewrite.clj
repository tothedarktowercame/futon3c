(ns futon3c.diagramprover.rewrite
  "Pure double-pushout rewriting of open hypergraphs."
  (:require [futon3c.diagramprover.graph :as graph]
            [futon3c.diagramprover.matcher :as matcher]))

(defn- unsupported-frobenius! [lhs-vertex]
  (throw (ex-info "Rewriting modulo Frobenius is not supported"
                  {:vertex lhs-vertex :reason :non-1-1-boundary})))

(defn- remove-lhs
  [{:keys [lhs]} {:keys [codomain vertex-map edge-map]}]
  (let [without-edges
        (reduce (fn [context lhs-edge]
                  (graph/remove-edge context (get edge-map lhs-edge)))
                codomain (graph/edges lhs))]
    (reduce
     (fn [{:keys [graph] :as state} lhs-vertex]
       (let [host-vertex (get vertex-map lhs-vertex)]
         (if (graph/is-boundary? lhs lhs-vertex)
           (let [input-count (count (get-in lhs [:vdata lhs-vertex
                                                 :in-indices]))
                 output-count (count (get-in lhs [:vdata lhs-vertex
                                                  :out-indices]))]
             (cond
               (or (> input-count 1) (> output-count 1))
               (unsupported-frobenius! lhs-vertex)

               (and (= 1 input-count) (= 1 output-count))
               (let [[context inputs outputs]
                     (graph/explode-vertex graph host-vertex)]
                 (when-not (and (= 1 (count inputs)) (= 1 (count outputs)))
                   (unsupported-frobenius! lhs-vertex))
                 (-> state
                     (assoc :graph context)
                     (assoc-in [:input-map lhs-vertex] (first inputs))
                     (assoc-in [:output-map lhs-vertex] (first outputs))))

               :else state))
           (assoc state :graph (graph/remove-vertex graph host-vertex true)))))
     {:graph without-edges :input-map {} :output-map {}}
     (graph/vertices lhs))))

(defn- replace-image [vertex-map removed kept]
  (reduce-kv (fn [result vertex image]
               (assoc result vertex (if (= image removed) kept image)))
             {} vertex-map))

(defn- embed-boundaries
  [context lhs rhs original-map input-map output-map]
  (let [with-inputs
        (reduce (fn [{:keys [vertex-map] :as state}
                     [lhs-vertex rhs-vertex]]
                  (assoc state :vertex-map
                         (assoc vertex-map rhs-vertex
                                (get input-map lhs-vertex
                                     (get original-map lhs-vertex)))))
                {:graph context :vertex-map {}}
                (map vector (:inputs lhs) (:inputs rhs)))]
    (reduce
     (fn [{:keys [graph vertex-map] :as state}
          [lhs-vertex rhs-vertex]]
       (let [image (get output-map lhs-vertex
                        (get original-map lhs-vertex))]
         (if-let [existing (get vertex-map rhs-vertex)]
           (if (= existing image)
             state
             (assoc state
                    :graph (graph/merge-vertices graph existing image)
                    :vertex-map (replace-image vertex-map image existing)))
           (assoc state :vertex-map (assoc vertex-map rhs-vertex image)))))
     with-inputs
     (map vector (:outputs lhs) (:outputs rhs)))))

(defn- embed-rhs
  [{:keys [lhs rhs]} original-map
   {:keys [graph input-map output-map]}]
  (let [{context :graph boundary-map :vertex-map}
        (embed-boundaries graph lhs rhs original-map input-map output-map)
        [context vertex-map]
        (reduce
         (fn [[current mapping] rhs-vertex]
           (if (graph/is-boundary? rhs rhs-vertex)
             [current mapping]
             (let [[next-graph fresh]
                   (graph/add-vertex
                    current
                    (dissoc (graph/vertex-data rhs rhs-vertex)
                            :in-edges :out-edges :in-indices :out-indices))]
               [next-graph (assoc mapping rhs-vertex fresh)])))
         [context boundary-map] (graph/vertices rhs))
        [context edge-map]
        (reduce
         (fn [[current mapping] rhs-edge]
           (let [[next-graph fresh]
                 (graph/add-edge
                  current
                  (mapv vertex-map (graph/source rhs rhs-edge))
                  (mapv vertex-map (graph/target rhs rhs-edge))
                  (dissoc (graph/edge-data rhs rhs-edge) :source :target))]
             [next-graph (assoc mapping rhs-edge fresh)]))
         [context {}] (graph/edges rhs))]
    {:graph context
     :match {:domain rhs
             :codomain context
             :vertex-map vertex-map
             :edge-map edge-map}}))

(defn dpo
  "Apply one match of a rule by double pushout.

  Returns the rewritten graph and the induced RHS match."
  [{:keys [lhs] :as rule} {:keys [domain vertex-map edge-map] :as match}]
  (when-not (= lhs domain)
    (throw (ex-info "Match domain is not the rule LHS" {})))
  (when-not (and (= (count vertex-map) (graph/num-vertices lhs))
                 (= (count edge-map) (graph/num-edges lhs)))
    (throw (ex-info "DPO requires a total match" {})))
  (embed-rhs rule vertex-map (remove-lhs rule match)))

(defn rule-applications
  "Return the lazy relation of all legal convex applications to graph."
  [rule graph]
  (map #(dpo rule %) (matcher/match-rule rule graph)))
