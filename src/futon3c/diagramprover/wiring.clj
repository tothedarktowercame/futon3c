(ns futon3c.diagramprover.wiring
  "Ingest mission wiring specs as typed open hypergraphs and report structural
  wiring findings."
  (:require [futon3c.diagramprover.graph :as graph]))

(defn- fields-in [boxes]
  (->> boxes
       (mapcat #(concat (or (:reads %) []) (or (:writes %) [])))
       distinct
       (sort-by str)))

(defn ingest
  "Turn an EDN wiring spec into an open hypergraph.

  Fields are wire vertices. Boxes are hyperedges from their read fields to
  their written fields. Missing read/write collections are empty."
  [{:keys [spec/id boxes]}]
  (let [[initial field->vertex]
        (reduce (fn [[g index] field]
                  (let [[g' vertex] (graph/add-vertex
                                     g {:vtype :wiring/field :field field})]
                    [g' (assoc index field vertex)]))
                [(graph/make-graph) {}]
                (fields-in boxes))]
    (reduce (fn [g box]
              (first
               (graph/add-edge
                g
                (mapv field->vertex (or (:reads box) []))
                (mapv field->vertex (or (:writes box) []))
                {:box/id (:box/id box)})))
            (assoc initial :spec/id id)
            (or boxes []))))

(defn written-never-read
  "Return findings for fields written by at least one box and read by none."
  [wiring-graph]
  (->> (graph/vertices wiring-graph)
       (keep (fn [vertex]
               (let [writers (graph/in-edges wiring-graph vertex)]
                 (when (and (seq writers)
                            (empty? (graph/out-edges wiring-graph vertex)))
                   {:finding :written-never-read
                    :field (:field (graph/vertex-data wiring-graph vertex))
                    :writers (->> writers
                                  (map #(-> (graph/edge-data wiring-graph %)
                                            :box/id))
                                  (sort-by str)
                                  vec)}))))
       (sort-by (comp str :field))
       vec))

(defn read-never-written
  "Return findings for fields read by at least one box and written by none."
  [wiring-graph]
  (->> (graph/vertices wiring-graph)
       (keep (fn [vertex]
               (let [readers (graph/out-edges wiring-graph vertex)]
                 (when (and (seq readers)
                            (empty? (graph/in-edges wiring-graph vertex)))
                   {:finding :read-never-written
                    :field (:field (graph/vertex-data wiring-graph vertex))
                    :readers (->> readers
                                  (map #(-> (graph/edge-data wiring-graph %)
                                            :box/id))
                                  (sort-by str)
                                  vec)}))))
       (sort-by (comp str :field))
       vec))

(defn multiply-written
  "Return findings for fields written by two or more boxes."
  [wiring-graph]
  (->> (graph/vertices wiring-graph)
       (keep (fn [vertex]
               (let [writers (graph/in-edges wiring-graph vertex)]
                 (when (<= 2 (count writers))
                   {:finding :multiply-written
                    :field (:field (graph/vertex-data wiring-graph vertex))
                    :writers (->> writers
                                  (map #(-> (graph/edge-data wiring-graph %)
                                            :box/id))
                                  (sort-by str)
                                  vec)}))))
       (sort-by (comp str :field))
       vec))
