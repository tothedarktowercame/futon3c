(ns futon3c.diagramprover.wiring
  "Ingest mission wiring specs as typed open hypergraphs and report structural
  wiring findings."
  (:require [clojure.string :as str]
            [futon3c.diagramprover.graph :as graph]))

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

(defn- site-path [repo-root site]
  (cond
    (:file site) (str (java.io.File. repo-root (:file site)))
    (:ns site) (str (java.io.File.
                     repo-root
                     (str "src/"
                          (-> (:ns site)
                              (str/replace "." "/")
                              (str/replace "-" "_"))
                          ".clj")))
    :else (throw (ex-info "Wiring site must name :file or :ns" {:site site}))))

(defn- field-occurs? [text field]
  (boolean
   (re-find (re-pattern
             (str (java.util.regex.Pattern/quote (str field)) "(?![\\w-])"))
            text)))

(defn conformance
  "Compare declared box reads/writes with occurrences in their named sites.

  Occurrence scanning is deliberately textual and counts comments. A commented
  reader is useful drift to surface, and this cheap check intentionally avoids
  pretending to provide parser-level or var-level precision. Boxes without a
  site are exempt from conformance findings."
  [repo-root {:keys [boxes]}]
  (let [boxes (or boxes [])
        field-universe (set (fields-in boxes))
        boxes-by-site (group-by :site (filter :site boxes))
        ;; An unreadable or malformed site is a FINDING, not an exception: the
        ;; checker's own boundary must not escape unstructured (the ToolBackend
        ;; lesson from the peripheral session, applied to the verifier itself).
        site-text (into {}
                        (map (fn [[site _]]
                               [site (try {:text (slurp (site-path repo-root site))}
                                          (catch Exception e
                                            {:error (str (.getMessage e))}))]))
                        boxes-by-site)
        unreadable-sites
        (for [[site {:keys [error]}] site-text
              :when error]
          {:finding :site-unreadable :site site :error error})
        missing-declarations
        (for [box boxes
              :let [site (:site box)
                    text (get-in site-text [site :text])]
              :when (and site text)
              [role fields] [[:reads (or (:reads box) [])]
                             [:writes (or (:writes box) [])]]
              field fields
              :when (not (field-occurs? text field))]
          {:finding :declaration-without-occurrence
           :box/id (:box/id box)
           :field field
           :role role
           :site site})
        undeclared-occurrences
        (for [[site site-boxes] boxes-by-site
              :let [text (get-in site-text [site :text])
                    declared-here (set (fields-in site-boxes))]
              :when text
              field field-universe
              :when (and (not (contains? declared-here field))
                         (field-occurs? text field))]
          {:finding :occurrence-without-declaration
           :field field
           :site site
           :declared-by []})]
    (->> (concat unreadable-sites missing-declarations undeclared-occurrences)
         (sort-by (juxt (comp str :finding) (comp str :field)))
         vec)))
