(ns futon3c.arxana.formal-f5-query
  "Public, record-derived and cardinality-checked geometry fibre query."
  (:require [futon3c.arxana.records :as records]))
(def theory-version "formal-arxana/geometry-v1")
(defn scene-record []
  (records/make-clusion {:type-revision [:geometry/entity 1] :endpoints []
                         :asserted-content {:entity/id :unit-circle-scene
                                            :entity/kind :scene
                                            :theory/version theory-version}}))
(defn- ok! [r] (or (:ok r) (throw (ex-info "geometry public read failed" {:response r}))))
(defn- record [read! id] (:record (ok! (read! :entity/get {:entity/id id}))))
(defn- reverse-ids [read! type role id]
  (:result/edge-ids (ok! (read! :graph/query {:query/type :role-reverse :edge/type type
                                              :endpoint/role role :endpoint/id id
                                              :time/order :asc :query/limit 200}))))
(defn- newest-ids
  "Newest first. futon1b orders role-reverse edges by the record's
  :event-time; ascending with a limit would return the OLDEST page."
  [read! type role id]
  (:result/edge-ids (ok! (read! :graph/query {:query/type :role-reverse :edge/type type
                                              :endpoint/role role :endpoint/id id
                                              :time/order :desc :query/limit 200}))))
(def iso-instant
  "The only selection/placement time format: futon1b compares :event-time as
  text, so mixed widths or formats (epoch ms vs 1000, with vs without
  millis) would order wrongly."
  #"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z")
(defn iso-instant? [x] (boolean (and (string? x) (re-matches iso-instant x))))
(def ^:private superscripts {\² 2 \³ 3 \⁴ 4 \⁵ 5 \⁶ 6 \⁷ 7 \⁸ 8 \⁹ 9})
(defn map-power
  "The exponent n of a stored power map, read from its :map/definition
  (:z↦zⁿ); nil for any other map."
  [map-record]
  (let [d (str (get-in map-record [:asserted-content :map/definition]))]
    (when-let [[_ sup] (re-matches #":?z↦z(.)" d)]
      (superscripts (first sup)))))
(defn- endpoint-id [r role]
  (get-in (first (filter #(= role (:role %)) (:endpoints r))) [:target :clusion/id]))
(defn- typed [category code detail]
  {:error/category category :error/code code :error/detail detail})
(defn named-record [read! name kind]
  (let [scene (record read! (:id (scene-record)))
        links (map #(record read! %)
                   (reverse-ids read! [:geometry/named 1] :scene (:id scene)))
        matches (filter #(and (= name (get-in % [:asserted-content :name]))
                              (= kind (get-in % [:asserted-content :object/kind]))) links)]
    (when (= 1 (count matches))
      (record read! (endpoint-id (first matches) :object)))))
(defn current-map-record [read!]
  (let [scene (record read! (:id (scene-record)))
        ids (newest-ids read! [:geometry/selection 1] :scene (:id scene))]
    (when (seq ids) (record read! (endpoint-id (record read! (first ids)) :map)))))
(defn current-map-id [read!]
  (let [scene (record read! (:id (scene-record)))
        ids (newest-ids read! [:geometry/selection 1] :scene (:id scene))
        selection (when (seq ids) (record read! (first ids)))
        map-record (when selection (record read! (endpoint-id selection :map)))]
    (get-in map-record [:asserted-content :map/id])))
(defn fibre [read! body]
  (let [map-id (:map/id body) value-id (:value/id body)
        map-record (named-record read! map-id :map)
        value-record (named-record read! value-id :point)]
    (cond
      (nil? map-record) (typed :missing :arxana.geometry/unknown-map {:map/id map-id})
      (nil? value-record) (typed :missing :arxana.geometry/unknown-value {:value/id value-id})
      :else
      (let [links (map #(record read! %)
                       (reverse-ids read! [:geometry/fibre-of 2] :map (:id map-record)))
            matching (filter #(= (:id value-record) (endpoint-id % :value)) links)]
        (if (not= 1 (count matching))
          (typed :missing :arxana.geometry/fibre-not-unique {:count (count matching)})
          (let [link (first matching) fibre-id (endpoint-id link :fibre)
                members (mapv #(record read! %)
                              (reverse-ids read! [:geometry/member 1] :fibre fibre-id))
                expected (get-in link [:asserted-content :fibre/cardinality])]
            (if (not= expected (count members))
              (typed :conflict :arxana.geometry/cardinality-mismatch
                     {:expected expected :actual (count members) :fibre/id fibre-id})
              (let [member-records (mapv #(record read! (endpoint-id % :member)) members)]
                {:result/status :complete :map/id map-id :value/id value-id
                 :fibre/id fibre-id :map/record-id (:id map-record)
                 :result/member-ids (mapv #(get-in % [:asserted-content :entity/id]) member-records)
                 :record/ids (vec (concat [(:id link) fibre-id (:id map-record) (:id value-record)]
                                          (map :id members) (map :id member-records)))
                 :kernel/attestation-id (get-in link [:asserted-content :kernel/attestation-id])
                 :lean/declaration (get-in link [:asserted-content :lean/declaration])
                 :fibre/cardinality expected}))))))))
