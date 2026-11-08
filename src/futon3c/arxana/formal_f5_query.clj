(ns futon3c.arxana.formal-f5-query
  "Public record-derived geometry fibre query."
  (:require [futon3c.arxana.records :as records]))
(def theory-version "formal-arxana/geometry-v1")
(defn- entity [id kind]
  (records/make-clusion {:type-revision [:geometry/entity 1] :endpoints []
                         :asserted-content {:entity/id id :entity/kind kind
                                            :theory/version theory-version}}))
(defn- endpoint [role x] {:role role :kind :clusion :target {:clusion/id (:id x)}})
(defn- square-map []
  (let [circle (entity :circle :space)]
    (records/make-clusion
     {:type-revision [:geometry/map 1]
      :endpoints [(endpoint :domain circle) (endpoint :codomain circle)]
      :asserted-content {:map/id :square :map/definition :z↦z²
                         :theory/version theory-version}})))
(defn- ok! [r] (or (:ok r) (throw (ex-info "geometry public read failed" {:response r}))))
(defn- record [read! id] (:record (ok! (read! :entity/get {:entity/id id}))))
(defn- reverse-ids [read! type role id]
  (:result/edge-ids (ok! (read! :graph/query {:query/type :role-reverse :edge/type type
                                              :endpoint/role role :endpoint/id id
                                              :time/order :asc :query/limit 100}))))
(defn fibre [read! body]
  (let [map-id (:map/id body) value-id (:value/id body)
        map-record (case map-id :square (square-map)
                         (throw (ex-info "unknown geometry map" {:map/id map-id})))
        value-record (case value-id :neg-one (entity :neg-one :point)
                           (throw (ex-info "unknown geometry value" {:value/id value-id})))
        _ (record read! (:id map-record)) _ (record read! (:id value-record))
        fibre-links (map #(record read! %)
                         (reverse-ids read! [:geometry/fibre-of 1] :map (:id map-record)))
        matching (filter #(= (:id value-record)
                             (get-in % [:endpoints 2 :target :clusion/id])) fibre-links)
        link (first matching)]
    (when-not (= 1 (count matching))
      (throw (ex-info "fibre is absent or ambiguous" {:count (count matching)})))
    (let [fibre-id (get-in link [:endpoints 0 :target :clusion/id])
          members (mapv #(record read! %)
                        (reverse-ids read! [:geometry/member 1] :fibre fibre-id))
          member-records (mapv #(record read! (get-in % [:endpoints 1 :target :clusion/id])) members)]
      {:result/status :complete :map/id map-id :value/id value-id
       :fibre/id fibre-id
       :result/member-ids (mapv #(get-in % [:asserted-content :entity/id]) member-records)
       :record/ids (vec (concat [(:id link) fibre-id (:id map-record) (:id value-record)]
                                (map :id members) (map :id member-records)))
       :kernel/attestation-id (get-in link [:asserted-content :kernel/attestation-id])})))
