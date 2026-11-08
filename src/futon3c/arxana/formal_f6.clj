(ns futon3c.arxana.formal-f6
  "F6 argument records: semantic support is separate from projection layout."
  (:require [futon3c.arxana.formal-f5-query :as f5-query]
            [futon3c.arxana.records :as records]))

(def forbidden-content-layout-keys
  #{:zone :order :margin? :coordinate :coordinates :x :y :offset/x :offset/y :placed-at})

(defn- leaked-layout-keys [value]
  (cond
    (map? value) (concat (filter forbidden-content-layout-keys (keys value))
                         (mapcat leaked-layout-keys (vals value)))
    (coll? value) (mapcat leaked-layout-keys value)
    :else []))

(defn endpoint [role record]
  {:role role :kind :clusion :target {:clusion/id (:id record)}})

(defn item [id kind content record-ids]
  (when-let [leaked (seq (distinct (leaked-layout-keys content)))]
    (throw (ex-info "layout field in F6 content"
                    {:error :arxana.f6/layout-in-content :fields (vec leaked)})))
  (records/make-clusion
   {:type-revision [:f6/item 1] :endpoints []
    :asserted-content {:item/id id :item/kind kind :content content
                       :record/ids (vec (sort record-ids))}}))

(defn relation [type endpoints content]
  (records/make-clusion {:type-revision [type 1] :endpoints endpoints
                         :asserted-content content}))

(defn placement [argument item-record zone order x y placed-at actor]
  (when-not (f5-query/iso-instant? placed-at)
    (throw (ex-info "placement time must be fixed-width ISO millis"
                    {:error :arxana.f6/invalid-event-time :placed-at placed-at})))
  (assoc (relation :f6/placement
                   [(endpoint :argument argument) (endpoint :item item-record)]
                   {:zone zone :order order :offset/x (or x 0) :offset/y (or y 0)
                    :placed-at placed-at :actor actor})
         :event-time placed-at))

(defn support [claim supporting method evidence actor asserted-at reason]
  (when-not (and actor reason (f5-query/iso-instant? asserted-at))
    (throw (ex-info "support attribution and fixed-width time required"
                    {:error :arxana.f6/support-attribution-required})))
  (assoc (relation :f6/supports
                   [(endpoint :supported-claim claim)
                    (endpoint :supporting-content supporting)]
                   {:method method :evidence evidence :actor actor
                    :asserted-at asserted-at :reason reason})
         :event-time asserted-at))

(defn plan [geometry panels kernel-attestation-id]
  (let [argument (item :unit-circle-argument :argument
                       {:title "Fibres of power maps on the unit circle"} [])
        square-claim (item :square-fibre-claim :claim
                           {:text "The square map's fibre over -1 is {i, -i}."}
                           [(:id (:fibre-of geometry))])
        cube-claim (item :cube-fibre-claim :claim
                         {:text "The cube map's fibre over -1 has three points."}
                         [(:id (get-in geometry [:maps :cube]))])
        definition (item :power-map-definition :definition
                         {:text "The n-th power map sends z to z^n on S¹."}
                         [(:id (get-in geometry [:maps :square]))
                          (:id (get-in geometry [:maps :cube]))])
        diagram (item :unit-circle-diagram :diagram
                      {:description "The stored Cartesian and angular projections."}
                      [(get-in panels [:cartesian :id]) (get-in panels [:angular :id])
                       (:id (get-in geometry [:entities :scene]))])
        sidenote (item :cut-sidenote :sidenote
                       {:text "The angular projection has a cut at zero turns."}
                       [(get-in panels [:angular :id])])
        items [square-claim cube-claim definition diagram sidenote]
        contains (mapv #(relation :f6/contains
                                  [(endpoint :argument argument) (endpoint :item %)] {}) items)
        supports [(support square-claim diagram {:id "formal-f5/render" :version 1}
                           [kernel-attestation-id] "formal-f6/fixture"
                           "2026-11-08T00:00:00.000Z" "diagram exhibits the square fibre")
                  (support square-claim definition {:id "definition/application" :version 1}
                           [kernel-attestation-id] "formal-f6/fixture"
                           "2026-11-08T00:00:00.001Z" "definition fixes the map")
                  (support cube-claim sidenote {:id "projection/qualification" :version 1}
                           [] "formal-f6/fixture" "2026-11-08T00:00:00.002Z"
                           "the cut qualifies the angular presentation")]
        placements (mapv (fn [record n zone]
                           (placement argument record zone n 0 0
                                      (format "2026-11-08T00:00:01.%03dZ" n)
                                      "formal-f6/fixture"))
                         items (range) [:main :main :main :main :margin])]
    {:argument argument :items {:square-claim square-claim :cube-claim cube-claim
                                :definition definition :diagram diagram :sidenote sidenote}
     :contains contains :supports supports :placements placements
     :records (vec (concat [argument] items contains supports placements))}))

(defn- ok! [response]
  (or (:ok response) (throw (ex-info "F6 public operation failed" {:response response}))))
(defn- read-record [read! id]
  (:record (ok! (read! :entity/get {:entity/id id}))))
(defn- reverse-ids [read! type role id order]
  (:result/edge-ids
   (ok! (read! :graph/query {:query/type :role-reverse :edge/type type
                             :endpoint/role role :endpoint/id id
                             :time/order order :query/limit 200}))))
(defn- endpoint-id [record role]
  (get-in (first (filter #(= role (:role %)) (:endpoints record))) [:target :clusion/id]))

(defn argument-view [read! argument-id]
  (let [argument (read-record read! argument-id)
        contains (mapv #(read-record read! %)
                       (reverse-ids read! [:f6/contains 1] :argument argument-id :asc))
        item-ids (mapv #(endpoint-id % :item) contains)
        items (mapv (fn [id]
                      (let [record (read-record read! id)
                            placements (mapv #(read-record read! %)
                                             (reverse-ids read! [:f6/placement 1]
                                                          :item id :desc))]
                        {:record/id id :record record :placement (first placements)
                         :placement/history-ids (mapv :id placements)})) item-ids)
        supports (->> item-ids
                      (mapcat #(reverse-ids read! [:f6/supports 1]
                                            :supporting-content % :desc))
                      distinct (mapv #(read-record read! %)))
        current (->> supports (group-by #(endpoint-id % :supporting-content))
                     vals (map (fn [rows]
                                 (first (sort-by :event-time
                                                 (fn [a b] (compare b a)) rows))))
                     (map :id) set)]
    {:result/status :complete :argument/id argument-id :argument argument
     :items items
     :supports (mapv (fn [record] {:record/id (:id record) :record record
                                   :current? (contains? current (:id record))}) supports)
     :current/support-ids (vec (sort current))
     :drops [:html :pixel-layout :editor-state]}))

(defn move! [read! write! principal message-id body]
  (let [argument (read-record read! (:argument/id body))
        item-record (read-record read! (:item/id body))
        record (placement argument item-record (:zone body) (:order body)
                          (:offset/x body) (:offset/y body) (:placed-at body) principal)
        result (write! :clusion/assert {:record record} (str message-id ":placement"))]
    (if-let [error (:error result)] error
      {:result/status :complete :placement/id (:id record) :item/id (:id item-record)
       :argument/id (:id argument)})))

(defn re-support! [read! write! principal message-id body]
  (when-not (and (string? principal) (not-empty principal) (not-empty (:reason body)))
    (throw (ex-info "re-support must be attributable"
                    {:error :arxana.f6/re-support-attribution-required})))
  (let [supporting (read-record read! (:supporting-content/id body))
        claim (read-record read! (:supported-claim/id body))
        existing (mapv #(read-record read! %)
                       (reverse-ids read! [:f6/supports 1] :supporting-content
                                    (:id supporting) :desc))
        current (first existing)]
    (if (= (:id claim) (some-> current (endpoint-id :supported-claim)))
      {:result/status :complete :support/id (:id current)
       :current/support-id (:id current) :created? false}
      (let [new-support (support claim supporting
                                 (or (:method body) {:id "operator/re-support" :version 1})
                                 (or (:evidence body) []) principal (:changed-at body)
                                 (:reason body))
            sw (write! :clusion/assert {:record new-support} (str message-id ":support"))
            supersession (when current
                           (assoc (relation :f6/re-supports
                                            [(endpoint :successor new-support)
                                             (endpoint :predecessor current)]
                                            {:actor principal :changed-at (:changed-at body)
                                             :reason (:reason body)})
                                  :event-time (:changed-at body)))
            xw (when (and (:ok sw) supersession)
                 (write! :clusion/assert {:record supersession}
                         (str message-id ":supersession")))]
        (if-let [error (or (:error sw) (:error xw))] error
          {:result/status :complete :support/id (:id new-support)
           :supersession/id (:id supersession) :current/support-id (:id new-support)
           :created? true})))))

(defn materialise! [dispatch! plan]
  (mapv (fn [record]
          (dispatch! {:protocol/version "arxana.v1"
                      :message/id (str "formal-f6a:" (:id record))
                      :operation :clusion/assert :principal/id "formal-f6/fixture"
                      :body {:record record}}))
        (:records plan)))
