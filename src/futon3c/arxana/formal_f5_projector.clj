(ns futon3c.arxana.formal-f5-projector
  "F5b mathematical values, panel records, placements, and render operation."
  (:require [futon3c.arxana.formal-f5 :as f5]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.service :as service]))

(defn endpoint [role x] {:role role :kind :clusion :target {:clusion/id (:id x)}})
(defn relation [type endpoints content]
  (records/make-clusion {:type-revision [type 1] :endpoints endpoints
                         :asserted-content (assoc content :theory/version f5/theory-version)}))
(defn plan []
  (let [base (f5/plan "kernel-attestation-placeholder")
        entities (:entities base)
        cartesian (f5/entity :panel/cartesian :projection-panel)
        angular (f5/entity :panel/angular :projection-panel)
        value (fn [point n d] (relation :geometry/value [(endpoint :point point)]
                                        {:turn/numerator n :turn/denominator d}))
        values [(value (:i entities) 1 4) (value (:neg-i entities) 3 4)
                (value (:neg-one entities) 1 2)]
        selection (assoc (relation :geometry/selection
                                   [(endpoint :scene (:scene entities))
                                    (endpoint :map (get-in base [:maps :square]))]
                                   {:selected-at 1000}) :event-time 1000)]
    {:panels {:cartesian cartesian :angular angular}
     :values values :selection selection
     :records (vec (concat [cartesian angular selection] values))}))
(defn write! [cfg record]
  (service/dispatch cfg {:protocol/version "arxana.v1"
                         :message/id (str "formal-f5b:" (:id record))
                         :operation :clusion/assert :principal/id "formal-f5b/projector"
                         :body {:record record}}))
(defn materialise! [cfg]
  (let [p (plan)] (assoc p :writes (mapv #(write! cfg %) (:records p)))))
(defn placement
  "A drag is an event: placed-at is part of its identity (dragging back to an
  earlier offset is a new placement) and is its :event-time, which is what
  the store's role-reverse index orders by."
  [panel point dx dy placed-at]
  (assoc (relation :geometry/placement [(endpoint :panel panel) (endpoint :subject point)]
                   {:offset/x dx :offset/y dy :placed-at placed-at})
         :event-time placed-at))
(defn selection [scene map-record selected-at]
  (assoc (relation :geometry/selection [(endpoint :scene scene) (endpoint :map map-record)]
                   {:selected-at selected-at}) :event-time selected-at))
(defn cube-plan [geometry kernel-attestation-id]
  (let [entities (:entities geometry) cube (get-in geometry [:maps :cube])
        root-a (f5/entity :cube-root-1 :point) root-c (f5/entity :cube-root-3 :point)
        fibre (f5/entity :cube-over-neg-one :fibre)
        value (fn [point n d] (relation :geometry/value [(endpoint :point point)]
                                        {:turn/numerator n :turn/denominator d}))
        values [(value root-a 1 6) (value root-c 5 6)]
        fibre-of (f5/relation-rev :geometry/fibre-of 2
                                  [(endpoint :fibre fibre) (endpoint :map cube)
                                   (endpoint :value (:neg-one entities))]
                                  {:kernel/attestation-id kernel-attestation-id
                                   :lean/declaration "cube_fibre_neg_one_card"
                                   :fibre/cardinality 3})
        members (mapv #(relation :geometry/member
                                 [(endpoint :fibre fibre) (endpoint :member %)] {})
                      [root-a (:neg-one entities) root-c])]
    {:fibre fibre :points [root-a (:neg-one entities) root-c]
     :fibre-of fibre-of :members members :values values
     :records (vec (concat [root-a root-c fibre fibre-of] values members))}))
(defn render! [cfg panels & [map-id]]
  (service/dispatch cfg {:protocol/version "arxana.v1" :message/id "formal-f5b:render"
                         :operation :geometry/render :principal/id "formal-f5b/client"
                         :body (cond-> {:value/id :neg-one
                                       :cartesian/panel-id (get-in panels [:cartesian :id])
                                       :angular/panel-id (get-in panels [:angular :id])}
                                 map-id (assoc :map/id map-id))}))
