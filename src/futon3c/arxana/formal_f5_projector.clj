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
                (value (:neg-one entities) 1 2)]]
    {:panels {:cartesian cartesian :angular angular}
     :values values :records (vec (concat [cartesian angular] values))}))
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
(defn render! [cfg panels]
  (service/dispatch cfg {:protocol/version "arxana.v1" :message/id "formal-f5b:render"
                         :operation :geometry/render :principal/id "formal-f5b/client"
                         :body {:map/id :square :value/id :neg-one
                                :cartesian/panel-id (get-in panels [:cartesian :id])
                                :angular/panel-id (get-in panels [:angular :id])}}))
