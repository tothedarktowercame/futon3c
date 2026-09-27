(ns futon3c.diagramprover.wm-wire-outer-record-products-15a
  "outer_cascade.clj:109-116,126-149 records these inputs; only eligible and
  delta-g feed the law. Two unequal deltas, seed 42, in every intervention."
  (:require [clojure.test :refer [is]]
            [futon2.aif.outer-cascade :as cascade]
            [futon2.aif.target-field :as field]
            [futon2.aif.enactment-habit :as habit]
            [futon2.aif.flight-runner :as runner]))

(defn entries []
  (mapv (fn [target delta]
          (assoc (@#'field/step {:target target :kind :mission} :read-criteria)
                 :eligible true :delta-g {:value delta :universe [:shared]}
                 :universe #{:shared} :constructed-candidate {:produces #{:shared}}))
        ["A" "B"] [0.0 2.0]))

(defn folded [click]
  (:enactment-records
   (habit/fold nil [(habit/increment
                    {:click click :candidate :c :attempts [{:pattern :p :success true}]}
                    [:pattern-cascade "A" [:p] {}] [])])))

(defn publication [status repair-id]
  (:publication-observed
   ((runner/observe-publication-fn
     {:repair-id-fn (fn [& _] repair-id)
      :fetch-run-record (fn [_] {:repair/publication [{:repair/id "repair"
                                                     :status status :receipt "receipt"}]})})
    {:target "A"} {:click-id "click"})))

(defn carriers [k es]
  (case k
    :next-step [(:next-step (first es))
                (:next-step (@#'field/step {:target "A"} :ready))]
    :pair-overlap [(:pair-overlap (first (field/with-pair-overlap es)))
                   (:pair-overlap (first (field/with-pair-overlap
                                         (assoc-in es [0 :constructed-candidate :produces] #{:elsewhere}))))
                   (:pair-overlap (first (field/with-pair-overlap
                                         (update es 0 dissoc :constructed-candidate))))]
    :enactment-records [(folded "click-1") (folded "click-2")]
    :publication-observed [(publication :receipt-committed "repair")
                           (publication :publication-refused "repair")
                           (publication :receipt-committed nil)]))

(defn assert-record-products [k]
  (let [es (entries)
        per-entry? (contains? #{:next-step :pair-overlap} k)
        values (carriers k es)
        path (cond-> [:target-selection :inputs k] per-entry? (conj "A"))
        results (mapv (fn [value]
                        (cascade/select
                         (cond-> {:field {:considered es :feasible es :exclusions []} :seed 42}
                           per-entry? (assoc-in [:field :feasible 0 k] value)
                           (not per-entry?) (assoc k value)))) values)
        baseline (first results)
        law-paths [:support :posterior :g :g-defined-on :law :draw :chosen]]
    (is (= :mixed (get-in baseline [:target-selection :law])))
    (is (not= (get-in baseline [:target-selection :posterior "A"])
              (get-in baseline [:target-selection :posterior "B"])))
    (is (not= (first values) (second values)))
    (doseq [[value result] (map vector values results)]
      (is (= value (get-in result path)))
      (is (= (select-keys (:target-selection baseline) law-paths)
             (select-keys (:target-selection result) law-paths)))
      (is (= (:chosen-target baseline) (:chosen-target result))))
    (when (= 3 (count values))
      (is (contains? (last values) :absent))
      (is (= (last values) (get-in (last results) path))))
    (println k :seed 42 :posterior (get-in baseline [:target-selection :posterior])
             :chosen (:chosen-target baseline) :recorded-inputs values)))
