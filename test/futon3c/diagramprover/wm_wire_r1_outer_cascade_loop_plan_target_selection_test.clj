(ns futon3c.diagramprover.wm-wire-r1-outer-cascade-loop-plan-target-selection-test
  "Wire [:r1-outer-cascade :loop-plan :target-selection]. Real loop calls;
  no live record carries both ends. See support/live-records-read."
  (:require [futon3c.diagramprover.wm-wire-loop-products-12a :as products]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-plan-support :as support]))

(defn observe
  ([] (observe identity))
  ([tamper] (support/observe :loop :target-selection tamper)))

(defn check [] (observe))

(def wire
  {:wire [:r1-outer-cascade :loop-plan :target-selection]
   :kind :witnessed-hermetically
   :test `the-writers-value-reaches-the-reader
   :second-layer {:test `selector-field-controls-loop-plan
                  :kind :record :product [:result :plan]
                  :intervention :before-reader}
   :check check :live-records-read support/live-records-read})

(deftest the-writers-value-reaches-the-reader
  (is (w/received? (check))))

(deftest typed-absence-at-the-reader-fails
  (let [o (observe #(assoc % :target-selection {:absent :not-carried}))]
    (is (w/typed-absence? (:reader o)))
    (is (not (w/received? o)))))

(deftest another-value-at-the-reader-fails
  (let [o (observe #(assoc % :target-selection {:chosen "M-other"}))]
    (is (some? (:reader o)))
    (is (not (w/received? o)))))

(deftest live-records-do-not-witness-this-wire
  (support/assert-live-records))

(deftest selector-field-controls-loop-plan
  (let [[a b] (products/products :target-selection)
        pa (get-in a [:result :plan]) pb (get-in b [:result :plan])]
    (is (= (:written a) (:written b)) "Same real field, seed, and selector output.")
    (is (not= (get-in pa [:placement :target-selection]) (get-in pb [:placement :target-selection])))
    (is (= (get-in b [:handed :target-selection]) (get-in pb [:placement :target-selection])))
    (is (= (:target-selection pb) (get-in pb [:placement :target-selection])))
    (is (= (update (dissoc pa :target-selection) :placement dissoc :target-selection)
           (update (dissoc pb :target-selection) :placement dissoc :target-selection)))
    (println :target-selection :values [(get-in pa [:placement :target-selection]) (get-in pb [:placement :target-selection])])) )
