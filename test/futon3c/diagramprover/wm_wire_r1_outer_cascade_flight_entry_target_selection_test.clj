(ns futon3c.diagramprover.wm-wire-r1-outer-cascade-flight-entry-target-selection-test
  "Wire [:r1-outer-cascade :flight-entry :target-selection]. Real entry calls;
  no live record carries both ends. See support/live-records-read."
  (:require [futon3c.diagramprover.wm-wire-entry-products-10a :as products]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-plan-support :as support]))

(defn observe
  ([] (observe identity))
  ([tamper] (support/observe :entry :target-selection tamper)))

(defn check [] (observe))

(def wire
  {:wire [:r1-outer-cascade :flight-entry :target-selection]
   :kind :witnessed-hermetically
   :test `the-writers-value-reaches-the-reader
   :second-layer {:test `entry-stores-provenance-without-changing-flight-wants
                  :kind :record :product [:products]
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

(deftest entry-stores-provenance-without-changing-flight-wants
  (doseq [mission products/missions]
    (let [r (products/products mission :target-selection)]
      (is (= (:written r) (:products r)))
      (is (apply not= (:products r)))
      (is (apply = (:flights r)))
      (is (apply = (:wants r)) "Real click-wants is unchanged, not just the placement.")
      (is (seq (get-in r [:wants 0 :wants])))
      (is (= [(:target mission) (:target mission)] (:target r)))
      (is (= [(:path mission) (:path mission)] (:path r)))
      (println :target-selection (:target mission) :products (:products r)
               :wants (get-in r [:wants 0 :wants])))))
