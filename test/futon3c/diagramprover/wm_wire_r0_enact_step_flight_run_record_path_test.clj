(ns futon3c.diagramprover.wm-wire-r0-enact-step-flight-run-record-path-test
  (:require [futon3c.diagramprover.wm-wire-temporal-run-products-14b :as products]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-temporal-courier-support :as support]))

(def wire-id [:r0-enact-step :flight-run :record-path])
(defn check [] (support/observe wire-id :none))
(def wire {:second-layer {:test 'futon3c.diagramprover.wm-wire-r0-enact-step-flight-run-record-path-test/stored-courier-changes-only-record
                          :kind :record :product [:stored] :intervention :before-reader}
           :wire wire-id :kind :witnessed-hermetically
           :test `real-courier-reaches-reader :check check
           :live-records-read support/live-records-read
           :note "MAP-2B-TEMPORAL: real writer and reader with isolated publication; no live temporal record claimed."})

(deftest real-courier-reaches-reader
  (let [r (check)]
    (is (some? (:writer r)))
    (is (some? (:product r)))
    (is (w/received? r) (pr-str r))))

(deftest carrier-intervention-is-detected
  (doseq [mode [:absent :different]]
    (let [r (support/observe wire-id mode)]
      (is (some? (:writer r)))
      (is (not (w/received? r)) (pr-str r)))))

(deftest historical-records-have-no-temporal-pair
  (is (support/live-absent?)))

(deftest stored-courier-changes-only-record
  (let [{:keys [values stored other-products products]} (products/run-products :record-path)]
    (prn :wire-2l-14b :record-path :before (first stored) :after (second stored)
         :status (mapv :status products))
    (is (every? some? values))
    (is (not= (first values) (second values)))
    (is (= values stored))
    (is (= (first other-products) (second other-products)))
    (is (= [:no-progress :no-progress] (mapv :status products)))))
