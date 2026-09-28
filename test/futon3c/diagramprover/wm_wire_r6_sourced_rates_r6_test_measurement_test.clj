(ns futon3c.diagramprover.wm-wire-r6-sourced-rates-r6-test-measurement-test
  "Rates wire, witnessed by real calls using ten real subjects admitted through the store and reader.
  See support/live-records-read for the live records lacking both ends."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))

(def wire-id [:r6-sourced-rates :r6-test :measurement])
(def producer (delay (producer-record/record "rates-observe-g29")))
(defn- fields [] (get-in @producer [:wires wire-id]))
(defn check [] (:primary (fields)))
(def wire {:second-layer {:test 'futon3c.diagramprover.wm-wire-r6-sourced-rates-r6-test-measurement-test/reader-product-changes-at-the-carrier :kind :value-varying
                   :product [:reports] :intervention :before-reader}
   :wire wire-id :kind :witnessed-hermetically
           :test `the-writers-value-reaches-the-reader :check check
           :live-records-read []})

(deftest the-writers-value-reaches-the-reader
  (let [o (check)]
    (is (seq (:writer o)))
    (is (every? #(and (pos? (get-in % [:false-neg :denominator] 0))
                      (pos? (get-in % [:false-pos :denominator] 0)))
                (vals (:writer o))) "both cells measured, never an :absent default")
    (is (w/received? o))))

(deftest typed-absence-carrier-does-not-witness-the-wire
  (is (not (w/received? (get-in (fields) [:interventions :absent])))))

(deftest different-carrier-does-not-witness-the-wire
  (let [o (get-in (fields) [:interventions :different])]
    (is (some? (:reader o)))
    (is (not (w/received? o)))))

(deftest live-records-lack-both-ends
  (is (seq (:live-records @producer))))

(deftest reader-product-changes-at-the-carrier
  (let [before (get-in (fields) [:second-layer :before])
        after (get-in (fields) [:second-layer :after])]
    (is (pos? (:calls before)))
    (is (pos? (:calls after)))
    (is (= [:pass] (:types before)))
    (is (= [:fail] (:types after)))
    (is (false? (:errors? after)))))
