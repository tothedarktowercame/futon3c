(ns futon3c.diagramprover.wm-wire-r6-sourced-rates-r6-cascade-lane-scoped-rates-test
  "Rates wire, witnessed by real calls using ten real subjects admitted through the store and reader.
  See support/live-records-read for the live records lacking both ends."
  (:require [futon3c.diagramprover.wm-wire-rates-products-support :as products]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-rates-support :as support]))

(defn check [] (support/observe :sourced-rates identity))
(def wire {:second-layer {:test 'futon3c.diagramprover.wm-wire-r6-sourced-rates-r6-cascade-lane-scoped-rates-test/reader-product-changes-at-the-carrier :kind :value-varying
                   :product [:G-efe] :intervention :before-reader}
   :wire [:r6-sourced-rates :r6-cascade-lane [:rates {:record :sourced-rates}]] :kind :witnessed-hermetically
           :test `the-writers-value-reaches-the-reader :check check
           :live-records-read support/live-records-read})

(deftest the-writers-value-reaches-the-reader
  (let [o (check)]
    (is (seq (:writer o)))
    (is (w/received? o))))

(deftest typed-absence-carrier-does-not-witness-the-wire
  (is (not (w/received? (support/observe :sourced-rates (constantly {:absent :not-carried}))))))

(deftest different-carrier-does-not-witness-the-wire
  (let [o (support/observe :sourced-rates #(support/different :sourced-rates %))]
    (is (some? (:reader o)))
    (is (not (w/received? o)))))

(deftest live-records-lack-both-ends
  (support/assert-live-records))

(deftest reader-product-changes-at-the-carrier
  (let [before (products/lane-product :rates identity)
        after (products/lane-product :rates products/changed-rates)]
    (is (seq (:G-efe before)))
    (is (every? number? (concat (:G-efe before) (:G-efe after))))
    (is (= (count (:G-efe before)) (count (:G-efe after))))
    (is (not= (:G-efe before) (:G-efe after)))
    (println :rates-product :rates before :after after)))
