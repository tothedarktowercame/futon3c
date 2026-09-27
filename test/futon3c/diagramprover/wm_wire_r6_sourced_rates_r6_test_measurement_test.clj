(ns futon3c.diagramprover.wm-wire-r6-sourced-rates-r6-test-measurement-test
  "Rates wire, witnessed by real calls using ten real subjects admitted through the store and reader.
  See support/live-records-read for the live records lacking both ends."
  (:require [futon3c.diagramprover.wm-wire-rates-products-support :as products]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-rates-support :as support]))

(defn check [] (support/observe :test-measurement identity))
(def wire {:second-layer {:test 'futon3c.diagramprover.wm-wire-r6-sourced-rates-r6-test-measurement-test/reader-product-changes-at-the-carrier :kind :value-varying
                   :product [:reports] :intervention :before-reader}
   :wire [:r6-sourced-rates :r6-test :measurement] :kind :witnessed-hermetically
           :test `the-writers-value-reaches-the-reader :check check
           :live-records-read support/live-records-read})

(deftest the-writers-value-reaches-the-reader
  (let [o (check)]
    (is (seq (:writer o)))
    (is (every? #(and (pos? (get-in % [:false-neg :denominator] 0))
                      (pos? (get-in % [:false-pos :denominator] 0)))
                (vals (:writer o))) "both cells measured, never an :absent default")
    (is (w/received? o))))

(deftest typed-absence-carrier-does-not-witness-the-wire
  (is (not (w/received? (support/observe :test-measurement (constantly {:absent :not-carried}))))))

(deftest different-carrier-does-not-witness-the-wire
  (let [o (support/observe :test-measurement #(support/different :test-measurement %))]
    (is (some? (:reader o)))
    (is (not (w/received? o)))))

(deftest live-records-lack-both-ends
  (support/assert-live-records))

(deftest reader-product-changes-at-the-carrier
  (let [before (products/test-report identity)
        after (products/test-report #(assoc % :t/wanted :absent))
        reports (fn [r] (filter #(= "the C4 token carries its counts" (:message %)) (:reports r)))]
    (is (pos? (:calls before)))
    (is (pos? (:calls after)))
    (is (= [:pass] (mapv :type (reports before))))
    (is (= [:fail] (mapv :type (reports after))))
    (is (not-any? #(= :error (:type %)) (:reports after)))
    (println :measurement-assertion (vec (reports before)) :after (vec (reports after)))))
