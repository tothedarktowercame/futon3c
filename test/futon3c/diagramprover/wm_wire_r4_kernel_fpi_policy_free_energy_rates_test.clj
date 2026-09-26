(ns futon3c.diagramprover.wm-wire-r4-kernel-fpi-policy-free-energy-rates-test
  "Rates wire, witnessed by real calls using ten real subjects admitted through the store and reader.
  See support/live-records-read for the live records lacking both ends."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-rates-support :as support]))

(defn check [] (support/observe :fpi-rates identity))
(def wire {:wire [:r4-kernel :fpi-policy-free-energy :rates] :kind :witnessed-hermetically
           :test `the-writers-value-reaches-the-reader :check check
           :live-records-read support/live-records-read})

(deftest the-writers-value-reaches-the-reader
  (let [o (check)]
    (is (seq (:writer o)))
    (is (w/received? o))))

(deftest typed-absence-carrier-does-not-witness-the-wire
  (is (not (w/received? (support/observe :fpi-rates (constantly {:absent :not-carried}))))))

(deftest different-carrier-does-not-witness-the-wire
  (let [o (support/observe :fpi-rates #(support/different :fpi-rates %))]
    (is (some? (:reader o)))
    (is (not (w/received? o)))))

(deftest live-records-lack-both-ends
  (support/assert-live-records))
