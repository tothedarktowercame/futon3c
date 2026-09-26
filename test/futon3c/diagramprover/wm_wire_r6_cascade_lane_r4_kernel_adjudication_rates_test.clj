(ns futon3c.diagramprover.wm-wire-r6-cascade-lane-r4-kernel-adjudication-rates-test
  "Rates wire, witnessed by real calls using two independently admitted labels.
  See support/live-records-read for the live records lacking both ends."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-rates-support :as support]))

(defn check [] (support/observe :adjudication-rates identity))
(def wire {:wire [:r6-cascade-lane :r4-kernel :adjudication-rates] :kind :witnessed-hermetically
           :test `the-writers-value-reaches-the-reader :check check
           :live-records-read support/live-records-read})

(deftest the-writers-value-reaches-the-reader
  (let [o (check)]
    (is (seq (:writer o)))
    (is (w/received? o))))

(deftest typed-absence-carrier-does-not-witness-the-wire
  (is (not (w/received? (support/observe :adjudication-rates (constantly {:absent :not-carried}))))))

(deftest different-carrier-does-not-witness-the-wire
  (let [o (support/observe :adjudication-rates #(support/different :adjudication-rates %))]
    (is (some? (:reader o)))
    (is (not (w/received? o)))))

(deftest live-records-lack-both-ends
  (support/assert-live-records))
