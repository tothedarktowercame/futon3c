(ns futon3c.diagramprover.wm-wire-r4-kernel-r9-selection-law-controller-score-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-kernel-out-support :as support]))
(def positive (delay (support/score :law :none)))
(defn check [] @positive)
(def wire {:wire [:r4-kernel :r9-selection-law :controller-score]
           :kind :witnessed-hermetically :test `the-reader-receives-the-kernel-value :check check
           :live-records-read support/live-records-read :note "Ranker over order-kernel-test chain fixture; selector re-emits controller-score at top level. Carrier mutated before real selector."})
(deftest the-reader-receives-the-kernel-value
  (is (seq (support/census)))
  (is (w/received? (check))))
(deftest typed-absence-before-reader
  (is (not (w/received? (support/score :law :absent)))))
(deftest different-value-before-reader
  (let [o (support/score :law :different)]
    (is (not (w/received? o)))
    (is (not= (:writer o) (:reader o)))))
