(ns futon3c.diagramprover.wm-wire-r4-kernel-r8-selection-candidate-controller-score-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-kernel-out-support :as support]))
(def positive (delay (support/score :candidate :none)))
(defn check [] @positive)
(def wire {:wire [:r4-kernel :r8-selection-candidate :controller-score]
           :kind :witnessed-hermetically :test `the-reader-receives-the-kernel-value :check check
           :live-records-read support/live-records-read :note "Real ranker chain G read by selection-candidate into :g; typed absence and changed score altered before reader."})
(deftest the-reader-receives-the-kernel-value
  (is (seq (support/census)))
  (is (w/received? (check))))
(deftest typed-absence-before-reader
  (is (not (w/received? (support/score :candidate :absent)))))
(deftest different-value-before-reader
  (let [o (support/score :candidate :different)]
    (is (not (w/received? o)))
    (is (not= (:writer o) (:reader o)))))
