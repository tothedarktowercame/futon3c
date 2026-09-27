(ns futon3c.diagramprover.wm-wire-r4-kernel-r4-test-order-use-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-kernel-out-support :as support]))
(def positive (delay (support/order-read :order :none)))
(defn check [] @positive)
(def wire {:wire [:r4-kernel :r4-test :order-use]
           :kind :witnessed-hermetically :test 'futon2.aif.order-kernel-test/the-chain-scores-its-order-and-equals-the-list-kernel :check check
           :live-records-read support/live-records-read :note "Existing chain test calls efe/rank-cascade-actions through order-kernel-test/rank and asserts entry order-use. Reader value captured from its clojure.test equality report."})
(deftest the-reader-receives-the-kernel-value
  (is (seq (support/census)))
  (is (w/received? (check))))
(deftest typed-absence-before-reader
  (is (not (w/received? (support/order-read :order :absent)))))
(deftest different-value-before-reader
  (let [o (support/order-read :order :different)]
    (is (not (w/received? o)))
    (is (not= (:writer o) (:reader o)))))
