(ns futon3c.diagramprover.wm-wire-r4-kernel-r4-coapply-test-order-use-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-kernel-out-support :as support]))
(def positive (delay (support/order-read :coapply :none)))
(defn check [] @positive)
(def wire {:wire [:r4-kernel :r4-coapply-test :order-use]
           :kind :witnessed-hermetically :test 'futon2.aif.coapply-kernel-test/the-ranking-scores-a-non-chain-by-co-application :check check
           :live-records-read support/live-records-read :note "Existing ranking co-application test calls the real ranker through order-kernel-test/rank and reads independent order-use. Reader value captured from its assertion report."})
(deftest the-reader-receives-the-kernel-value
  (is (seq (support/census)))
  (is (w/received? (check))))
(deftest typed-absence-before-reader
  (is (not (w/received? (support/order-read :coapply :absent)))))
(deftest different-value-before-reader
  (let [o (support/order-read :coapply :different)]
    (is (not (w/received? o)))
    (is (not= (:writer o) (:reader o)))))
