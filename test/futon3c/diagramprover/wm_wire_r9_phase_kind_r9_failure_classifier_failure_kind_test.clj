(ns futon3c.diagramprover.wm-wire-r9-phase-kind-r9-failure-classifier-failure-kind-test
  (:require [clojure.test :refer [deftest is]] [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))
(def wire-id [:r9-phase-kind :r9-failure-classifier :failure-kind])
(def producer (delay (producer-record/record "r9-run-tick")))
(defn- fields [] (get-in @producer [:wires wire-id]))
(defn check [] (:primary (fields)))
(def wire {:second-layer {:test `typed-precedence-is-preserved :kind :record :product [:classification]
                          :intervention :before-reader}
           :wire wire-id :kind :witnessed-hermetically :test `the-phase-kind-reaches-the-failure-classifier
           :check check :live-records-read []})
(deftest the-phase-kind-reaches-the-failure-classifier
  (let [o (check)] (is (= :substrate-mission-registry-empty (:writer o)))
       (is (= :substrate-mission-registry-empty (:reader o))) (is (= (:reader o) (:closed o)))
       (is (w/received? o) (str "writer-reader " (pr-str o)))))
(deftest a-typed-absence-at-the-reader-fails-the-wire
  (let [o (:typed-absence (fields))] (is (nil? (:writer o))) (is (w/typed-absence? (:reader o)))
       (is (not (w/received? o)))))
(deftest a-different-kind-fails-the-wire
  (let [o (:different (fields))] (is (= :invalid-temperature (:reader o))) (is (not (w/received? o)))))
(deftest the-live-record-closed-untyped
  (let [x (get-in @producer [:live-controls :finding])]
    (is (:sha-ok? x)) (is (= {:kind :substrate-unreachable} (:failure-data x)))
    (is (= :untyped-failure (:failure-kind x)))))
(deftest typed-precedence-is-preserved
  (is (= [{:explicit :invalid-temperature :classified :invalid-temperature}
          {:explicit :invalid-temperature :classified :invalid-temperature}
          {:explicit nil :classified :transport-unavailable} {:explicit :outer-kind :classified :outer-kind}]
         (get-in (fields) [:second-layer :rows]))))
