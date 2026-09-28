(ns futon3c.diagramprover.wm-wire-r9-judge-refusal-abstention-r9-failure-classifier-outcome-test
  (:require [clojure.test :refer [deftest is]] [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))
(def wire-id [:r9-judge-refusal-abstention :r9-failure-classifier :outcome])
(def producer (delay (producer-record/record "r9-run-tick")))
(defn- fields [] (get-in @producer [:wires wire-id]))
(defn check [] (:primary (fields)))
(def wire {:second-layer {:test `typed-precedence-is-preserved :kind :record :product [:classification]
                          :intervention :before-reader}
           :wire wire-id :kind :witnessed-hermetically :test `the-abstentions-outcome-reaches-the-failure-classifier
           :check check :live-records-read []})
(deftest the-abstentions-outcome-reaches-the-failure-classifier
  (let [o (check)] (is (= :abstained (:writer o))) (is (= :abstained (:reader o)))
       (is (w/received? o) (str "writer-reader " (pr-str o)))))
(deftest a-typed-absence-under-outcome-fails-the-wire
  (let [o (:typed-absence (fields))] (is (w/typed-absence? (:reader o))) (is (not (w/received? o)))))
(deftest a-different-outcome-fails-the-wire
  (let [o (:different (fields))] (is (= :grounded-change (:reader o))) (is (not (w/received? o)))))
(deftest the-live-record-closed-untyped
  (let [x (get-in @producer [:live-controls :old-refusal])]
    (is (:sha-ok? x)) (is (= {:status :absent :reason :no-selection-decision-recorded} (:abstention x)))))
(deftest typed-precedence-is-preserved
  (is (= [{:explicit :abstained :classified :abstained} {:explicit :abstained :classified :abstained}
          {:explicit nil :classified :transport-unavailable} {:explicit :outer-kind :classified :outer-kind}]
         (get-in (fields) [:second-layer :rows]))))
