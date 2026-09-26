(ns futon3c.diagramprover.wm-wire-r9-decision-r9-judge-refusal-read-kind-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-selection-out-support :as support]))
(def positive (delay (support/refusal :none)))
(defn check [] @positive)
(def wire {:wire [:r9-decision :r9-judge-refusal-read :kind]
           :kind :witnessed-hermetically :test `the-real-reader-handoff :check check
           :live-records-read support/live-records-read
           :note "Real cascade-decision emits live-c-refused from refused Live-C input. Tamper its exception data before the real runner judge-refusal path; read selection sorry judge-refusal kind."})
(deftest the-real-reader-handoff
  (is (seq (support/census)))
  (let [o (check)] (is (w/received? o))
    (is (= :live-c-refused (:reader o)))))
(deftest absence-before-reader
  (let [o (support/refusal :absent)]
    (is (not (w/received? o)))
    (is (nil? (:reader o)))))
(deftest different-value-before-reader
  (let [o (support/refusal :different)]
    (is (not (w/received? o)))
    (is (= :different-refusal (:reader o)))))
