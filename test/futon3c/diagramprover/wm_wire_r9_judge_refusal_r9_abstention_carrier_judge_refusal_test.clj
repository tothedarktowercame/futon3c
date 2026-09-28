(ns futon3c.diagramprover.wm-wire-r9-judge-refusal-r9-abstention-carrier-judge-refusal-test
  (:require [clojure.test :refer [deftest is]] [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))
(def wire-id [:r9-judge-refusal :r9-abstention-carrier :judge-refusal])
(def producer (delay (producer-record/record "r9-run-tick")))
(defn- fields [] (get-in @producer [:wires wire-id]))
(defn check [] (:primary (fields)))
(def wire {:wire wire-id :kind :witnessed-hermetically :test `the-sorry-cells-judge-refusal-reaches-the-abstention-carrier
           :check check :live-records-read []})
(deftest the-sorry-cells-judge-refusal-reaches-the-abstention-carrier
  (let [o (check)]
    (is (= {:kind :live-c-stale :target "M-t" :missing :live-c :data {:kind :live-c-stale :target "M-t"}}
           (:writer o))) (is (= :abstained (:carrier-status o)))
    (is (w/received? o) (str "writer-reader " (pr-str o)))))
(deftest no-refusal-is-a-typed-absence-and-fails-the-wire
  (let [o (:typed-absence (fields))] (is (nil? (:writer o)))
       (is (= {:absent :no-judge-refusal} (:reader o))) (is (not (w/received? o)))))
(deftest a-different-refusal-fails-the-wire
  (let [o (:different (fields))] (is (some? (:reader o))) (is (not (w/received? o)))))
(deftest the-live-records-carry-no-judge-refusal
  (let [a (get-in @producer [:live-controls :old-abstention]) r (get-in @producer [:live-controls :old-refusal])]
    (is (:sha-ok? a)) (is (:sha-ok? r)) (is (= :abstained (:status a)))
    (is (:targets-have-no-data? a)) (is (:sorry-has-no-refusal? a))
    (is (= {:status :absent :reason :no-selection-decision-recorded} (:abstention r)))))
