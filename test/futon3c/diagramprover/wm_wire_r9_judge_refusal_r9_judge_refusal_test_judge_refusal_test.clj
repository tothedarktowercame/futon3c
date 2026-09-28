(ns futon3c.diagramprover.wm-wire-r9-judge-refusal-r9-judge-refusal-test-judge-refusal-test
  "Wire [:r9-judge-refusal :r9-judge-refusal-test :judge-refusal]: the
  :no-selection sorry cell's :judge-refusal (judge-refusal-sorry) reaching
  the component's own test, futon2/test/futon2/aif/judge_refusal_abstention_test.clj,
  whose read of the field is

    (get-in result [:checkpoints :selection :sorry :judge-refusal :kind])

  asserted equal to the refusing kind. A test box has no runtime var to
  drive through, so the hermetic witness performs exactly that read on one
  hermetic tick whose judge throws a typed \"cascade decision refused\": the
  writer's value is judge-refusal-sorry's output for that refusal, the
  reader's value the run's checkpoint cell read as the reader reads it.

  No live record carries either end (live-records-read, the same two
  records the abstention-carrier wire reads: the pre-WM-CLICK-REFUSAL-I
  abstained tick carries no :judge-refusal; the fourth flight's refused
  tick keeps no sorry cell). So the wire is WITNESSED-HERMETICALLY.

  The values are read from the producer record `r9-run-tick`."
  (:require [clojure.test :refer [deftest is]] [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))
(def wire-id [:r9-judge-refusal :r9-judge-refusal-test :judge-refusal])
(def producer (delay (producer-record/record "r9-run-tick")))
(defn- fields [] (get-in @producer [:wires wire-id]))
(defn check [] (:primary (fields)))
(def wire {:wire wire-id :kind :witnessed-hermetically :test `the-judge-refusal-reaches-the-components-test
           :check check :live-records-read []})
(deftest the-judge-refusal-reaches-the-components-test
  (let [o (check)]
    (is (= {:kind :live-c-stale :target "M-t" :missing :live-c :data {:kind :live-c-stale :target "M-t"}}
           (:writer o))) (is (= :live-c-stale (get-in o [:reader :kind])))
    (is (w/received? o) (str "writer-reader " (pr-str o)))))
(deftest no-refusal-is-a-typed-absence-and-fails-the-wire
  (let [o (:typed-absence (fields))] (is (nil? (:writer o))) (is (not (w/received? o)))))
(deftest a-different-refusal-fails-the-wire
  (let [o (:different (fields))] (is (some? (:reader o))) (is (not= (:writer o) (:reader o)))
       (is (not (w/received? o)))))
