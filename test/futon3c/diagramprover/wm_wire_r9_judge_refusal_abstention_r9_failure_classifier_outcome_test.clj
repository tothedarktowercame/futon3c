(ns futon3c.diagramprover.wm-wire-r9-judge-refusal-abstention-r9-failure-classifier-outcome-test
  "Wire [:r9-judge-refusal-abstention :r9-failure-classifier :outcome]: the
  abstention exception's {:outcome :abstained} (judge-refusal-abstention,
  WM-MAP-REPLAY-I defect G) reaching the runner's failure classifier
  (explicit-failure-kind), which reads :outcome off the ex-data anywhere in
  the cause chain. This is the read that closes a refused tick :abstained
  instead of :untyped-failure.

  No live record carries both ends: every flight predates the fix
  (321d82c8). The fourth flight's refused tick (live-records-read, pinned)
  closed :untyped-failure — the classifier read nothing of the refusal. So
  the wire is WITNESSED-HERMETICALLY: the writer's var is called and its
  thrown ex-data's :outcome observed; the reader's value is the tick's
  recorded [:data :failure-kind], which is explicit-failure-kind's read of
  :outcome (failure-kind-from consults it first), off one hermetic tick
  whose judge throws a typed refusal.

  The values are read from the producer record `r9-run-tick`."
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
