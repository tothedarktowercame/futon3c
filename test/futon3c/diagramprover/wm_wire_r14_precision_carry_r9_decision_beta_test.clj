(ns futon3c.diagramprover.wm-wire-r14-precision-carry-r9-decision-beta-test
  "Wire [:r14-precision-carry :r9-decision [:beta {:record :precision}]]:
  the carried beta reaching the joint cascade decision.

  The writer is futon2.aif.policy-precision-carry/advance's advancing
  return (see futon3c.diagramprover.wm-wire-temperature-support). The
  reader is war-machine/cascade-decision-admitted
  (war_machine.clj:6825-6848): `beta-state (precision-carry/advance ...)`,
  then `{:beta (:beta beta-state) :beta-state beta-state ...}` into
  policy/select-action-cascades, whose decision records
  :beta {:value beta :status :declared} — the reader's produced value
  under the field.

  This wire is VERIFIED: the pinned tick record carries both ends — the
  writer's sealed record's :beta at
  [:decision :selection-certificate :policy-precision-state :beta] and the
  decision's recorded beta at
  [:decision :selection-certificate :beta :value] — and they are equal.
  (The record's precision-state is :status :held: the live tick took a
  hold branch, which returns the previous record's beta; the advancing
  branch is the hermetic witness path in the sibling wires' tests.)

  The values are read from the producer record `temperature-observe`."
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))

(def producer (delay (producer-record/record "temperature-observe")))
(defn- fields [] (:fields @producer))
(defn check [] (select-keys (fields) [:writer :reader]))

(def wire
  {:second-layer
   {:test `changed-beta-refuses-the-sealed-carrier
    :kind :refusal :product [:refusal :kind]
    :intervention :before-reader :expected :precision-consumption-mismatch}
   :wire [:r14-precision-carry :r9-decision [:beta {:record :precision}]]
   :kind :verified
   :test `the-carried-beta-reaches-the-decision
   :check check
   :record (:source-record @producer)})

(deftest the-carried-beta-reaches-the-decision
  (let [recorded (fields)
        observed (check)]
    (is (:hash-matches? recorded) "the pin is the record read")
    (is (:writer-present? recorded) "writer")
    (is (false? (:writer-typed-absence? recorded)) "writer is not a typed absence")
    (is (:writer-is-one? recorded))
    (is (:precision-schema? recorded))
    (is (:precision-held? recorded))
    (is (:declared-beta? recorded))
    (is (w/received? observed) (str "writer-reader " (pr-str observed)))))

(deftest a-typed-absence-at-the-reader-fails-the-wire
  (is (false? (get-in (fields) [:interventions :absent :received?]))))

(deftest a-different-beta-at-the-reader-fails-the-wire
  (let [result (get-in (fields) [:interventions :different])]
    (is (= 2 (:reader result)))
    (is (false? (:received? result)))))

(deftest changed-beta-refuses-the-sealed-carrier
  (doseq [[field passed?] (:second-layer (fields))]
    (testing (name field)
      (is (true? passed?) (str field " relation failed")))))

(deftest coherent-producer-records-flatten-the-posterior
  (doseq [[field passed?] (:supplementary (fields))]
    (testing (name field)
      (is (true? passed?) (str field " relation failed")))))
