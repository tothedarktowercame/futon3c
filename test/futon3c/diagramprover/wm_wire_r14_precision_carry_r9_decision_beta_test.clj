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
  branch is the hermetic witness path in the sibling wires' tests.)"
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-temperature-support :as t]))

(def reader-value-path (conj t/decision-reader-field :value))

(defn check [] (t/observe reader-value-path))

(def wire
  {:wire [:r14-precision-carry :r9-decision [:beta {:record :precision}]]
   :kind :verified
   :test `the-carried-beta-reaches-the-decision
   :check check
   :record t/record})

(deftest the-carried-beta-reaches-the-decision
  (is (= (:sha256 t/record) (w/sha256-file (:path t/record)))
      "the pin is the record read")
  (let [o (check)
        r (w/read-record (:path t/record))
        precision-state (get-in r [:decision :selection-certificate :policy-precision-state])]
    (is (= 1 (:writer o)))
    (is (= :wm/precision-carry-v1 (:schema precision-state)))
    (is (= :held (:status precision-state))
        "the live tick took a hold branch: the beta is the predecessor's")
    (is (= {:value 1 :status :declared}
           (get-in r t/decision-reader-field))
        "the decision records the beta it selected at")
    (is (w/received? o))))

(deftest a-typed-absence-at-the-reader-fails-the-wire
  (let [o (t/observe reader-value-path
                     #(assoc-in % t/decision-reader-field {:absent :precision-carry-refused}))]
    (is (not (w/received? o)))))

(deftest a-different-beta-at-the-reader-fails-the-wire
  (let [o (t/observe reader-value-path
                     #(assoc-in % t/decision-reader-field {:value 2 :status :declared}))]
    (is (= 2 (:reader o)))
    (is (not (w/received? o)))))
