(ns futon3c.diagramprover.wm-wire-r9-phase-kind-phase-kind-test-failure-kind-test
  "Wire [:r9-phase-kind :phase-kind-test :failure-kind]: the selection
  catch's re-thrown :failure-kind (phase-kind-failure, WM-PHASE-KIND-I)
  reaching the component's own test,
  futon2/test/futon2/aif/phase_kind_test.clj, whose read of the field is

    (get-in r [:result :data :failure-kind])

  asserted equal to the thrower's kind. A test box has no runtime var to
  drive through, so the hermetic witness performs exactly that read on one
  hermetic tick whose judge throws a bare-:kind exception; the writer's
  value is phase-kind-failure's re-thrown ex-data :failure-kind for the
  same exception.

  No live record carries the reader's end: every flight predates
  WM-PHASE-KIND-I (futon2 a41f4c31). The eighth flight's finding
  (live-records-read, pinned) carries the thrower's kind only as
  [:failure-data :kind] :substrate-unreachable and closed :untyped-failure.
  So the wire is WITNESSED-HERMETICALLY.

  The values are read from the producer record `r9-run-tick`."
  (:require [clojure.test :refer [deftest is]] [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))
(def wire-id [:r9-phase-kind :phase-kind-test :failure-kind])
(def producer (delay (producer-record/record "r9-run-tick")))
(defn- fields [] (get-in @producer [:wires wire-id]))
(defn check [] (:primary (fields)))
(def wire {:wire wire-id :kind :witnessed-hermetically :test `the-phase-kind-reaches-the-components-test
           :check check :live-records-read []})
(deftest the-phase-kind-reaches-the-components-test
  (let [o (check)] (is (= :substrate-mission-registry-empty (:writer o)))
       (is (= :substrate-mission-registry-empty (:reader o)))
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
