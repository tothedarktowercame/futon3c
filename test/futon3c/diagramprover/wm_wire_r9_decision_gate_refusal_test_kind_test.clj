(ns futon3c.diagramprover.wm-wire-r9-decision-gate-refusal-test-kind-test
  "WIRE-23-C2 wire 4: [:r9-decision :gate-refusal-test :kind].
  Since futon2 7a3b61eda the box is driven by the real writer:
  gate-refusal-abstention-test/the-real-gate-refusal-is-the-ticks-typed-abstention
  installs a judge-fn that calls the REAL war-machine/cascade-decision, whose
  decision gate throws \"Inadmissible decision\" {:reason
  :missing-observation-locators} inside the tick (the fixture family admitted
  with one guard token's C4 locator missing :decl). This test wraps the writer
  var, tampering the thrown ex-data's :reason -- the abstention's :kind is the
  runner's record of that field -- and reads the value the box consumed from
  its clojure.test report (order-read's technique)."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-c2-support :as support]))

(def positive (delay (support/refusal-box :gate :none)))
(defn check [] (let [o @positive] {:writer (:writer o) :reader (:reader o)}))

(def wire {
   :second-layer {:test 'futon3c.diagramprover.wm-wire-r9-decision-gate-refusal-test-kind-test/different-value-before-reader :kind :value-varying
                  :product [:report-type] :intervention :before-reader}
  :wire [:r9-decision :gate-refusal-test :kind]
           :kind :witnessed-hermetically :test `the-gate-refusal-kind-reaches-the-box
           :check check
           :live-records-read support/refusal-live-records-read
           :note "Writer: war_machine.clj cascade-decision-admitted -> decision-gate/emit! (\"Inadmissible decision\", :reason). Reader: futon2 gate-refusal-abstention-test, driven by the real writer since futon2 7a3b61eda. Test box: witnessed by wrapping the writer var, never by a fixture."})

(deftest the-gate-refusal-kind-reaches-the-box
  (let [o @positive]
    (is (= :missing-observation-locators (:writer o))
        "the real writer refused with the gate's guard-locator reason")
    (is (= :pass (:report-type o)) "the untampered box passes")
    (is (w/received? (check)) (pr-str (check)))))

(deftest absence-before-reader
  (let [o (support/refusal-box :gate :absent)]
    (is (= :fail (:report-type o)) "the box does not receive the tampered refusal")
    (is (= {:absent :no-reason-given} (:reader o))
        "no :reason reads as the typed absence, never a guessed kind")
    (is (not (w/received? {:writer (:writer o) :reader (:reader o)})))))

(deftest different-value-before-reader
  (let [o (support/refusal-box :gate :different)]
    (is (= :fail (:report-type o)))
    (is (= :different-refusal-kind (:reader o))
        "the box consumed the tampered kind, not the writer's")
    (is (not (w/received? {:writer (:writer o) :reader (:reader o)})))))

(deftest live-records-carry-no-test-box-end
  (support/assert-live-records support/refusal-live-records-read :measured-a))
