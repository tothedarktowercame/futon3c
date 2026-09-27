(ns futon3c.diagramprover.wm-wire-r9-decision-r9-judge-refusal-test-kind-test
  "WIRE-23-C2 wire 5: [:r9-decision :r9-judge-refusal-test :kind].
  Since futon2 7a3b61eda the box is driven by the real writer:
  judge-refusal-abstention-test/the-real-judge-refusal-is-the-ticks-typed-abstention
  installs a judge-fn that calls the REAL war-machine/cascade-decision with a
  live-C derivation carrying a refusal, so cascade-decision-admitted throws
  \"cascade decision refused\" {:kind :live-c-refused} inside the tick. This
  test wraps the writer var, tampering the thrown ex-data's :kind, and reads
  the value the box consumed from its clojure.test report (order-read's
  technique)."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-c2-support :as support]))

(def positive (delay (support/refusal-box :judge :none)))
(defn check [] (let [o @positive] {:writer (:writer o) :reader (:reader o)}))

(def wire {
   :second-layer {:test 'futon3c.diagramprover.wm-wire-r9-decision-r9-judge-refusal-test-kind-test/different-value-before-reader :kind :value-varying
                  :product [:report-type] :intervention :before-reader}
  :wire [:r9-decision :r9-judge-refusal-test :kind]
           :kind :witnessed-hermetically :test `the-judge-refusal-kind-reaches-the-box
           :check check
           :live-records-read support/refusal-live-records-read
           :note "Writer: war_machine.clj cascade-decision-admitted's :live-c guard (\"cascade decision refused\", :kind). Reader: futon2 judge-refusal-abstention-test, driven by the real writer since futon2 7a3b61eda. Test box: witnessed by wrapping the writer var, never by a fixture."})

(deftest the-judge-refusal-kind-reaches-the-box
  (let [o @positive]
    (is (= :live-c-refused (:writer o))
        "the real writer refused with the live-C refusal kind")
    (is (= :pass (:report-type o)) "the untampered box passes")
    (is (w/received? (check)) (pr-str (check)))))

(deftest absence-before-reader
  (let [o (support/refusal-box :judge :absent)]
    (is (= :fail (:report-type o)) "the box does not receive the tampered refusal")
    (is (nil? (:reader o))
        "without :kind the runner reads no judge refusal at all (untyped)")
    (is (not (w/received? {:writer (:writer o) :reader (:reader o)})))))

(deftest different-value-before-reader
  (let [o (support/refusal-box :judge :different)]
    (is (= :fail (:report-type o)))
    (is (= :different-refusal-kind (:reader o))
        "the box consumed the tampered kind, not the writer's")
    (is (not (w/received? {:writer (:writer o) :reader (:reader o)})))))

(deftest live-records-carry-no-test-box-end
  (support/assert-live-records support/refusal-live-records-read :measured-a))
