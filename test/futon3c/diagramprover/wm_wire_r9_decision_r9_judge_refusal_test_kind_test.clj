(ns futon3c.diagramprover.wm-wire-r9-decision-r9-judge-refusal-test-kind-test
  "WIRE-23-C2 wire 5: [:r9-decision :r9-judge-refusal-test :kind].
  Since futon2 7a3b61eda the box is driven by the real writer:
  judge-refusal-abstention-test/the-real-judge-refusal-is-the-ticks-typed-abstention
  installs a judge-fn that calls the REAL war-machine/cascade-decision with a
  live-C derivation carrying a refusal, so cascade-decision-admitted throws
  \"cascade decision refused\" {:kind :live-c-refused} inside the tick. This
  test wraps the writer var, tampering the thrown ex-data's :kind, and reads
  the value the box consumed from its clojure.test report (order-read's
  technique).

  The values are read from the producer record `c2-refusal-box`."
  (:require [clojure.test :refer [deftest is]] [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))
(def wire-id [:r9-decision :r9-judge-refusal-test :kind])
(def producer (delay (producer-record/record "c2-refusal-box")))
(defn- fields [] (get-in @producer [:wires wire-id]))
(defn check [] (select-keys (:primary (fields)) [:writer :reader]))
(def wire {:note "Writer: war_machine.clj cascade-decision-admitted's :live-c guard (\"cascade decision refused\", :kind). Reader: futon2 judge-refusal-abstention-test, driven by the real writer since futon2 7a3b61eda. Test box: witnessed by wrapping the writer var, never by a fixture. The values are read from the producer record `c2-refusal-box`."
           :second-layer {:test `different-value-before-reader :kind :value-varying
                          :product [:report-type] :intervention :before-reader}
           :wire wire-id :kind :witnessed-hermetically :test `the-judge-refusal-kind-reaches-the-box
           :check check :live-records-read []})
(deftest the-judge-refusal-kind-reaches-the-box
  (let [o (:primary (fields))]
    (is (= :live-c-refused (:writer o))) (is (= :pass (:report-type o)))
    (is (w/received? (check)) (str "writer-reader " (pr-str (check))))))
(deftest absence-before-reader
  (let [o (get-in (fields) [:interventions :absent])]
    (is (= :fail (:report-type o))) (is (nil? (:reader o))) (is (not (w/received? o)))))
(deftest different-value-before-reader
  (let [o (get-in (fields) [:interventions :different])]
    (is (= :fail (:report-type o))) (is (= :different-refusal-kind (:reader o)))
    (is (not (w/received? o)))))
(deftest live-records-carry-no-test-box-end
  (is (true? (:live-records-verified? @producer))))
