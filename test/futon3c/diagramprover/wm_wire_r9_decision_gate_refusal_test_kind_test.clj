(ns futon3c.diagramprover.wm-wire-r9-decision-gate-refusal-test-kind-test
  (:require [clojure.test :refer [deftest is]] [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))
(def wire-id [:r9-decision :gate-refusal-test :kind])
(def producer (delay (producer-record/record "c2-refusal-box")))
(defn- fields [] (get-in @producer [:wires wire-id]))
(defn check [] (select-keys (:primary (fields)) [:writer :reader]))
(def wire {:second-layer {:test `different-value-before-reader :kind :value-varying
                          :product [:report-type] :intervention :before-reader}
           :wire wire-id :kind :witnessed-hermetically :test `the-gate-refusal-kind-reaches-the-box
           :check check :live-records-read []})
(deftest the-gate-refusal-kind-reaches-the-box
  (let [o (:primary (fields))]
    (is (= :missing-observation-locators (:writer o))) (is (= :pass (:report-type o)))
    (is (w/received? (check)) (str "writer-reader " (pr-str (check))))))
(deftest absence-before-reader
  (let [o (get-in (fields) [:interventions :absent])]
    (is (= :fail (:report-type o))) (is (= {:absent :no-reason-given} (:reader o)))
    (is (not (w/received? o)))))
(deftest different-value-before-reader
  (let [o (get-in (fields) [:interventions :different])]
    (is (= :fail (:report-type o))) (is (= :different-refusal-kind (:reader o)))
    (is (not (w/received? o)))))
(deftest live-records-carry-no-test-box-end
  (is (true? (:live-records-verified? @producer))))
