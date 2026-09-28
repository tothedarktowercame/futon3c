(ns futon3c.diagramprover.wm-wire-r13-policy-depth-anticipation-r7-fold-call-horizon-steps-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))

(def reader-kind :horizon)
(def producer (delay (producer-record/record "fold-in-observe")))
(defn- wire-fields [] (get-in @producer [:fields :wires reader-kind]))
(defn check []
  (let [fields (wire-fields)]
    {:writer (:writer fields) :reader (:reader fields)}))
(def wire {:second-layer {:test 'futon3c.diagramprover.wm-wire-r13-policy-depth-anticipation-r7-fold-call-horizon-steps-test/anticipated-depth-is-recorded-without-changing-selection
                          :kind :record :product [:policy-depth-used] :intervention :before-reader}
           :wire [:r13-policy-depth-anticipation :r7-fold-call [:horizon-steps {:record :depth-anticipation}]]
           :kind :witnessed-hermetically
           :test 'futon3c.diagramprover.wm-wire-r13-policy-depth-anticipation-r7-fold-call-horizon-steps-test/the-judge-produces-the-received-value
           :check check
           :live-records-read []
           :note "Writer, judge reader, and intervention relations come from the content-addressed fold-in-observe producer record."})

(deftest the-judge-produces-the-received-value
  (let [fields (wire-fields)
        value (check)]
    (is (true? (get-in @producer [:fields :live-pins-valid?])) "live-pins-valid?")
    (is (true? (:no-error? fields)) "no-error?")
    (is (true? (:writer-present? fields)) "writer-present?")
    (is (false? (:writer-typed-absence? fields)) "writer-typed-absence?")
    (is (w/received? value) (str "writer-reader " (pr-str value)))))

(deftest absence-before-the-judge-does-not-witness-the-wire
  (let [result (get-in (wire-fields) [:interventions :absent])]
    (is (false? (:received? result)) "absent received?")
    (doseq [[relation passed?] (dissoc result :received?)]
      (testing (name relation)
        (is (true? passed?) (str relation " relation failed"))))))

(deftest different-carrier-changes-the-produced-value
  (let [result (get-in (wire-fields) [:interventions :different])]
    (is (false? (:received? result)) "different received?")
    (doseq [[relation passed?] (dissoc result :received?)]
      (testing (name relation)
        (is (true? passed?) (str relation " relation failed"))))))

(deftest anticipated-depth-is-recorded-without-changing-selection
  (doseq [[relation passed?] (get-in @producer [:fields :second-layer reader-kind])]
    (testing (name relation)
      (is (true? passed?) (str relation " relation failed")))))
