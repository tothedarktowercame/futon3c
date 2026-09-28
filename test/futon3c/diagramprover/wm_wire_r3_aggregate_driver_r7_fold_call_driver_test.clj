(ns futon3c.diagramprover.wm-wire-r3-aggregate-driver-r7-fold-call-driver-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))

(def reader-kind :driver)
(def producer (delay (producer-record/record "fold-in-observe")))
(defn- wire-fields [] (get-in @producer [:fields :wires reader-kind]))
(defn check []
  (let [fields (wire-fields)]
    {:writer (:writer fields) :reader (:reader fields)}))
(def wire {:second-layer {:test 'futon3c.diagramprover.wm-wire-r3-aggregate-driver-r7-fold-call-driver-test/different-carrier-changes-the-produced-value
                          :kind :value-varying :product [:result :belief] :intervention :before-reader}
           :wire [:r3-aggregate-driver :r7-fold-call [:driver {:record :r3d-driver}]]
           :kind :witnessed-hermetically
           :test 'futon3c.diagramprover.wm-wire-r3-aggregate-driver-r7-fold-call-driver-test/the-judge-produces-the-received-value
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
