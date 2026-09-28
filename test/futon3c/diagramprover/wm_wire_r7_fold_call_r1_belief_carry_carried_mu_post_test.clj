(ns futon3c.diagramprover.wm-wire-r7-fold-call-r1-belief-carry-carried-mu-post-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))

(def reader-kind :carry)
(def producer (delay (producer-record/record "fold-out-simple")))
(defn- wire-fields [] (get-in @producer [:fields :wires reader-kind]))
(defn check []
  (let [fields (wire-fields)]
    {:writer (:writer fields) :reader (:reader fields)}))
(def wire {:second-layer {:test 'futon3c.diagramprover.wm-wire-r7-fold-call-r1-belief-carry-carried-mu-post-test/reconciliation-retains-each-intervened-posterior
                          :kind :record :product [:reader]
                          :intervention :before-reader}
           :wire [:r7-fold-call :r1-belief-carry :carried-mu-post]
           :kind :witnessed-hermetically
           :test 'futon3c.diagramprover.wm-wire-r7-fold-call-r1-belief-carry-carried-mu-post-test/the-real-reader-produces-the-received-value
           :check check
           :live-records-read []
           :note "Writer, reader, and intervention relations come from the content-addressed fold-out-simple producer record."})

(deftest the-real-reader-produces-the-received-value
  (let [fields (wire-fields)
        value (check)]
    (is (true? (get-in @producer [:fields :live-pins-valid?])) "live-pins-valid?")
    (is (true? (:writer-present? fields)) "writer-present?")
    (is (false? (:writer-typed-absence? fields)) "writer-typed-absence?")
    (is (w/received? value) (str "writer-reader " (pr-str value)))
    (doseq [[relation passed?] (:ordinary fields)]
      (testing (name relation)
        (is (true? passed?) (str relation " relation failed"))))))

(deftest absence-at-the-reader-door-is-not-a-witness
  (let [result (get-in (wire-fields) [:interventions :absent])]
    (is (false? (:received? result)) "absent received?")
    (doseq [[relation passed?] (dissoc result :received?)]
      (testing (name relation)
        (is (true? passed?) (str relation " relation failed"))))))

(deftest different-carrier-changes-the-reader-product
  (let [result (get-in (wire-fields) [:interventions :different])]
    (is (false? (:received? result)) "different received?")
    (doseq [[relation passed?] (dissoc result :received?)]
      (testing (name relation)
        (is (true? passed?) (str relation " relation failed"))))))

(deftest reconciliation-retains-each-intervened-posterior
  (doseq [[relation passed?] (get-in @producer [:fields :second-layer reader-kind])]
    (testing (name relation)
      (is (true? passed?) (str relation " relation failed")))))
