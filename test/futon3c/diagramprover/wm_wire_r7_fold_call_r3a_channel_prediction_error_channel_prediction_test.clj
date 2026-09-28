(ns futon3c.diagramprover.wm-wire-r7-fold-call-r3a-channel-prediction-error-channel-prediction-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))

(def reader-kind :prediction)
(def producer (delay (producer-record/record "fold-out-simple")))
(defn- wire-fields [] (get-in @producer [:fields :wires reader-kind]))
(defn check []
  (let [fields (wire-fields)]
    {:writer (:writer fields) :reader (:reader fields)}))
(def wire {:second-layer {:test 'futon3c.diagramprover.wm-wire-r7-fold-call-r3a-channel-prediction-error-channel-prediction-test/absence-at-the-reader-door-is-not-a-witness
                          :kind :refusal :product [:error :reason]
                          :intervention :before-reader :expected :malformed-prediction-triple}
           :wire [:r7-fold-call :r3a-channel-prediction-error :channel-prediction]
           :kind :witnessed-hermetically
           :test 'futon3c.diagramprover.wm-wire-r7-fold-call-r3a-channel-prediction-error-channel-prediction-test/the-real-reader-produces-the-received-value
           :check check
           :live-records-read []
           :note "The error record retains predicted mean and variance. Missing prediction is refused as malformed-prediction-triple, not an observation omission. The values are read from the producer record `fold-out-simple`."})

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
