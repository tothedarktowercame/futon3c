(ns futon3c.diagramprover.wm-wire-r1-target-field-r1-outer-cascade-next-step-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))

(def reader-field :next-step)
(def producer (delay (producer-record/record "outer-inputs-observe")))
(defn- wire-fields [] (get-in @producer [:fields :wires reader-field]))
(defn check []
  (let [fields (wire-fields)]
    {:writer (:writer fields) :reader (:reader fields)}))
(def wire {:wire [:r1-target-field :r1-outer-cascade :next-step]
           :kind :witnessed-hermetically
           :test 'futon3c.diagramprover.wm-wire-r1-target-field-r1-outer-cascade-next-step-test/the-produced-input-is-received
           :check check
           :second-layer {:test 'futon3c.diagramprover.wm-wire-r1-target-field-r1-outer-cascade-next-step-test/recorded-input-does-not-change-mixed-selection
                          :kind :record :product [:target-selection :inputs :next-step]
                          :intervention :before-reader}
           :live-records-read []
           :note "Writer and reader values and intervention relations come from the content-addressed outer-inputs-observe producer record."})

(deftest the-produced-input-is-received
  (let [fields (wire-fields)
        value (check)]
    (is (some? (:writer value)) "writer")
    (is (not (w/typed-absence? (:writer value))) "writer-typed-absence")
    (is (w/received? value) (str "writer-reader " (pr-str value)))
    (is (true? (:unchanged-law? fields)) "unchanged-law?")
    (is (= [:eligible :delta-g] (:law-uses fields)) "law-uses")))

(deftest typed-absence-at-the-reader-door-is-not-received
  (let [result (get-in (wire-fields) [:interventions :absent])]
    (is (false? (:received? result)) "absent received?")
    (is (= {:absent :writer-unavailable} (:reader result)) "absent reader")
    (is (true? (:unchanged-law? result)) "absent unchanged-law?")))

(deftest missing-input-is-recorded-without-changing-choice
  (let [result (get-in (wire-fields) [:interventions :missing])]
    (is (false? (:received? result)) "missing received?")
    (is (= {:absent :no-such-key-on-entry} (:reader result))
        "missing reader")
    (is (true? (:unchanged-law? result)) "missing unchanged-law?")))

(deftest changed-value-at-the-reader-door-is-not-the-writers
  (let [result (get-in (wire-fields) [:interventions :different])]
    (is (true? (:reader-present? result)) "different reader-present?")
    (is (false? (:received? result)) "different received?")
    (is (true? (:unchanged-law? result)) "different unchanged-law?")))

(deftest pinned-live-records-lack-the-reader-end
  (is (true? (get-in @producer [:fields :live-reader-absent?]))))

(deftest recorded-input-does-not-change-mixed-selection
  (doseq [[relation passed?] (get-in @producer [:fields :second-layer reader-field])]
    (testing (name relation)
      (is (true? passed?) (str relation " relation failed")))))
