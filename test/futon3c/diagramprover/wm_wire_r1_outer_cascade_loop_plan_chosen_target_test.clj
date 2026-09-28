(ns futon3c.diagramprover.wm-wire-r1-outer-cascade-loop-plan-chosen-target-test
  "Wire [:r1-outer-cascade :loop-plan :chosen-target]. Real loop calls;
  no live record carries both ends. See support/live-records-read."
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))

(def wire-id [:r1-outer-cascade :loop-plan :chosen-target])
(def producer (delay (producer-record/record "plan-observe")))
(defn- wire-fields [] (get-in @producer [:fields :wires wire-id]))
(defn check []
  (let [fields (wire-fields)]
    {:writer (:writer fields) :reader (:reader fields)}))

(def wire
  {:wire wire-id
   :kind :witnessed-hermetically
   :test `the-writers-value-reaches-the-reader
   :second-layer {:test `selector-field-controls-loop-plan
                  :kind :value-varying :product [:result :plan]
                  :intervention :before-reader}
   :check check :live-records-read []
   :note "Writer and reader values come from the content-addressed plan-observe producer record."})

(deftest the-writers-value-reaches-the-reader
  (is (true? (:received? (wire-fields))))
  (is (w/received? (check)) (str "writer-reader " (pr-str (check)))))

(deftest typed-absence-at-the-reader-fails
  (let [r (get-in (wire-fields) [:interventions :absent])]
    (is (:writer-present? r) "writer-present?")
    (is (:reader-typed-absence? r) "reader typed absence")
    (is (false? (:received? r)) "received?")))

(deftest another-value-at-the-reader-fails
  (let [r (get-in (wire-fields) [:interventions :different])]
    (is (:writer-present? r) "writer-present?")
    (is (:reader-present? r) "reader present")
    (is (false? (:received? r)) "received?")))

(deftest live-records-do-not-witness-this-wire
  (is (true? (get-in @producer [:fields :live-records-pinned?]))))

(deftest selector-field-controls-loop-plan
  (doseq [[field passed?] (get-in @producer [:fields :second-layer wire-id :relations])]
    (testing (name field)
      (is (true? passed?) (str field " relation failed")))))

(deftest target-outside-field-is-a-typed-absence-and-is-not-planned
  (doseq [[field passed?] (get-in @producer [:fields :second-layer wire-id :missing-target])]
    (testing (name field)
      (is (true? passed?) (str field " relation failed")))))
