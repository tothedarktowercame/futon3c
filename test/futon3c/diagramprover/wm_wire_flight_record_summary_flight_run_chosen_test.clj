(ns futon3c.diagramprover.wm-wire-flight-record-summary-flight-run-chosen-test
  "Real calls with IO isolated; no live record carries both ends.
  See support/live-records-read for the pinned record survey.

  Writer and reader values come from the content-addressed small-observe
  producer record."
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))
(def wire-id [:flight-record-summary :flight-run :chosen])
(def producer (delay (producer-record/record "small-observe")))
(defn- wire-fields [] (get-in @producer [:fields :wires wire-id]))
(defn check [] (select-keys (wire-fields) [:writer :reader]))
(def wire {:wire wire-id :second-layer {:test `chosen-precedence-changes-conditioning-evidence
                  :kind :value-varying :product [:enactments 0 :step :p-o]
                  :intervention :before-reader}
   :kind :witnessed-hermetically
           :test `the-writer-reaches-the-reader :check check
           :live-records-read []
           :note "Writer and reader values come from the content-addressed small-observe producer record."})
(deftest the-writer-reaches-the-reader
  (let [r (check)]
    (is (some? (:writer r)) "writer")
    (is (w/received? r) (str "writer-reader " (pr-str r)))
    (is (true? (:received? (wire-fields))))))
(deftest typed-absence-before-reader-fails
  (let [r (get-in (wire-fields) [:interventions :absent])]
    (is (:writer-present? r) "absent writer-present?")
    (is (false? (:received? r)) "absent received?")))
(deftest different-carrier-before-reader-fails
  (let [r (get-in (wire-fields) [:interventions :different])]
    (is (:writer-present? r) "different writer-present?")
    (is (:reader-present? r) "different reader-present?")
    (is (false? (:received? r)) "different received?")))
(deftest live-records-do-not-carry-both-ends
  (doseq [[field passed?] (get-in @producer [:fields :live])]
    (testing (name field)
      (is (true? passed?) (str field " relation failed")))))

(deftest summary-field-is-recorded-without-changing-progress
  ;; flight/record-click:426,429 stores the fields; :409-413 determines
  ;; progress from wants/before/after. run!:577 delegates that decision.
  ;; Relations among the product values are recorded by the producer.
  (doseq [[field passed?] (get-in @producer [:fields :second-layer wire-id :summary-run-chosen])]
    (testing (name field)
      (is (true? passed?) (str field " relation failed")))))

(deftest chosen-precedence-changes-conditioning-evidence
  (doseq [[field passed?] (get-in @producer [:fields :second-layer wire-id :conditioning-chosen])]
    (testing (name field)
      (is (true? passed?) (str field " relation failed")))))
