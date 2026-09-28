(ns futon3c.diagramprover.wm-wire-dispatch-clock-in-mission-id-test
  "Writer and reader values come from the content-addressed small-observe
  producer record."
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))
(def wire-id [:dispatch :clock-in :mission-id])
(def producer (delay (producer-record/record "small-observe")))
(defn- wire-fields [] (get-in @producer [:fields :wires wire-id]))
(defn check [] (select-keys (wire-fields) [:writer :reader]))
(def wire {:wire wire-id :kind :witnessed-hermetically
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
