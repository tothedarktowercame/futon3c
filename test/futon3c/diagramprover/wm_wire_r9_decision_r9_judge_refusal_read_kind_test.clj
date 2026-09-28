(ns futon3c.diagramprover.wm-wire-r9-decision-r9-judge-refusal-read-kind-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))

(def producer (delay (producer-record/record "selection-out-refusal")))
(defn- fields [] (:fields @producer))
(defn check [] (select-keys (fields) [:writer :reader]))

(def wire
  {:wire [:r9-decision :r9-judge-refusal-read :kind]
   :kind :witnessed-hermetically
   :test `the-real-reader-handoff
   :check check
   :live-records-read []
   :note "Writer and reader values come from the content-addressed selection-out-refusal producer record."})

(deftest the-real-reader-handoff
  (let [recorded (fields)
        observed (check)]
    (is (:census-present? recorded))
    (is (:writer-present? recorded) "writer")
    (is (false? (:writer-typed-absence? recorded)) "writer is not a typed absence")
    (is (w/received? observed) (str "writer-reader " (pr-str observed)))
    (is (:reader-live-c-refused? recorded))))

(deftest absence-before-reader
  (let [result (get-in (fields) [:interventions :absent])]
    (is (false? (:received? result)))
    (is (:reader-absent? result))))

(deftest different-value-before-reader
  (let [result (get-in (fields) [:interventions :different])]
    (is (false? (:received? result)))
    (is (:reader-different-refusal? result))))
