(ns futon3c.diagramprover.wm-wire-r14-precision-carry-r9-decision-beta-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))

(def producer (delay (producer-record/record "temperature-observe")))
(defn- fields [] (:fields @producer))
(defn check [] (select-keys (fields) [:writer :reader]))

(def wire
  {:second-layer
   {:test `changed-beta-refuses-the-sealed-carrier
    :kind :refusal :product [:refusal :kind]
    :intervention :before-reader :expected :precision-consumption-mismatch}
   :wire [:r14-precision-carry :r9-decision [:beta {:record :precision}]]
   :kind :verified
   :test `the-carried-beta-reaches-the-decision
   :check check
   :record (:source-record @producer)})

(deftest the-carried-beta-reaches-the-decision
  (let [recorded (fields)
        observed (check)]
    (is (:hash-matches? recorded) "the pin is the record read")
    (is (:writer-present? recorded) "writer")
    (is (false? (:writer-typed-absence? recorded)) "writer is not a typed absence")
    (is (:writer-is-one? recorded))
    (is (:precision-schema? recorded))
    (is (:precision-held? recorded))
    (is (:declared-beta? recorded))
    (is (w/received? observed) (str "writer-reader " (pr-str observed)))))

(deftest a-typed-absence-at-the-reader-fails-the-wire
  (is (false? (get-in (fields) [:interventions :absent :received?]))))

(deftest a-different-beta-at-the-reader-fails-the-wire
  (let [result (get-in (fields) [:interventions :different])]
    (is (= 2 (:reader result)))
    (is (false? (:received? result)))))

(deftest changed-beta-refuses-the-sealed-carrier
  (doseq [[field passed?] (:second-layer (fields))]
    (testing (name field)
      (is (true? passed?) (str field " relation failed")))))

(deftest coherent-producer-records-flatten-the-posterior
  (doseq [[field passed?] (:supplementary (fields))]
    (testing (name field)
      (is (true? passed?) (str field " relation failed")))))
