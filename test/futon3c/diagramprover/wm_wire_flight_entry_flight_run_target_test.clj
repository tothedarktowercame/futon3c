(ns futon3c.diagramprover.wm-wire-flight-entry-flight-run-target-test
  "Scoped target handoff. Negative controls change the flight before the real reader.

  Scoped target handoff read from the content-addressed target-observe producer record."
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))

(def reader-kind :flight-run)
(def producer (delay (producer-record/record "target-observe")))
(defn- wire-fields [] (get-in @producer [:fields :wires reader-kind]))
(defn check []
  (let [fields (wire-fields)]
    {:writer (:writer fields) :reader (:reader fields)}))
(def wire {:wire [:flight-entry :flight-run [:target {:record :flight}]]
           :kind :witnessed-hermetically
           :test 'futon3c.diagramprover.wm-wire-flight-entry-flight-run-target-test/the-target-reaches-the-reader
           :second-layer {:test 'futon3c.diagramprover.wm-wire-flight-entry-flight-run-target-test/target-reader-product :kind :value-varying
                          :product [:status] :intervention :before-reader}
           :check check
           :live-records-read []})

(deftest the-target-reaches-the-reader
  (let [r (check)]
    (is (some? (:writer r)) "writer")
    (is (not (w/typed-absence? (:writer r))) "writer-typed-absence")
    (is (w/received? r) (str "writer-reader " (pr-str r)))))

(deftest typed-absence-before-reader-fails
  (is (false? (get-in (wire-fields) [:interventions :absent :received?]))
      "typed-absence received?"))

(deftest different-target-before-reader-fails
  (let [result (get-in (wire-fields) [:interventions :different])]
    (is (= "M-other-target" (:reader result)) "different-target reader")
    (is (false? (:received? result)) "different-target received?")))

(deftest target-reader-product
  (doseq [[field passed?] (get-in @producer [:fields :second-layer reader-kind])]
    (testing (name field)
      (is (true? passed?) (str field " relation failed")))))
