(ns futon3c.diagramprover.wm-wire-r7-fold-call-r9-decision-conditioning-steps-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))

(def reader-kind :conditioning-steps)
(def producer (delay (producer-record/record "fold-out-decision")))
(defn- wire-fields [] (get-in @producer [:fields :wires reader-kind]))
(defn check []
  (let [fields (wire-fields)]
    {:writer (:writer fields) :reader (:reader fields)}))

(def wire
  {:second-layer
   {:test 'futon3c.diagramprover.wm-wire-r7-fold-call-r9-decision-conditioning-steps-test/absence-at-the-reader-door-is-not-a-witness
    :kind :refusal :product [:prefixes] :intervention :before-reader
    :expected :no-flight-records}
   :wire [:r7-fold-call :r9-decision :conditioning-steps]
   :kind :witnessed-hermetically
   :test `the-real-reader-produces-the-received-value
   :check check
   :live-records-read []
   :note "Real run! step persisted under a temporary flights directory, read by judge and tampered at cascade-decision-admitted entry. Reader product is the admitted prefix observation update. The values are read from the producer record `fold-out-decision`."})

(deftest the-real-reader-produces-the-received-value
  (let [fields (wire-fields)
        observed (check)]
    (is (:writer-present? fields) "writer")
    (is (false? (:writer-typed-absence? fields)) "writer is not a typed absence")
    (is (w/received? observed) (str "writer-reader " (pr-str observed)))
    (doseq [[field passed?] (:ordinary fields)]
      (testing (name field)
        (is (true? passed?) (str field " relation failed"))))))

(deftest absence-at-the-reader-door-is-not-a-witness
  (doseq [[field passed?] (get-in @producer [:fields :second-layer reader-kind])]
    (testing (name field)
      (is (if (= field :received?)
            (false? passed?)
            (true? passed?))
          (str field " relation failed")))))

(deftest different-carrier-changes-the-reader-product
  (doseq [[field passed?] (get-in (wire-fields) [:interventions :different])]
    (testing (name field)
      (is (if (= field :received?)
            (false? passed?)
            (true? passed?))
          (str field " relation failed")))))
