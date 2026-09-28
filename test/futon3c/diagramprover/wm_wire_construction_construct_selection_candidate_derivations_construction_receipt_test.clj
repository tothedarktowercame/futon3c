(ns futon3c.diagramprover.wm-wire-construction-construct-selection-candidate-derivations-construction-receipt-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))

(def producer (delay (producer-record/record "construction-live-digest")))
(defn- fields [] (:fields @producer))
(defn observe [mutation] (get-in (fields) [:interventions mutation]))
(defn check [] (select-keys (fields) [:writer :reader :recomputed]))

(def wire
  {:second-layer
   {:test 'futon3c.diagramprover.wm-wire-construction-construct-selection-candidate-derivations-construction-receipt-test/different-value-before-reader-fails
    :kind :value-varying :product [:reader] :intervention :before-reader}
   :wire [:construction-construct :selection-candidate-derivations :construction-receipt]
   :kind :verified
   :test `the-observed-handoff
   :check check
   :live-records-read []
   :record (:source-record @producer)
   :note "Writer and reader digests come from the content-addressed construction-live-digest producer record."})

(deftest the-observed-handoff
  (let [recorded (fields)
        observed (check)]
    (is (:writer-present? recorded) "writer")
    (is (false? (:writer-typed-absence? recorded)) "writer is not a typed absence")
    (is (w/received? observed) (str "writer-reader " (pr-str observed)))
    (doseq [field [:writer-recomputed?
                   :receipt-machine-constructed?
                   :outer-receipt-absent?
                   :entry-retains-receipt?]]
      (testing (name field)
        (is (true? (get recorded field)) (str field " relation failed"))))))

(deftest absence-before-reader-fails
  (is (false? (:received? (observe :absent))) "received?"))

(deftest different-value-before-reader-fails
  (doseq [[field passed?] (:second-layer (fields))]
    (testing (name field)
      (is (if (= field :received?)
            (false? passed?)
            (true? passed?))
          (str field " relation failed")))))
