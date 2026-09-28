(ns futon3c.diagramprover.wm-wire-ask-merge-published-construction-construct-interpretations-test
  (:require [clojure.test :refer [deftest is]] [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))
(def wire-id [:ask-merge-published :construction-construct :interpretations])
(def producer (delay (producer-record/record "ask-out-live-census")))
(defn- fields [] (get-in @producer [:wires wire-id]))
(defn check [] (:primary (fields)))
(def wire {:second-layer {:test `different-carrier-changes-the-reader-product :kind :value-varying
                          :product [:reader] :intervention :before-reader}
           :wire wire-id :kind :witnessed-hermetically :test `the-real-reader-receives-the-published-value
           :check check :live-records-read []})
(deftest the-real-reader-receives-the-published-value
  (is (seq (:live-census @producer)))
  (let [o (check)] (is (w/received? o) (str "writer-reader " (pr-str o)))
       (is (= :constructed (:constructed-status o)))
       (is (= [:writing-coherence/meet-the-reader-where-they-are] (:constructed-precedence o)))))
(deftest absent-carrier-before-reader-is-not-a-witness
  (let [o (get-in (fields) [:interventions :absent])]
    (is (not (w/received? o))) (is (= :invalid-input (:constructed-kind o)))))
(deftest different-carrier-changes-the-reader-product
  (let [o (get-in (fields) [:interventions :different])]
    (is (not (w/received? o))) (is (not= (:writer o) (:reader o)))
    (is (:reader-contains-argue? o))))
