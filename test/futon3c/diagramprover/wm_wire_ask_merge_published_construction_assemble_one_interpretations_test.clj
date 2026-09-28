(ns futon3c.diagramprover.wm-wire-ask-merge-published-construction-assemble-one-interpretations-test
  (:require [clojure.test :refer [deftest is]] [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))
(def wire-id [:ask-merge-published :construction-assemble-one :interpretations])
(def producer (delay (producer-record/record "ask-out-live-census")))
(defn- fields [] (get-in @producer [:wires wire-id]))
(defn check [] (:primary (fields)))
(def wire {:second-layer {:test `absent-carrier-before-reader-is-not-a-witness :kind :refusal
                          :product [:assembled :kind] :intervention :before-reader
                          :expected :no-admitted-interpretation}
           :wire wire-id :kind :witnessed-hermetically :test `the-real-reader-receives-the-published-value
           :check check :live-records-read []})
(deftest the-real-reader-receives-the-published-value
  (is (seq (:live-census @producer)))
  (let [o (check)] (is (w/received? o) (str "writer-reader " (pr-str o)))
       (is (:tokens-contain-document? o)) (is (:constructed-candidates-present? o))))
(deftest absent-carrier-before-reader-is-not-a-witness
  (let [o (get-in (fields) [:interventions :absent])]
    (is (not (w/received? o))) (is (= :no-admitted-interpretation (:assembled-kind o)))))
(deftest different-carrier-changes-the-reader-product
  (let [o (get-in (fields) [:interventions :different])]
    (is (not (w/received? o))) (is (not= (:writer o) (:reader o)))))
