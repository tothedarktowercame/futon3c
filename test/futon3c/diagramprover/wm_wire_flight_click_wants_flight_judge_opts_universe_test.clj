(ns futon3c.diagramprover.wm-wire-flight-click-wants-flight-judge-opts-universe-test
  (:require [clojure.test :refer [deftest is]] [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))
(def wire-id [:flight-click-wants :flight-judge-opts :universe])
(def producer (delay (producer-record/record "ask-out-live-census")))
(defn- fields [] (get-in @producer [:wires wire-id]))
(defn check [] (:primary (fields)))
(def wire {:wire wire-id :kind :witnessed-hermetically :test `the-real-reader-receives-the-published-value
           :second-layer {:test `reader-carries-the-intervened-value :kind :record :product [:products]
                          :intervention :before-reader}
           :check check :live-records-read []})
(deftest the-real-reader-receives-the-published-value
  (is (seq (:live-census @producer)))
  (let [o (check)] (is (w/received? o) (str "writer-reader " (pr-str o))) (is (seq (:reader o)))))
(deftest absent-carrier-before-reader-is-not-a-witness
  (is (not (w/received? (get-in (fields) [:interventions :absent])))))
(deftest different-carrier-changes-the-reader-product
  (let [o (get-in (fields) [:interventions :different])]
    (is (not (w/received? o))) (is (not= (:writer o) (:reader o)))))
(deftest reader-carries-the-intervened-value
  (let [r (:second-layer (fields))]
    (is (:before-present? r)) (is (:written-equals-products? r))
    (is (:products-differ? r)) (is (:other-output-unchanged? r))))
