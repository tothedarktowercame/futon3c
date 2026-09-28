(ns futon3c.diagramprover.wm-wire-construction-assemble-one-r9-decision-want-test
  (:require [clojure.test :refer [deftest is testing]] [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))
(def wire-id [:construction-assemble-one :r9-decision [:want {:record :cascade-problem}]])
(def producer (delay (producer-record/record "construction-decision")))
(defn- fields [] (get-in @producer [:wires wire-id]))
(defn observe [mutation] (if (= mutation :none) (:primary (fields)) (get-in (fields) [:interventions mutation])))
(defn check [] (observe :none))
(def wire {:second-layer {:test `changed-carrier-changes-the-derived-product :kind :value-varying
                          :product [:scores] :intervention :before-reader}
           :wire wire-id :kind :witnessed-hermetically :test `the-observed-handoff
           :check check :live-records-read []})
(deftest the-observed-handoff
  (let [o (check)] (is (w/received? o) (str "writer-reader " (pr-str o))) (is (:action-present? o))))
(deftest absence-before-reader-fails (is (not (w/received? (observe :absent)))))
(deftest different-value-before-reader-fails
  (let [o (observe :different)] (is (some? (:reader o))) (is (not (w/received? o)))))
(deftest changed-carrier-changes-the-derived-product
  (let [r (:second-layer (fields))]
    (doseq [k [:competing-before? :scores-numeric? :scores-differ?]]
      (testing (name k) (is (true? (get r k)))))
    (is (= [3 2] (:candidate-counts r)))
    (is (not= (get-in r [:before :scores]) (get-in r [:after :scores])))))
