(ns futon3c.diagramprover.wm-wire-construction-construct-r9-decision-construction-receipt-test
  (:require [clojure.test :refer [deftest is]] [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))
(def wire-id [:construction-construct :r9-decision :construction-receipt])
(def producer (delay (producer-record/record "construction-decision")))
(defn- fields [] (get-in @producer [:wires wire-id]))
(defn observe [mutation] (if (= mutation :none) (:primary (fields)) (get-in (fields) [:interventions mutation])))
(defn check [] (observe :none))
(def wire {:wire wire-id :kind :witnessed-hermetically :test `the-observed-handoff
           :check check :live-records-read []})
(deftest the-observed-handoff
  (let [o (check)] (is (w/received? o) (str "writer-reader " (pr-str o)))
       (is (= :machine-constructed (get-in o [:writer :kind]))) (is (:action-present? o))))
(deftest absence-before-reader-fails (is (not (w/received? (observe :absent)))))
(deftest different-value-before-reader-fails
  (let [o (observe :different)] (is (some? (:reader o))) (is (not (w/received? o)))))
