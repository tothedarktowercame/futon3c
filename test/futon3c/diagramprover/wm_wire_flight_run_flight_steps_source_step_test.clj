(ns futon3c.diagramprover.wm-wire-flight-run-flight-steps-source-step-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))
(def wire-id [:flight-run :flight-steps-source [:step {:record :enactment-entry}]])
(def producer (delay (producer-record/record "measured-step-observe")))
(defn- fields [] (get-in @producer [:wires wire-id]))
(defn check [] (:primary (fields)))
(def wire {:wire wire-id
           :kind :witnessed-hermetically :test `the-observed-handoff :check check
           :live-records-read []
           })
(deftest the-observed-handoff
  (let [o (check)]
    (is (w/received? o) (pr-str o))
    (is (= :present (get-in o [:writer :status])))))
(deftest absence-before-reader-fails
  (let [o (get-in (fields) [:interventions :absent])]
    (is (not (w/received? o)))))
(deftest different-value-before-reader-fails
  (let [o (get-in (fields) [:interventions :different])]
    (is (some? (:reader o)))
    (is (not (w/received? o)))))
