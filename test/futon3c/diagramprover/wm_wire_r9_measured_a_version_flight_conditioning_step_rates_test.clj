(ns futon3c.diagramprover.wm-wire-r9-measured-a-version-flight-conditioning-step-rates-test
  "Writer and reader values come from the content-addressed
  measured-tick-observe producer record."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))
(def wire-id [:r9-measured-a-version :flight-conditioning-step [:rates {:record :measured-a}]])
(def producer (delay (producer-record/record "measured-tick-observe")))
(defn- wire-fields [] (get-in @producer [:fields :wires wire-id]))
(defn- observe [mode] (get-in (wire-fields) [:modes mode]))
(defn check [] (select-keys (observe :none) [:writer :reader]))
(def wire {:wire wire-id
           :kind :witnessed-hermetically :test `the-observed-handoff :check check
           :live-records-read []
           :note "Values come from the content-addressed measured-tick-observe producer record."})
(deftest the-observed-handoff
  (let [o (check) n (observe :none)]
    (is (w/received? o) (pr-str o))
    (is (= :present (:step-status n)))
    (is (every? #(= {:false-neg 1/12 :false-pos 1/12} %) (vals (:rates n))))
    (is (every? #(= {:false-neg {:numerator 0 :denominator 5}
                    :false-pos {:numerator 0 :denominator 5}} %) (vals (:measurement n))))))
(deftest absence-before-reader-fails
  (let [a (observe :absent)]
    (is (not (w/received? (select-keys a [:writer :reader]))))))
(deftest different-value-before-reader-fails
  (let [d (observe :different)]
    (is (some? (:reader d)))
    (is (not (w/received? (select-keys d [:writer :reader]))))))
