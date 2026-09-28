(ns futon3c.diagramprover.wm-wire-r4-kernel-fpi-policy-free-energy-rates-test
  "Rates wire, witnessed by real calls using ten real subjects admitted through the store and reader.
  See support/live-records-read for the live records lacking both ends."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))

(def wire-id [:r4-kernel :fpi-policy-free-energy :rates])
(def producer (delay (producer-record/record "rates-observe-g30")))
(defn- fields [] (get-in @producer [:wires wire-id]))
(defn check [] (:primary (fields)))
(def wire {:wire wire-id :kind :witnessed-hermetically
           :test `the-writers-value-reaches-the-reader :check check
           :live-records-read []})

(deftest the-writers-value-reaches-the-reader
  (let [o (check)]
    (is (seq (:writer o)))
    (is (w/received? o))))

(deftest typed-absence-carrier-does-not-witness-the-wire
  (is (not (w/received? (get-in (fields) [:interventions :absent])))))

(deftest different-carrier-does-not-witness-the-wire
  (let [o (get-in (fields) [:interventions :different])]
    (is (some? (:reader o)))
    (is (not (w/received? o)))))

(deftest live-records-lack-both-ends
  (is (seq (:live-records @producer))))
