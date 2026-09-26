(ns futon3c.diagramprover.wm-wire-loop-entry-loop-plan-trigger-test
  "Wire [:loop-entry :loop-plan :trigger]. Real trigger calls;
  no live record carries both ends. See support/live-records-read."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-plan-support :as support]))

(defn observe
  ([] (observe identity))
  ([tamper] (support/observe :trigger :trigger tamper)))

(defn check [] (observe))

(def wire
  {:wire [:loop-entry :loop-plan :trigger]
   :kind :witnessed-hermetically
   :test `the-writers-value-reaches-the-reader
   :check check :live-records-read support/live-records-read})

(deftest the-writers-value-reaches-the-reader
  (is (w/received? (check))))

(deftest typed-absence-at-the-reader-fails
  (let [o (observe #(assoc % :trigger {:absent :not-carried}))]
    (is (w/typed-absence? (:reader o)))
    (is (not (w/received? o)))))

(deftest another-value-at-the-reader-fails
  (let [o (observe #(assoc % :trigger :different-clock))]
    (is (some? (:reader o)))
    (is (not (w/received? o)))))

(deftest live-records-do-not-witness-this-wire
  (support/assert-live-records))
