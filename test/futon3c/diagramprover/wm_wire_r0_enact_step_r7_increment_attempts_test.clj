(ns futon3c.diagramprover.wm-wire-r0-enact-step-r7-increment-attempts-test
  "Wire [:r0-enact-step :r7-increment :attempts]: the enactment step's
  attempts reaching the habit increment.

  The writer is flight-runner/enact-fn (:r0-enact-step's site,
  flight_runner.clj:901): `:attempts attempts` on the enactment record.
  The reader is futon2.aif.enactment-habit/increment
  (enactment_habit.clj:66-95): `attempts (:attempts enactment)` — counted
  into the receipt's :attempts and checked against the policy key's shown
  patterns, an attempt outside the candidate refusing
  :attempt-outside-candidate. The reader's produced value under the field
  is the receipt's :attempts count.

  No live record carries both ends (live-records-read, shared with the
  r7-flight-call wire): the census finds :attempts in no spike record, and
  the exemplar enactment is hand-authored. So the wire is
  WITNESSED-HERMETICALLY: wm-wire-enact-driver/enact-wc drives the REAL
  enact-fn, and the REAL increment reads the enactment — TAMPERed at its
  door in the bad cases: absent, and an attempt at a pattern outside the
  candidate."
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.cascade-prior :as prior]
            [futon2.aif.enactment-habit :as habit]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-enact-driver :as d]
            [futon3c.diagramprover.wm-wire-r0-enact-step-r7-flight-call-attempts-test
             :as flight-call]))

(def live-records-read flight-call/live-records-read)

(defn observe
  "Drive the real writer (enact-fn via enact-wc), then call the real
   enactment-habit/increment on the enactment — TAMPERed at the reader's
   door — at the policy key of its own attempts' patterns, with a W_c
   pass. The writer's value is the count of the attempts enact-fn wrote;
   the reader's is the receipt's :attempts."
  ([] (observe identity))
  ([tamper]
   (let [{:keys [enactment]} (d/enact-wc)
         identity (prior/policy-key {:mission "M-t"
                                     :shown (mapv :pattern (:attempts enactment))
                                     :semilattice {}})
         receipt (habit/increment (tamper enactment) identity [])]
     {:writer (count (:attempts enactment))
      :reader (:attempts receipt)
      :receipt receipt})))

(defn check [] (observe))

(def wire
  {
   :second-layer {:test 'futon3c.diagramprover.wm-wire-r0-enact-step-r7-increment-attempts-test/absent-attempts-at-the-reader-fail-the-wire :kind :value-varying
                  :product [:reader] :intervention :before-reader}
  :wire [:r0-enact-step :r7-increment :attempts]
   :kind :witnessed-hermetically
   :test `the-attempts-reach-the-increment
   :check check
   :live-records-read live-records-read})

(deftest the-attempts-reach-the-increment
  (let [o (check)]
    (is (= 7 (:writer o)))
    (is (= 1 (get-in o [:receipt :delta])) "a W_c pass increments")
    (is (= :cand/a-registry-first (second (get-in o [:receipt :record-id]))))
    (is (w/received? o))))

(deftest absent-attempts-at-the-reader-fail-the-wire
  (let [o (observe #(assoc % :attempts nil))]
    (is (= 0 (:reader o))
        "no attempts are invented: the receipt counts what it read")
    (is (not (w/received? o)))))

(deftest an-attempt-outside-the-candidate-refuses
  (let [o (observe #(assoc % :attempts [{:pattern :p/outside}]))
        receipt (:receipt o)]
    (is (= :refused (:status receipt)))
    (is (= :attempt-outside-candidate (:reason receipt)))
    (is (= [:p/outside] (:outside receipt)))
    (is (nil? (:reader o)) "a refusal carries no :attempts count")
    (is (not (w/received? o)))))
