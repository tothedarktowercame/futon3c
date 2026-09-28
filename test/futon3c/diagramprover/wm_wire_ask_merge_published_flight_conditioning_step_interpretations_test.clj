(ns futon3c.diagramprover.wm-wire-ask-merge-published-flight-conditioning-step-interpretations-test
  "Ask-out step wire, read from the content-addressed ask-out-step producer
  record. The producer ran the real support/step pipeline (publish, assemble,
  cascade decision, persisted offline run, flight/conditioning-step); this
  reader loads no product code."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))

(def producer (delay (producer-record/record "ask-out-step")))
(def live-census (delay (:live-census @producer)))
(def live-records-read (delay (:live-records-read @producer)))
(defn step [mutation] (get-in @producer [:cases mutation]))
(defn check [] (step :none))
(def wire {
   :second-layer {:test 'futon3c.diagramprover.wm-wire-ask-merge-published-flight-conditioning-step-interpretations-test/different-carrier-changes-the-reader-product :kind :value-varying
                  :product [:step :q] :intervention :before-reader}
  :wire [:ask-merge-published :flight-conditioning-step :interpretations]
           :kind :witnessed-hermetically :test `the-real-reader-receives-the-published-value :check check
           :live-records-read @live-records-read
           :note "Tamper on merged sources BEFORE decision and persist-run-record!. Reader converter feeds rollout; altered produces changes q and B digest. Absent sources refuse assembly no-admitted-interpretation, so the persisted abstention gives no-measured-a, earlier than no-interpretation."})
(deftest the-real-reader-receives-the-published-value
  (is (seq @live-census))
  (let [o (check)]
    (is (w/received? o))
    (is (= :present (get-in o [:step :status])))
    (is (= [:writing-coherence/meet-the-reader-where-they-are] (get-in o [:step :b :precedence])))
    (is (every? #(= {:false-neg 1/12 :false-pos 1/12} %) (vals (get-in o [:step :measured-a :rates]))))))
(deftest absent-carrier-before-reader-is-not-a-witness
  (let [o (step :absent)]
    (is (not (w/received? o)))
    (is (= :no-admitted-interpretation (get-in o [:assembled :refusals 0 :kind])))
    (is (= :no-measured-a (get-in o [:step :reason])))))
(deftest different-carrier-changes-the-reader-product
  (let [o (step :different)]
    (is (not (w/received? o)))
    (is (not= (:writer o) (:reader o)))
    (is (not= (get-in (check) [:step :b]) (get-in o [:step :b])))
    (is (not= (get-in (check) [:step :q]) (get-in o [:step :q])))))
