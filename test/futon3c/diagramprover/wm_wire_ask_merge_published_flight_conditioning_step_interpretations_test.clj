(ns futon3c.diagramprover.wm-wire-ask-merge-published-flight-conditioning-step-interpretations-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-ask-out-support :as support]))
(def positive (delay (support/step :none)))
(defn check [] @positive)
(def wire {
   :second-layer {:test 'futon3c.diagramprover.wm-wire-ask-merge-published-flight-conditioning-step-interpretations-test/different-carrier-changes-the-reader-product :kind :value-varying
                  :product [:step :q] :intervention :before-reader}
  :wire [:ask-merge-published :flight-conditioning-step :interpretations]
           :kind :witnessed-hermetically :test `the-real-reader-receives-the-published-value :check check
           :live-records-read support/live-records-read
           :note "Tamper on merged sources BEFORE decision and persist-run-record!. Reader converter feeds rollout; altered produces changes q and B digest. Absent sources refuse assembly no-admitted-interpretation, so the persisted abstention gives no-measured-a, earlier than no-interpretation."})
(deftest the-real-reader-receives-the-published-value
  (is (seq (support/live-census)))
  (let [o (check)]
    (is (w/received? o))
    (is (= :present (get-in o [:step :status])))
    (is (= [support/pattern-id] (get-in o [:step :b :precedence])))
    (is (every? #(= {:false-neg 1/12 :false-pos 1/12} %) (vals (get-in o [:step :measured-a :rates]))))))
(deftest absent-carrier-before-reader-is-not-a-witness
  (let [o (support/step :absent)]
    (is (not (w/received? o)))
    (is (= :no-admitted-interpretation (get-in o [:assembled :refusals 0 :kind])))
    (is (= :no-measured-a (get-in o [:step :reason])))))
(deftest different-carrier-changes-the-reader-product
  (let [o (support/step :different)]
    (is (not (w/received? o)))
    (is (not= (:writer o) (:reader o)))
    (is (not= (get-in (check) [:step :b]) (get-in o [:step :b])))
    (is (not= (get-in (check) [:step :q]) (get-in o [:step :q])))))
