(ns futon3c.diagramprover.wm-wire-r7-fold-call-r9-decision-conditioning-steps-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-fold-out-support :as support]))
(defn check [] (support/decision :conditioning-steps :none))
(def wire {:wire [:r7-fold-call :r9-decision :conditioning-steps]
           :kind :witnessed-hermetically :test `the-real-reader-produces-the-received-value :check check
           :live-records-read support/live-records-read
           :note "Real run! step persisted under a temporary flights directory, read by judge and tampered at cascade-decision-admitted entry. Reader product is the admitted prefix observation update."})
(deftest the-real-reader-produces-the-received-value
  (support/assert-live-pins)
  (let [o (check)]
    (is (w/received? o))
    (is (= :present (get-in o [:flight :enactments 0 :step :status])))
    (is (some #(= :admitted (:conditioning-status %)) (vals (:prefixes o))))))
(deftest absence-at-the-reader-door-is-not-a-witness
  (let [o (support/decision :conditioning-steps :absent)]
    (is (not (w/received? o)))
    (is (every? #(= :no-flight-records (:conditioning-status %)) (vals (:prefixes o))))))
(deftest different-carrier-changes-the-reader-product
  (let [o (support/decision :conditioning-steps :different)]
    (is (not (w/received? o)))
    (is (not= (:writer o) (:reader o)))
    (is (= (inc (get-in o [:writer :f])) (get-in o [:reader :f])))))
