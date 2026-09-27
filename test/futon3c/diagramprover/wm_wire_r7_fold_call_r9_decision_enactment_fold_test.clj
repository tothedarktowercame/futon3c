(ns futon3c.diagramprover.wm-wire-r7-fold-call-r9-decision-enactment-fold-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-fold-out-support :as support]))
(defn check [] (support/decision :enactment-fold :none))
(def wire {
   :second-layer {:test 'futon3c.diagramprover.wm-wire-r7-fold-call-r9-decision-enactment-fold-test/different-carrier-changes-the-reader-product :kind :value-varying
                  :product [:reader] :intervention :before-reader}
  :wire [:r7-fold-call :r9-decision :enactment-fold]
           :kind :witnessed-hermetically :test `the-real-reader-produces-the-received-value :check check
           :live-records-read support/live-records-read
           :note "The live source labels carry no nonempty fold. Real run! increment, persisted flight, judge fold and decision habit receipt; tamper the fold at cascade-decision-admitted entry."})
(deftest the-real-reader-produces-the-received-value
  (support/assert-live-pins)
  (let [o (check)]
    (is (w/received? o))
    (is (= {:records 1 :samples 1} (:reader o)))
    (is (= :present (get-in o [:read :receipt :status])))))
(deftest absence-at-the-reader-door-is-not-a-witness
  (let [o (support/decision :enactment-fold :absent)]
    (is (not (w/received? o)))
    (is (= {:records 0 :samples 0} (:reader o)))
    (is (= :no-enactment-fold (get-in o [:read :receipt :reason])))))
(deftest different-carrier-changes-the-reader-product
  (let [o (support/decision :enactment-fold :different)]
    (is (not (w/received? o)))
    (is (not= (:writer o) (:reader o)))
    (is (= {:records 2 :samples 2} (:reader o)))))
