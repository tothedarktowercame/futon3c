(ns futon3c.diagramprover.wm-wire-r7-fold-call-r3a-channel-prediction-error-channel-prediction-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-fold-out-support :as support]))
(defn check [] (support/simple :prediction :none))
(def wire {
   :second-layer {:test 'futon3c.diagramprover.wm-wire-r7-fold-call-r3a-channel-prediction-error-channel-prediction-test/absence-at-the-reader-door-is-not-a-witness :kind :refusal
                  :product [:error :reason] :intervention :before-reader :expected :malformed-prediction-triple}
  :wire [:r7-fold-call :r3a-channel-prediction-error :channel-prediction]
           :kind :witnessed-hermetically :test `the-real-reader-produces-the-received-value :check check
           :live-records-read support/live-records-read
           :note "The error record retains predicted mean and variance. Missing prediction is refused as malformed-prediction-triple, not an observation omission."})
(deftest the-real-reader-produces-the-received-value
  (support/assert-live-pins)
  (let [o (check)]
    (is (w/received? o))
    (is (= :present (get-in o [:error :status])))
    (is (number? (get-in o [:error :error])))))
(deftest absence-at-the-reader-door-is-not-a-witness
  (let [o (support/simple :prediction :absent)]
    (is (not (w/received? o)))
    (is (= :refused (get-in o [:error :status])))
    (is (= :malformed-prediction-triple (get-in o [:error :reason])))))
(deftest different-carrier-changes-the-reader-product
  (let [o (support/simple :prediction :different)]
    (is (not (w/received? o)))
    (is (not= (:writer o) (:reader o)))
    (is (= :present (get-in o [:error :status])))))
