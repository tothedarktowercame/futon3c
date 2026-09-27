(ns futon3c.diagramprover.wm-wire-morning-brief-fold-r7-fold-call-belief-test
  (:require [futon3c.diagramprover.wm-wire-fold-products-4a :as products]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-fold-in-support :as support]))
(defn check [] (support/observe :brief :none))
(def wire {:second-layer {:test 'futon3c.diagramprover.wm-wire-morning-brief-fold-r7-fold-call-belief-test/same-observation-changes-the-updated-posterior
                          :kind :value-varying :product [:posterior] :intervention :before-reader}
           :wire [:morning-brief-fold :r7-fold-call [:belief {:record :morning-brief-fold}]]
           :kind :witnessed-hermetically :test `the-judge-produces-the-received-value :check check
           :live-records-read support/live-records-read
           :note "Produced judge :belief equals the folded belief when scan channels are absent. Typed-absence belief maps remain absent in the produced belief; they are not wire evidence."})
(deftest the-judge-produces-the-received-value
  (support/assert-live-pins)
  (let [o (check)]
    (is (nil? (get-in o [:result :wire-error])))
    (is (w/received? o))))
(deftest absence-before-the-judge-does-not-witness-the-wire
  (let [o (support/observe :brief :absent)]
    (is (not (w/received? o)))
    (is (= {:absent :not-carried} (:reader o)))
    (is (nil? (get-in o [:result :wire-error])))))
(deftest different-carrier-changes-the-produced-value
  (let [o (support/observe :brief :different)]
    (is (nil? (get-in o [:result :wire-error])))
    (is (not (w/received? o)))
    (is (not= (:writer o) (:reader o)))))

(deftest same-observation-changes-the-updated-posterior
  (products/assert-updated :brief))
