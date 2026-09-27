(ns futon3c.diagramprover.wm-wire-r7-fold-call-r1-belief-carry-carried-mu-post-test
  (:require [futon3c.diagramprover.wm-wire-fold-products-4a :as products]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-fold-out-support :as support]))
(defn check [] (support/simple :carry :none))
(def wire {:second-layer {:test 'futon3c.diagramprover.wm-wire-r7-fold-call-r1-belief-carry-carried-mu-post-test/reconciliation-retains-each-intervened-posterior
                          :kind :record :product [:reader] :intervention :before-reader}
           :wire [:r7-fold-call :r1-belief-carry :carried-mu-post]
           :kind :witnessed-hermetically :test `the-real-reader-produces-the-received-value :check check
           :live-records-read support/live-records-read
           :note "Reconcile returns the carried posterior for surviving entities; absence is nil at the reader door and produces the fresh prior."})
(deftest the-real-reader-produces-the-received-value
  (support/assert-live-pins)
  (let [o (check)]
    (is (w/received? o))
    (is (= (:reader o) (get-in o [:result :belief-pre])))))
(deftest absence-at-the-reader-door-is-not-a-witness
  (let [o (support/simple :carry :absent)]
    (is (not (w/received? o)))
    (is (= (:fresh o) (:reader o)))))
(deftest different-carrier-changes-the-reader-product
  (let [o (support/simple :carry :different)]
    (is (not (w/received? o)))
    (is (not= (:writer o) (:reader o)))
    ))

(deftest reconciliation-retains-each-intervened-posterior
  (products/assert-carried))
