(ns futon3c.diagramprover.wm-wire-r7-fold-call-r3-apply-belief-events-loop-belief-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-fold-out-support :as support]))
(defn check [] (support/simple :belief :none))
(def wire {:wire [:r7-fold-call :r3-apply-belief-events :loop-belief]
           :kind :witnessed-hermetically :test `the-real-reader-produces-the-received-value :check check
           :live-records-read support/live-records-read
           :note "The judge prior reaches apply-arena-belief-events. Compare its produced posterior with update-belief-batch on the untouched prior and actual events; mutate the prior before the reader."})
(deftest the-real-reader-produces-the-received-value
  (support/assert-live-pins)
  (let [o (check)]
    (is (w/received? o))
    (is (seq (:events o)))
    (is (not= (:input o) (:reader o)))))
(deftest absence-at-the-reader-door-is-not-a-witness
  (let [o (support/simple :belief :absent)]
    (is (not (w/received? o)))
    ))
(deftest different-carrier-changes-the-reader-product
  (let [o (support/simple :belief :different)]
    (is (not (w/received? o)))
    (is (not= (:writer o) (:reader o)))
    ))
