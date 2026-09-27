(ns futon3c.diagramprover.wm-wire-ask-merge-published-construction-construct-interpretations-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-ask-out-support :as support]))
(def positive (delay (support/interpretations :construct :none)))
(defn check [] @positive)
(def wire {
   :second-layer {:test 'futon3c.diagramprover.wm-wire-ask-merge-published-construction-construct-interpretations-test/different-carrier-changes-the-reader-product :kind :value-varying
                  :product [:reader] :intervention :before-reader}
  :wire [:ask-merge-published :construction-construct :interpretations]
           :kind :witnessed-hermetically :test `the-real-reader-receives-the-published-value :check check
           :live-records-read support/live-records-read
           :note "Published patterns passed to real construct; receipt unreached-wants determines which open wants were produced. Missing interpretations refuse invalid-input."})
(deftest the-real-reader-receives-the-published-value
  (is (seq (support/live-census)))
  (let [o (check)]
    (is (w/received? o))
    (is (= :constructed (get-in o [:constructed :status])))
    (is (= [support/pattern-id] (get-in o [:constructed :candidates 0 :precedence])))))
(deftest absent-carrier-before-reader-is-not-a-witness
  (let [o (support/interpretations :construct :absent)]
    (is (not (w/received? o)))
    (is (= :invalid-input (get-in o [:constructed :kind])))))
(deftest different-carrier-changes-the-reader-product
  (let [o (support/interpretations :construct :different)]
    (is (not (w/received? o)))
    (is (not= (:writer o) (:reader o)))
    (is (contains? (:reader o) support/argue))))
