(ns futon3c.diagramprover.wm-wire-ask-merge-published-construction-assemble-one-interpretations-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-ask-out-support :as support]))
(def positive (delay (support/interpretations :assemble :none)))
(defn check [] @positive)
(def wire {
   :second-layer {:test 'futon3c.diagramprover.wm-wire-ask-merge-published-construction-assemble-one-interpretations-test/absent-carrier-before-reader-is-not-a-witness :kind :refusal
                  :product [:assembled :kind] :intervention :before-reader :expected :no-admitted-interpretation}
  :wire [:ask-merge-published :construction-assemble-one :interpretations]
           :kind :witnessed-hermetically :test `the-real-reader-receives-the-published-value :check check
           :live-records-read support/live-records-read
           :note "Returned sources tampered before assemble-one; assembled interpretations and problem-tokens witness the read."})
(deftest the-real-reader-receives-the-published-value
  (is (seq (support/live-census)))
  (let [o (check)]
    (is (w/received? o))
    (is (contains? (:tokens o) support/document))
    (is (seq (get-in o [:assembled :constructed-candidates])))))
(deftest absent-carrier-before-reader-is-not-a-witness
  (let [o (support/interpretations :assemble :absent)]
    (is (not (w/received? o)))
    (is (= :no-admitted-interpretation (get-in o [:assembled :kind])))))
(deftest different-carrier-changes-the-reader-product
  (let [o (support/interpretations :assemble :different)]
    (is (not (w/received? o)))
    (is (not= (:writer o) (:reader o)))
    ))
