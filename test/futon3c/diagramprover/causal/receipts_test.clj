(ns futon3c.diagramprover.causal.receipts-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.causal.dag :as dag]
            [futon3c.diagramprover.causal.diagram :as diagram]
            [futon3c.diagramprover.causal.receipts :as receipts]))

(deftest q1-primary-identification
  (let [receipt (receipts/q1)]
    (is (= [#{}] (:adjustment-sets receipt)))
    (is (true? (:holds? (first (:verdicts receipt)))))
    (is (= receipt (receipts/q1)))))

(deftest q2-leak-and-mediation-refusal
  (let [receipt (receipts/q2)]
    (is (= [:L1 :L2 :L3 :L4] (mapv :leak (:leaks receipt))))
    (is (every? #(seq (:opens-path %)) (:leaks receipt)))
    (is (every? :severed-blocks? (:leaks receipt)))
    (is (= :mediator-channel-unobserved
           (get-in receipt [:refusals 0 :reason])))
    (is (= receipt (receipts/q2)))))

(deftest q3-two-topology-acceptance
  (let [memory (dag/load-spec receipts/memory-spec-path)
        variants (receipts/q3-variants memory)
        receipt (receipts/q3 memory)
        star (first (filter #(= :star-forest (:graph %))
                            (:verdicts receipt)))
        populated (first (filter #(= :populated-graph (:graph %))
                                 (:verdicts receipt)))]
    (doseq [[_ causal-dag] variants]
      (is (= causal-dag (dag/validate causal-dag)))
      (is (diagram/canonical? (diagram/dag->diagram causal-dag))))
    (is (true? (:holds? star)))
    (is (false? (:holds? populated)))
    (is (= [[:M-in-store :shared-patterns :V12-minus-M]]
           (:paths populated)))
    (is (= receipt (receipts/q3 memory)))))

(deftest r1-controlled-and-selection-regimes
  (let [receipt (receipts/r1)
        controlled (first (:verdicts receipt))
        uncontrolled (second (:verdicts receipt))
        adjusted (nth (:verdicts receipt) 2)]
    (is (true? (:holds? controlled)))
    (is (false? (:holds? uncontrolled)))
    (is (seq (:paths uncontrolled)))
    (is (some #(and (= [:P20 :P01] (subvec % 0 2))
                    (= :P16 (peek %)))
              (:paths uncontrolled)))
    (is (true? (:holds? adjusted)))
    (is (= receipt (receipts/r1)))))

(deftest all-receipts-are-deterministic
  (is (= (receipts/all-receipts) (receipts/all-receipts))))
