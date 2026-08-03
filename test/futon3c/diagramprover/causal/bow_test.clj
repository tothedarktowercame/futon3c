(ns futon3c.diagramprover.causal.bow-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.causal.bow :as bow]))

(deftest simpson-adjustment
  (let [receipt (bow/simpson-receipt)]
    (is (false? (get-in receipt [:verdicts 0 :holds?])))
    (is (= [[:treatment :severity :recovery]]
           (get-in receipt [:verdicts 0 :paths])))
    (is (true? (get-in receipt [:verdicts 1 :holds?])))))

(deftest sprinkler-collider
  (let [receipt (bow/sprinkler-receipt)]
    (is (true? (get-in receipt [:verdicts 0 :holds?])))
    (is (false? (get-in receipt [:verdicts 1 :holds?])))
    (is (= [[:rain :wet-grass :sprinkler]]
           (get-in receipt [:verdicts 1 :paths])))))

(deftest observed-only-front-door-refusal-is-exhaustive
  (let [receipt (bow/front-door-receipt)
        refusal (first (:refusals receipt))]
    (is (false? (get-in receipt [:verdicts 0 :holds?])))
    (is (= :front-door-identification (:missing-capability refusal)))
    (is (= 2 (:candidate-set-count refusal)))
    (is (= [#{} #{:tar}] (mapv :given (:candidate-attempts refusal))))
    (is (every? #(= [[:smoking :U :cancer]] (:surviving-paths %))
                (:candidate-attempts refusal)))))

(deftest monty-collider
  (let [receipt (bow/monty-receipt)]
    (is (true? (get-in receipt [:verdicts 0 :holds?])))
    (is (false? (get-in receipt [:verdicts 1 :holds?])))
    (is (= [[:choice :host-opens :prize]]
           (get-in receipt [:verdicts 1 :paths])))))

(deftest firing-squad-boundary
  (let [receipt (bow/firing-squad-receipt)]
    (is (true? (get-in receipt [:verdicts 0 :holds?])))
    (is (= [[:soldier-A :death]] (get-in receipt [:verdicts 0 :paths])))
    (is (= :counterfactual-identification
           (get-in receipt [:refusals 0 :missing-capability])))))

(deftest deterministic-receipts
  (is (= (bow/all-bow-receipts) (bow/all-bow-receipts))))
