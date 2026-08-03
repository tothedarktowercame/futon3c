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

(deftest smoking-front-door-is-identified
  (let [receipt (bow/front-door-receipt)]
    (is (= :front-door (get-in receipt [:identification :method])))
    (is (= #{:tar} (get-in receipt [:identification :mediators])))
    (is (every? :holds? (:verdicts receipt)))
    (is (empty? (:refusals receipt)))))

(deftest napkin-refuses-after-both-exhaustions
  (let [receipt (bow/napkin-receipt)
        refusal (first (:refusals receipt))]
    (is (= :refusal (get-in receipt [:identification :method])))
    (is (= :do-calculus-identification (:missing-capability refusal)))
    (is (pos? (get-in refusal [:backdoor-exhaustion :candidate-set-count])))
    (is (pos? (get-in refusal [:front-door-exhaustion :candidate-set-count])))
    (is (every? seq
                (map :failed-conditions
                     (get-in refusal
                             [:front-door-exhaustion :candidate-attempts]))))))

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
