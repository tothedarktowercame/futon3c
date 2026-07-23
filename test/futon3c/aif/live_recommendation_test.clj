(ns futon3c.aif.live-recommendation-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.aif.live-recommendation :as recommendation]))

(defn- frozen-judgement
  []
  (-> "holes/labs/M-typed-memories/wm-selection-regression-20260723.edn"
      io/file slurp edn/read-string :judgement))

(deftest frozen-abstention-cannot-suppress-the-live-recommendation
  (let [result (recommendation/project (frozen-judgement))]
    (testing "1. a reason-bearing strategic recommendation is always present"
      (is (= :recommendation-issued (:status result)))
      (is (= "M-expressions-of-interest"
             (get-in result [:recommendation :target])))
      (is (= :live
             (get-in result [:recommendation :recommendation-authority])))
      (is (true? (get-in result [:recommendation :live-selection?])))
      (is (false? (get-in result [:recommendation :advisory?]))))
    (testing "2. controller, habit-adjusted, and counterfactual rankings stay separate"
      (is (= "M-expressions-of-interest"
             (get-in result [:rankings :controller :winner :target])))
      (is (= "M-learning-loop"
             (get-in result [:rankings :habit-adjusted :winner :target])))
      (is (= "M-expressions-of-interest"
             (get-in result [:rankings :counterfactual :winner :target])))
      (is (true? (get-in result [:comparison :rankings-disagree?]))))
    (testing "3. no operator override is requested"
      (is (false? (get-in result
                          [:recommendation :requires-operator-override?])))
      (is (false? (get-in result
                          [:selection-boundary
                           :operator-override-required?]))))
    (testing "4. selection-layer abstention is quarantined from recommendation"
      (is (true? (get-in result
                         [:selection-boundary :legacy-abstained?])))
      (is (false? (get-in result
                          [:selection-boundary
                           :blocks-recommendation?])))
      (is (= :downstream-act-gate
             (get-in result [:actuation :gate-owner])))
      (is (= :pending-downstream-gates
             (get-in result [:actuation :status])))
      (is (false? (get-in result [:actuation :authorized?]))))
    (testing "5. absent strategic-memory machinery is explicitly non-influential"
      (is (false? (get-in result
                          [:strategic-memory :influenced?])))
      (is (= :no-live-strategic-trace
             (get-in result [:strategic-memory :reason]))))))

(deftest reviewed-strategic-checkpoint-can-influence-with-complete-reasons
  (let [judgement
        (assoc (frozen-judgement)
               :strategic-checkpoint
               {:status :advice-issued
                :recommendation
                {:mission-ids ["M-expressions-of-interest"]
                 :memory-ids ["e-wm-r15-example"]
                 :relation-contributions
                 [{:control-pattern-id "p4ng/R15"
                   :relation :repairs-control}]}})
        result (recommendation/project judgement)]
    (is (= :reviewed-strategic-checkpoint
           (get-in result [:recommendation :source])))
    (is (true? (get-in result [:strategic-memory :influenced?])))
    (is (= ["e-wm-r15-example"]
           (get-in result [:strategic-memory :memory-ids])))
    (is (true? (get-in result
                       [:comparison
                        :newer-strategic-memory-influenced?])))))

(deftest selector-noninfluence-report-is-preserved
  (let [judgement (assoc-in
                   (frozen-judgement)
                   [:decision :strategic-memory]
                   {:influenced? false
                    :reason :no-reviewed-live-strategic-trace
                    :authority :not-yet-live})
        result (recommendation/project judgement)]
    (is (false? (get-in result [:strategic-memory :influenced?])))
    (is (= :no-reviewed-live-strategic-trace
           (get-in result [:strategic-memory :reason])))
    (is (= :not-yet-live
           (get-in result
                   [:strategic-memory :selector-report :authority])))))
