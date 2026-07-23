(ns futon3c.peripheral.strategic-canary-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.peripheral.strategic-canary :as canary]
            [futon3c.peripheral.strategic-policies :as policies]
            [futon3c.peripheral.strategic-policies-test
             :refer [phase7-input]]))

(defn- read-edn
  [path]
  (-> path io/file slurp edn/read-string))

(defn- canary-input
  []
  (let [{:keys [outer-result fixture]} (phase7-input)]
    {:shadow-result (policies/run-shadow-window outer-result fixture)
     :fixture
     (read-edn
      "holes/labs/M-typed-memories/phase8-advice-only-canary.edn")}))

(deftest advice-only-preserves-operator-choice-and-the-old-controller
  (let [{:keys [shadow-result fixture]} (canary-input)
        result (canary/advice-only shadow-result fixture)]
    (is (= :advice-issued (:status result)))
    (is (= :advice-only (:effective-rung result)))
    (is (= ["M-shared-memory-control-build-test"
            "M-aif-policy-conditioned-eig"]
           (get-in result [:recommendation :mission-ids])))
    (is (= ["M-shared-memory-control-build-test"]
           (get-in result
                   [:counterfactual-baseline :mission-ids])))
    (is (true? (get-in result [:operator :agrees-with-advice?])))
    (is (false? (get-in result [:operator :override-required?])))
    (is (true? (get-in result [:operator :retains-choice?])))
    (is (= :independently-witnessed
           (get-in result
                   [:observed-outcome :witness-status])))
    (is (= ["e-wm-eig-support"]
           (get-in result [:memory-use :used-ids])))
    (is (true? (:old-controller-available? result)))
    (is (false? (get-in result [:enactment :authorized?])))
    (is (false? (get-in result [:enactment :executed?])))
    (is (false? (get-in result [:next-rung :eligible?])))
    (is (nil? (:selected-mission result)))
    (is (false? (:live-ordering-changed? result)))))

(deftest confirmation-cannot-cross-the-advice-only-gate
  (let [{:keys [shadow-result fixture]} (canary-input)
        result
        (canary/advice-only
         shadow-result
         (assoc fixture :requested-rung :confirm-to-enact))]
    (is (= :advice-withheld (:status result)))
    (is (= :advice-only (:effective-rung result)))
    (is (= :reviewed-window-required-before-advance
           (:rollback-reason result)))
    (is (false? (get-in result [:enactment :authorized?])))
    (is (true? (:old-controller-available? result)))))

(deftest safety-provenance-and-query-failures-fall-back
  (let [{:keys [shadow-result fixture]} (canary-input)]
    (testing "tripwire"
      (is (= :tripwire-fired
             (:rollback-reason
              (canary/advice-only
               shadow-result
               (assoc fixture :tripwire-clear? false))))))
    (testing "query bound"
      (is (= :query-or-resource-bound-failed
             (:rollback-reason
              (canary/advice-only
               shadow-result
               (assoc fixture :query-limit 11))))))
    (testing "independent outcome"
      (is (= :independent-outcome-incomplete
             (:rollback-reason
              (canary/advice-only
               shadow-result
               (assoc-in fixture
                         [:observed-outcome :witness-status]
                         :self-asserted))))))
    (testing "memory use provenance"
      (is (= :independent-outcome-incomplete
             (:rollback-reason
              (canary/advice-only
               shadow-result
               (assoc-in fixture
                         [:observed-outcome :memory-ids-used]
                         ["e-not-surfaced"]))))))))
