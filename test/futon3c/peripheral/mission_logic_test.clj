(ns futon3c.peripheral.mission-logic-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.peripheral.mission-logic :as logic]))

(def ^:private clean-mission-state
  {:mission/obligations
   {"O-main" {:item/status :open
              :item/evidence-type :mixed}
    "O-dep" {:item/status :done
             :item/evidence-type :test}}
   :mission/cycles
   [{:cycle/id "M-test-C001"
     :cycle/blocker-id "O-main"
     :cycle/phase :validate
     :cycle/phases-completed [:observe :propose :execute]
     :cycle/phase-data {:observe {:blocker-id "O-main"}
                        :propose {:approach "thin extraction"}
                        :execute {:artifacts ["src/futon3c/peripheral/mission_logic.clj"]}}}]})

(deftest query-violations-clean-when-mission-state-well-formed
  (testing "thin mission logic stays clean when backend-shaped data is coherent"
    (let [violations (logic/check-mission-state clean-mission-state)]
      (is (empty? (:missing-blockers violations)))
      (is (empty? (:invalid-statuses violations)))
      (is (empty? (:done-with-assertion-only violations)))
      (is (empty? (:phase-order-violations violations)))
      (is (empty? (:missing-phase-outputs violations)))
      (is (not (logic/violations? violations))))))

(deftest query-violations-catches-missing-blocker
  (testing "cycle blockers must refer to real obligations"
    (let [violations (logic/check-mission-state
                      {:mission/obligations {}
                       :mission/cycles [{:cycle/id "C1"
                                         :cycle/blocker-id "O-missing"
                                         :cycle/phase :observe
                                         :cycle/phases-completed []
                                         :cycle/phase-data {}}]})]
      (is (= [["C1" "O-missing" :blocker]]
             (:missing-blockers violations))))))

(deftest query-violations-catches-status-discipline-breaches
  (testing "invalid statuses and :done-with-assertion are both reported"
    (let [violations (logic/check-mission-state
                      {:mission/obligations
                       {"O-bad" {:item/status :banana}
                        "O-assert" {:item/status :done
                                    :item/evidence-type :assertion}}
                       :mission/cycles []})]
      (is (= #{["O-bad" :banana]}
             (set (:invalid-statuses violations))))
      (is (= #{"O-assert"}
             (set (:done-with-assertion-only violations)))))))

(deftest query-violations-catches-phase-order-mismatch
  (testing "completed phases must form the exact prefix of the current phase"
    (let [violations (logic/check-mission-state
                      {:mission/obligations {"O-main" {:item/status :open}}
                       :mission/cycles [{:cycle/id "C1"
                                         :cycle/blocker-id "O-main"
                                         :cycle/phase :validate
                                         :cycle/phases-completed [:observe]
                                         :cycle/phase-data {:observe {:blocker-id "O-main"}
                                                            :propose {:approach "x"}
                                                            :execute {:artifacts ["x"]}}}]})]
      (is (= [{:cycle "C1"
               :current-phase :validate
               :expected #{:observe :propose :execute}
               :actual #{:observe}}]
             (:phase-order-violations violations))))))

(deftest query-violations-catches-missing-phase-output
  (testing "advancing beyond :propose requires an :approach output"
    (let [violations (logic/check-mission-state
                      {:mission/obligations {"O-main" {:item/status :open}}
                       :mission/cycles [{:cycle/id "C1"
                                         :cycle/blocker-id "O-main"
                                         :cycle/phase :execute
                                         :cycle/phases-completed [:observe :propose]
                                         :cycle/phase-data {:observe {:blocker-id "O-main"}}}]})]
      (is (= [{:cycle "C1" :phase :propose :missing #{:approach}}]
             (:missing-phase-outputs violations))))))    
