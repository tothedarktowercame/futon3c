(ns futon3c.logic.invariant-runner-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.logic.invariant-runner :as runner]))

(deftest run-domain-reports-dormant-without-calling-check
  (testing "dormant domains are reported honestly instead of being treated as clean"
    (let [called? (atom false)
          report (runner/run-domain
                  {:tickle false}
                  {:domain-id :tickle
                   :input :ignored
                   :check (fn [_]
                            (reset! called? true)
                            {:violations [:should-not-run]})})]
      (is (= :dormant (:state report)))
      (is (false? (:loaded? report)))
      (is (nil? (:violations report)))
      (is (false? @called?)))))

(deftest run-domain-executes-loaded-domains
  (testing "loaded domains still run and preserve their violation map"
    (let [report (runner/run-domain
                  {:proof true}
                  {:domain-id :proof
                   :input {:x 1}
                   :check (fn [_] {:dangling-refs [["A" "ghost" :depends-on]]})})]
      (is (= :active (:state report)))
      (is (:loaded? report))
      (is (:has-violations? report))
      (is (= {:dangling-refs [["A" "ghost" :depends-on]]}
             (:violations report))))))

(deftest run-domains-summarizes-mixed-load-profile
  (testing "summary distinguishes active clean, active violating, and dormant domains"
    (let [reports (runner/run-domains
                   {:proof true
                    :tickle false
                    :mission true}
                   [{:domain-id :proof
                     :input nil
                     :check (fn [_] {:dangling-refs []})}
                    {:domain-id :tickle
                     :input nil
                     :check (fn [_] {:route-breaks []})}
                    {:domain-id :mission
                     :input nil
                     :check (fn [_] {:missing-blockers [["C1" "O-x" :blocker]]})}])]
      (is (= {:active 2
              :dormant 1
              :violating 1
              :clean-active 1}
             (runner/summarize reports)))
      (is (= ["proof: active, clean"
              "tickle: dormant"
              "mission: active, violations present"]
             (clojure.string/split-lines (runner/render-report reports)))))))
