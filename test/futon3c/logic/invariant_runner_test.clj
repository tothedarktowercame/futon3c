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

(deftest run-domain-supports-build-db-query-violations-shape
  (testing "domain specs can use build-db -> query-violations instead of a one-shot check fn"
    (let [report (runner/run-domain
                  {:proof true}
                  {:domain-id :proof
                   :input {:items [1 2 3]}
                   :build-db (fn [{:keys [items]}] {:count (count items)})
                   :query-violations (fn [{:keys [count]}]
                                       {:count-mismatch (if (= count 3) [] [count])})})]
      (is (= :active (:state report)))
      (is (= {:count-mismatch []} (:violations report)))
      (is (false? (:has-violations? report))))))

(deftest run-domain-does-not-force-input-for-dormant-domains
  (testing "input-fn is not called for dormant domains"
    (let [called? (atom false)
          report (runner/run-domain
                  {:codex false}
                  {:domain-id :codex
                   :input-fn (fn []
                               (reset! called? true)
                               {:jobs []})
                   :check (fn [_] {:orphan-announcements []})})]
      (is (= :dormant (:state report)))
      (is (false? @called?)))))

(deftest run-aggregate-produces-obligations-and-dispatchable-tasks
  (testing "aggregate output includes reports, obligation classes, and queue-compatible tasks"
    (let [aggregate (runner/run-aggregate
                     {:mission true
                      :agency true
                      :tickle false}
                     [{:domain-id :mission
                       :input nil
                       :check (fn [_]
                                {:missing-blockers [["C1" "O-x" :blocker]]
                                 :missing-phase-outputs [{:cycle "C1"
                                                          :phase :propose
                                                          :missing #{:approach}}]})}
                      {:domain-id :agency
                       :input nil
                       :check (fn [_]
                                {:entry-exit-asymmetry [[:mission :explore]]
                                 :proxy-agents [["claude-r" "http://remote"]]})}
                      {:domain-id :tickle
                       :input nil
                       :check (fn [_]
                                {:unregistered-pages ["page-1"]})}])]
      (is (= {:active 2
              :dormant 1
              :violating 2
              :clean-active 0
              :obligations-total 4
              :auto-fixable 2
              :needs-review 1
              :informational 1
              :dispatchable-tasks 2}
             (:summary aggregate)))
      (is (= 2 (count (:dispatchable-tasks aggregate))))
      (is (= 1 (count (get-in aggregate [:obligations-by-actionability :needs-review]))))
      (is (= 1 (count (get-in aggregate [:obligations-by-actionability :informational]))))
      (is (= ["mission: active, violations present"
              "agency: active, violations present"
              "tickle: dormant"
              "obligations: 4 total, 2 auto-fixable, 1 review, 1 informational"
              "next dispatchable: INV-mission-missing-blockers-001 — Repair mission cycle blocker reference: [\"C1\" \"O-x\" :blocker]"]
             (clojure.string/split-lines (runner/render-aggregate aggregate)))))))
