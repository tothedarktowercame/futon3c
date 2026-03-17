(ns futon3c.logic.obligation-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.logic.obligation :as obligation]))

(deftest report->obligations-classifies-auto-fixable-mission-cases
  (testing "mission structural breaches become dispatchable auto-fixable obligations"
    (let [obligations (obligation/report->obligations
                       {:domain-id :mission
                        :state :active
                        :violations {:missing-blockers [["C1" "O-missing" :blocker]]
                                     :missing-phase-outputs [{:cycle "C1"
                                                              :phase :propose
                                                              :missing #{:approach}}]}})
          grouped (obligation/group-by-actionability obligations)]
      (is (= 2 (count obligations)))
      (is (= 2 (count (:auto-fixable grouped))))
      (is (every? :dispatchable? obligations))
      (is (= ["INV-mission-missing-blockers-001"
              "INV-mission-missing-phase-outputs-001"]
             (mapv :id obligations)))
      (is (= #{:existence :required-outputs}
             (set (map :family obligations))))
      (is (= 2 (count (obligation/dispatchable-tasks obligations)))))))

(deftest report->obligations-preserves-needs-review-and-informational-splits
  (testing "agency review questions are not silently promoted to dispatchable tasks"
    (let [obligations (obligation/report->obligations
                       {:domain-id :agency
                        :state :active
                        :violations {:entry-exit-asymmetry [[:mission :explore]]
                                     :proxy-agents [["claude-r" "http://remote"]]
                                     :agents-invoking ["claude-1"]}})
          grouped (obligation/group-by-actionability obligations)]
      (is (= 3 (count obligations)))
      (is (= 1 (count (:needs-review grouped))))
      (is (= 2 (count (:informational grouped))))
      (is (empty? (:auto-fixable grouped)))
      (is (= "Review hop asymmetry from :mission to :explore"
             (:label (first (:needs-review grouped)))))
      (is (empty? (obligation/dispatchable-tasks obligations))))))

(deftest report->obligations-classifies-codex-cross-store-residual-as-review
  (testing "session continuity drift stays explicit and non-dispatchable"
    (let [obligations (obligation/report->obligations
                       {:domain-id :codex
                        :state :active
                        :violations {:running-session-mismatches
                                     [["job-1" "codex-1" "sid-job" "sid-reg"]]
                                     :orphan-announcements
                                     [["a-1" "missing-job"]]}})
          by-key (into {} (map (juxt :violation-key identity) obligations))]
      (is (= :needs-review
             (get-in by-key [:running-session-mismatches :actionability])))
      (is (= :cross-store-agreement
             (get-in by-key [:running-session-mismatches :family])))
      (is (= :auto-fixable
             (get-in by-key [:orphan-announcements :actionability])))
      (is (= 1 (count (obligation/dispatchable-tasks obligations)))))))

(deftest report->obligations-ignores-dormant-domains
  (testing "dormant domains do not produce fake clean or fake pending work"
    (is (empty?
         (obligation/report->obligations
          {:domain-id :tickle
           :state :dormant
           :violations {:unregistered-pages [["p1" "ghost"]]
                        :orphan-escalations [["e1" "ghost" "ob-1"]]}})))))

(deftest report->obligations-falls-back-honestly-for-unknown-keys
  (testing "unclassified keys surface as needs-review rather than being dropped"
    (let [obligations (obligation/report->obligations
                       {:domain-id :mystery
                        :state :active
                        :violations {:new-thing [[:x 1]]}})]
      (is (= 1 (count obligations)))
      (is (= :needs-review (:actionability (first obligations))))
      (is (= :fallback (:classification-source (first obligations))))
      (is (str/includes? (:label (first obligations)) "unclassified structural-law violation")))))
