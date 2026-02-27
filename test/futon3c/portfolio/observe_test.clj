(ns futon3c.portfolio.observe-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.portfolio.observe :as obs]))

;; =============================================================================
;; Utility tests
;; =============================================================================

(deftest clamp01-test
  (testing "clamp01 bounds"
    (is (= 0.0 (obs/clamp01 -1.0)))
    (is (= 0.0 (obs/clamp01 0.0)))
    (is (= 0.5 (obs/clamp01 0.5)))
    (is (= 1.0 (obs/clamp01 1.0)))
    (is (= 1.0 (obs/clamp01 2.5)))))

(deftest rescale-test
  (testing "rescale maps [lo,hi] → [0,1]"
    (is (= 0.0 (obs/rescale -1.0 -1.0 1.0)))
    (is (= 0.5 (obs/rescale 0.0 -1.0 1.0)))
    (is (= 1.0 (obs/rescale 1.0 -1.0 1.0))))
  (testing "rescale clamps out-of-range"
    (is (= 0.0 (obs/rescale -2.0 -1.0 1.0)))
    (is (= 1.0 (obs/rescale 3.0 -1.0 1.0))))
  (testing "rescale handles zero range"
    (is (= 0.5 (obs/rescale 5.0 5.0 5.0)))))

;; =============================================================================
;; Observation channel tests
;; =============================================================================

(def sample-mc-state
  "A representative portfolio state for testing."
  {:total 20
   :complete 10
   :blocked 3
   :coverage-pct 0.65
   :coverage-slope 0.05
   :mana-pool-balance 500.0
   :mana-cap 1000.0
   :mana-available? true
   :evidence-per-day 8
   :max-chain 2
   :gaps 4
   :stalled 2
   :spinoff-candidates 1
   :pattern-reuse-ratio 0.3
   :days-since-review 7.0})

(deftest observe-produces-15-channels
  (let [o (obs/observe sample-mc-state)]
    (testing "all 15 channels present (12 mc + 3 heartbeat)"
      (is (= 15 (count o)))
      (doseq [k obs/channel-keys]
        (is (contains? o k) (str "missing channel: " k))))
    (testing "all values in [0,1]"
      (doseq [[k v] o]
        (is (<= 0.0 v 1.0) (str k " = " v " out of bounds"))))))

(deftest observe-specific-channels
  (let [o (obs/observe sample-mc-state)]
    (testing "mission-complete-ratio = 10/20 = 0.5"
      (is (== 0.5 (:mission-complete-ratio o))))
    (testing "coverage-pct passes through"
      (is (== 0.65 (:coverage-pct o))))
    (testing "coverage-trajectory: 0.05 rescaled from [-1,1] → [0,1]"
      (is (< 0.5 (:coverage-trajectory o) 0.6)))
    (testing "mana-available = 500/1000 = 0.5"
      (is (== 0.5 (:mana-available o))))
    (testing "blocked-ratio = 3/20 = 0.15"
      (is (== 0.15 (:blocked-ratio o))))
    (testing "evidence-velocity = 8/20 = 0.4"
      (is (== 0.4 (:evidence-velocity o))))
    (testing "dependency-depth = 2/10 = 0.2"
      (is (== 0.2 (:dependency-depth o))))
    (testing "gap-count = 4/120 ≈ 0.033"
      (is (< 0.033 (:gap-count o) 0.034)))
    (testing "stall-count = 2/20 = 0.1"
      (is (== 0.1 (:stall-count o))))
    (testing "spinoff-pressure = 1/40 = 0.025"
      (is (== 0.025 (:spinoff-pressure o))))
    (testing "pattern-reuse = 0.3"
      (is (== 0.3 (:pattern-reuse o))))
    (testing "review-age = 7/14 = 0.5"
      (is (== 0.5 (:review-age o))))))

(deftest observe-empty-portfolio
  (testing "empty portfolio produces valid [0,1] channels"
    (let [o (obs/observe {:total 0 :complete 0 :blocked 0})]
      (doseq [[k v] o]
        (is (<= 0.0 v 1.0) (str k " = " v " out of bounds"))))))

(deftest observe-extreme-values
  (testing "extreme values clamp to [0,1]"
    (let [o (obs/observe {:total 5
                          :complete 100   ; more complete than total
                          :blocked 0
                          :coverage-pct 1.5
                          :coverage-slope 5.0
                          :evidence-per-day 999
                          :max-chain 50
                          :gaps 200
                          :days-since-review 365.0})]
      (doseq [[k v] o]
        (is (<= 0.0 v 1.0) (str k " = " v " out of bounds"))))))

(deftest obs->vector-test
  (let [o (obs/observe sample-mc-state)
        v (obs/obs->vector o)]
    (testing "vector has 15 elements"
      (is (= 15 (count v))))
    (testing "vector order matches channel-keys"
      (is (= (mapv #(get o %) obs/channel-keys) v)))))

(deftest heartbeat-channels-default-neutral
  (let [o (obs/observe sample-mc-state)]
    (testing "effort-prediction-error defaults to 0 (no error)"
      (is (== 0.0 (:effort-prediction-error o))))
    (testing "bid-completion-rate defaults to 0.5 (neutral)"
      (is (== 0.5 (:bid-completion-rate o))))
    (testing "unplanned-work-ratio defaults to 0 (no unplanned)"
      (is (== 0.0 (:unplanned-work-ratio o))))))

(deftest heartbeat-channels-with-data
  (let [mc-with-hb (assoc sample-mc-state
                          :effort-prediction-error 0.4
                          :bid-completion-rate 0.8
                          :unplanned-work-ratio 0.2)
        o (obs/observe mc-with-hb)]
    (testing "effort-prediction-error passes through"
      (is (== 0.4 (:effort-prediction-error o))))
    (testing "bid-completion-rate passes through"
      (is (== 0.8 (:bid-completion-rate o))))
    (testing "unplanned-work-ratio passes through"
      (is (== 0.2 (:unplanned-work-ratio o))))))

(deftest merge-heartbeat-summary-test
  (testing "merges action-error summary into mc-state"
    (let [errors {:action-mismatches []
                  :effort-mismatches []
                  :outcome-mismatches []
                  :unplanned []
                  :summary {:planned 4 :taken 3 :not-taken 1
                            :unplanned 1 :effort-error-sum 2.0}}
          merged (obs/merge-heartbeat-summary sample-mc-state errors)]
      (is (== 0.25 (:effort-prediction-error merged)))  ; 2.0 / (2*4)
      (is (== 0.75 (:bid-completion-rate merged)))       ; 3/4
      (is (== 0.25 (:unplanned-work-ratio merged)))))    ; 1/(3+1)
  (testing "returns mc-state unchanged when nil"
    (is (= sample-mc-state (obs/merge-heartbeat-summary sample-mc-state nil))))
  (testing "returns mc-state unchanged when no summary"
    (is (= sample-mc-state (obs/merge-heartbeat-summary sample-mc-state {})))))

(deftest custom-priors
  (testing "custom priors change normalization"
    (let [loose (obs/observe sample-mc-state
                             {:evidence-per-day-cap 100.0
                              :max-chain-cap 20.0
                              :gap-cap 200.0
                              :spinoff-cap 60.0
                              :review-age-cap 30.0})
          tight (obs/observe sample-mc-state obs/default-priors)]
      ;; With looser caps, values should be smaller (less surprising)
      (is (< (:evidence-velocity loose) (:evidence-velocity tight)))
      (is (< (:gap-count loose) (:gap-count tight))))))
