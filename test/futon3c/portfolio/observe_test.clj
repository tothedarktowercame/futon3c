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

(deftest observe-produces-12-channels
  (let [o (obs/observe sample-mc-state)]
    (testing "all 12 channels present"
      (is (= 12 (count o)))
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
    (testing "dependency-depth = 2/5 = 0.4"
      (is (== 0.4 (:dependency-depth o))))
    (testing "gap-count = 4/10 = 0.4"
      (is (== 0.4 (:gap-count o))))
    (testing "stall-count = 2/20 = 0.1"
      (is (== 0.1 (:stall-count o))))
    (testing "spinoff-pressure = 1/5 = 0.2"
      (is (== 0.2 (:spinoff-pressure o))))
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
    (testing "vector has 12 elements"
      (is (= 12 (count v))))
    (testing "vector order matches channel-keys"
      (is (= (mapv #(get o %) obs/channel-keys) v)))))

(deftest custom-priors
  (testing "custom priors change normalization"
    (let [loose (obs/observe sample-mc-state
                             {:evidence-per-day-cap 100.0
                              :max-chain-cap 20.0
                              :gap-cap 50.0
                              :spinoff-cap 20.0
                              :review-age-cap 30.0})
          tight (obs/observe sample-mc-state obs/default-priors)]
      ;; With looser caps, values should be smaller (less surprising)
      (is (< (:evidence-velocity loose) (:evidence-velocity tight)))
      (is (< (:gap-count loose) (:gap-count tight))))))
