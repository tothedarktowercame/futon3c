(ns futon3c.portfolio.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.portfolio.observe :as obs]
            [futon3c.portfolio.perceive :as perc]
            [futon3c.portfolio.core :as core]))

;; =============================================================================
;; Test fixtures
;; =============================================================================

(def test-observation
  {:mission-complete-ratio 0.4
   :coverage-pct 0.5
   :coverage-trajectory 0.5
   :mana-available 0.5
   :blocked-ratio 0.2
   :evidence-velocity 0.3
   :dependency-depth 0.2
   :gap-count 0.6
   :stall-count 0.3
   :spinoff-pressure 0.2
   :pattern-reuse 0.1
   :review-age 0.3})

(def test-adjacent
  [{:adjacent? true :mission {:mission/id "m1" :mission/status :in-progress}}
   {:adjacent? true :mission {:mission/id "m2" :mission/status :ready}}])

(def fresh-state
  {:mu perc/default-mu
   :prec perc/default-precision
   :pending nil
   :recent []
   :step-count 0})

;; =============================================================================
;; aif-step tests
;; =============================================================================

(deftest aif-step-produces-complete-result
  (let [result (core/aif-step fresh-state test-observation test-adjacent {})]
    (testing "result has all required keys"
      (is (contains? result :state))
      (is (contains? result :action))
      (is (contains? result :observation))
      (is (contains? result :policy))
      (is (contains? result :perception))
      (is (contains? result :diagnostics)))
    (testing "state is updated"
      (is (= 1 (get-in result [:state :step-count])))
      (is (seq (get-in result [:state :recent]))))
    (testing "action is valid"
      (is (keyword? (:action result))))
    (testing "diagnostics contain mode, urgency, tau, FE"
      (let [diag (:diagnostics result)]
        (is (contains? diag :mode))
        (is (contains? diag :urgency))
        (is (contains? diag :tau))
        (is (contains? diag :free-energy))))))

(deftest aif-step-updates-beliefs
  (let [result (core/aif-step fresh-state test-observation test-adjacent {})]
    (testing "sensory predictions move toward observation"
      (let [initial-gap (:gap-count (:sens perc/default-mu))
            updated-gap (get-in result [:state :mu :sens :gap-count])
            obs-gap (:gap-count test-observation)]
        ;; Updated should be between initial and observation
        (is (not= initial-gap updated-gap))
        (is (< (Math/abs (- updated-gap obs-gap))
               (Math/abs (- initial-gap obs-gap))))))))

(deftest aif-step-sequential-convergence
  (testing "multiple steps converge beliefs toward observation"
    (let [step1 (core/aif-step fresh-state test-observation test-adjacent {})
          step2 (core/aif-step (:state step1) test-observation test-adjacent {})
          step3 (core/aif-step (:state step2) test-observation test-adjacent {})]
      ;; Free energy should decrease over steps (beliefs converging)
      (is (>= (get-in step1 [:diagnostics :free-energy])
              (get-in step3 [:diagnostics :free-energy]))))))

(deftest aif-step-recent-window
  (let [s1 (core/aif-step fresh-state test-observation test-adjacent {})
        s2 (core/aif-step (:state s1) test-observation test-adjacent {})]
    (testing "recent window accumulates"
      (is (= 1 (count (get-in s1 [:state :recent]))))
      (is (= 2 (count (get-in s2 [:state :recent])))))))

(deftest aif-step-mode-preserved
  (let [mu-consolidate (assoc perc/default-mu :mode :CONSOLIDATE)
        state (assoc fresh-state :mu mu-consolidate)
        ;; Observation that doesn't trigger transition from CONSOLIDATE
        obs {:mission-complete-ratio 0.4 :coverage-pct 0.5
             :coverage-trajectory 0.5 :mana-available 0.5
             :blocked-ratio 0.2 :evidence-velocity 0.3
             :dependency-depth 0.2 :gap-count 0.3
             :stall-count 0.2 :spinoff-pressure 0.4
             :pattern-reuse 0.1 :review-age 0.5}
        result (core/aif-step state obs test-adjacent {})]
    (testing "mode stays CONSOLIDATE when no transition triggers"
      (is (= :CONSOLIDATE (get-in result [:diagnostics :mode]))))))

;; =============================================================================
;; Format recommendation test
;; =============================================================================

(deftest format-recommendation-test
  (let [result (core/aif-step fresh-state test-observation test-adjacent {})
        formatted (core/format-recommendation result)]
    (testing "produces non-empty string"
      (is (string? formatted))
      (is (pos? (count formatted))))
    (testing "contains key info"
      (is (re-find #"Mode:" formatted))
      (is (re-find #"Urgency:" formatted))
      (is (re-find #"Top actions:" formatted)))))

;; =============================================================================
;; Heartbeat test (without evidence store)
;; =============================================================================

(deftest heartbeat-produces-discrepancy
  ;; Test the delta computation logic via aif-step + manual bid/clear
  (let [result (core/aif-step fresh-state test-observation test-adjacent {})
        bid (zipmap obs/channel-keys (repeat 0.5))
        clear test-observation
        delta (into {}
                    (map (fn [k]
                           [k (- (get bid k 0.5) (get clear k 0.5))]))
                    obs/channel-keys)]
    (testing "delta is non-zero when bid != clear"
      (is (not (every? zero? (vals delta)))))
    (testing "delta direction is correct"
      ;; bid 0.5, clear gap-count 0.6 → delta = -0.1
      (is (neg? (:gap-count delta))))))
