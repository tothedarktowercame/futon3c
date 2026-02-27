(ns futon3c.portfolio.perceive-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.portfolio.observe :as obs]
            [futon3c.portfolio.perceive :as perc]))

;; =============================================================================
;; Test fixtures
;; =============================================================================

(def test-observation
  "An observation that differs from default mu (all 0.5)."
  {:mission-complete-ratio 0.7
   :coverage-pct 0.8
   :coverage-trajectory 0.6
   :mana-available 0.3
   :blocked-ratio 0.1
   :evidence-velocity 0.4
   :dependency-depth 0.2
   :gap-count 0.3
   :stall-count 0.1
   :spinoff-pressure 0.2
   :pattern-reuse 0.1
   :review-age 0.4})

;; =============================================================================
;; Prediction error tests
;; =============================================================================

(deftest compute-errors-basic
  (let [errors (perc/compute-errors perc/default-mu test-observation perc/default-precision)]
    (testing "raw errors are obs - predicted"
      (is (< (Math/abs (- 0.2 (get-in errors [:raw :mission-complete-ratio]))) 1e-10))
      (is (< (Math/abs (- 0.3 (get-in errors [:raw :coverage-pct]))) 1e-10))
      (is (< (Math/abs (- -0.4 (get-in errors [:raw :blocked-ratio]))) 1e-10)))
    (testing "weighted errors incorporate precision"
      ;; mission-complete-ratio: precision 1.0 × error 0.2 ≈ 0.2
      (is (< (Math/abs (- 0.2 (get-in errors [:weighted :mission-complete-ratio]))) 1e-10))
      ;; coverage-trajectory: precision 0.6 × error 0.1 ≈ 0.06
      (is (< (Math/abs (- 0.06 (get-in errors [:weighted :coverage-trajectory]))) 0.001)))
    (testing "free energy is positive"
      (is (pos? (:free-energy errors))))))

(deftest compute-errors-zero-when-predicted-equals-observed
  (let [obs-at-mu (zipmap obs/channel-keys (repeat 0.5))
        errors (perc/compute-errors perc/default-mu obs-at-mu perc/default-precision)]
    (testing "raw errors are zero when prediction matches observation"
      (doseq [k obs/channel-keys]
        (is (zero? (get-in errors [:raw k])))))
    (testing "free energy is zero"
      (is (zero? (:free-energy errors))))))

;; =============================================================================
;; Belief update tests
;; =============================================================================

(deftest update-sens-moves-toward-observation
  (let [errors (perc/compute-errors perc/default-mu test-observation perc/default-precision)
        new-sens (perc/update-sens (:sens perc/default-mu) (:weighted errors) 0.55)]
    (testing "predictions move toward observations"
      ;; mission-complete-ratio: 0.5 + 0.55 * 0.2 = 0.61 (obs is 0.7)
      (is (> (get new-sens :mission-complete-ratio) 0.5))
      (is (< (get new-sens :mission-complete-ratio) 0.7)))
    (testing "all updated values in [0,1]"
      (doseq [[_k v] new-sens]
        (is (<= 0.0 v 1.0))))))

(deftest update-urgency-responds-to-pressure
  (testing "high gap-count error increases urgency"
    (let [errors {:gap-count 0.5 :stall-count 0.3 :review-age 0.2 :blocked-ratio 0.1
                  :mission-complete-ratio 0.0 :coverage-pct 0.0}
          new-urgency (perc/update-urgency 0.5 errors 0.30)]
      (is (> new-urgency 0.5))))
  (testing "high completion error decreases urgency"
    (let [errors {:gap-count 0.0 :stall-count 0.0 :review-age 0.0 :blocked-ratio 0.0
                  :mission-complete-ratio 0.5 :coverage-pct 0.5}
          new-urgency (perc/update-urgency 0.5 errors 0.30)]
      (is (< new-urgency 0.5)))))

;; =============================================================================
;; Full perceive loop tests
;; =============================================================================

(deftest perceive-converges-toward-observation
  (let [result (perc/perceive perc/default-mu test-observation perc/default-precision)]
    (testing "mu.sens moves closer to observation after 3 micro-steps"
      (let [initial-err (Math/abs (- 0.5 (:mission-complete-ratio test-observation)))
            final-err (Math/abs (- (get-in result [:mu :sens :mission-complete-ratio])
                                   (:mission-complete-ratio test-observation)))]
        (is (< final-err initial-err))))
    (testing "free energy is positive and accumulated over steps"
      (is (pos? (:free-energy result))))
    (testing "trace has 3 entries (one per micro-step)"
      (is (= 3 (count (:trace result)))))
    (testing "free energy decreases over steps (convergence)"
      (let [fes (mapv :free-energy (:trace result))]
        ;; Each step should have lower FE than the previous
        (is (> (first fes) (last fes)))))))

(deftest perceive-preserves-mode-and-focus
  (let [mu (assoc perc/default-mu :mode :CONSOLIDATE :focus "mission-x")
        result (perc/perceive mu test-observation perc/default-precision)]
    (testing "mode is preserved through perceive"
      (is (= :CONSOLIDATE (get-in result [:mu :mode]))))
    (testing "focus is preserved through perceive"
      (is (= "mission-x" (get-in result [:mu :focus]))))))

(deftest perceive-custom-opts
  (let [slow (perc/perceive perc/default-mu test-observation perc/default-precision
                            {:alpha 0.1 :beta 0.05 :micro-steps 1})
        fast (perc/perceive perc/default-mu test-observation perc/default-precision
                            {:alpha 0.9 :beta 0.5 :micro-steps 5})]
    (testing "slower alpha produces less change"
      (let [slow-delta (Math/abs (- (get-in slow [:mu :sens :coverage-pct]) 0.5))
            fast-delta (Math/abs (- (get-in fast [:mu :sens :coverage-pct]) 0.5))]
        (is (< slow-delta fast-delta))))
    (testing "more micro-steps produces more trace entries"
      (is (= 1 (count (:trace slow))))
      (is (= 5 (count (:trace fast)))))))

(deftest perceive-idempotent-at-equilibrium
  (testing "when prediction = observation, mu doesn't change"
    (let [equilibrium-obs (zipmap obs/channel-keys (repeat 0.5))
          result (perc/perceive perc/default-mu equilibrium-obs perc/default-precision)]
      (is (= (:sens perc/default-mu) (get-in result [:mu :sens])))
      (is (zero? (:free-energy result))))))
