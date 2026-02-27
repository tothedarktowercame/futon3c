(ns futon3c.portfolio.policy-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.portfolio.observe :as obs]
            [futon3c.portfolio.perceive :as perc]
            [futon3c.portfolio.policy :as pol]))

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
   :gap-count 0.6      ; high gaps → should favor :work-on
   :stall-count 0.3
   :spinoff-pressure 0.2
   :pattern-reuse 0.1
   :review-age 0.3})

(def test-mu perc/default-mu)
(def test-precision perc/default-precision)

(def test-adjacent
  [{:adjacent? true :mission {:mission/id "m1"}}
   {:adjacent? true :mission {:mission/id "m2"}}
   {:adjacent? false :mission {:mission/id "m3"} :reasons [:blocked-by-dependency]}])

;; =============================================================================
;; EFE tests
;; =============================================================================

(deftest expected-free-energy-produces-terms
  (let [result (pol/expected-free-energy
                :work-on test-observation (:sens test-mu)
                test-precision test-adjacent {} pol/default-lambdas)]
    (testing "result has required keys"
      (is (contains? result :action))
      (is (contains? result :G))
      (is (contains? result :terms)))
    (testing "terms has 4 components"
      (is (= #{:pragmatic :epistemic :upvote :effort} (set (keys (:terms result))))))
    (testing "G is a number"
      (is (number? (:G result))))))

(deftest efe-review-high-when-review-age-high
  (let [stale-obs (assoc test-observation :review-age 0.9)
        review-efe (pol/expected-free-energy
                    :review stale-obs (:sens test-mu)
                    test-precision test-adjacent {} pol/default-lambdas)
        work-efe (pol/expected-free-energy
                  :work-on stale-obs (:sens test-mu)
                  test-precision test-adjacent {} pol/default-lambdas)]
    (testing "review has lower G (better) when review-age is high"
      ;; Review should be more valuable when observations are stale
      (is (< (:G review-efe) 0) "review G should be negative (good)"))))

(deftest efe-wait-preferred-when-calm
  (let [calm-obs (zipmap obs/channel-keys (repeat 0.5))
        calm-mu (assoc-in test-mu [:sens] calm-obs)
        wait-efe (pol/expected-free-energy
                  :wait calm-obs (:sens calm-mu)
                  test-precision [] {} pol/default-lambdas)
        work-efe (pol/expected-free-energy
                  :work-on calm-obs (:sens calm-mu)
                  test-precision [] {} pol/default-lambdas)]
    (testing "wait has lower G when everything is calm"
      ;; When prediction matches observation, wait should be preferred
      (is (< (:G wait-efe) (:G work-efe))))))

;; =============================================================================
;; Softmax tests
;; =============================================================================

(deftest softmax-basic
  (let [probs (pol/softmax [1.0 2.0 3.0])]
    (testing "probabilities sum to 1"
      (is (< (Math/abs (- 1.0 (reduce + probs))) 1e-10)))
    (testing "higher logit → higher probability"
      (is (< (nth probs 0) (nth probs 1) (nth probs 2))))))

(deftest softmax-equal-logits
  (let [probs (pol/softmax [1.0 1.0 1.0])]
    (testing "equal logits → equal probabilities"
      (is (< (Math/abs (- (nth probs 0) (nth probs 1))) 1e-10)))))

;; =============================================================================
;; Policy selection tests
;; =============================================================================

(deftest choose-action-returns-complete-result
  (let [result (pol/choose-action test-mu test-precision test-observation
                                  test-adjacent {})]
    (testing "result has required keys"
      (is (contains? result :action))
      (is (contains? result :policies))
      (is (contains? result :tau))
      (is (contains? result :abstain?)))
    (testing "policies has 5 entries (one per action)"
      (is (= 5 (count (:policies result)))))
    (testing "probabilities sum to ~1"
      (let [total (reduce + (map :probability (:policies result)))]
        (is (< (Math/abs (- 1.0 total)) 1e-10))))
    (testing "action is one of the valid actions"
      (is (contains? (set (:arena/actions pol/portfolio-arena))
                     (:action result))))))

(deftest choose-action-abstains-at-low-tau
  (let [low-tau-prec (assoc test-precision :tau 0.3)
        result (pol/choose-action test-mu low-tau-prec test-observation
                                  test-adjacent {})]
    (testing "abstains when tau < 0.55"
      (is (:abstain? result))
      (is (= :wait (:action result))))))

(deftest choose-action-deterministic-without-rng
  (let [r1 (pol/choose-action test-mu test-precision test-observation
                              test-adjacent {})
        r2 (pol/choose-action test-mu test-precision test-observation
                              test-adjacent {})]
    (testing "deterministic (argmax) when no rng provided"
      (is (= (:action r1) (:action r2))))))

(deftest choose-action-stochastic-with-rng
  (let [rng (java.util.Random. 42)
        result (pol/choose-action test-mu test-precision test-observation
                                  test-adjacent {:rng rng})]
    (testing "selects valid action with rng"
      (is (contains? (set (:arena/actions pol/portfolio-arena))
                     (:action result))))))
