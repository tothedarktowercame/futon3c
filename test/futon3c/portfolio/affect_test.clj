(ns futon3c.portfolio.affect-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.portfolio.affect :as affect]))

;; =============================================================================
;; Mode transition tests
;; =============================================================================

(deftest candidate-mode-from-build
  (testing "BUILD → MAINTAIN when high coverage, low stalls"
    (is (= :MAINTAIN (affect/candidate-mode :BUILD
                                            {:coverage-pct 0.8
                                             :stall-count 0.1
                                             :spinoff-pressure 0.1
                                             :review-age 0.2
                                             :gap-count 0.2
                                             :coverage-trajectory 0.5}))))
  (testing "BUILD → CONSOLIDATE when high spinoff pressure"
    (is (= :CONSOLIDATE (affect/candidate-mode :BUILD
                                               {:coverage-pct 0.5
                                                :stall-count 0.3
                                                :spinoff-pressure 0.7
                                                :review-age 0.3
                                                :gap-count 0.3
                                                :coverage-trajectory 0.5}))))
  (testing "BUILD → BUILD when conditions don't match"
    (is (= :BUILD (affect/candidate-mode :BUILD
                                         {:coverage-pct 0.5
                                          :stall-count 0.3
                                          :spinoff-pressure 0.2
                                          :review-age 0.3
                                          :gap-count 0.3
                                          :coverage-trajectory 0.5})))))

(deftest candidate-mode-from-maintain
  (testing "MAINTAIN → BUILD when many gaps"
    (is (= :BUILD (affect/candidate-mode :MAINTAIN
                                         {:gap-count 0.6
                                          :coverage-trajectory 0.5
                                          :spinoff-pressure 0.1
                                          :review-age 0.2}))))
  (testing "MAINTAIN → CONSOLIDATE when review overdue"
    (is (= :CONSOLIDATE (affect/candidate-mode :MAINTAIN
                                               {:gap-count 0.2
                                                :coverage-trajectory 0.5
                                                :spinoff-pressure 0.3
                                                :review-age 0.9})))))

(deftest candidate-mode-from-consolidate
  (testing "CONSOLIDATE → BUILD when gaps and spinoff resolved"
    (is (= :BUILD (affect/candidate-mode :CONSOLIDATE
                                         {:gap-count 0.6
                                          :spinoff-pressure 0.2
                                          :coverage-pct 0.5
                                          :review-age 0.5}))))
  (testing "CONSOLIDATE → MAINTAIN when coverage high and review fresh"
    (is (= :MAINTAIN (affect/candidate-mode :CONSOLIDATE
                                            {:gap-count 0.3
                                             :spinoff-pressure 0.4
                                             :coverage-pct 0.8
                                             :review-age 0.2})))))

;; =============================================================================
;; Hysteresis tests
;; =============================================================================

(deftest next-mode-hysteresis
  (let [obs-trigger {:coverage-pct 0.8 :stall-count 0.1
                     :spinoff-pressure 0.1 :review-age 0.2
                     :gap-count 0.2 :coverage-trajectory 0.5}]
    (testing "first observation records pending, doesn't transition"
      (let [result (affect/next-mode :BUILD obs-trigger nil)]
        (is (= :BUILD (:mode result)))
        (is (= :MAINTAIN (get-in result [:pending :candidate])))
        (is (= 1 (get-in result [:pending :count])))))
    (testing "second consecutive observation triggers transition"
      (let [pending {:candidate :MAINTAIN :count 1}
            result (affect/next-mode :BUILD obs-trigger pending)]
        (is (= :MAINTAIN (:mode result)))
        (is (nil? (:pending result)))))))

(deftest next-mode-clears-pending-when-no-transition
  (let [obs-stable {:coverage-pct 0.5 :stall-count 0.3
                    :spinoff-pressure 0.2 :review-age 0.3
                    :gap-count 0.3 :coverage-trajectory 0.5}
        pending {:candidate :MAINTAIN :count 1}
        result (affect/next-mode :BUILD obs-stable pending)]
    (testing "pending cleared when transition pressure disappears"
      (is (= :BUILD (:mode result)))
      (is (nil? (:pending result))))))

(deftest next-mode-resets-on-different-candidate
  (let [obs-consolidate {:coverage-pct 0.5 :stall-count 0.3
                         :spinoff-pressure 0.7 :review-age 0.3
                         :gap-count 0.3 :coverage-trajectory 0.5}
        pending {:candidate :MAINTAIN :count 1}
        result (affect/next-mode :BUILD obs-consolidate pending)]
    (testing "pending resets when candidate changes"
      (is (= :BUILD (:mode result)))
      (is (= :CONSOLIDATE (get-in result [:pending :candidate])))
      (is (= 1 (get-in result [:pending :count]))))))

;; =============================================================================
;; Tau coupling tests
;; =============================================================================

(deftest urgency->tau-test
  (testing "low urgency → high tau (explore)"
    (is (> (affect/urgency->tau 0.0) 2.0)))
  (testing "high urgency → low tau (exploit)"
    (is (< (affect/urgency->tau 1.0) 0.5)))
  (testing "mid urgency → mid tau"
    (let [tau (affect/urgency->tau 0.5)]
      (is (< 1.0 tau 2.0)))))

;; =============================================================================
;; Precision modulation tests
;; =============================================================================

(deftest modulate-precisions-test
  (let [base {:Pi-o {:gap-count 0.9 :stall-count 0.7
                     :spinoff-pressure 0.4 :review-age 1.0
                     :coverage-pct 1.0 :coverage-trajectory 0.6}
              :tau 1.0}]
    (testing "BUILD mode boosts gap-count and stall-count"
      (let [result (affect/modulate-precisions base :BUILD 0.5)]
        (is (> (get-in result [:Pi-o :gap-count])
               (get-in base [:Pi-o :gap-count])))
        (is (> (get-in result [:Pi-o :stall-count])
               (get-in base [:Pi-o :stall-count])))))
    (testing "CONSOLIDATE mode boosts spinoff-pressure and review-age"
      (let [result (affect/modulate-precisions base :CONSOLIDATE 0.5)]
        (is (> (get-in result [:Pi-o :spinoff-pressure])
               (get-in base [:Pi-o :spinoff-pressure])))
        (is (> (get-in result [:Pi-o :review-age])
               (get-in base [:Pi-o :review-age])))))
    (testing "tau reflects urgency"
      (let [low-urgency (affect/modulate-precisions base :BUILD 0.1)
            high-urgency (affect/modulate-precisions base :BUILD 0.9)]
        (is (> (:tau low-urgency) (:tau high-urgency)))))))
