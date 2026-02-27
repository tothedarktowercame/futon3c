(ns futon3c.portfolio.logic-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.portfolio.logic :as logic]))

;; =============================================================================
;; Test data
;; =============================================================================

(def test-missions
  [{:mission/id "alpha" :mission/status :complete}
   {:mission/id "beta" :mission/status :in-progress
    :mission/blocked-by "None"}
   {:mission/id "gamma" :mission/status :in-progress
    :mission/blocked-by "M-alpha (predecessor)"}
   {:mission/id "delta" :mission/status :blocked
    :mission/blocked-by "M-beta"}
   {:mission/id "epsilon" :mission/status :blocked
    :mission/blocked-by "M-beta; M-gamma"}
   {:mission/id "zeta" :mission/status :ready}
   {:mission/id "eta" :mission/status :blocked
    :mission/blocked-by "M-nope"}])

(def test-mana {:mana/available true :mana/pool-balance 100.0})
(def test-mana-empty {:mana/available true :mana/pool-balance 0.0})

(def test-opts
  {:evidence-counts {"alpha" 10 "beta" 5 "gamma" 3}
   :patterns-used {"beta" ["mode-gate" "learn-as-you-go"]
                   "gamma" ["mode-gate" "expected-free-energy-scorecard"]}})

;; =============================================================================
;; Database construction
;; =============================================================================

(deftest build-db-populates-facts
  (let [db (logic/build-db test-missions test-mana test-opts)
        all (logic/query-all-missions db)]
    (testing "all missions present"
      (is (= 7 (count all))))
    (testing "statuses correct"
      (let [by-id (into {} all)]
        (is (= :complete (get by-id "alpha")))
        (is (= :in-progress (get by-id "beta")))
        (is (= :blocked (get by-id "delta")))))))

;; =============================================================================
;; Adjacency queries
;; =============================================================================

(deftest query-adjacent-basic
  (let [db (logic/build-db test-missions test-mana test-opts)
        adj (set (logic/query-adjacent db))]
    (testing "beta is adjacent (no blockers, not complete)"
      (is (contains? adj "beta")))
    (testing "gamma is adjacent (blocker alpha is complete)"
      (is (contains? adj "gamma")))
    (testing "zeta is adjacent (ready, no blockers)"
      (is (contains? adj "zeta")))
    (testing "alpha is not adjacent (already complete)"
      (is (not (contains? adj "alpha"))))
    (testing "delta is not adjacent (blocked by beta which isn't complete)"
      (is (not (contains? adj "delta"))))
    (testing "eta is not adjacent (blocked by nope which doesn't exist as complete)"
      (is (not (contains? adj "eta"))))))

(deftest query-adjacent-no-mana
  (let [db (logic/build-db test-missions test-mana-empty {})
        adj (logic/query-adjacent db)]
    (testing "no missions adjacent when mana pool is empty"
      (is (empty? adj)))))

;; =============================================================================
;; Structural queries
;; =============================================================================

(deftest query-what-if-complete-test
  (let [db (logic/build-db test-missions test-mana test-opts)
        unlocked (set (logic/query-what-if-complete db "beta"))]
    (testing "completing beta unlocks delta"
      (is (contains? unlocked "delta")))
    (testing "completing beta alone doesn't unlock epsilon (also needs gamma)"
      (is (not (contains? unlocked "epsilon"))))))

(deftest query-unblocked-by-test
  (let [db (logic/build-db test-missions test-mana test-opts)
        unblocked (set (logic/query-unblocked-by db "beta"))]
    (testing "beta blocks delta and epsilon"
      (is (contains? unblocked "delta"))
      (is (contains? unblocked "epsilon")))))

(deftest query-dependency-clusters-test
  (let [db (logic/build-db test-missions test-mana test-opts)
        clusters (logic/query-dependency-clusters db)]
    (testing "delta and epsilon are co-blocked by beta"
      (is (some (fn [[m1 m2 blocker]]
                  (and (= blocker "beta")
                       (= #{"delta" "epsilon"} #{m1 m2})))
                clusters)))))

(deftest query-blocked-missions-test
  (let [db (logic/build-db test-missions test-mana test-opts)
        blocked (logic/query-blocked-missions db)]
    (testing "returns blocker pairs"
      (is (some (fn [[mid blocker]] (and (= mid "delta") (= blocker "beta")))
                blocked)))
    (testing "epsilon has two blockers"
      (let [eps-blockers (set (map second (filter #(= "epsilon" (first %)) blocked)))]
        (is (= #{"beta" "gamma"} eps-blockers))))))

(deftest query-chain-depth-test
  (let [db (logic/build-db test-missions test-mana test-opts)]
    (testing "alpha has depth 0 (no deps)"
      (is (= 0 (logic/query-chain-depth db "alpha"))))
    (testing "gamma has depth 1 (blocked by alpha)"
      (is (= 1 (logic/query-chain-depth db "gamma"))))
    (testing "epsilon has depth 2 (blocked by beta + gamma, gamma blocked by alpha)"
      (is (= 2 (logic/query-chain-depth db "epsilon"))))))

(deftest query-critical-path-test
  (let [db (logic/build-db test-missions test-mana test-opts)
        path (logic/query-critical-path db)]
    (testing "critical path is non-empty"
      (is (seq path)))
    (testing "deepest mission has depth >= 1"
      (is (>= (:depth (first path)) 1)))))

(deftest query-pattern-co-occurrence-test
  (let [db (logic/build-db test-missions test-mana test-opts)
        co (logic/query-pattern-co-occurrence db)]
    (testing "mode-gate appears in both beta and gamma"
      (is (some #(= "mode-gate" (:pattern %)) co))
      (let [mg (first (filter #(= "mode-gate" (:pattern %)) co))]
        (is (= 2 (:count mg)))
        (is (= #{"beta" "gamma"} (set (:missions mg))))))))

;; =============================================================================
;; Structural summary
;; =============================================================================

(deftest structural-summary-test
  (let [db (logic/build-db test-missions test-mana test-opts)
        summary (logic/structural-summary db)]
    (testing "summary has all keys"
      (is (contains? summary :total))
      (is (contains? summary :by-status))
      (is (contains? summary :adjacent))
      (is (contains? summary :adjacent-count))
      (is (contains? summary :critical-path)))
    (testing "total matches"
      (is (= 7 (:total summary))))
    (testing "adjacent count is positive"
      (is (pos? (:adjacent-count summary))))))
