(ns futon3c.portfolio.adjacent-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.portfolio.adjacent :as adj]
            [futon3c.portfolio.logic :as logic]))

(def missions
  [{:mission/id "foo" :mission/status :complete}
   {:mission/id "bar" :mission/status :in-progress
    :mission/blocked-by "None"}
   {:mission/id "baz" :mission/status :in-progress
    :mission/blocked-by "M-foo (predecessor)"}
   {:mission/id "qux" :mission/status :blocked
    :mission/blocked-by "M-nope (not done yet)"}
   {:mission/id "free" :mission/status :ready}])

(def mana-available {:mana/available true :mana/pool-balance 500.0})
(def mana-unavailable {:mana/available false})

(deftest adjacent-unblocked-mission
  (let [db (logic/build-db missions mana-available {})
        result (adj/adjacent? (first (filter #(= "bar" (:mission/id %)) missions)) db)]
    (testing "mission with no blockers is adjacent"
      (is (:adjacent? result))
      (is (empty? (:reasons result))))))

(deftest adjacent-resolved-dependency
  (let [db (logic/build-db missions mana-available {})
        result (adj/adjacent? (first (filter #(= "baz" (:mission/id %)) missions)) db)]
    (testing "mission with resolved blocker is adjacent"
      (is (:adjacent? result)))))

(deftest adjacent-unresolved-dependency
  (let [db (logic/build-db missions mana-available {})
        result (adj/adjacent? (first (filter #(= "qux" (:mission/id %)) missions)) db)]
    (testing "mission with unresolved blocker is not adjacent"
      (is (not (:adjacent? result)))
      (is (= [:blocked-by-dependency] (:reasons result))))))

(deftest adjacent-no-mana
  (let [mana {:mana/available true :mana/pool-balance 0.0}
        result (adj/compute-adjacent-set missions mana)]
    (testing "no missions adjacent when mana pool is empty"
      (is (every? #(not (:adjacent? %)) result)))))

(deftest adjacent-mana-system-not-initialized
  (let [db (logic/build-db missions mana-unavailable {})
        result (adj/adjacent? (first (filter #(= "bar" (:mission/id %)) missions)) db)]
    (testing "when mana system not available, defaults to adjacent"
      (is (:adjacent? result)))))

(deftest compute-adjacent-set-filters-complete
  (let [result (adj/compute-adjacent-set missions mana-available)]
    (testing "complete missions are excluded"
      (is (not (some #(= "foo" (:mission/id (:mission %))) result))))
    (testing "non-complete missions are included"
      (is (= 4 (count result))))
    (testing "each result has adjacent? and reasons"
      (doseq [r result]
        (is (contains? r :adjacent?))
        (is (contains? r :reasons))))))

;; =============================================================================
;; Structural query tests
;; =============================================================================

(def chain-missions
  [{:mission/id "a" :mission/status :complete}
   {:mission/id "b" :mission/status :in-progress
    :mission/blocked-by "M-a"}
   {:mission/id "c" :mission/status :blocked
    :mission/blocked-by "M-b"}])

(deftest what-if-complete-test
  (let [unlocked (adj/what-if-complete chain-missions mana-available "a")]
    (testing "completing 'a' unlocks 'b'"
      (is (= ["b"] unlocked)))))

(deftest critical-path-test
  (let [path (adj/critical-path chain-missions mana-available)]
    (testing "c has deepest chain (depth 2)"
      (is (= "c" (:mission (first path))))
      (is (= 2 (:depth (first path)))))))
