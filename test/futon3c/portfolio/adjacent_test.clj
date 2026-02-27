(ns futon3c.portfolio.adjacent-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.portfolio.adjacent :as adj]))

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
  (let [inv (into {} (map (juxt :mission/id identity)) missions)
        result (adj/adjacent? (get inv "bar") inv mana-available)]
    (testing "mission with no blockers is adjacent"
      (is (:adjacent? result))
      (is (empty? (:reasons result))))))

(deftest adjacent-resolved-dependency
  (let [inv (into {} (map (juxt :mission/id identity)) missions)
        result (adj/adjacent? (get inv "baz") inv mana-available)]
    (testing "mission with resolved blocker is adjacent"
      (is (:adjacent? result)))))

(deftest adjacent-unresolved-dependency
  (let [inv (into {} (map (juxt :mission/id identity)) missions)
        result (adj/adjacent? (get inv "qux") inv mana-available)]
    (testing "mission with unresolved blocker is not adjacent"
      (is (not (:adjacent? result)))
      (is (= [:blocked-by-dependency] (:reasons result))))))

(deftest adjacent-no-mana
  (let [inv (into {} (map (juxt :mission/id identity)) missions)
        mana {:mana/available true :mana/pool-balance 0.0}
        result (adj/adjacent? (get inv "bar") inv mana)]
    (testing "mission without mana is not adjacent"
      (is (not (:adjacent? result)))
      (is (= [:insufficient-mana] (:reasons result))))))

(deftest adjacent-mana-system-not-initialized
  (let [inv (into {} (map (juxt :mission/id identity)) missions)
        result (adj/adjacent? (get inv "bar") inv mana-unavailable)]
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
