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

;; =============================================================================
;; Coverage & tension relations
;; =============================================================================

(def test-devmaps
  [{:devmap/id :social-exotype
    :devmap/state :active
    :devmap/components [{:component/id :S-presence}
                        {:component/id :S-dispatch}
                        {:component/id :S-invoke}]}
   {:devmap/id :peripheral-gauntlet
    :devmap/state :argue
    :devmap/components [{:component/id :C-arena}
                        {:component/id :C-bridge}
                        {:component/id :C-mode}]}])

(def test-coverage
  [{:coverage/devmap-id :social-exotype
    :coverage/covered [:S-presence :S-dispatch]
    :coverage/uncovered [:S-invoke]
    :coverage/by-component {:S-presence #{"transport-adapters"}
                            :S-dispatch #{"agency-refactor"}}}
   {:coverage/devmap-id :peripheral-gauntlet
    :coverage/covered []
    :coverage/uncovered [:C-arena :C-bridge :C-mode]
    :coverage/by-component {}}])

(deftest build-coverage-db-test
  (let [base (logic/build-db test-missions test-mana test-opts)
        db (logic/build-coverage-db base test-devmaps test-coverage {})]
    (testing "devmap facts populated"
      (is (= 2 (count (clojure.core.logic.pldb/with-db db
                         (clojure.core.logic/run* [dm]
                           (logic/devmapo dm)))))))
    (testing "component facts populated"
      (is (= 6 (count (clojure.core.logic.pldb/with-db db
                         (clojure.core.logic/run* [q]
                           (clojure.core.logic/fresh [dm c]
                             (logic/componento dm c)
                             (clojure.core.logic/== q [dm c]))))))))))

(deftest query-uncovered-components-test
  (let [base (logic/build-db test-missions test-mana test-opts)
        db (logic/build-coverage-db base test-devmaps test-coverage {})]
    (testing "uncovered components found"
      (let [uncovered (logic/query-uncovered-components db)]
        ;; S-invoke has no coverage, C-arena/C-bridge/C-mode have no coverage
        (is (= 4 (count uncovered)))
        (is (some (fn [[dm c]] (and (= dm :peripheral-gauntlet) (= c :C-arena)))
                  uncovered))
        (is (some (fn [[dm c]] (and (= dm :social-exotype) (= c :S-invoke)))
                  uncovered))))
    (testing "covered components not in uncovered"
      (let [uncovered-comps (set (map second (logic/query-uncovered-components db)))]
        (is (not (contains? uncovered-comps :S-presence)))
        (is (not (contains? uncovered-comps :S-dispatch)))))))

(deftest query-derived-tensions-test
  (let [base (logic/build-db test-missions test-mana test-opts)
        db (logic/build-coverage-db base test-devmaps test-coverage {})
        result (logic/query-derived-tensions db)]
    (testing "derives correct tension count"
      (is (= 4 (:derived-count result))))
    (testing "all derived tensions are uncovered-component type"
      (is (every? #(= :uncovered-component (:tension/type %))
                  (:derived result))))
    (testing "stored count is 0 when no hyperedges loaded"
      (is (= 0 (:stored-count result))))))

(deftest query-unported-invariants-test
  (let [base (logic/build-db test-missions test-mana test-opts)
        db (-> base
               ;; Add some invariant facts
               (clojure.core.logic.pldb/db-fact logic/invarianto "entity-lifecycle" "futon1")
               (clojure.core.logic.pldb/db-fact logic/invarianto "referential-integrity" "futon1")
               (clojure.core.logic.pldb/db-fact logic/invarianto "penholder-auth" "futon1")
               ;; Only penholder-auth was ported
               (clojure.core.logic.pldb/db-fact logic/implementedo "penholder-auth" "futon1a"))]
    (testing "finds unported invariants"
      (let [unported (logic/query-unported-invariants db "futon1" "futon1a")]
        (is (= 2 (count unported)))
        (is (= #{"entity-lifecycle" "referential-integrity"} (set unported)))))
    (testing "consistency check detects drift"
      (let [db2 (logic/build-coverage-db db test-devmaps test-coverage {})
            result (logic/query-consistency db2)]
        (is (= :drifted (:invariant-drift result)))
        (is (= 2 (count (:unported-invariants result))))))))
