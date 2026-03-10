(ns futon3c.peripheral.proof-logic-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.peripheral.proof-logic :as logic]))

;; =============================================================================
;; Clean state fixtures
;; =============================================================================

(def ^:private clean-ledger
  "A well-formed ledger: three items with valid deps and statuses."
  {"L1" {:item/status :proved
         :item/evidence-type :analytical
         :item/depends-on #{"L2" "L3"}
         :item/unlocks #{}}
   "L2" {:item/status :proved
         :item/evidence-type :analytical
         :item/depends-on #{}
         :item/unlocks #{"L1"}}
   "L3" {:item/status :proved
         :item/evidence-type :analytical
         :item/depends-on #{}
         :item/unlocks #{"L1"}}})

(def ^:private clean-cycles
  "A well-formed cycle at :validate phase with required outputs for prior phases."
  [{:cycle/id "C1"
    :cycle/phase :validate
    :cycle/blocker-id "L1"
    :cycle/phase-data {:observe {:blocker-id "L1"}
                       :propose {:approach "direct"}
                       :execute {:artifacts ["a.clj"]}}}])

(def ^:private clean-proof-state
  {:proof/ledger clean-ledger
   :proof/cycles clean-cycles
   :proof/current-mode :FALSIFY
   :proof/falsify-completed? false
   :proof/failed-routes []})

;; =============================================================================
;; Clean state — all invariants hold
;; =============================================================================

(deftest query-violations-clean-when-proof-state-well-formed
  (testing "No violations in a well-formed proof state"
    (let [v (logic/check-proof-state clean-proof-state)]
      (is (empty? (:asymmetric-edges v))
          "DAG edges are symmetric")
      (is (empty? (:dangling-refs v))
          "No dangling references")
      (is (empty? (:invalid-statuses v))
          "All statuses are valid")
      (is (empty? (:proved-without-analytical v))
          "No SR-5 violations")
      (is (empty? (:proved-with-unproved-deps v))
          "All proved items have proved deps")
      (is (empty? (:missing-phase-outputs v))
          "All past phases have required outputs")
      (is (empty? (:mode-violations v))
          "Mode gating is respected")
      (is (empty? (:routes-without-obstruction v))
          "No failed routes to check")
      (is (not (logic/violations? v))
          "violations? returns false for clean state"))))

;; =============================================================================
;; DAG integrity violations
;; =============================================================================

(deftest query-violations-catches-asymmetric-edges
  (testing "A unlocks B but B does not depend-on A"
    (let [ledger {"A" {:item/status :open
                       :item/unlocks #{"B"}
                       :item/depends-on #{}}
                  "B" {:item/status :open
                       :item/unlocks #{}
                       :item/depends-on #{}}}  ; missing dep on A
          v (logic/check-proof-state {:proof/ledger ledger})]
      (is (seq (:asymmetric-edges v)))
      (is (some (fn [[a b dir]] (and (= "A" a) (= "B" b) (= :unlocks-without-dep dir)))
                (:asymmetric-edges v))))))

(deftest query-violations-catches-dangling-refs
  (testing "Item depends on non-existent item"
    (let [ledger {"A" {:item/status :open
                       :item/depends-on #{"GHOST"}
                       :item/unlocks #{}}}
          v (logic/check-proof-state {:proof/ledger ledger})]
      (is (seq (:dangling-refs v)))
      (is (some (fn [[ref missing dir]]
                  (and (= "A" ref) (= "GHOST" missing) (= :depends-on dir)))
                (:dangling-refs v))))))

;; =============================================================================
;; Status discipline violations
;; =============================================================================

(deftest query-violations-catches-invalid-status
  (testing "Item with unrecognized status"
    (let [ledger {"A" {:item/status :banana}}
          v (logic/check-proof-state {:proof/ledger ledger})]
      (is (seq (:invalid-statuses v)))
      (is (some (fn [[iid s]] (and (= "A" iid) (= :banana s)))
                (:invalid-statuses v))))))

(deftest query-violations-catches-sr5-violation
  (testing ":proved with only :numerical evidence (SR-5)"
    (let [ledger {"A" {:item/status :proved
                       :item/evidence-type :numerical}}
          v (logic/check-proof-state {:proof/ledger ledger})]
      (is (seq (:proved-without-analytical v)))
      (is (some #{"A"} (:proved-without-analytical v))))))

(deftest query-violations-catches-proved-with-unproved-deps
  (testing ":proved item with :open dependency"
    (let [ledger {"A" {:item/status :proved
                       :item/depends-on #{"B"}}
                  "B" {:item/status :open}}
          v (logic/check-proof-state {:proof/ledger ledger})]
      (is (seq (:proved-with-unproved-deps v)))
      (is (some #(and (= "A" (:item %)) (= "B" (:dep %)) (= :open (:dep-status %)))
                (:proved-with-unproved-deps v))))))

(deftest proved-with-proved-deps-is-clean
  (testing ":proved item with :proved dependency is not flagged"
    (let [ledger {"A" {:item/status :proved
                       :item/depends-on #{"B"}}
                  "B" {:item/status :proved}}
          v (logic/check-proof-state {:proof/ledger ledger})]
      (is (empty? (:proved-with-unproved-deps v))))))

;; =============================================================================
;; Cycle phase integrity
;; =============================================================================

(deftest query-violations-catches-missing-phase-outputs
  (testing "Cycle at :execute but missing :propose outputs"
    (let [cycles [{:cycle/id "C1"
                   :cycle/phase :execute
                   :cycle/phase-data {:observe {:blocker-id "X"}}}]
          v (logic/check-proof-state {:proof/cycles cycles})]
      (is (seq (:missing-phase-outputs v)))
      (is (some #(and (= "C1" (:cycle %))
                      (= :propose (:phase %))
                      (contains? (:missing %) :approach))
                (:missing-phase-outputs v))))))

;; =============================================================================
;; Mode gating violations
;; =============================================================================

(deftest query-violations-catches-construct-without-falsify
  (testing "CONSTRUCT mode without falsify-completed"
    (let [v (logic/check-proof-state
              {:proof/current-mode :CONSTRUCT
               :proof/falsify-completed? false})]
      (is (seq (:mode-violations v)))
      (is (= [:construct-without-falsify] (:mode-violations v))))))

(deftest mode-gating-clean-when-falsified
  (testing "CONSTRUCT mode with falsify-completed is clean"
    (let [v (logic/check-proof-state
              {:proof/current-mode :CONSTRUCT
               :proof/falsify-completed? true})]
      (is (empty? (:mode-violations v))))))

(deftest mode-gating-clean-for-falsify-mode
  (testing "FALSIFY mode doesn't need falsify-completed"
    (let [v (logic/check-proof-state
              {:proof/current-mode :FALSIFY
               :proof/falsify-completed? false})]
      (is (empty? (:mode-violations v))))))

;; =============================================================================
;; Failed route integrity
;; =============================================================================

(deftest query-violations-catches-route-without-obstruction
  (testing "Failed route missing structural obstruction (SR-8)"
    (let [routes [{:route/id "R1" :route/blocker-id "B1"}]
          v (logic/check-proof-state {:proof/failed-routes routes})]
      (is (seq (:routes-without-obstruction v)))
      (is (some #{"R1"} (:routes-without-obstruction v))))))

(deftest route-with-obstruction-is-clean
  (testing "Failed route with structural obstruction is not flagged"
    (let [routes [{:route/id "R1"
                   :route/blocker-id "B1"
                   :route/structural-obstruction "blocked by X"}]
          v (logic/check-proof-state {:proof/failed-routes routes})]
      (is (empty? (:routes-without-obstruction v))))))
