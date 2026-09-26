(ns futon3c.peripheral.dynamic-queries-rung4-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.peripheral.dynamic-queries :as dynamic-queries]
            [futon3c.peripheral.dynamic-queries-rung4 :as rung4]))

(defn- battery
  []
  (-> "holes/labs/M-typed-memories/rung4-collapse-battery.edn"
      io/file slurp edn/read-string))

(defn- battery-case
  []
  (first (:cases (battery))))

(defn- run-case
  [case exploration-floor k]
  (rung4/coupled-propagation
   (-> case
       (select-keys
        [:projection :candidate-activation :pattern-activation
         :relation-weights :challenge-memories :query])
       (assoc :exploration-floor exploration-floor
              :k k))))

(deftest battery-preregisters-the-collapse-and-recovery-arms
  (let [{:keys [preregistered]} (battery)]
    (is (= :confirmation-collapse (:battery preregistered)))
    (is (= "M-planted-target" (:planted-target preregistered)))
    (is (= "M-accidental-decoy" (:decoy preregistered)))
    (is (true? (get-in preregistered
                       [:expected-floor-on :target-recovered?])))
    (is (true? (get-in preregistered
                       [:expected-floor-off :target-hidden?])))))

(deftest k-one-floor-off-is-identical-to-rung-one
  (let [case (battery-case)
        rung1
        (dynamic-queries/fixed-typed-ranking
         (:projection case)
         (select-keys case
                      [:candidate-activation :pattern-activation
                       :relation-weights]))
        result (run-case case 0.0 1)]
    (is (= (:typed-ranking rung1) (:typed-ranking result)))
    (is (= (:control-ranking rung1)
           (get-in result [:control-rankings :endpoint])))
    (is (= (:typed-ranking rung1)
           (get-in result [:control-rankings :rung1-typed])))
    (is (= :budget-exhausted (:termination result)))
    (is (= 1 (:steps-executed result)))))

(deftest exploration-floor-prevents-confirmation-collapse
  (let [{:keys [k floor-off floor-on planted-target decoy] :as case}
        (battery-case)
        collapsed (run-case case floor-off k)
        recovered (run-case case floor-on k)
        collapsed-last (last (:per-step-trace collapsed))
        recovered-steps (:per-step-trace recovered)]
    (testing "the ablation concentrates on the accidental decoy"
      (is (= decoy (first (:typed-ranking collapsed))))
      (is (not= planted-target (first (:typed-ranking collapsed))))
      (is (= 1.0
             (get-in collapsed-last
                     [:theta-next :repairs-control])))
      (is (= 0.0
             (get-in collapsed-last
                     [:theta-next :requires-control]))))
    (testing "the explicit floor recovers the planted target"
      (is (= planted-target (first (:typed-ranking recovered))))
      (is (some #(= planted-target (first (:typed-ranking %)))
                recovered-steps))
      (is (every?
           (fn [{:keys [theta theta-next]}]
             (and (every? #(>= (double %) floor-on)
                          (vals theta))
                  (every? #(>= (double %) floor-on)
                          (vals theta-next))))
           recovered-steps)))
    (testing "independently witnessed challenges remain reachable"
      (is (every? :challenge-reachable?
                  (:per-step-trace collapsed)))
      (is (every? :challenge-reachable?
                  recovered-steps))
      (is (every?
           (fn [step]
             (every? #(and (:independently-witnessed? %)
                           (:reachable? %))
                     (:challenge-memory-reachability step)))
           (concat (:per-step-trace collapsed) recovered-steps))))
    (testing "the dark boundary and control arms remain explicit"
      (doseq [result [collapsed recovered]]
        (is (true? (:candidate-set-preserved? result)))
        (is (nil? (:selected-mission result)))
        (is (false? (:live-ordering-changed? result)))
        (is (= :search-heuristic-not-posterior
               (:theta-semantics result)))
        (is (every? #(= (:control-rankings result)
                        (:control-rankings %))
                    (:per-step-trace result)))))))

(deftest propagation-is-deterministic-and-fully-audited
  (let [{:keys [k floor-on] :as case} (battery-case)
        first-run (run-case case floor-on k)
        replay (run-case case floor-on k)
        steps (:per-step-trace first-run)]
    (is (= first-run replay))
    (is (= k (count steps)))
    (is (= :budget-exhausted (:termination first-run)))
    (is (= {:initial k :spent k :remaining 0}
           (:budget first-run)))
    (is (= (range (dec k) -1 -1)
           (map #(get-in % [:step-budget :remaining]) steps)))
    (is (every? vector? (map :contributions steps)))
    (is (every? number? (map :x-entropy steps)))
    (is (every? number? (map :theta-entropy steps)))
    (is (every? pos?
                (map #(get-in % [:path-diversity :distinct-path-count])
                     steps)))
    (is (every? keyword? (keep :termination steps)))))

(deftest exploration-floor-is-explicit-and-feasible
  (let [case (battery-case)
        base (-> case
                 (select-keys
                  [:projection :candidate-activation :pattern-activation
                   :relation-weights :challenge-memories])
                 (assoc :k 2))]
    (testing "the floor may not be omitted"
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"invalid coupled propagation input"
           (rung4/coupled-propagation base))))
    (testing "the floor cannot overfill the relation simplex"
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"invalid coupled propagation input"
           (rung4/coupled-propagation
            (assoc base :exploration-floor 0.6)))))))
