(ns futon3c.peripheral.strategic-embedding-experiment-test
  "Phase 6b optional embedding experiment tests (informative only)."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [futon3c.peripheral.strategic-embedding-experiment :as exp]))

(def mini-fixture
  {:patterns
   [{:p4ng/id "p4ng/R1-alpha-gate" :title "alpha gate witness"}
    {:p4ng/id "p4ng/R2-beta-repair" :title "beta repair loop"}]
   :missions
   [{:mission/id "M-alpha" :title "alpha mission gate"}
    {:mission/id "M-beta" :title "beta mission repair"}]
   :train-edges [["p4ng/R1-alpha-gate" "M-alpha"]]
   :held-out-edges [["p4ng/R2-beta-repair" "M-beta"]]})

(deftest determinism-test
  (testing "ranking is deterministic under repeated runs and stable ordering"
    (let [r1 (exp/rank-pairs :generic-vector (:patterns mini-fixture) (:missions mini-fixture) (:train-edges mini-fixture))
          r2 (exp/rank-pairs :generic-vector (:patterns mini-fixture) (:missions mini-fixture) (:train-edges mini-fixture))]
      (is (= r1 r2))
      (is (= r1 (vec (sort-by (juxt (comp - :score) :pattern-id :mission-id) r1)))))))

(deftest metrics-hand-check-test
  (testing "recall@k and precision@k are hand-checkable"
    (let [ranked (exp/rank-pairs :lexical-structural
                                 (:patterns mini-fixture) (:missions mini-fixture)
                                 (:train-edges mini-fixture))
          ;; 4 total pairs; assert exact counts against a held-out set placed
          ;; in known positions of the ranking.
          held #{["p4ng/R2-beta-repair" "M-beta"] ["p4ng/R1-alpha-gate" "M-alpha"]}
          top (map (juxt :pattern-id :mission-id) (take 2 ranked))]
      (is (= 4 (count ranked)))
      (is (= 1.0 (exp/recall-at 2 held ranked)))
      (is (= 1.0 (exp/precision-at 2 held ranked)))
      (is (< 0.666 (exp/precision-at 3 held ranked) 0.667))
      (is (= 0.0 (exp/recall-at 2 #{["p4ng/nope" "M-also-nope"]} ranked)))
      (is (pos? (count top))))))

(deftest proposal-is-not-evidence-test
  (testing "vector proposals can never mint a witnessed or reviewed attachment"
    (let [proposal (exp/as-review-proposal
                    {:pattern-id "p4ng/R5-policy-evaluation"
                     :mission-id "M-aif-policy-conditioned-eig"
                     :score 0.999
                     :proposer :dedicated-vector})]
      (is (= :proposed (:witness-status proposal)))
      (is (= :proposed (:attachment-status proposal)))
      (is (false? (:promotable? proposal)))
      (is (nil? (:supporting-typed-edge proposal)))
      (is (false? (exp/proposal-substitutes-witness? proposal)))
      (is (false? (exp/proposal-substitutes-witness? (assoc proposal :witness-status :independently-witnessed)))))))

(deftest experiment-run-test
  (testing "run-experiment is informative-only and guard-verified"
    (let [result (exp/run-experiment mini-fixture)]
      (is (= :informative-only (:status result)))
      (is (false? (:promote? result)))
      (is (= :none (:architectural-effect result)))
      (is (= #{:lexical-structural :generic-vector :dedicated-vector}
             (set (keys (:proposers result)))))
      (doseq [[_proposer report] (:proposers result)]
        (is (= 4 (:ranking report)))
        (is (pos? (get-in report [:metrics (keyword "recall@5")] 0))))
      (let [guard (:bypass-guard result)]
        (is (false? (:promotable? guard)))
        (is (false? (:substitutes-witness? guard)))
        (is (true? (:witness-status-remains-proposed? guard)))))))

(deftest invalid-fixture-test
  (testing "missing parts fail closed with a typed reason"
    (is (= :invalid-fixture (:status (exp/run-experiment {}))))
    (is (contains? (set (:problems (exp/run-experiment {}))) :no-patterns))))

(deftest frozen-fixture-test
  (testing "the frozen Phase 6b fixture parses and runs deterministically"
    (let [fixture (edn/read-string (slurp "holes/labs/M-typed-memories/phase6b-embedding-experiment.edn"))
          r1 (exp/run-experiment fixture)
          r2 (exp/run-experiment fixture)]
      (is (= "phase6b-embedding-experiment-v1-20260830" (:freeze-id fixture)))
      (is (= 5 (:held-out-edge-count r1)))
      (is (= 6 (:train-edge-count r1)))
      (is (= r1 r2))
      (is (false? (:promote? r1))))))
