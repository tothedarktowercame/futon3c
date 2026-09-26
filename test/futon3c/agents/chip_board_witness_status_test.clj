(ns futon3c.agents.chip-board-witness-status-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.agents.chip-board :as board]
            [futon3c.agents.inbox-zero-board :as inbox]
            [futon3c.agents.cascade-verifier-board :as cascade]))

(deftest status-names-completed-model-and-unproved-correspondence
  (doseq [[run commit theorem]
          [[(inbox/run {:sweep []} (fn [_] nil))
            "a9a24a3b9e070550ede41ebd10621c5bb9b843f0"
            "DarkTower.WarMachine.ChipBoardWitness.Repaired.repairedHazard"]
           [(cascade/run {:verification-debt []} (fn [_] nil))
            "e407ec20cbedb8667070dee89cd45f27a52207a0"
            "DarkTower.WarMachine.CascadeVerifierBoardWitness.cascadeHasNoActEffects"]]]
    (let [status (get-in run [:certificate :lean/status])]
      (is (= :witnessed-under-declared-registry (:status status)))
      (is (= :lean-model (:scope status)))
      (is (= commit (get-in status [:witness :commit])))
      (is (= theorem (get-in status [:witness :theorem])))
      (is (= :registered-verbs-implement-declared-semantics (:assumption status)))
      (is (= :not-proven (:runtime-correspondence status)))
      (is (= (:verbs/digest run) (get-in status [:registry :digest])))
      (is (false? (get-in status [:registry :semantic-approval?]))))))

(deftest unsafe-registry-is-not-presented-as-proved
  (with-redefs [board/verb-registry (atom @board/verb-registry)]
    (board/register-verb! :smell-backlog
                          (fn [s _ _] {:branch :false
                                       :effects [[:commit {:repo "counterexample-only"}]]
                                       :state' s}))
    (let [run (cascade/run {:verification-debt []} (fn [_] nil))
          status (get-in run [:certificate :lean/status])]
      (is (= :commit (ffirst (get-in run [:trace 0 :effects]))))
      (is (true? (get-in run [:certificate :verified?])))
      (is (= :lean-model (:scope status)))
      (is (= :not-proven (:runtime-correspondence status)))
      (is (false? (get-in status [:registry :semantic-approval?]))))))
