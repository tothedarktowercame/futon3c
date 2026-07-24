(ns futon3c.peripheral.posthoc-system-verification-test
  "Post-hoc VERIFY witness for the typed-memory → dynamic-query → strategic
   policy replay chain. This test deliberately distinguishes replay mechanism
   verification from live-store readiness."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [futon3c.peripheral.strategic-canary :as canary]
            [futon3c.peripheral.strategic-cascade :as cascade]
            [futon3c.peripheral.strategic-outcomes :as outcomes]
            [futon3c.peripheral.strategic-policies :as policies]
            [futon3c.peripheral.wm-memory :as wm-memory]))

(def fixture-root "holes/labs/M-typed-memories/")

(defn- fixture
  [name]
  (edn/read-string (slurp (str fixture-root name))))

(defn- replay-chain
  []
  (let [{:keys [episodes control-edges]}
        (fixture "phase4-wm-corpus.edn")
        {:keys [cascade dependencies transition-warrants checkpoint]}
        (fixture "phase5-outer-cascade.edn")
        phase6 (fixture "phase6-strategic-outcomes.edn")
        phase7 (fixture "phase7-strategic-policy-shadow.edn")
        phase8 (fixture "phase8-advice-only-canary.edn")
        recall-fn
        (fn [_ endpoint _]
          {:ok true
           :endpoint endpoint
           :memories
           (filterv #(some #{endpoint} (:memory/pattern-ids %))
                    episodes)})
        outer
        (cascade/outer-frontier
         {:cascade cascade
          :dependencies dependencies
          :transition-warrants transition-warrants
          :budget (count (:shown cascade))
          :query-step-fn
          (fn [pattern-id _]
            (wm-memory/dark-candidate-projection
             {:recall-fn recall-fn
              :trace-id "posthoc-system-verification"}
             [pattern-id] control-edges {:limit 10}))})
        checkpoint
        (cascade/checkpoint-ranking
         outer
         (merge
          checkpoint
          {:pattern-activation
           {"p4ng/R5-policy-evaluation" 0.6
            "p4ng/R6-candidate-pattern-action-space" 1.0
            "p4ng/R9-independent-witness" 0.25
            "p4ng/R10-liveness" 1.0}
           :relation-weights
           {:requires-control 1.0
            :repairs-control 1.0
            :instantiates-control 0.75
            :produces-evidence-for 0.5
            :blocked-by-control 0.0}}))
        phase6-result (outcomes/run-dark-ablation outer phase6)
        phase7-result (policies/run-shadow-window outer phase7)
        phase8-result (canary/advice-only phase7-result phase8)]
    {:outer outer
     :checkpoint checkpoint
     :phase6 phase6-result
     :phase7 phase7-result
     :phase8 phase8-result}))

(deftest frozen-chain-works-as-designed-without-claiming-live-readiness
  (let [{:keys [outer checkpoint phase6 phase7 phase8]} (replay-chain)]
    (testing "typed retrieval changes rank, not membership, and beats fixed hit@1"
      (is (= #{"M-aif-policy-conditioned-eig"
               "M-shared-memory-control-build-test"
               "M-wm-aif-policy-grain-compliance"}
             (set (map :mission-id
                       (get-in outer
                               [:admissible-projection :candidates])))))
      (is (= 0.0 (get-in checkpoint
                         [:retrieval-checkpoint :held-out
                          :control-hit-at-k])))
      (is (= 1.0 (get-in checkpoint
                         [:retrieval-checkpoint :held-out
                          :typed-hit-at-k])))
      (is (true? (get-in checkpoint
                         [:retrieval-checkpoint
                          :useful-inspectable-difference?]))))
    (testing "outcome model improves the frozen point estimate but refuses promotion"
      (is (= 1.0 (get-in phase6
                         [:ranking-evaluation :support+outcome
                          :top-choice-accuracy])))
      (is (= 0.5 (get-in phase6
                         [:ranking-evaluation :current-additive
                          :top-choice-accuracy])))
      (is (= 2 (get-in phase6
                       [:misleading-seed-recovery :recovered-count])))
      (is (false? (get-in phase6 [:outcome-promotion :advance?])))
      (is (= :exploratory-sample-too-small
             (get-in phase6 [:outcome-promotion :decision-reason]))))
    (testing "strategic policy keeps typed support, memory, E_S, and G_S together"
      (is (= 1.0 (get-in phase7
                         [:evaluation :model-reviewed-agreement])))
      (is (true? (get-in phase7
                          [:evaluation :candidate-set-preserved?])))
      (is (true? (get-in phase7
                          [:evaluation :provenance-preserved?])))
      (is (true? (get-in phase7
                          [:promotion :promotion-eligible-for-review?])))
      (is (false? (get-in phase7 [:promotion :promote?]))))
    (testing "canary recommendation contains concrete memories and remains non-actuating"
      (is (= :advice-issued (:status phase8)))
      (is (= ["e-wm-eig-support" "e-wm-memory-support"]
             (get-in phase8 [:recommendation :memory-ids])))
      (is (map? (get-in phase8 [:recommendation :e-s])))
      (is (map? (get-in phase8 [:recommendation :predicted-g-s])))
      (is (false? (get-in phase8 [:enactment :authorized?])))
      (is (nil? (:selected-mission phase8)))
      (is (false? (:live-ordering-changed? phase8))))))
