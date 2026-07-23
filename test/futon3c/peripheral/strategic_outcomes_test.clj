(ns futon3c.peripheral.strategic-outcomes-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.peripheral.strategic-outcomes :as outcomes]
            [futon3c.peripheral.wm-memory :as wm-memory]))

(defn- read-edn
  [path]
  (-> path io/file slurp edn/read-string))

(defn- phase6-input
  []
  (let [{:keys [episodes control-edges]}
        (read-edn
         "holes/labs/M-typed-memories/phase4-wm-corpus.edn")
        patterns (->> episodes (mapcat :memory/pattern-ids) distinct vec)
        recall-fn
        (fn [_ endpoint _]
          {:ok true
           :endpoint endpoint
           :memories
           (filterv #(some #{endpoint} (:memory/pattern-ids %))
                    episodes)})
        projection
        (:projection
         (wm-memory/dark-candidate-projection
          {:recall-fn recall-fn :trace-id "phase6-outcome-test"}
          patterns control-edges {:limit 10}))]
    {:outer-result
     {:admissible-projection {:candidates (:candidates projection)}
      :excluded-missions (:excluded-missions projection)}
     :fixture
     (read-edn
      "holes/labs/M-typed-memories/phase6-strategic-outcomes.edn")}))

(deftest outcome-model-requires-independent-witnesses-and-abstains
  (let [{:keys [fixture]} (phase6-input)
        model (outcomes/fit-outcome-model
               (:training-transitions fixture))]
    (is (= (/ 2.0 3.0)
           (get-in model
                   [:mission-estimates
                    "M-aif-policy-conditioned-eig"
                    :outcome-probability])))
    (is (= (/ 5.0 6.0)
           (get-in model
                   [:mission-estimates
                    "M-shared-memory-control-build-test"
                    :outcome-probability])))
    (is (false?
         (:abstain?
          (outcomes/predict-outcome
           model "M-shared-memory-control-build-test"))))
    (is (= :unknown-mission
           (:abstention-reason
            (outcomes/predict-outcome model "M-never-observed"))))
    (is (false? (:rung3-entropy-consumed? model)))
    (testing "self-certified outcomes cannot train the model"
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"independently witnessed"
           (outcomes/fit-outcome-model
            [{:transition-id "self"
              :mission-id "M-one"
              :outcome :success
              :witness-status :self-asserted
              :witness-id "same-runner"}]))))))

(deftest frozen-dark-ablation-keeps-factor-semantics-and-live-order-separate
  (let [{:keys [outer-result fixture]} (phase6-input)
        result (outcomes/run-dark-ablation outer-result fixture)]
    (is (= :dark (:status result)))
    (is (nil? (:selected-mission result)))
    (is (false? (:live-ordering-changed? result)))
    (is (= 1.0 (:explanation-completeness result)))
    (is (= 1.0 (get-in result
                       [:outcome-evaluation :coverage])))
    (is (= 0.0 (get-in result
                       [:outcome-evaluation :abstention-rate])))
    (is (< (get-in result [:outcome-evaluation :brier-score])
           (get-in result
                   [:global-rate-baseline-evaluation :brier-score])))
    (is (< (get-in result [:outcome-evaluation :log-loss])
           (get-in result
                   [:global-rate-baseline-evaluation :log-loss])))
    (is (= 1.0
           (get-in result
                   [:ranking-evaluation :support+outcome
                    :top-choice-accuracy])))
    (is (= 0.5
           (get-in result
                   [:ranking-evaluation :current-additive
                    :top-choice-accuracy])))
    (is (= 0.75
           (get-in result
                   [:ranking-evaluation :support+outcome+centrality
                    :top-choice-accuracy])))
    (is (= :exploratory-sample-too-small
           (get-in result
                   [:centrality-ablation :decision-reason])))
    (is (false?
         (get-in result [:centrality-ablation :retire-centrality?])))
    (is (false? (get-in result [:outcome-promotion :advance?])))
    (is (= {:recovered-count 2 :degraded-count 0}
           (:misleading-seed-recovery result)))
    (is (= :reserved-for-phase7
           (get-in result [:factor-semantics :habit])))
    (is (true?
         (get-in result
                 [:evidence-separation :unknown-is-abstention?])))
    (is (true?
         (get-in result
                 [:evidence-separation
                  :blocked-disjoint-from-candidates?])))
    (is (false?
         (get-in result
                 [:rung3-trace
                  :entropy-consumed-as-calibrated-probability?])))
    (is (every? nil? (map :selected-mission
                          (:judgement-traces result))))
    (is (every? false? (map :live-ordering-changed?
                            (:judgement-traces result))))))

(deftest fixture-freeze-and-split-are-enforced
  (let [{:keys [outer-result fixture]} (phase6-input)
        overlap (assoc fixture :held-out-outcomes
                       [(first (:training-transitions fixture))])]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"invalid or unfrozen"
         (outcomes/run-dark-ablation
          outer-result (dissoc fixture :freeze-id))))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"invalid or unfrozen"
         (outcomes/run-dark-ablation outer-result overlap)))))
