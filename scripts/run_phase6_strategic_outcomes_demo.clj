(require '[clojure.edn :as edn]
         '[clojure.pprint :as pprint]
         '[futon3c.peripheral.strategic-cascade :as strategic-cascade]
         '[futon3c.peripheral.strategic-outcomes :as outcomes]
         '[futon3c.peripheral.wm-memory :as wm-memory])

(let [{:keys [episodes control-edges]}
      (edn/read-string
       (slurp "holes/labs/M-typed-memories/phase4-wm-corpus.edn"))
      fixture
      (edn/read-string
       (slurp
        "holes/labs/M-typed-memories/phase6-strategic-outcomes.edn"))
      {:keys [cascade dependencies transition-warrants]}
      (edn/read-string
       (slurp
        "holes/labs/M-typed-memories/phase5-outer-cascade.edn"))
      recall-fn
      (fn [_ endpoint _]
        {:ok true
         :endpoint endpoint
         :memories
         (filterv #(some #{endpoint} (:memory/pattern-ids %))
                  episodes)})
      outer-result
      (strategic-cascade/outer-frontier
       {:cascade cascade
        :dependencies dependencies
        :transition-warrants transition-warrants
        :budget (count (:shown cascade))
        :query-step-fn
        (fn [pattern-id _]
          (wm-memory/dark-candidate-projection
           {:recall-fn recall-fn :trace-id "phase6-dark-demo"}
           [pattern-id] control-edges {:limit 10}))})
      result
      (outcomes/run-dark-ablation outer-result fixture)]
  (pprint/pprint
   (select-keys
    result
    [:status :fixture :outcome-evaluation
     :global-rate-baseline-evaluation :ranking-evaluation
     :centrality-ablation :outcome-promotion :evidence-separation
     :factor-semantics :misleading-seed-recovery
     :explanation-completeness :latency-ms :rung3-trace
     :selected-mission :live-ordering-changed?])))
