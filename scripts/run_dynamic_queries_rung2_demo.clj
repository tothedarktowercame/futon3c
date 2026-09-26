(require '[clojure.edn :as edn]
         '[clojure.pprint :as pprint]
         '[futon3c.peripheral.strategic-cascade :as strategic-cascade]
         '[futon3c.peripheral.strategic-outcomes :as outcomes]
         '[futon3c.peripheral.wm-memory :as wm-memory])

(let [{:keys [episodes control-edges]}
      (edn/read-string
       (slurp "holes/labs/M-typed-memories/phase4-wm-corpus.edn"))
      {:keys [cascade dependencies transition-warrants]}
      (edn/read-string
       (slurp "holes/labs/M-typed-memories/phase5-outer-cascade.edn"))
      phase6
      (edn/read-string
       (slurp
        "holes/labs/M-typed-memories/phase6-strategic-outcomes.edn"))
      rung2
      (edn/read-string
       (slurp
        "holes/labs/M-typed-memories/rung2-operator-update.edn"))
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
           {:recall-fn recall-fn :trace-id "rung2-dark-demo"}
           [pattern-id] control-edges {:limit 10}))})
      ablation (outcomes/run-dark-ablation outer-result phase6)
      judgement
      (first (filter #(= (:judgement-id rung2) (:judgement-id %))
                     (:judgements phase6)))
      transition
      (first (filter #(= (:transition-id rung2) (:transition-id %))
                     (:held-out-outcomes phase6)))
      result
      (outcomes/outcome-conditioned-operator-update
       (:admissible-projection outer-result)
       (:training-transitions phase6)
       transition
       judgement
       {:min-observations (:min-outcome-observations phase6)
        :minimum-promotion-sample-size
        (:minimum-promotion-sample-size phase6)
        :phase6-promotion (:outcome-promotion ablation)
        :phase6-outcome-evaluation (:outcome-evaluation ablation)})]
  (pprint/pprint
   (select-keys
    result
    [:status :algorithm :transition :outcome-update :operator-update
     :rankings :candidate-set-preserved? :evaluation :promotion
     :counterfactual-ranking :selected-mission
     :live-ordering-changed?])))
