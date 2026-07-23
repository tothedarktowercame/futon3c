(require '[clojure.edn :as edn])
(require '[clojure.pprint :refer [pprint]])
(require '[futon3c.peripheral.strategic-cascade :as strategic])
(require '[futon3c.peripheral.wm-memory :as wm-memory])

(let [{:keys [episodes control-edges]}
      (-> "holes/labs/M-typed-memories/phase4-wm-corpus.edn"
          slurp edn/read-string)
      {:keys [cascade dependencies transition-warrants checkpoint]}
      (-> "holes/labs/M-typed-memories/phase5-outer-cascade.edn"
          slurp edn/read-string)
      query-step
      (fn [pattern-id _remaining-budget]
        (wm-memory/dark-candidate-projection
         {:trace-id (str "phase5-demo-" pattern-id)
          :recall-fn
          (fn [_ endpoint _]
            {:ok true
             :endpoint endpoint
             :memories
             (filterv #(some #{endpoint} (:memory/pattern-ids %))
                      episodes)})}
         [pattern-id] control-edges {:limit 10}))
      outer
      (strategic/outer-frontier
       {:cascade cascade
        :dependencies dependencies
        :transition-warrants transition-warrants
        :query-step-fn query-step
        :budget 4})
      compared
      (strategic/checkpoint-ranking
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
          :blocked-by-control 0.0}}))]
  (pprint
   (select-keys compared
                [:status :algorithm :budget :frontier :excluded-missions
                 :holes :mint-proposals :retrieval-checkpoint
                 :selected-mission :live-ordering-changed?])))
