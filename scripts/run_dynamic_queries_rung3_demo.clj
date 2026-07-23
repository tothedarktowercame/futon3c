(require '[clojure.edn :as edn])
(require '[clojure.pprint :refer [pprint]])
(require '[futon3c.peripheral.strategic-cascade :as strategic])
(require '[futon3c.peripheral.wm-memory :as wm-memory])

(let [{:keys [episodes control-edges]}
      (-> "holes/labs/M-typed-memories/phase4-wm-corpus.edn"
          slurp edn/read-string)
      {:keys [cascade dependencies transition-warrants]}
      (-> "holes/labs/M-typed-memories/phase5-outer-cascade.edn"
          slurp edn/read-string)
      {:keys [budget information-models additional-transition-warrants]}
      (-> "holes/labs/M-typed-memories/rung3-facet-refinement.edn"
          slurp edn/read-string)
      query-step
      (fn [pattern-id _remaining-budget]
        (wm-memory/dark-candidate-projection
         {:trace-id (str "dynamic-queries-rung3-" pattern-id)
          :recall-fn
          (fn [_ endpoint _]
            {:ok true
             :endpoint endpoint
             :memories
             (filterv #(some #{endpoint} (:memory/pattern-ids %))
                      episodes)})}
         [pattern-id] control-edges {:limit 10}))
      result
      (strategic/budgeted-facet-frontier
       {:cascade cascade
        :dependencies dependencies
        :transition-warrants
        (into transition-warrants additional-transition-warrants)
        :information-models information-models
        :budget budget
        :query-step-fn query-step})]
  (pprint
   {:status (:status result)
    :facet-refinement (:facet-refinement result)
    :frontier
    (mapv #(select-keys
            % [:mission-id :frontier-status :dependency-status])
          (:frontier result))
    :excluded-missions
    (mapv #(select-keys % [:mission-id :exclusion])
          (:excluded-missions result))
    :holes (:holes result)
    :selected-mission (:selected-mission result)
    :live-ordering-changed? (:live-ordering-changed? result)}))
