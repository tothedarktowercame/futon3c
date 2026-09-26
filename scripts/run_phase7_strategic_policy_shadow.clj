(require '[clojure.edn :as edn]
         '[clojure.pprint :as pprint]
         '[futon3c.peripheral.strategic-cascade :as strategic-cascade]
         '[futon3c.peripheral.strategic-policies :as policies]
         '[futon3c.peripheral.wm-memory :as wm-memory])

(let [{:keys [episodes control-edges]}
      (edn/read-string
       (slurp "holes/labs/M-typed-memories/phase4-wm-corpus.edn"))
      {:keys [cascade dependencies transition-warrants]}
      (edn/read-string
       (slurp "holes/labs/M-typed-memories/phase5-outer-cascade.edn"))
      fixture
      (edn/read-string
       (slurp
        "holes/labs/M-typed-memories/phase7-strategic-policy-shadow.edn"))
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
           {:recall-fn recall-fn :trace-id "phase7-shadow-demo"}
           [pattern-id] control-edges {:limit 10}))})
      result (policies/run-shadow-window outer-result fixture)]
  (pprint/pprint
   (select-keys
    result
    [:status :fixture :policy-construction :e-s :shadow-traces
     :evaluation :promotion :source-budget :selected-mission
     :live-ordering-changed?])))
