(require '[clojure.edn :as edn]
         '[clojure.pprint :as pprint]
         '[futon3c.peripheral.strategic-canary :as canary]
         '[futon3c.peripheral.strategic-cascade :as strategic-cascade]
         '[futon3c.peripheral.strategic-policies :as policies]
         '[futon3c.peripheral.wm-memory :as wm-memory])

(let [{:keys [episodes control-edges]}
      (edn/read-string
       (slurp "holes/labs/M-typed-memories/phase4-wm-corpus.edn"))
      {:keys [cascade dependencies transition-warrants]}
      (edn/read-string
       (slurp "holes/labs/M-typed-memories/phase5-outer-cascade.edn"))
      phase7
      (edn/read-string
       (slurp
        "holes/labs/M-typed-memories/phase7-strategic-policy-shadow.edn"))
      phase8
      (edn/read-string
       (slurp
        "holes/labs/M-typed-memories/phase8-advice-only-canary.edn"))
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
           {:recall-fn recall-fn :trace-id "phase8-advice-demo"}
           [pattern-id] control-edges {:limit 10}))})
      shadow-result (policies/run-shadow-window outer-result phase7)
      result (canary/advice-only shadow-result phase8)]
  (pprint/pprint result))
