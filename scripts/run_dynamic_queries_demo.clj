(require '[clojure.edn :as edn])
(require '[clojure.pprint :refer [pprint]])
(require '[futon3c.peripheral.dynamic-queries :as dynamic-queries])
(require '[futon3c.peripheral.wm-memory :as wm-memory])

(let [{:keys [episodes control-edges]}
      (-> "holes/labs/M-typed-memories/phase4-wm-corpus.edn"
          slurp edn/read-string)
      patterns (->> episodes (mapcat :memory/pattern-ids) distinct vec)
      recall-fn
      (fn [_ endpoint _]
        {:ok true
         :endpoint endpoint
         :memories
         (filterv #(some #{endpoint} (:memory/pattern-ids %)) episodes)})
      dark
      (wm-memory/dark-candidate-projection
       {:recall-fn recall-fn :trace-id "dynamic-queries-rung1-demo"}
       patterns control-edges {:limit 10})
      ranking
      (dynamic-queries/fixed-typed-ranking
       (:projection dark)
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
         :blocked-by-control 0.0}})]
  (pprint
   (select-keys ranking
                [:status :algorithm :control-ranking :typed-ranking
                 :ranked-candidates :candidate-set-preserved?
                 :live-ordering-changed?])))
