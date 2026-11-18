(ns futon3c.diagramprover.store-finding-view
  "Current reverse-index discovery followed by valid-time reads of every candidate.
  Index truncation or staleness cannot establish a complete finding view."
  (:require [futon3c.arxana.dependence :as dependence]
            [futon3c.arxana.store-http :as http]
            [futon3c.diagramprover.finding-view :as view]))

(defn store-finding-view [graph-store finding-id as-of]
  (let [read (http/get-entity-as-of graph-store finding-id as-of)]
    (cond
      (dependence/missing? read) {:status :missing :finding-id finding-id :as-of as-of :error (:error read)}
      (:error read) read
      :else
      (let [finding (:ok read)
            rules (mapv #(dependence/target-id finding %) [:rule-a :rule-b])
            queries (conj (mapv #(dependence/reverse-query [:supersedes 1] :predecessor %) rules)
                          (dependence/reverse-query [:residual/accepted 1] :finding finding-id))
            result (dependence/reverse-record-queries
                    graph-store queries as-of
                    {:receipts? true :distinct? true
                     :unexpected-response (fn [response]
                                            {:error {:error/code :arxana.conflict/unexpected-query-response
                                                     :error/detail response}})})]
        (if (not= :ok (:status result)) result
            (assoc (view/finding-view (into [finding] (:records result)) finding-id as-of)
                   :status :ok :queries (:queries result)))))))
