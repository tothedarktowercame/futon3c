(ns futon3c.diagramprover.store-finding-view
  "Current reverse-index discovery followed by valid-time reads of every candidate.
  Index truncation or staleness cannot establish a complete finding view."
  (:require [futon3c.arxana.store :as store]
            [futon3c.arxana.store-http :as http]
            [futon3c.diagramprover.finding-view :as view]))

(defn- missing? [result]
  (= :arxana.store/record-not-found (get-in result [:error :error/code])))
(defn- reverse-query [type role id]
  {:query/type :role-reverse :edge-type type :endpoint-role role :endpoint-id id})
(defn store-finding-view [graph-store finding-id as-of]
  (let [read (http/get-entity-as-of graph-store finding-id as-of)]
    (cond
      (missing? read) {:status :missing :finding-id finding-id :as-of as-of :error (:error read)}
      (:error read) read
      :else
      (let [finding (:ok read)
            rules (mapv (fn [role] (some #(when (= role (:role %))
                                            (get-in % [:target :clusion/id])) (:endpoints finding)))
                        [:rule-a :rule-b])
            queries (conj (mapv #(reverse-query [:supersedes 1] :predecessor %) rules)
                          (reverse-query [:residual/accepted 1] :finding finding-id))]
        (loop [remaining queries receipts [] candidates []]
          (if-let [q (first remaining)]
            (let [{:keys [ok error] :as response} (store/bounded-query! graph-store q)
                  receipts (conj receipts {:query q :total (:total ok) :watermark (:watermark ok)
                                           :response response})]
              (cond
                error (assoc response :queries receipts)
                (= :stale-index (:status ok)) {:status :inconclusive :reason :stale-index :queries receipts}
                (:truncated? ok) {:status :inconclusive :reason :truncated :queries receipts}
                (not= :ok (:status ok)) {:error {:error/code :arxana.conflict/unexpected-query-response
                                                :error/detail response} :queries receipts}
                :else (recur (rest remaining) receipts (into candidates (:edge-ids ok)))))
            (loop [ids (seq (distinct candidates)) records [finding]]
              (if-let [id (first ids)]
                (let [r (http/get-entity-as-of graph-store id as-of)]
                  (cond
                    (missing? r) (recur (next ids) records)
                    (:error r) (assoc r :queries receipts)
                    :else (recur (next ids) (conj records (:ok r)))))
                (assoc (view/finding-view records finding-id as-of)
                       :status :ok :queries receipts)))))))))
