(ns futon3c.arxana.batch-read-support
  "Adapt existing single-read fixture oracles to batch protocol shapes."
  (:require [futon3c.arxana.store :as store]))
(defn wrap [s]
  (reify store/ArxanaGraphStore
    (bounded-query! [_ q]
      (let [kind (:query/type q)]
        (if (#{:records-by-ids :attestations-by-subjects :role-reverse-many} kind)
          (let [ids (or (:record-ids q) (:subject-ids q) (:endpoint-ids q))
                rs (mapv (fn [id]
                           (let [single (case kind
                                          :records-by-ids {:query/type :record-by-id :record-id id :valid-time (:valid-time q)}
                                          :attestations-by-subjects {:query/type :attestations :subject-clusion-id id :limit (:limit q)}
                                          :role-reverse-many (-> q (dissoc :endpoint-ids) (assoc :query/type :role-reverse :endpoint-id id)))
                                 r (store/bounded-query! s single)]
                             (if (:error r) r
                               (update r :ok assoc (if (= kind :records-by-ids) :record-id :id) id)))) ids)]
            (or (first (filter :error rs))
                {:ok {:status :ok :watermark 0 :truncated? false :results (mapv :ok rs)}}))
          (store/bounded-query! s q))))))
