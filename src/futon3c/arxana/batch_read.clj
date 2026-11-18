(ns futon3c.arxana.batch-read
  "Staged bounded acquisition. Cache misses/revalidations are coalesced within
  each known input group; no speculative reads or unbounded response acceptance."
  (:require [futon3c.arxana.queries :as q]
            [futon3c.arxana.read-cache :as cache]))
(def ^:dynamic *receipts* nil)
(defn fetch! [f s options]
  (let [r (f s options)]
    (when *receipts* (swap! *receipts* conj {:query (assoc options :query/type (cond (= f q/records-by-type) :records-by-type
                                                                      (= f q/records-by-ids) :records-by-ids
                                                                      (= f q/attestations-by-subjects) :attestations-by-subjects
                                                                      :else :role-reverse-many)) :response (select-keys r [:status :watermark :truncated?])})) r))
(defn checked [r]
  (when (or (not= :ok (:status r)) (:truncated? r))
    (throw (ex-info "batch read incomplete" {:reason (if (:truncated? r) :truncated (or (:status r) :query-error)) :response r}))) r)
(defn- batch-results [r requested]
  (checked r)
  (when-not (and (vector? (:results r)) (= (count requested) (count (:results r))) (= (set requested) (set (map #(or (:id %) (:record-id %)) (:results r)))))
    (throw (ex-info "incomplete batch membership" {:reason :invalid-response :response r})))
  (:results r))
(defn records [s ids at optional?]
  (vec (mapcat
        (fn [chunk]
          (let [loaded (delay (into {} (map (juxt :record-id identity)
                                          (batch-results (fetch! q/records-by-ids s {:record-ids chunk :valid-time at}) chunk))))]
            (keep (fn [id]
                    (let [r (cache/record-query! s {:query/type :record-by-id :record-id id :valid-time at}
                                                 (fn [_] {:ok (get @loaded id)})) b (:ok r)]
                      (cond (= :ok (:status b)) (:record b)
                            (and optional? (= :missing (:status b))) nil
                            :else (throw (ex-info "record unavailable" {:reason :record-unavailable :response r}))))) chunk)))
        (mapv vec (partition-all 1000 (distinct ids))))))
(defn sets
  "QUERY is a single-set template; ids are subject or endpoint ids. All cold
  sets share a full batch, all warm sets share a limit-1 probe batch."
  [s query ids]
  (vec (mapcat
        (fn [chunk]
          (let [att? (= :attestations (:query/type query))
                key (if att? :subject-clusion-id :endpoint-id)
                batches (atom {})
                fetch (fn [request]
                        (let [limit (:limit request)
                              loaded (or (get @batches limit)
                                         (let [opts (-> query (dissoc :query/type key)
                                                        (assoc :limit limit (if att? :subject-ids :endpoint-ids) chunk))
                                               r (fetch! (if att? q/attestations-by-subjects q/role-reverse-many) s opts)
                                               rs (into {} (map (fn [v] [(:id v) (assoc v :watermark (:watermark r))])
                                                                (batch-results r chunk)))]
                                           (swap! batches assoc limit rs) rs))]
                          {:ok (get loaded (get request key))}))]
            (mapv (fn [id] (let [r (:ok (cache/set-query! s (assoc query key id) fetch))]
                            (checked r))) chunk)))
        (mapv vec (partition-all 1000 (distinct ids))))))
(defn reverse-records [s type role ids at]
  (let [rs (sets s {:query/type :role-reverse :edge-type type :endpoint-role role :limit 200} ids)]
    (records s (mapcat :edge-ids rs) at true)))
(defn evidence [s subjects at]
  (let [rs (sets s {:query/type :attestations :limit 200} subjects)]
    (records s (mapcat :attestation-ids rs) at true)))
