(ns futon3c.arxana.dependence
  "Bounded reverse discovery followed by valid-time reads. No persistence."
  (:require [futon3c.arxana.store :as store]
            [futon3c.arxana.store-http :as http]))

(defn missing? [r]
  (= :arxana.store/record-not-found (get-in r [:error :error/code])))
(defn reverse-query [type role id]
  {:query/type :role-reverse :edge-type type :endpoint-role role :endpoint-id id :limit 200})
(defn target-id
  "Clusion endpoint identity used by application and supersession relations."
  [record role]
  (some #(when (= role (:role %)) (get-in % [:target :clusion/id])) (:endpoints record)))

(defn reverse-record-queries
  "Query all indexes before temporal reads. Options preserve callers' receipt,
  deduplication and unexpected-response contracts. Default error shape is the
  historical sentence reader's shape; no receipts are added unless requested."
  [s queries at {:keys [receipts? distinct? unexpected-response]
                 :or {unexpected-response
                      (fn [r] {:error {:error/code :sentence/query-response :response r}})}}]
  (letfn [(finish [r receipts] (if receipts? (assoc r :queries receipts) r))]
    (loop [qs queries receipts [] candidates []]
      (if-let [q (first qs)]
        (let [{:keys [ok error] :as response} (store/bounded-query-settled! s q)
              receipts (conj receipts {:query q :total (:total ok) :watermark (:watermark ok)
                                      :response response})]
          (cond
            error (finish response receipts)
            (= :stale-index (:status ok)) (finish {:status :inconclusive :reason :stale-index} receipts)
            (:truncated? ok) (finish {:status :inconclusive :reason :truncated} receipts)
            (not= :ok (:status ok)) (finish (unexpected-response response) receipts)
            :else (recur (rest qs) receipts (into candidates (:edge-ids ok)))))
        (loop [ids (seq (if distinct? (distinct candidates) candidates)) records []]
          (if-let [id (first ids)]
            (let [r (http/get-entity-as-of s id at)]
              (cond (missing? r) (recur (next ids) records)
                    (:error r) (finish r receipts)
                    :else (recur (next ids) (conj records (:ok r)))))
            (finish {:status :ok :records records} receipts)))))))

(defn reverse-records [s edge-type endpoint-role endpoint-id at]
  (reverse-record-queries s [(reverse-query edge-type endpoint-role endpoint-id)] at {}))

(defn fibre
  "Return sorted distinct :ids of visible clusion dependants, or the read/query
  refusal. Applications and dependants are both read at the requested time."
  [s application-type producer-role dependant-role producer-id at]
  (let [apps (reverse-records s application-type producer-role producer-id at)]
    (if (not= :ok (:status apps)) apps
        (loop [ids (sort (distinct (map #(target-id % dependant-role) (:records apps)))) out []]
          (if-let [id (first ids)]
            (let [r (http/get-entity-as-of s id at)]
              (cond (missing? r) (recur (rest ids) out)
                    (:error r) r
                    :else (recur (rest ids) (conj out id))))
            {:status :ok :ids out})))))

(defn staleness
  "Read a clusion dependant, its applications and direct producer supersessions.
  A missing dependant is explicit. Supersession chains are read link by link."
  [s application-type dependant-role producer-role dependant-id at]
  (let [dependant (http/get-entity-as-of s dependant-id at)]
    (cond (missing? dependant) {:status :missing :id dependant-id}
          (:error dependant) dependant
          :else
          (let [apps (reverse-records s application-type dependant-role dependant-id at)]
            (if (not= :ok (:status apps)) apps
                (loop [producers (distinct (map #(target-id % producer-role) (:records apps))) because []]
                  (if-let [id (first producers)]
                    (let [sup (reverse-records s [:supersedes 1] :predecessor id at)]
                      (if (not= :ok (:status sup)) sup
                          (recur (rest producers) (into because (map :id (:records sup))))))
                    {:status :ok :stale? (boolean (seq because))
                     :stale-because (vec (sort (distinct because)))})))))))
