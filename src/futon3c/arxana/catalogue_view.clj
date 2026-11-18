(ns futon3c.arxana.catalogue-view
  "Read-only catalogue projection and complete-or-inconclusive acquisition."
  (:require [futon3c.arxana.design-catalogue :as catalogue]
            [futon3c.arxana.queries :as queries]
            [futon3c.arxana.store-http :as http])
  (:import [java.time Instant]))

(defn valid-time? [at]
  (and (string? at) (boolean (re-matches #"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z" at))
       (try (Instant/parse at) true (catch Exception _ false))))
(defn- typed? [r t] (= [t 1] (:type-revision r)))
(defn- target [r role]
  (some #(when (= role (:role %)) (or (get-in % [:target :clusion/id])
                                     (get-in % [:target :ref/id]))) (:endpoints r)))
(defn- summary [r]
  (if r
    (assoc (select-keys (:asserted-content r)
                       [:choice/key :statement :kind :lifecycle :sources :applies-to]) :id (:id r))
    {:missing? true}))
(defn- run-view [r]
  {:id (:id r) :result (get-in r [:content :result]) :violations (get-in r [:content :violations])
   :event-time (:event-time r) :method (:method r)})
(defn- latest-run [rs choice check]
  (let [runs (filter #(and (= choice (:subject-clusion-id %))
                           (= check (get-in % [:method :check/id]))) rs)]
    (when (seq runs)
      (try
        (let [groups (group-by #(Instant/parse (:event-time %)) runs)
              latest (sort-by :id (get groups (last (sort (keys groups)))))]
          (if (= 1 (count latest)) (run-view (first latest))
              {:status :tied :runs (mapv run-view latest)}))
        (catch Exception _ {:status :invalid-run :record-ids (vec (sort (map :id runs)))})))))

(defn catalogue-view
  "Pure snapshot view at AT. Future dated records are excluded. Malformed run
  times remain visible as invalid rather than silently passing. Equal-time runs
  are shown together; enforcement-status owns conflict/violation precedence."
  [records at]
  (if-not (valid-time? at)
    {:status :inconclusive :reason :invalid-valid-time}
    (let [instant (Instant/parse at)
          rs (filterv #(or (nil? (:event-time %))
                           (try (not (.isAfter (Instant/parse (:event-time %)) instant))
                                (catch Exception _ true))) records)
          index (into {} (map (juxt :id identity) rs))
          links (fn [type id] (sort-by :id (filter #(and (typed? % type) (= id (target % :choice))) rs)))]
      {:status :ok :valid-time at
       :choices
       (mapv
        (fn [choice]
          (let [id (:id choice)]
            (assoc (summary choice)
                   :checks (mapv (fn [check-id]
                                   (let [r (index check-id)]
                                     (assoc (select-keys (:asserted-content r) [:check/key :runtime :entry-point])
                                            :id check-id :missing? (not (typed? r :design/check))
                                            :latest-run (latest-run rs id check-id))))
                                 (sort (distinct (keep #(target % :check) (links :design/checked-by id)))))
                   :shaped-by (mapv #(assoc (summary (index (target % :ruling)))
                                           :id (target % :ruling) :link-id (:id %))
                                    (links :design/shaped-by id))
                   :constrains (mapv (fn [link]
                                       (let [ref (index (target link :target))]
                                         {:link-id (:id link) :id (target link :target)
                                          :missing? (nil? ref)
                                          :coordinate (dissoc ref :id)}))
                                     (links :design/constrains id))
                   :enforcement-status (catalogue/enforcement-status rs id))))
        (sort-by (juxt #(get-in % [:asserted-content :choice/key]) :id)
                 (filter #(typed? % :design/choice) rs)))})))

(defn- read-catalogue-once
  "Acquire all five catalogue type sets, each check's run history, and linked
  external references by ID at AT. Any refusal/truncation prevents a partial
  view. Bounded to 200 per query; no pagination or external-byte dereferencing."
  [graph-store at]
  (if-not (valid-time? at)
    {:status :inconclusive :reason :invalid-valid-time}
    (let [receipts (atom [])]
      (try
        (letfn [(read! [query f]
                  (let [r (f)]
                    (swap! receipts conj {:query query :response (select-keys r [:status :truncated? :watermark :total])})
                    (when (or (not= :ok (:status r)) (:truncated? r) (not (sequential? (:records r))))
                      (throw (ex-info "catalogue query incomplete"
                                      {:catalogue/refusal true :reason (cond (:truncated? r) :truncated
                                                     (not= :ok (:status r)) (or (:status r) :query-error)
                                                     :else :invalid-response)
                                       :query query :response r})))
                    (:records r)))]
          (let [by-type (into {} (for [type [:design/choice :design/check :design/checked-by
                                            :design/shaped-by :design/constrains]
                                      :let [q {:type-revision [type 1] :valid-time at :limit 200}]]
                                  [type (read! (assoc q :query/type :records-by-type)
                                               #(queries/records-by-type graph-store q))]))
                runs (mapcat (fn [check]
                               (let [q {:record-kind :attestation :field [:method :check/id]
                                        :value (:id check) :valid-time at :limit 200}]
                                 (read! (assoc q :query/type :records-by-field)
                                        #(queries/records-by-field graph-store q))))
                             (by-type :design/check))
                refs (mapv (fn [id]
                             (let [r (http/get-entity-as-of graph-store id at)]
                               (when (or (:error r) (nil? (:ok r)))
                                 (throw (ex-info "catalogue reference unavailable"
                                                 {:catalogue/refusal true :reason :reference-unavailable
                                                  :record-id id :response r})))
                               (:ok r)))
                           (sort (distinct (map #(target % :target) (by-type :design/constrains)))))
                records (vec (vals (into {} (map (juxt :id identity)
                                                 (concat (mapcat val by-type) runs refs)))))]
            (assoc (catalogue-view records at) :queries @receipts)))
        (catch Exception e
          (let [detail (ex-data e)]
            (merge {:status :inconclusive :reason :query-error}
                   (if (:catalogue/refusal detail)
                     (dissoc detail :catalogue/refusal)
                     {:message (.getMessage e)}))))))))

(defn read-catalogue
  "As read-catalogue-once, retried up to three times when the only refusal is
  :stale-index. The sidecar lags briefly while other writers append; a stale
  answer is transient by definition, and a view of about twenty queries met it
  on most live reads (2026-11-18). Any other refusal or truncation is returned
  at once. When retried, the count is reported in :attempts."
  [graph-store at]
  (loop [n 1]
    (let [r (read-catalogue-once graph-store at)]
      (if (and (= :inconclusive (:status r)) (= :stale-index (:reason r)) (< n 3))
        (do (Thread/sleep 500) (recur (inc n)))
        (if (> n 1) (assoc r :attempts n) r)))))
