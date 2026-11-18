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

(defn debt-summary
  "Derived from the displayed enforcement states. Reasons can overlap when a
  choice has several checks. Coverage is read from the coverage check's latest run (Demo B's citation
  manifest against the catalogue); without a run it is :not-assessed, never clean. Immutable check records do
  not provide a dated implementation-change history, so stale-run is not inferred."
  [choices]
  {:by-reason
   (mapv (fn [reason]
           (let [keys (->> choices
                           (filter (fn [choice]
                                     (let [state (:enforcement-status choice)]
                                       (if (= reason :violated) (= :violated (:status state))
                                           (or (= reason (:reason state))
                                               (some #{reason} (:reasons state)))))))
                           (map :choice/key) distinct sort vec)]
             {:reason reason :count (count keys) :choice-keys keys}))
         [:no-check :no-run :violated])
   :coverage (let [run (some (fn [choice]
                               (when (some #{:demo-coverage} (:applies-to choice))
                                 (some :latest-run (:checks choice))))
                             choices)]
               (if run
                 {:status :assessed :result (:result run) :run-id (:id run) :event-time (:event-time run)
                  :undocumented-features (vec (:violations run))}
                 {:status :not-assessed :reason :no-coverage-run :undocumented-features []}))
   :limitations [:stale-run-requires-dated-implementation-change-history]})

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
          links (fn [type id] (sort-by :id (filter #(and (typed? % type) (= id (target % :choice))) rs)))
          choices
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
                 (filter #(typed? % :design/choice) rs)))]
        {:status :ok :valid-time at :choices choices :debt (debt-summary choices)})))

(defn- read-catalogue-once
  "Acquire all five catalogue type sets, each linked choice's subject history, and linked
  external references by ID at AT. Any refusal/truncation prevents a partial
  view. Bounded to 200 per query; no pagination or external-byte dereferencing."
  [graph-store at]
  (if-not (valid-time? at)
    {:status :inconclusive :reason :invalid-valid-time}
    (let [receipts (atom [])]
      (try
        (letfn [(read! [query field f]
                  (let [r (f)]
                    (swap! receipts conj {:query query :response (select-keys r [:status :truncated? :watermark :total])})
                    (when (or (not= :ok (:status r)) (:truncated? r) (not (sequential? (get r field))))
                      (throw (ex-info "catalogue query incomplete"
                                      {:catalogue/refusal true :reason (cond (:truncated? r) :truncated
                                                     (not= :ok (:status r)) (or (:status r) :query-error)
                                                     :else :invalid-response)
                                       :query query :response r})))
                    (get r field)))
                (hydrate! [id optional?]
                  (let [r (http/get-entity-as-of graph-store id at)]
                    (cond
                      (and optional? (= :arxana.store/record-not-found (get-in r [:error :error/code]))) nil
                      (or (:error r) (nil? (:ok r)))
                      (throw (ex-info "catalogue record unavailable"
                                      {:catalogue/refusal true :reason (if optional? :run-unavailable :reference-unavailable)
                                       :record-id id :response r}))
                      :else (:ok r))))]
          (let [by-type (into {} (for [type [:design/choice :design/check :design/checked-by
                                            :design/shaped-by :design/constrains]
                                      :let [q {:type-revision [type 1] :valid-time at :limit 200}]]
                                  [type (read! (assoc q :query/type :records-by-type) :records
                                               #(queries/records-by-type graph-store q))]))
                runs (mapcat
                      (fn [choice]
                        (let [id (:id choice)
                              checks (set (keep #(when (= id (target % :choice)) (target % :check))
                                                (by-type :design/checked-by)))]
                          (when (seq checks)
                            (let [q {:subject-clusion-id id :limit 200}
                                  ids (read! (assoc q :query/type :attestations) :attestation-ids
                                             #(queries/attestations graph-store q))]
                              (filter #(and (= id (:subject-clusion-id %))
                                            (checks (get-in % [:method :check/id])))
                                      (keep #(hydrate! % true) (distinct ids)))))))
                      (by-type :design/choice))
                refs (mapv #(hydrate! % false)
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
