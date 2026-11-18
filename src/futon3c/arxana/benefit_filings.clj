(ns futon3c.arxana.benefit-filings
  "Historical benefit filings, with substrate identities kept in a separate lane."
  (:require [futon3c.arxana.catalogue-view :as catalogue]
            [futon3c.arxana.dependence :as dep]
            [futon3c.arxana.queries :as queries]
            [futon3c.arxana.store-http :as http]))

(defn- endpoint [r role] (some #(when (= role (:role %)) (val (first (:target %)))) (:endpoints r)))
(defn- typed [rs t] (filter #(= t (:type-revision %)) rs))
(defn benefit-view
  "Project a complete, already acquired snapshot. Filings are historical source
  acts, not current status verdicts. No identity conversion between item lanes."
  [rs benefit-id at]
  (let [index (into {} (map (juxt :id identity) rs))
        reading (fn [r] {:record r :evidence-completeness :complete
                         :evidence (vec (sort-by :id (filter #(= (:id r) (:subject-clusion-id %)) rs)))})
        serving (fn [v] (filter #(= benefit-id (endpoint % :benefit-line)) (typed rs [:workplan/serves v])))
        parents (group-by #(endpoint % :item) (serving 1))
        items (mapv (fn [[id links]]
                      {:item (reading (index id)) :serves (mapv reading (sort-by :id links))
                       :names (mapv (fn [link] {:link (reading link) :entity (reading (index (endpoint link :entity)))})
                                    (sort-by :id (filter #(= id (endpoint % :item)) (typed rs [:workplan/names 1]))))})
                    (sort-by key parents))]
    {:status :ok :mode :detail :valid-time at :interpretation :historical-filings
     :benefit (reading (index benefit-id)) :items items
     :substrate (mapv #(hash-map :entity/id (endpoint % :item) :serves (reading %)) (sort-by :id (serving 2)))
     :counts {:serves-v1 (count (serving 1)) :serves-v2 (count (serving 2))
              :items (count items) :naming-rows (reduce + 0 (map #(count (:names %)) items))
              :named-items (count (filter #(seq (:names %)) items))
              :entities (count (set (map #(get-in % [:entity :record :id]) (mapcat :names items))))}}))

(defn read-filings
  "Without benefit-id list all benefits with visible serving counts per lane.
  With it, acquire both serving sets, item/lexicon records and all their evidence.
  Caps are 200; any incomplete/refused acquisition returns no partial view.
  Reverse discovery is current; candidate records and evidence are read at AT."
  [store {:keys [benefit-id valid-time]}]
  (if-not (and (catalogue/valid-time? valid-time) (or (nil? benefit-id) (and (string? benefit-id) (seq benefit-id))))
    {:status :inconclusive :reason :invalid-request}
    (let [saved (atom {}) queried (atom []) evidenced (atom #{})]
      (try
        (letfn [(fail! [reason response] (throw (ex-info "Benefit acquisition incomplete" {:reason reason :response response})))
                (get! [id optional?]
                  (or (@saved id)
                      (let [r (http/get-entity-as-of store id valid-time)]
                        (cond (and optional? (dep/missing? r)) nil
                              (or (:error r) (nil? (:ok r))) (fail! :record-unavailable r)
                              :else (do (swap! saved assoc id (:ok r)) (:ok r))))))
                (evidence! [r]
                  (when-not (@evidenced (:id r))
                    (let [q {:subject-clusion-id (:id r) :limit 200}
                          a (queries/attestations store q)]
                      (swap! queried conj (assoc q :query/type :attestations))
                      (when (or (not= :ok (:status a)) (:truncated? a) (not (sequential? (:attestation-ids a))))
                        (fail! (if (:truncated? a) :truncated :evidence-unavailable) a))
                      (doseq [id (:attestation-ids a)] (get! id true))
                      (swap! evidenced conj (:id r)))))
                (reverse! [type role id]
                  (let [r (dep/reverse-records store type role id valid-time)]
                    (swap! queried conj (dep/reverse-query type role id))
                    (when (not= :ok (:status r)) (fail! (or (:reason r) :reverse-unavailable) r))
                    (doseq [record (:records r)] (swap! saved assoc (:id record) record))
                    (:records r)))
                (lanes! [id] [(reverse! [:workplan/serves 1] :benefit-line id)
                              (reverse! [:workplan/serves 2] :benefit-line id)])]
          (if benefit-id
            (let [benefit (get! benefit-id false)]
              (when-not (= [:benefit-line 1] (:type-revision benefit)) (fail! :not-a-benefit benefit))
              (evidence! benefit)
              (let [[old substrate] (lanes! benefit-id)]
                (doseq [link (concat old substrate)] (evidence! link))
                (doseq [id (sort (set (map #(endpoint % :item) old)))]
                  (evidence! (get! id false))
                  (doseq [link (reverse! [:workplan/names 1] :item id)]
                    (evidence! link) (evidence! (get! (endpoint link :entity) false))))
                (assoc (benefit-view (vals @saved) benefit-id valid-time) :queries @queried)))
            (let [q {:type-revision [:benefit-line 1] :valid-time valid-time :limit 200}
                  r (queries/records-by-type store q)]
              (when (or (not= :ok (:status r)) (:truncated? r) (not (sequential? (:records r))))
                (fail! (if (:truncated? r) :truncated :benefits-unavailable) r))
              (swap! queried conj (assoc q :query/type :records-by-type))
              {:status :ok :mode :list :valid-time valid-time :interpretation :historical-filings
               :benefits (mapv (fn [b] (let [[old substrate] (lanes! (:id b))]
                                        (swap! saved assoc (:id b) b) (evidence! b)
                                        {:benefit b :evidence (vec (filter #(= (:id b) (:subject-clusion-id %)) (vals @saved)))
                                         :evidence-completeness :complete
                                         :serves-v1 (count old) :serves-v2 (count substrate)}))
                              (sort-by #(get-in % [:asserted-content :benefit/line]) (:records r)))
               :queries @queried})))
        (catch Exception e (merge {:status :inconclusive :reason :query-error} (ex-data e)))))))
