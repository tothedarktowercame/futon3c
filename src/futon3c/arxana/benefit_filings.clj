(ns futon3c.arxana.benefit-filings
  "Historical benefit filings, with substrate identities kept in a separate lane."
  (:require [futon3c.arxana.catalogue-view :as catalogue]
            [futon3c.arxana.benefit-names :as names]
            [futon3c.arxana.batch-read :as batch]
            [futon3c.arxana.queries :as queries]))

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
  "Acquire each dependency level in batches; any incomplete set refuses the page."
  [store {:keys [benefit-id valid-time]}]
  (if-not (and (catalogue/valid-time? valid-time) (or (nil? benefit-id) (string? benefit-id)))
    {:status :inconclusive :reason :invalid-request}
    (binding [batch/*receipts* (atom [])]
     (try
      (let [benefits (if benefit-id (batch/records store [benefit-id] valid-time false)
                         (:records (batch/checked (batch/fetch! queries/records-by-type store {:type-revision [:benefit-line 1] :limit 200 :valid-time valid-time}))))
            _ (when-not (sequential? benefits)
                (throw (ex-info "benefits unavailable" {:reason :invalid-response})))
            _ (when (some #(not= [:benefit-line 1] (:type-revision %)) benefits)
                (throw (ex-info "not a benefit" {:reason :not-a-benefit})))
            ids (mapv :id benefits)
            old (batch/reverse-records store [:workplan/serves 1] :benefit-line ids valid-time)
            substrate (batch/reverse-records store [:workplan/serves 2] :benefit-line ids valid-time)
            item-ids (when benefit-id (distinct (map #(endpoint % :item) old)))
            items (batch/records store item-ids valid-time false)
            names (batch/reverse-records store [:workplan/names 1] :item item-ids valid-time)
            entities (batch/records store (map #(endpoint % :entity) names) valid-time false)
            base (concat benefits (when benefit-id (concat old substrate items names entities)))
            evidence (batch/evidence store (map :id base) valid-time)
            records (concat base evidence)]
        (if benefit-id
          (assoc (benefit-view records benefit-id valid-time) :queries @batch/*receipts*)
          {:status :ok :mode :list :valid-time valid-time :interpretation :historical-filings
           :queries @batch/*receipts*
           :benefits (mapv (fn [b] {:label (names/label (get-in b [:asserted-content :benefit/line]))
                                   :cluster (or (:trunk (names/label (get-in b [:asserted-content :benefit/line]))) "unnamed")
                                   :benefit b :evidence-completeness :complete
                                   :evidence (filterv #(= (:id b) (:subject-clusion-id %)) evidence)
                                   :serves-v1 (count (filter #(= (:id b) (endpoint % :benefit-line)) old))
                                   :serves-v2 (count (filter #(= (:id b) (endpoint % :benefit-line)) substrate))})
                           (sort-by #(get-in % [:asserted-content :benefit/line]) benefits))}))
      (catch Exception e (merge {:status :inconclusive :reason :query-error} (ex-data e)))))))
