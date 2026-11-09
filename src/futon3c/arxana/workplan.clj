(ns futon3c.arxana.workplan
  "Materialise generated workplan slices and project one item through public ports."
  (:require [clojure.edn :as edn]
            [clojure.walk :as walk]
            [futon3c.arxana.records :as records]))

(def default-slice "/home/joe/notes/cascade-live-outline/arxana/all-items.edn")

(defn read-slice ([] (read-slice default-slice)) ([path] (edn/read-string (slurp path))))

(defn- local-refs [x]
  (->> (tree-seq coll? seq x) (filter #(and (map? %) (= #{:local-id} (set (keys %)))))
       (map :local-id) set))

(defn plan
  "Resolve generated local ids in dependency order and return immutable records."
  [slice]
  (let [rows (:records slice) ids (keep :local-id rows) duplicates (seq (for [[k xs] (group-by identity ids) :when (< 1 (count xs))] k))]
    (when duplicates (throw (ex-info "duplicate local id" {:error :arxana.workplan/duplicate-local-id :local-ids (vec duplicates)})))
    (loop [pending (vec rows) resolved {} out []]
      (if (empty? pending) {:slice (:slice slice) :by-local-id resolved :records out}
          (let [[ready blocked] (reduce (fn [[a b] r]
                                         (if (every? #(contains? resolved %) (cond-> (local-refs r) (:subject r) (conj (:subject r))))
                                           [(conj a r) b] [a (conj b r)])) [[] []] pending)]
            (when (empty? ready)
              (throw (ex-info "dangling local id" {:error :arxana.workplan/dangling-local-id
                                                     :local-ids (vec (sort (mapcat #(cond-> (local-refs %) (:subject %) (conj (:subject %))) blocked)))})))
            (let [made (mapv (fn [r]
                               (let [replace-ref (fn [x] (if (and (map? x) (= #{:local-id} (set (keys x))))
                                                                    {:clusion/id (get-in resolved [(:local-id x) :id])} x))]
                                 (case (:record r)
                                   :clusion (records/make-clusion {:type-revision (:type-revision r)
                                                                   :endpoints (walk/postwalk replace-ref (or (:endpoints r) []))
                                                                   :asserted-content (:identity r)})
                                   :attestation (records/make-attestation
                                                 (-> r (dissoc :record :subject)
                                                     (assoc :subject-clusion-id (get-in resolved [(:subject r) :id]))))
                                   :assessment (records/make-assessment
                                                (-> r (dissoc :record :subject)
                                                    (assoc :subject-clusion-id (get-in resolved [(:subject r) :id]))))))) ready)
                  resolved' (reduce (fn [m [r made-r]] (if-let [lid (:local-id r)] (assoc m lid made-r) m)) resolved (map vector ready made))]
              (recur blocked resolved' (into out made))))))))

(defn- kind [item-id] (some-> (re-find #"^[A-Za-z]+" item-id) str))
(defn- attestation-rows [slice subject]
  (filter #(and (= :attestation (:record %)) (= subject (:subject %))) (:records slice)))
(defn catalog-record [slice p]
  (let [rows (:records slice) by-local (:by-local-id p)
        local-rows (into {} (keep #(when-let [id (:local-id %)] [id %]) rows))
        item-rows (filter #(= [:workplan/item 1] (:type-revision %)) rows)
        endpoint-local (fn [r role] (get-in (first (filter #(= role (:role %)) (:endpoints r))) [:target :local-id]))
        summaries
        (mapv (fn [item]
                (let [lid (:local-id item) id (get-in item [:identity :item/id]) atts (attestation-rows slice lid)
                      rels (filter #(= lid (endpoint-local % :item)) rows)
                      serves (filter #(= [:workplan/serves 1] (:type-revision %)) rels)
                      names (filter #(= [:workplan/names 1] (:type-revision %)) rels)
                      mentions (filter #(= [:workplan/mention 1] (:type-revision %)) rels)
                      assessments (filter #(and (= :assessment (:record %)) (= lid (:subject %))) rows)]
                  {:item/id id :item/kind (kind id)
                   :pending/level (some #(get-in % [:content :pending/level]) atts)
                   :story (some #(when (get-in % [:content :story/evidence-kind])
                                   {:content (:content %) :producer (:producer %) :basis (:basis %)}) atts)
                   :benefits (vec (mapcat (fn [rel]
                                            (let [blid (endpoint-local rel :benefit-line)
                                                  benefit (local-rows blid)
                                                  ba (first (attestation-rows slice blid))]
                                              (for [a (attestation-rows slice (:local-id rel))]
                                                {:benefit/line (get-in benefit [:identity :benefit/line])
                                                 :benefit/name (get-in ba [:content :benefit/name])
                                                 :producer (:producer a)}))) serves))
                   :entities (mapv (fn [rel] (let [elid (endpoint-local rel :entity) e (local-rows elid) a (first (attestation-rows slice elid))]
                                               {:entity/slug (get-in e [:identity :entity/slug]) :entity/name (get-in a [:content :entity/name])})) names)
                   :mention/count (count mentions)
                   :rulings (mapv #(select-keys % [:verdict :actor :policy :reason :time :source-refs]) assessments)
                   :record/id (get-in by-local [lid :id])})) item-rows)]
    (records/make-clusion {:type-revision [:workplan/catalog 1] :endpoints []
                           :asserted-content {:catalog/id "workplan/all-items" :items summaries}})))

(defn materialise! [dispatch! slice]
  (let [p (plan slice) run-id (random-uuid) started (System/nanoTime)
        batches (partition-all 100 (:records p))
        results (vec (mapcat (fn [batch]
                               (map (fn [r]
                                      (let [op (cond (:verdict r) :assessment/append (:subject-clusion-id r) :evidence/append :else :clusion/assert)
                                            response (dispatch! {:protocol/version "arxana.v1" :message/id (str "workplan:" run-id ":" (:id r))
                                                                 :operation op :principal/id "workplan/materialiser" :body {:record r}})]
                                        {:record/id (:id r) :operation op :response response})) batch)) batches))
        catalog (catalog-record slice p)
        catalog-response (dispatch! {:protocol/version "arxana.v1" :message/id (str "workplan:" run-id ":catalog")
                                     :operation :clusion/assert :principal/id "workplan/materialiser" :body {:record catalog}})
        failures (mapv (fn [{:keys [record/id operation response]}] {:record/id id :operation operation :error (:error response)})
                       (filter #(get-in % [:response :error]) results))]
    (assoc p :results results :catalog catalog :catalog/response catalog-response :failures failures
           :created (count (filter #(true? (get-in % [:response :ok :created?])) results))
           :counts {:records (count (:records p)) :clusions (count (filter :type-revision (:records p)))
                    :attestations (count (filter #(and (:subject-clusion-id %) (nil? (:verdict %))) (:records p)))
                    :assessments (count (filter :verdict (:records p)))
                    :items (count (filter #(= [:workplan/item 1] (:type-revision %)) (:records p)))}
           :elapsed-ms (/ (- (System/nanoTime) started) 1e6))))

(defn- ok! [r] (or (:ok r) (throw (ex-info "public read failed" {:response r}))))
(defn- entity [read! id] (:record (ok! (read! :entity/get {:entity/id id}))))
(defn- query [read! body] (ok! (read! :graph/query (assoc body :query/limit 200))))
(defn- reverse-ids [read! type role id]
  (:result/edge-ids (query read! {:query/type :role-reverse :edge/type type :endpoint/role role :endpoint/id id :time/order :asc})))
(defn- attestations [read! id]
  (mapv #(entity read! %) (:result/attestation-ids (query read! {:query/type :attestations :subject/id id :time/order :asc}))))
(defn- endpoint-id [r role] (get-in (first (filter #(= role (:role %)) (:endpoints r))) [:target :clusion/id]))

(defn item-view [read! item-id]
  (let [item (records/make-clusion {:type-revision [:workplan/item 1] :endpoints [] :asserted-content {:item/id item-id}})
        item (entity read! (:id item))
        edges (fn [t] (mapv #(entity read! %) (reverse-ids read! t :item (:id item))))
        mentions (mapv (fn [r] (let [a (first (attestations read! (:id r))) c (:content a)]
                                  {:record/id (:id r) :record r :attestation a :role (get-in r [:asserted-content :mention/role])
                                   :author (:author c) :time (:time c) :evidence/id (:evidence/id c)
                                   :quote (:quote c) :codepoint-range (:codepoint-range c)
                                   :record/ids [(:id r) (:id a)]})) (edges [:workplan/mention 1]))
        names (mapv (fn [r] (let [e (entity read! (endpoint-id r :entity)) a (first (attestations read! (:id e)))]
                              {:record/id (:id r) :entity e :attestation a :draft? (#{:model :parser} (:basis a))
                               :record/ids [(:id r) (:id e) (:id a)]})) (edges [:workplan/names 1]))
        serves (vec (mapcat (fn [r] (let [b (entity read! (endpoint-id r :benefit-line)) bas (attestations read! (:id b))]
                                      (for [a (attestations read! (:id r))]
                                        {:record/id (:id r) :benefit-line b :benefit/attestations bas :attestation a
                                         :producer (:producer a) :draft? (#{:model :parser} (:basis a))
                                         :record/ids (vec (concat [(:id r) (:id b) (:id a)] (map :id bas)))}))) (edges [:workplan/serves 1])))
        item-atts (attestations read! (:id item))
        assessments (:result/assessments (query read! {:query/type :assessments :subject/id (:id item) :time/order :asc}))]
    {:result/status :complete :item item
     :attestations {:provenance (filterv #(= :exact (:basis %)) item-atts)
                    :drafts (filterv #(#{:model :parser} (:basis %)) item-atts)}
     :mentions (vec (sort-by :time mentions)) :names names :serves serves
     :assessments (vec assessments) :standing (if (seq assessments) :assessed "no ruling by Joe")
     :record/ids (vec (distinct (concat [(:id item)] (mapcat :record/ids mentions) (mapcat :record/ids names) (mapcat :record/ids serves))))}))

(defn items-view [read! filters]
  (let [catalog-id (:id (records/make-clusion {:type-revision [:workplan/catalog 1] :endpoints []
                                                :asserted-content {:catalog/id "workplan/all-items"}}))
        catalog (entity read! catalog-id) all (get-in catalog [:asserted-content :items])
        selected (filterv (fn [row]
                            (and (or (nil? (:pending/level filters)) (= (:pending/level filters) (:pending/level row)))
                                 (or (nil? (:benefit/line filters)) (some #(= (:benefit/line filters) (:benefit/line %)) (:benefits row)))
                                 (or (nil? (:entity/slug filters)) (some #(= (:entity/slug filters) (:entity/slug %)) (:entities row)))
                                 (or (nil? (:item/kind filters)) (= (:item/kind filters) (:item/kind row))))) all)
        total (count selected) limit (min 200 (max 1 (long (or (:query/limit filters) 100))))
        offset (max 0 (long (or (:query/offset filters) 0))) items (vec (take limit (drop offset selected)))
        truncated? (< (+ offset (count items)) total)]
    (cond-> {:result/status (if truncated? :truncated :complete) :result/total total
             :query/offset offset :query/limit limit :items items :record/ids [catalog-id]}
      truncated? (assoc :result/truncation :limit))))
