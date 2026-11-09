(ns futon3c.arxana.workplan
  "Materialise generated workplan slices and project one item through public ports."
  (:require [clojure.edn :as edn]
            [clojure.walk :as walk]
            [futon3c.arxana.records :as records]))

(def default-slice "/home/joe/notes/cascade-live-outline/arxana/t-pattern-search-slice.edn")

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
                                                     (assoc :subject-clusion-id (get-in resolved [(:subject r) :id]))))))) ready)
                  resolved' (reduce (fn [m [r made-r]] (if-let [lid (:local-id r)] (assoc m lid made-r) m)) resolved (map vector ready made))]
              (recur blocked resolved' (into out made))))))))

(defn materialise! [dispatch! slice]
  (let [p (plan slice) run-id (random-uuid)
        results (mapv (fn [r]
                        (dispatch! {:protocol/version "arxana.v1" :message/id (str "workplan:" run-id ":" (:id r))
                                    :operation (if (:subject-clusion-id r) :evidence/append :clusion/assert)
                                    :principal/id "workplan/materialiser" :body {:record r}})) (:records p))]
    (assoc p :results results :created (count (filter #(true? (get-in % [:ok :created?])) results)))))

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
        serves (mapv (fn [r] (let [b (entity read! (endpoint-id r :benefit-line)) a (first (attestations read! (:id r)))]
                               {:record/id (:id r) :benefit-line b :attestation a :draft? (#{:model :parser} (:basis a))
                                :record/ids [(:id r) (:id b) (:id a)]})) (edges [:workplan/serves 1]))
        item-atts (attestations read! (:id item))
        assessments (:result/assessments (query read! {:query/type :assessments :subject/id (:id item) :time/order :asc}))]
    {:result/status :complete :item item
     :attestations {:provenance (filterv #(= :exact (:basis %)) item-atts)
                    :drafts (filterv #(#{:model :parser} (:basis %)) item-atts)}
     :mentions (vec (sort-by :time mentions)) :names names :serves serves
     :assessments (vec assessments) :standing (if (seq assessments) :assessed "no ruling by Joe")
     :record/ids (vec (distinct (concat [(:id item)] (mapcat :record/ids mentions) (mapcat :record/ids names) (mapcat :record/ids serves))))}))
