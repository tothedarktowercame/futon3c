(ns futon3c.arxana.pattern-proposals
  "Stored 驴 proposals: one atomic turn batch and a ranked reverse-index reader."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.arxana.digest :as digest]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.store :as store]
            [futon3c.arxana.store-http :as http]
            [futon3c.arxana.turn-mention-writer :as mentions]
            [futon3c.xiang.turn-store :as turns]))

(def default-library "/home/joe/code/futon3/library")

(defn pattern-reference [library id]
  (let [id (str/replace-first (str id) #"^~" "")
        f (io/file library (str id ".flexiarg"))]
    (when (.isFile f)
      (records/make-external-reference
       {:ref/kind :pattern-file :store/id id :store/endpoint (str library)
        :record/id (str id ".flexiarg") :schema/authority :futon3/pattern-library
        :valid-time "content-addressed"
        :revision/digest (digest/sha256-hex (java.nio.file.Files/readAllBytes (.toPath f)))}))))

(defn plan [cfg turn lu]
  (if-let [turn-ref (:reference (mentions/field-evidence cfg turn :source_text))]
    (let [skipped (->> (:ranked lu)
                       (remove #(pattern-reference (or (:pattern-library cfg) default-library) (:id %)))
                       (mapv :id))
          rows (mapcat
                (fn [{:keys [id rank quote]}]
                  (when-let [pattern-ref (pattern-reference (or (:pattern-library cfg) default-library) id)]
                    (let [p (records/make-clusion
                             {:type-revision [:pattern/proposal 1]
                              :endpoints [{:role :turn :kind :external-ref :target {:ref/id (:id turn-ref)}}
                                          {:role :pattern :kind :external-ref :target {:ref/id (:id pattern-ref)}}]
                              :asserted-content {:intent "lu-rank"}})
                          a (records/make-attestation
                             {:subject-clusion-id (:id p) :producer "驴" :basis :model
                              :standing :unverified :coverage :complete
                              :method {:name :lu-ranked :version (or (:version lu) 1)}
                              :content {:rank rank :quote quote :source (:source lu)
                                        :version (or (:version lu) 1) :at (:at lu)}
                              :source-refs []})]
                      [{:operation :external-reference/assert :record turn-ref}
                       {:operation :external-reference/assert :record pattern-ref}
                       {:operation :clusion/assert :record p}
                       {:operation :evidence/append :record a}])))
                (:ranked lu))]
      {:rows (vec rows) :turn-reference turn-ref :skipped skipped})
    {:failed :operator-evidence-unavailable}))

;; futon1b replays a repeated message id as the earlier result (:ok, nothing
;; created), so the id carries the batch's record ids: a changed batch for
;; the same turn is a new message, an identical one stays a no-op.
(defn batch-message-id [prefix turn rows]
  (str prefix (:id turn) "/"
       (digest/sha256-hex (.getBytes (str/join "," (map #(get-in % [:record :id]) rows)) "UTF-8"))))

(defn write! [cfg turn lu]
  (let [{:keys [rows failed skipped] :as p} (plan cfg turn lu)]
    (cond failed p
          (empty? rows) {:failed :patterns-unavailable}
          :else (let [r (http/append-batch! (:graph-store cfg)
                                             (batch-message-id "pattern-proposals/" turn rows) rows)]
                  (if (:ok r) {:written (quot (count rows) 4) :skipped skipped}
                      {:failed :proposal-write-refused})))))

(defn read-ranked [cfg turn]
  (if-let [ref (:reference (mentions/field-evidence cfg turn :source_text))]
    (let [q (:ok (store/bounded-query! (:graph-store cfg)
                                       {:query/type :role-reverse :edge-type [:pattern/proposal 1]
                                        :endpoint-role :turn :endpoint-id (:id ref)
                                        :limit 200 :order :asc}))
          ps (mapv #(-> (store/get-entity (:graph-store cfg) %) :ok) (:edge-ids q))]
      (if (empty? ps) {:pending :lu-proposals-pending}
          ;; futon1b's :attestations query reads :subject-id and :limit and
          ;; answers :attestation-ids (futon1b_server.clj, arxana query route).
          (let [ranked (mapv (fn [p]
                               (let [aq (:ok (store/bounded-query! (:graph-store cfg)
                                                                  {:query/type :attestations
                                                                   :subject-id (:id p) :limit 20}))
                                     aid (first (:attestation-ids aq))
                                     a (when aid (:ok (store/get-entity (:graph-store cfg) aid)))
                                     ref-id (get-in p [:endpoints 1 :target :ref/id])
                                     pattern (get-in (store/get-entity (:graph-store cfg) ref-id) [:ok :store/id])]
                                 [(get-in a [:content :rank]) pattern]))
                             ps)]
            ;; A proposal whose rank cannot be read gives no order at all:
            ;; report it pending rather than guess one.
            (if (some (comp not number? first) ranked)
              {:pending :lu-proposal-rank-unreadable}
              {:ids (->> ranked (sort-by first) (mapv second))}))))
    {:pending :lu-proposals-pending}))

(defn backfill! [cfg {:keys [write? pause-ms] :or {write? false pause-ms 200}}]
  (let [cutoff (- (System/currentTimeMillis) (* 6 3600000))
        rows (->> (turns/list-records (:turn-store cfg) :limit Long/MAX_VALUE)
                  (keep (fn [{:keys [id record]}]
                          (let [at (some-> (:created_at record) str java.time.Instant/parse .toEpochMilli)]
                            (when (and at (< cutoff at) (= "operator" (:origin record)) (:lu_ranked record))
                              (assoc record :id id))))))
        outcomes (mapv (fn [turn]
                         (let [existing (read-ranked cfg turn)]
                           (if-not (:pending existing) :already-written
                             (if write?
                               (let [r (write! cfg turn (:lu_ranked turn))]
                                 (when (pos? pause-ms) (Thread/sleep pause-ms))
                                 (if (:written r) :written (:failed r)))
                               (if (:rows (plan cfg turn (:lu_ranked turn))) :would-write
                                   :operator-evidence-unavailable))))) rows)]
    {:mode (if write? :write :dry-run) :turns (count rows) :outcomes (frequencies outcomes)}))
