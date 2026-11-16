(ns futon3c.arxana.pattern-occurrences
  "Stored 馬 occurrence verdicts and bounded GCC readers."
  (:require [clojure.string :as str]
            [futon3c.arxana.pattern-proposals :as proposals]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.store :as store]
            [futon3c.arxana.store-http :as http]
            [futon3c.arxana.turn-mention-writer :as mentions]
            [futon3c.xiang.turn-store :as turns]))

(defn- pending-verdict? [verdict]
  (= :lu-proposals-pending (some-> (:state verdict) keyword)))

(defn plan [cfg turn verdict]
  (cond
    (pending-verdict? verdict) {:failed :verdict-pending}
    (not (seq (:patterns verdict))) {:failed :verdict-unavailable}
    :else
    (if-let [turn-ref (:reference (mentions/field-evidence cfg turn :source_text))]
      (let [library (or (:pattern-library cfg) proposals/default-library)
            planned (mapv
                     (fn [row]
                       (let [ref (proposals/pattern-reference library (:pattern row))]
                         (if (and ref (or (str/blank? (str (:sha256 row)))
                                          (= (:sha256 row) (:revision/digest ref))))
                           {:row row :ref ref}
                           {:skipped (:pattern row)})))
                     (:patterns verdict))
            skipped (mapv :skipped (filter :skipped planned))
            written-at (or (:at verdict) (str (java.time.Instant/now)))
            rows (mapcat
                  (fn [{:keys [row ref]}]
                    (when row
                      (let [claim (records/make-clusion
                                   {:type-revision [:pattern/occurrence 1]
                                    :endpoints [{:role :pattern :kind :external-ref
                                                 :target {:ref/id (:id ref)}}
                                                {:role :turn :kind :external-ref
                                                 :target {:ref/id (:id turn-ref)}}]
                                    :asserted-content
                                    {:window (vec (:window verdict))
                                     :proposal (vec (:sources row))}})
                            att (records/make-attestation
                                 {:subject-clusion-id (:id claim)
                                  :producer (str (or (:reader verdict) "馬"))
                                  :basis :model :standing :unverified :coverage :complete
                                  :method {:id "ma/occurrence" :version (or (:version verdict) 1)}
                                  :content {:status (:status row)
                                            :turn_mapped (:turn_mapped row)
                                            :chain_mapped (:chain_mapped row)
                                            :turn (:turn row) :chain (:chain row)
                                            :shown (:shown row) :sources (:sources row)
                                            :written-at written-at}
                                  :source-refs [{:evidence/id (:evidence_id turn)}]})]
                        [{:operation :external-reference/assert :record turn-ref}
                         {:operation :external-reference/assert :record ref}
                         {:operation :clusion/assert :record claim}
                         {:operation :evidence/append :record att}])))
                  planned)]
        {:rows (vec rows) :written (count (filter :row planned)) :skipped skipped})
      {:failed :operator-evidence-unavailable})))

(defn write! [cfg turn verdict]
  (let [{:keys [rows written failed skipped] :as p} (plan cfg turn verdict)]
    (cond failed p
          (zero? written) {:failed :patterns-unavailable :skipped skipped}
          :else (let [r (http/append-batch!
                         (:graph-store cfg)
                         (proposals/batch-message-id "pattern-occurrences/" turn rows) rows)]
                  (if (:ok r) {:written written :skipped skipped}
                      {:failed :occurrence-write-refused})))))

(defn- latest-attestation [cfg claim]
  (let [q (:ok (store/bounded-query! (:graph-store cfg)
                                      {:query/type :attestations
                                       :subject-id (:id claim) :limit 50}))]
    (->> (:attestation-ids q)
         (keep #(-> (store/get-entity (:graph-store cfg) %) :ok))
         (sort-by #(str (get-in % [:content :written-at])))
         last)))

(defn read-turn [cfg turn]
  (if-let [turn-ref (:reference (mentions/field-evidence cfg turn :source_text))]
    (let [q (:ok (store/bounded-query! (:graph-store cfg)
                                       {:query/type :role-reverse
                                        :edge-type [:pattern/occurrence 1]
                                        :endpoint-role :turn :endpoint-id (:id turn-ref)
                                        :limit 200 :order :asc}))
          rows (keep (fn [id]
                       (when-let [claim (:ok (store/get-entity (:graph-store cfg) id))]
                         (let [att (latest-attestation cfg claim)
                               ref-id (get-in claim [:endpoints 0 :target :ref/id])
                               pattern (get-in (store/get-entity (:graph-store cfg) ref-id)
                                               [:ok :store/id])]
                           (when att {:pattern pattern
                                      :status (get-in att [:content :status])
                                      :written-at (get-in att [:content :written-at])}))))
                     (:edge-ids q))]
      {:rows (->> rows
                  (group-by :pattern)
                  (map (fn [[_ xs]] (last (sort-by #(str (:written-at %)) xs))))
                  vec)})
    {:pending :operator-evidence-unavailable}))

(defn backfill! [cfg {:keys [write? pause-ms] :or {write? false pause-ms 200}}]
  (let [cutoff (- (System/currentTimeMillis) (* 6 3600000))
        candidates (->> (turns/list-records (:turn-store cfg) :limit Long/MAX_VALUE)
                        (keep (fn [{:keys [id record]}]
                                (let [at (some-> (:created_at record) str
                                                 java.time.Instant/parse .toEpochMilli)]
                                  (when (and at (< cutoff at) (= "operator" (:origin record))
                                             (:pattern_verdicts record)
                                             (not (pending-verdict? (:pattern_verdicts record))))
                                    (assoc record :id id))))))
        outcomes (mapv
                  (fn [turn]
                    (if (seq (:rows (read-turn cfg turn))) :already-written
                      (if-not write?
                        (let [p (plan cfg turn (:pattern_verdicts turn))]
                          (if (:failed p) (:failed p) :would-write))
                        (let [result (write! cfg turn (:pattern_verdicts turn))]
                          (turns/update-record!
                           (:turn-store cfg) (:id turn)
                           #(if (:written (:pattern_occurrences %)) %
                                (assoc % :pattern_occurrences result)))
                          (when (pos? pause-ms) (Thread/sleep pause-ms))
                          (if (:written result) :written (:failed result))))))
                  candidates)]
    {:mode (if write? :write :dry-run) :turns (count candidates)
     :outcomes (frequencies outcomes)}))
