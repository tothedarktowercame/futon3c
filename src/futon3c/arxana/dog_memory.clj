(ns futon3c.arxana.dog-memory
  "狗 recall projection: rendered turn recalls to GCC records and back."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [futon3c.arxana.digest :as digest]
            [futon3c.arxana.pattern-proposals :as proposals]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.store :as store]
            [futon3c.arxana.store-http :as http]
            [futon3c.arxana.turn-mention-writer :as mentions]
            [futon3c.xiang.turn-store :as turns])
  (:import [java.net URI URLEncoder]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers
            HttpResponse$BodyHandlers]
           [java.time Duration]))

(def default-memory-base
  (or (System/getProperty "FUTON_MEMORY_BASE")
      (System/getenv "FUTON_MEMORY_BASE") "http://127.0.0.1:7073"))

(defonce ^:private memory-http-client
  (-> (HttpClient/newBuilder) (.connectTimeout (Duration/ofSeconds 2)) .build))

(defn- http-memory-lookup [base handle]
  (let [slug (peek (str/split (str handle) #"/"))
        url (str (str/replace (str base) #"/$" "")
                 "/api/alpha/evidence/text-search?q="
                 (URLEncoder/encode slug "UTF-8")
                 "&limit=50&scope=all&tags=source%2Fclaude-code")
        req (-> (HttpRequest/newBuilder (URI/create url))
                (.timeout (Duration/ofSeconds 5))
                (.header "Accept" "application/json")
                (.method "GET" (HttpRequest$BodyPublishers/noBody)) .build)
        resp (.send memory-http-client req (HttpResponse$BodyHandlers/ofString))]
    (when (<= 200 (.statusCode resp) 299)
      (let [body (json/parse-string (.body resp) true)]
        (or (:results body) (get-in body [:response :results]))))))

(defn memory-reference
  "Resolve HANDLE (project/slug) through its existing cc: memory subject.
  Returns an Arxana external reference pinned to the current evidence version,
  or nil; it never invents an unresolved memory."
  [cfg handle]
  (let [cc-id (str "cc:" handle)
        lookup (or (:memory-lookup cfg) http-memory-lookup)
        results (lookup (or (:memory-base cfg) default-memory-base) handle)
        entry (->> results (map :entry)
                   (filter #(and (= "memory" (some-> (get-in % [:evidence/subject :ref/type]) name))
                                 (= cc-id (get-in % [:evidence/subject :ref/id]))))
                   (sort-by #(str (:evidence/at %))) last)]
    (when-let [version (:evidence/id entry)]
      (with-meta
        (records/make-external-reference
         {:ref/kind :memory-version :store/id cc-id
          :store/endpoint (or (:memory-base cfg) default-memory-base)
          :record/id cc-id :schema/authority :futon1b/memory
          :valid-time (or (:evidence/at entry) "unknown")
          :revision/digest (digest/sha256-hex (.getBytes (str version) "UTF-8"))})
        {:memory/version-id version}))))

(defn- resolve-memory [cfg handle retry?]
  (or (memory-reference cfg handle)
      (when retry?
        (Thread/sleep (long (or (:memory-resolution-retry-ms cfg) 200)))
        (memory-reference cfg handle))))

(defn- turn-and-memory-rows [turn-ref memory-ref type content att]
  (let [claim (records/make-clusion
               {:type-revision type
                :endpoints [{:role :turn :kind :external-ref
                             :target {:ref/id (:id turn-ref)}}
                            {:role :memory :kind :external-ref
                             :target {:ref/id (:id memory-ref)}}]
                :asserted-content content})]
    [{:operation :external-reference/assert :record turn-ref}
     {:operation :external-reference/assert :record memory-ref}
     {:operation :clusion/assert :record claim}
     {:operation :evidence/append
      :record (records/make-attestation
               (assoc att :subject-clusion-id (:id claim)))}]))

(defn plan [cfg turn recall]
  (cond
    (not (seq (:items recall)))
    {:rows [] :written 0 :skipped [] :status (or (:status recall) "none")}

    :else
    (if-let [turn-ref (:reference (mentions/field-evidence cfg turn :source_text))]
      (let [delivered? (not= false (:delivered recall))
            planned (mapv (fn [rank item]
                            (if-let [memory-ref (memory-reference cfg (:memory item))]
                              {:rank (inc rank) :item item :memory-ref memory-ref}
                              {:skipped (:memory item)}))
                          (range) (:items recall))
            written-at (or (:at recall) (str (java.time.Instant/now)))
            rows (mapcat
                  (fn [{:keys [rank item memory-ref]}]
                    (when item
                      (let [claim (records/make-clusion
                                   {:type-revision [:memory/recall 1]
                                    :endpoints [{:role :turn :kind :external-ref
                                                 :target {:ref/id (:id turn-ref)}}
                                                {:role :memory :kind :external-ref
                                                 :target {:ref/id (:id memory-ref)}}]
                                    :asserted-content {:arm (:arm recall)
                                                       :delivered delivered?}})
                            att (records/make-attestation
                                 {:subject-clusion-id (:id claim) :producer "狗" :basis :model
                                  :standing :unverified :coverage :complete
                                  :method {:id "dog/recall" :version 1}
                                  :content {:rank rank :score (:score item) :why (:why item)
                                            :query-source (:query-source recall)
                                            :query-ms (:query-ms recall)
                                            :memory (:memory item)
                                            :written-at written-at
                                            :delivered delivered?}
                                  :source-refs [{:evidence/id (:evidence_id turn)}]})]
                        [{:operation :external-reference/assert :record turn-ref}
                         {:operation :external-reference/assert :record memory-ref}
                         {:operation :clusion/assert :record claim}
                         {:operation :evidence/append :record att}])))
                  planned)]
        {:rows (vec rows) :written (count (filter :item planned))
         :skipped (mapv :skipped (filter :skipped planned))})
      {:failed :operator-evidence-unavailable})))

(defn write! [cfg turn recall]
  (let [{:keys [rows written failed skipped status] :as p} (plan cfg turn recall)]
    (cond failed p
          (zero? written) {:written 0 :skipped skipped :status status}
          :else
          (let [result (http/append-batch!
                        (:graph-store cfg)
                        (proposals/batch-message-id "dog-recall/" turn rows) rows)]
            (if (:ok result) {:written written :skipped skipped}
                {:failed :recall-write-refused :skipped skipped})))))

(defn write-turn! [cfg store turn-id]
  (let [turn (assoc (turns/read-record store turn-id) :id turn-id)
        result (try (write! cfg turn (:dog_recall turn))
                    (catch Throwable e {:failed :recall-write-exception
                                        :detail (.getMessage e)}))]
    (turns/update-record!
     store turn-id
     ;; Only a write of real items is final: a not-ready first render records
     ;; {:written 0}, and a later render that showed items must replace it.
     #(if (pos? (or (get-in % [:dog_recall_gcc :written]) 0)) %
          (assoc % :dog_recall_gcc result)))
    result))

(defn write-late-turn! [cfg store turn-id]
  (let [turn (assoc (turns/read-record store turn-id) :id turn-id)
        result (try (write! cfg turn (:dog_recall_late turn))
                    (catch Throwable e {:failed :recall-write-exception
                                        :detail (.getMessage e)}))]
    (turns/update-record!
     store turn-id
     #(if (pos? (or (get-in % [:dog_recall_late_gcc :written]) 0)) %
          (assoc % :dog_recall_late_gcc result)))
    result))

(defn use-plan [cfg turn use]
  (cond
    (not= "ok" (:status use))
    {:rows [] :written 0 :skipped [] :status (or (:status use) "pending")}

    :else
    (if-let [turn-ref (:reference (mentions/field-evidence cfg turn :source_text))]
      (let [stamp (or (:at use) (str (java.time.Instant/now)))
            planned (mapv (fn [item]
                            (if-let [ref (resolve-memory cfg (:memory item) false)]
                              {:item item :memory-ref ref}
                              {:skipped (:memory item)}))
                          (:items use))
            rows (mapcat
                  (fn [{:keys [item memory-ref]}]
                    (when item
                      (turn-and-memory-rows
                       turn-ref memory-ref [:memory/use 1] {}
                       {:producer "狗" :basis :model :standing :unverified
                        :coverage :complete
                        :method {:id "dog/use-check" :version 1
                                 :parameters {:echo-min-shared 2}}
                        :content {:memory (:memory item) :used (boolean (:used item))
                                  :kind (:kind item)
                                  ;; Public GCC: retain the structured class,
                                  ;; never the reply-derived explanatory prose.
                                  :evidence {:label (keyword (:kind item))}
                                  :written-at stamp}
                        :source-refs [{:evidence/id (:evidence_id turn)}]}))) planned)]
        {:rows (vec rows) :written (count (filter :item planned))
         :skipped (mapv :skipped (filter :skipped planned))})
      {:failed :operator-evidence-unavailable})))

(defn write-use! [cfg turn use]
  (let [{:keys [rows written failed skipped status] :as p} (use-plan cfg turn use)]
    (cond failed p
          (zero? written) {:written 0 :skipped skipped :status status}
          :else (let [r (http/append-batch! (:graph-store cfg)
                                            (proposals/batch-message-id "dog-use/" turn rows) rows)]
                  (if (:ok r) {:written written :skipped skipped}
                      {:failed :use-write-refused :skipped skipped})))))

(defn- burial-plan [cfg turn buried]
  (if-let [turn-ref (:reference (mentions/field-evidence cfg turn :source_text))]
    (let [stamp (or (:at buried) (str (java.time.Instant/now)))
          burials (for [item (:items buried) :when (= "buried" (:status item))]
                    (if-let [ref (resolve-memory cfg (str "狗/" (:slug item)) true)]
                      {:kind :burial :item item :memory-ref ref}
                      {:skipped (str "狗/" (:slug item))}))
          repetitions (mapcat
                       (fn [[event-index event]]
                         (for [handle (:memory-ids event)]
                           (if-let [ref (resolve-memory cfg handle false)]
                             {:kind :repetition :event event :event-index event-index
                              :handle handle :memory-ref ref}
                             {:skipped handle})))
                       (map-indexed vector (:said-again buried)))
          planned (vec (concat burials repetitions))
          raw-rows (mapcat
                (fn [{:keys [kind item event event-index handle memory-ref]}]
                  (case kind
                    :burial
                    (turn-and-memory-rows
                     turn-ref memory-ref [:memory/burial 1] {}
                     {:producer "狗" :basis :exact :standing :verified :coverage :complete
                      :method {:id "dog/bury" :version 1}
                      :content {:memory (str "狗/" (:slug item))
                                :source-act-id (get-in item [:basis :act-id])
                                :range (get-in item [:basis :range])
                                :memory-version-id (:memory/version-id (meta memory-ref))
                                :written-at stamp}
                      :source-refs [{:evidence/id (:evidence_id turn)}]})
                    :repetition
                    (turn-and-memory-rows
                     turn-ref memory-ref [:memory/repetition 1] {}
                     {:producer "狗" :basis :model :standing :unverified :coverage :complete
                      :method {:id "dog/repetition" :version 1}
                      :content {:memory handle :source-act-id (:act event)
                                :range (:range event) :classifier-version 1
                                :shared-bigram-count (count (:shared event))
                                :event-index event-index :written-at stamp}
                      :source-refs [{:evidence/id (:evidence_id turn)}]})
                    [])) planned)
          ;; Two said-again events can share one (turn,memory) clusion. Assert
          ;; that shared claim/reference once, but retain every attestation.
          rows (reduce (fn [{:keys [seen out] :as acc} row]
                         (let [dedupe? (not= :evidence/append (:operation row))
                               k [(:operation row) (get-in row [:record :id])]]
                           (if (and dedupe? (contains? seen k)) acc
                             {:seen (cond-> seen dedupe? (conj k))
                              :out (conj out row)})))
                       {:seen #{} :out []} raw-rows)]
      {:rows (:out rows)
       :written (count (filter :kind planned))
       :burials (count (filter #(= :burial (:kind %)) planned))
       :repetitions (count (filter #(= :repetition (:kind %)) planned))
       :skipped (mapv :skipped (filter :skipped planned))})
    {:failed :operator-evidence-unavailable}))

(defn write-buried! [cfg turn buried]
  (let [{:keys [rows written failed skipped burials repetitions] :as p}
        (burial-plan cfg turn buried)]
    (cond failed p
          (zero? written) {:written 0 :burials 0 :repetitions 0 :skipped skipped
                           :status (or (:status buried) "none")}
          :else (let [r (http/append-batch! (:graph-store cfg)
                                            (proposals/batch-message-id "dog-buried/" turn rows) rows)]
                  (if (:ok r) {:written written :burials burials
                               :repetitions repetitions :skipped skipped}
                      {:failed :buried-write-refused :skipped skipped})))))

(defn- write-output-turn! [cfg turn-store turn-id field outcome-field writer]
  (let [turn (assoc (turns/read-record turn-store turn-id) :id turn-id)
        result (try (writer cfg turn (get turn field))
                    (catch Throwable e {:failed :dog-memory-write-exception
                                        :detail (.getMessage e)}))]
    (turns/update-record!
     turn-store turn-id
     #(if (pos? (or (get-in % [outcome-field :written]) 0)) %
          (assoc % outcome-field result)))
    result))

(defn write-use-turn! [cfg turn-store turn-id]
  (write-output-turn! cfg turn-store turn-id :dog_use :dog_use_gcc write-use!))

(defn write-buried-turn! [cfg turn-store turn-id]
  (write-output-turn! cfg turn-store turn-id :dog_buried :dog_buried_gcc write-buried!))

(defn- latest-attestation [cfg claim]
  (let [q (:ok (store/bounded-query! (:graph-store cfg)
                                      {:query/type :attestations
                                       :subject-id (:id claim) :limit 50}))]
    (->> (:attestation-ids q)
         (keep #(-> (store/get-entity (:graph-store cfg) %) :ok))
         (sort-by #(str (get-in % [:content :written-at]))) last)))

(defn read-recall [cfg turn]
  (if-let [turn-ref (:reference (mentions/field-evidence cfg turn :source_text))]
    (let [q (:ok (store/bounded-query! (:graph-store cfg)
                                       {:query/type :role-reverse
                                        :edge-type [:memory/recall 1]
                                        :endpoint-role :turn :endpoint-id (:id turn-ref)
                                        :limit 200 :order :asc}))
          rows (keep (fn [id]
                       (when-let [claim (:ok (store/get-entity (:graph-store cfg) id))]
                         (when-not (= false (get-in claim [:asserted-content :delivered]))
                           (when-let [att (latest-attestation cfg claim)]
                             {:claim claim :att att
                              :written-at (get-in att [:content :written-at])}))))
                     (:edge-ids q))]
      (if (empty? rows) {:status "no-recall" :items []}
        ;; A re-render supersedes the earlier exposure set. Keep only claims
        ;; attested at the newest render stamp; never merge two renders.
        (let [stamp (:written-at (last (sort-by :written-at rows)))
              selected (filter #(= stamp (:written-at %)) rows)]
          {:status "ok" :at stamp
           :items (->> selected
                       (map (fn [{:keys [claim att]}]
                              {:memory (get-in att [:content :memory])
                               :rank (get-in att [:content :rank])
                               :score (get-in att [:content :score])
                               :why (get-in att [:content :why])
                               :arm (get-in claim [:asserted-content :arm])}))
                       (sort-by :rank) vec)})))
    {:status "no-recall" :items []}))

(defn- claim-attestations [cfg claim]
  (let [q (:ok (store/bounded-query! (:graph-store cfg)
                                      {:query/type :attestations
                                       :subject-id (:id claim) :limit 100}))]
    (->> (:attestation-ids q)
         (keep #(-> (store/get-entity (:graph-store cfg) %) :ok)))))

(defn- read-memory-claims [cfg turn type]
  (if-let [turn-ref (:reference (mentions/field-evidence cfg turn :source_text))]
    (let [q (:ok (store/bounded-query! (:graph-store cfg)
                                       {:query/type :role-reverse :edge-type type
                                        :endpoint-role :turn :endpoint-id (:id turn-ref)
                                        :limit 500 :order :asc}))]
      (keep (fn [id]
              (when-let [claim (:ok (store/get-entity (:graph-store cfg) id))]
                {:claim claim :attestations (claim-attestations cfg claim)}))
            (:edge-ids q)))
    []))

(defn read-use [cfg turn]
  (let [rows (for [{:keys [claim attestations]} (read-memory-claims cfg turn [:memory/use 1])
                   :let [att (last (sort-by #(str (get-in % [:content :written-at])) attestations))]
                   :when att]
               {:claim claim :att att :stamp (str (get-in att [:content :written-at]))})]
    (if (empty? rows) {:status "no-use" :items []}
      (let [stamp (:stamp (last (sort-by :stamp rows)))]
        {:status "ok" :at stamp
         :items (->> rows (filter #(= stamp (:stamp %)))
                     (mapv #(select-keys (:content (:att %))
                                         [:memory :used :kind :evidence])))}))))

(defn read-burials [cfg turn]
  (let [rows (for [{:keys [attestations]} (read-memory-claims cfg turn [:memory/burial 1])
                   :let [att (last (sort-by #(str (get-in % [:content :written-at])) attestations))]
                   :when att] (:content att))]
    {:status (if (seq rows) "ok" "no-burials")
     :items (vec (sort-by #(str (:written-at %)) rows))}))

(defn read-repetitions [cfg turn]
  (let [rows (mapcat
              (fn [{:keys [attestations]}]
                (let [stamped (group-by #(str (get-in % [:content :written-at])) attestations)
                      newest (last (sort (keys stamped)))]
                  (map :content (get stamped newest))))
              (read-memory-claims cfg turn [:memory/repetition 1]))]
    {:status (if (seq rows) "ok" "no-repetitions")
     :items (vec (sort-by (juxt #(str (:written-at %)) :event-index) rows))}))

(defn backfill! [cfg {:keys [write? pause-ms]
                      :or {write? false pause-ms 200}}]
  (let [cutoff (.toEpochMilli (java.time.Instant/parse "2026-11-11T00:00:00Z"))
        candidates (->> (turns/list-records (:turn-store cfg) :limit Long/MAX_VALUE)
                        ;; list-records is newest first; no ordering assumption.
                        (keep (fn [{:keys [id record]}]
                                (let [at (some-> (:created_at record) str
                                                 java.time.Instant/parse .toEpochMilli)]
                                  (when (and at (<= cutoff at)
                                             (seq (get-in record [:dog_recall :items])))
                                    (assoc record :id id))))))
        outcomes (mapv
                  (fn [turn]
                    (if (seq (:items (read-recall cfg turn))) :already-written
                      (if-not write? :would-write
                        (let [r (write-turn! cfg (:turn-store cfg) (:id turn))]
                          (when (pos? pause-ms) (Thread/sleep pause-ms))
                          (if (:written r) :written (:failed r)))))) candidates)]
    {:mode (if write? :write :dry-run) :turns (count candidates)
     :outcomes (frequencies outcomes)}))

(defn backfill-remaining! [cfg {:keys [write? pause-ms]
                                :or {write? false pause-ms 200}}]
  (let [cutoff (.toEpochMilli (java.time.Instant/parse "2026-11-11T00:00:00Z"))
        turns (->> (turns/list-records (:turn-store cfg) :limit Long/MAX_VALUE)
                   (keep (fn [{:keys [id record]}]
                           (let [at (try (some-> (:created_at record) str
                                                java.time.Instant/parse .toEpochMilli)
                                         (catch Throwable _ nil))]
                             (when (and at (<= cutoff at)) (assoc record :id id))))))
        run (fn [kind candidates reader writer]
              (let [outcomes
                    (mapv (fn [turn]
                            (if (seq (:items (reader cfg turn))) :already-written
                              (if-not write? :would-write
                                (let [r (writer cfg (:turn-store cfg) (:id turn))]
                                  (when (pos? pause-ms) (Thread/sleep pause-ms))
                                  (if (pos? (or (:written r) 0)) :written
                                      (or (:failed r) :empty)))))) candidates)]
                {:type kind :turns (count candidates) :outcomes (frequencies outcomes)}))
        uses (filter #(and (= "ok" (get-in % [:dog_use :status]))
                           (seq (get-in % [:dog_use :items]))) turns)
        burials (filter #(some (fn [item] (= "buried" (:status item)))
                               (get-in % [:dog_buried :items])) turns)
        repetitions (filter #(some (comp seq :memory-ids)
                                    (get-in % [:dog_buried :said-again])) turns)]
    {:mode (if write? :write :dry-run)
     :use (run :memory/use uses read-use write-use-turn!)
     ;; Both writers below are the same combined writer. In --write mode the
     ;; first projection writes both kinds in one batch; the second reader then
     ;; observes its kind and skips it.
     :burial (run :memory/burial burials read-burials write-buried-turn!)
     :repetition (run :memory/repetition repetitions read-repetitions write-buried-turn!)}))

(defn -main [& args]
  (when-not (every? #{"--dry-run" "--write"} args)
    (throw (ex-info "usage: --dry-run (default) | --write" {})))
  (when (= #{"--dry-run" "--write"} (set args))
    (throw (ex-info "choose dry-run or write" {})))
  (try
    (prn (backfill-remaining!
          (assoc (mentions/default-config) :turn-store (turns/store))
          {:write? (boolean (some #{"--write"} args))}))
    (finally (shutdown-agents))))
