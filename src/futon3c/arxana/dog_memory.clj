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
      (records/make-external-reference
       {:ref/kind :memory-version :store/id cc-id
        :store/endpoint (or (:memory-base cfg) default-memory-base)
        :record/id cc-id :schema/authority :futon1b/memory
        :valid-time (or (:evidence/at entry) "unknown")
        :revision/digest (digest/sha256-hex (.getBytes (str version) "UTF-8"))}))))

(defn plan [cfg turn recall]
  (cond
    (not (seq (:items recall)))
    {:rows [] :written 0 :skipped [] :status (or (:status recall) "none")}

    :else
    (if-let [turn-ref (:reference (mentions/field-evidence cfg turn :source_text))]
      (let [planned (mapv (fn [rank item]
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
                                    :asserted-content {:arm (:arm recall)}})
                            att (records/make-attestation
                                 {:subject-clusion-id (:id claim) :producer "狗" :basis :model
                                  :standing :unverified :coverage :complete
                                  :method {:id "dog/recall" :version 1}
                                  :content {:rank rank :score (:score item) :why (:why item)
                                            :query-source (:query-source recall)
                                            :query-ms (:query-ms recall)
                                            :memory (:memory item)
                                            :written-at written-at}
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
     #(if (:written (:dog_recall_gcc %)) % (assoc % :dog_recall_gcc result)))
    result))

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
                         (when-let [att (latest-attestation cfg claim)]
                           {:claim claim :att att
                            :written-at (get-in att [:content :written-at])})))
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

(defn -main [& args]
  (when-not (every? #{"--dry-run" "--write"} args)
    (throw (ex-info "usage: --dry-run (default) | --write" {})))
  (when (= #{"--dry-run" "--write"} (set args))
    (throw (ex-info "choose dry-run or write" {})))
  (try
    (prn (backfill! (assoc (mentions/default-config) :turn-store (turns/store))
                    {:write? (boolean (some #{"--write"} args))}))
    (finally (shutdown-agents))))
