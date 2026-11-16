(ns futon3c.arxana.turn-mention-writer
  "Exact turn-field revisions, appended through the futon1b Arxana batch port.
  The current file census is a candidate inventory: every candidate used must also
  exist as its workplan/item clusion in that store. No item is created here."
  (:require [cheshire.core :as json]
            [futon3c.xiang.turn-record :as turn-record]
            [clojure.string :as str]
            [futon3c.arxana.digest :as digest]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.store :as store]
            [futon3c.arxana.store-http :as http]
            [futon3c.arxana.turn-mentions :as mentions]
            [futon3c.arxana.item-inventory :as items]
            [futon3c.xiang.turn-store :as turns])
  (:import [java.nio.charset StandardCharsets]
           [java.net URI URLEncoder]
           [java.time Duration]
           [java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers]))

(defn default-config []
  {:graph-store (http/http-store "http://localhost:7073" "arxana-service")
   :turn-store (turns/store) :pause-ms 200})

(defn inventory
  "Read candidate IDs from the current file census and verify their exact clusions. A text
  limits queries to possible matches; nil verifies the whole census for CLI.
  Missing records are excluded; unavailable reads refuse rather than silently
  shrinking the inventory. :item-ids injects an already verified inventory."
  [cfg text]
  (if (contains? cfg :item-ids)
    (filterv #(or (nil? text) (str/includes? text %)) (:item-ids cfg))
    (let [ids (mapv :id (items/current-items cfg))]
      (into []
            (keep (fn [id]
                    (when (or (nil? text) (str/includes? text id))
                      (let [record (records/make-clusion
                                    {:type-revision [:workplan/item 1] :endpoints []
                                     :asserted-content {:item/id id}})
                            result (store/get-entity (:graph-store cfg) (:id record))]
                        (cond
                          (= record (:ok result)) id
                          (= :missing (get-in result [:error :error/category])) nil
                          :else (throw (ex-info "item inventory read refused"
                                                {:item id :result result}))))))) ids))))

(defonce ^:private evidence-client
  (-> (HttpClient/newBuilder) (.connectTimeout (Duration/ofSeconds 2)) .build))

(defn evidence-text
  "GET the Evidence Landscape's exact body/text, not the quote-elided turn.
  :read-evidence is an injectable read port taking the evidence id."
  [cfg id]
  (when-not (and (string? id) (seq id))
    (throw (ex-info "missing evidence id" {:reason :invalidEvidence})))
  (let [entry (if-let [read! (:read-evidence cfg)] (read! id)
                  (let [url (str (:evidence-url cfg "http://localhost:7073")
                                 "/api/alpha/evidence/" (URLEncoder/encode id "UTF-8"))
                        request (-> (HttpRequest/newBuilder (URI/create url))
                                    (.timeout (Duration/ofSeconds 5))
                                    (.header "Accept" "application/json") .GET .build)
                        response (.send ^HttpClient evidence-client request
                                        (HttpResponse$BodyHandlers/ofString StandardCharsets/UTF_8))]
                    (when-not (= 200 (.statusCode response))
                      (throw (ex-info "evidence unavailable" {:reason :invalidEvidence})))
                    (json/parse-string (.body response) true)))
        text (get-in entry [:evidence/body :text])]
    (when-not (string? text)
      (throw (ex-info "evidence text missing" {:reason :invalidEvidence})))
    text))

(defn field-evidence
  "Source authority is EL body/text. Replies currently remain turn-file
  revisions: reply storage does not persist a linked EL reply id. Emacs emits
  segments (and sometimes unified-text), but text equality alone does not
  identify the operator turn; do not guess a reference from a text search."
  [cfg turn field]
  (try
    (let [source? (= field :source_text)
          text (if source? (evidence-text cfg (:evidence_id turn)) (get turn field))]
      (when (string? text)
        {:text text
         :reference (records/make-external-reference
                     {:ref/kind (if source? :evidence-text :file-record)
                      :store/id (if source? "futon1b/evidence" "futon3c/turns")
                      :store/endpoint (if source? (:evidence-url cfg "http://localhost:7073")
                                          (:dir (or (:turn-store cfg) (turns/store))))
                      :record/id (if source? (str (:evidence_id turn) "/body/text")
                                     (str (:id turn) "/" (name field)))
                      :schema/authority (if source? :futon1b/evidence-text :futon3c/turn-record)
                      :valid-time (or (:created_at turn) "unknown")
                      :revision/digest (digest/sha256-hex (.getBytes ^String text StandardCharsets/UTF_8))})}))
    (catch Exception _
      (binding [*out* *err*] (println "[turn-mentions] evidence unavailable" (:id turn) field))
      nil)))

(defn- quoted-mentions [text ms]
  (let [spans (turn-record/quote-spans text)]
    (mapv (fn [m]
            (let [[a b] (get-in m [:asserted-content :mention/range])]
              (if (some (fn [[start end]] (<= start a b end)) spans)
                (records/make-clusion (assoc-in (dissoc m :id) [:asserted-content :mention/role] :quoted))
                m))) ms)))

(defn plan-field [cfg turn field]
  (when-not (#{:source_text :reply_text} field)
    (throw (ex-info "unknown turn text field" {:field field})))
  (let [{:keys [text reference]} (field-evidence cfg turn field) id (:id turn)]
    (if-not (string? text)
      {:mentions [] :batches []}
      (do
        (when-not (and (string? id) (seq id))
          (throw (ex-info "turn id required" {:field field})))
        (let [sha (digest/sha256-hex (.getBytes ^String text StandardCharsets/UTF_8))
              ref reference
              ms (cond->> (mentions/turn-mentions text ref (inventory cfg text))
                   (= field :source_text) (quoted-mentions text))
              rows (when (seq ms)
                     (into [{:operation :external-reference/assert :record ref}]
                           (mapcat (fn [m]
                                     [{:operation :clusion/assert :record m}
                                      {:operation :evidence/append
                                       :record (records/make-attestation
                                                {:subject-clusion-id (:id m) :basis :exact
                                                 :standing :verified :coverage :complete
                                                 :producer "futon3c/turn-mentions"
                                                 :method {:name :exact-item-token :version 2}})}]) ms)))]
          {:reference ref :text text :mentions ms
           :batches (mapv (fn [n batch]
                            ;; Include the reference identity: old turn-file batches
                            ;; with the same text digest must not replay as EL batches.
                            {:message-id (str "turn-mentions/v2/" id "/" (name field) "/" sha "/" (:id ref) "/" n)
                             :records (vec batch)})
                          (range) (partition-all 500 rows))})))))

(defn- write-plan! [cfg plan]
  (doseq [{:keys [message-id records]} (:batches plan)]
    (let [r (http/append-batch! (:graph-store cfg) message-id records)]
      (when-not (:ok r)
        (throw (ex-info "turn mentions append refused" {:message-id message-id :result r}))))
    (when (pos? (or (:pause-ms cfg) 0)) (Thread/sleep (long (:pause-ms cfg))))))

(defn write-turn-mentions! [cfg turn-record field]
  (let [plan (plan-field cfg turn-record field)]
    (write-plan! cfg plan)
    {:mentions (count (:mentions plan)) :requests (count (:batches plan))}))

(defn backfill!
  "Dry-run by default. Calls no write port unless :write? is true. The same
  plans drive reporting and execution; each request contains up to 500 records."
  [cfg turn-records]
  (let [plans (for [turn turn-records field [:source_text :reply_text]
                    :when (and (string? (get turn field)) (seq (get turn field)))]
                (plan-field cfg turn field))
        summary (reduce (fn [s p]
                          (when (:write? cfg) (write-plan! cfg p))
                          (-> s (update :fields inc)
                              (update :mentions + (count (:mentions p)))
                              (update :planned-batch-requests + (count (:batches p)))
                              (update :items into (map #(get-in % [:endpoints 0 :target :clusion/id]) (:mentions p)))))
                        {:turns (count turn-records) :fields 0 :mentions 0
                         :planned-batch-requests 0 :items #{}} plans)]
    (-> summary (assoc :distinct-items (count (:items summary))) (dissoc :items))))

(defn -main [& args]
  (let [opts (loop [xs args opts {:limit Long/MAX_VALUE :write? false}]
               (if-let [x (first xs)]
                 (case x
                   "--dry-run" (recur (next xs) (assoc opts :write? false))
                   "--write" (recur (next xs) (assoc opts :write? true))
                   "--limit" (recur (nnext xs) (assoc opts :limit (Long/parseLong (second xs))))
                   "--pause-ms" (recur (nnext xs) (assoc opts :pause-ms (Long/parseLong (second xs))))
                   (throw (ex-info "unknown argument" {:argument x}))) opts))
        _ (when (or (neg? (:limit opts)) (neg? (get opts :pause-ms 200)))
            (throw (ex-info "limit and pause must be nonnegative" {})))
        cfg (merge (default-config) opts)
        cfg (assoc cfg :item-ids (inventory cfg nil))
        rows (mapv (fn [{:keys [id record]}] (assoc record :id id))
                   (turns/list-records (:turn-store cfg) :limit (:limit opts)))]
    (prn (assoc (backfill! cfg rows) :mode (if (:write? cfg) :write :dry-run)
                :inventory (count (:item-ids cfg))))))
