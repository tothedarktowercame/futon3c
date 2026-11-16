(ns futon3c.arxana.turn-mention-writer
  "Exact turn-field revisions, appended through the futon1b Arxana batch port.
  The slice is a candidate inventory only: every candidate used must also
  exist as its workplan/item clusion in that store. No item is created here."
  (:require [clojure.string :as str]
            [futon3c.arxana.digest :as digest]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.store :as store]
            [futon3c.arxana.store-http :as http]
            [futon3c.arxana.turn-mentions :as mentions]
            [futon3c.arxana.workplan :as workplan]
            [futon3c.xiang.turn-store :as turns])
  (:import [java.nio.charset StandardCharsets]))

(defn default-config []
  {:graph-store (http/http-store "http://localhost:7073" "arxana-service")
   :turn-store (turns/store) :pause-ms 200})

(def ^:private !slice-ids (atom nil))

(defn- slice-item-ids
  "Candidate item ids from the workplan slice, re-read only when the file
  changes: the hook runs per turn field, and the slice is ~2.7 MB of EDN."
  []
  (let [f (java.io.File. ^String workplan/default-slice)
        stamp [(.lastModified f) (.length f)]
        cached @!slice-ids]
    (if (= stamp (:stamp cached))
      (:ids cached)
      (let [ids (vec (distinct (keep #(when (= [:workplan/item 1] (:type-revision %))
                                        (get-in % [:identity :item/id]))
                                     (:records (workplan/read-slice)))))]
        (reset! !slice-ids {:stamp stamp :ids ids})
        ids))))

(defn inventory
  "Read candidate IDs from the slice and verify their exact clusions. A text
  limits queries to possible matches; nil verifies the whole slice for CLI.
  Missing records are excluded; unavailable reads refuse rather than silently
  shrinking the inventory. :item-ids injects an already verified inventory."
  [cfg text]
  (if (contains? cfg :item-ids)
    (filterv #(or (nil? text) (str/includes? text %)) (:item-ids cfg))
    (let [ids (slice-item-ids)]
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

(defn plan-field [cfg turn field]
  (when-not (#{:source_text :reply_text} field)
    (throw (ex-info "unknown turn text field" {:field field})))
  (let [text (get turn field) id (:id turn)]
    (if-not (and (string? text) (seq text))
      {:mentions [] :batches []}
      (do
        (when-not (and (string? id) (seq id))
          (throw (ex-info "turn id required" {:field field})))
        (let [sha (digest/sha256-hex (.getBytes ^String text StandardCharsets/UTF_8))
              ref (records/make-external-reference
                   {:ref/kind :file-record :store/id "futon3c/turns"
                    :store/endpoint (:dir (or (:turn-store cfg) (turns/store)))
                    :record/id (str id "/" (name field))
                    :schema/authority :futon3c/turn-record
                    ;; A content revision is identified by its digest, not
                    ;; the time this writer happened to run.
                    :valid-time (or (:created_at turn) "unknown") :revision/digest sha})
              ms (mentions/turn-mentions text ref (inventory cfg text))
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
          {:mentions ms
           :batches (mapv (fn [n batch]
                            {:message-id (str "turn-mentions/v2/" id "/" (name field) "/" sha "/" n)
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
