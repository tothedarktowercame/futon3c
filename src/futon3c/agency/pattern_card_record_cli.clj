(ns futon3c.agency.pattern-card-record-cli
  "Dry-run-by-default CLI and minted write path for pattern-card acts."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [futon3c.agency.act-harness :as act-harness]
            [futon3c.agency.rule-record :as store]
            [futon3c.agency.pattern-card-acts :as acts]
            [futon3c.agency.pattern-card-record :as record])
  (:import [java.net URLEncoder]
           [java.time Instant]))

(def ^:private mint-validation-id "act:pending-mint")

(defn- refuse! [reason field]
  (throw (ex-info "Invalid pattern-card write" {:reason reason :field field})))

(defn- text? [value]
  (and (string? value) (not (str/blank? value))))

(defn- drop-nils [value]
  (if (map? value)
    (into {} (keep (fn [[k v]] (when-not (nil? v) [k (drop-nils v)]))) value)
    value))

(defn- request-record! [{:keys [record idempotency-key] :as request}]
  (when-not (= #{:record :idempotency-key} (set (keys request)))
    (refuse! :invalid-request :request))
  (when-not (and (map? record) (not (contains? record :id))
                 (not (contains? record :act/harness)))
    (refuse! :caller-assigned-storage-field :record))
  (when-not (text? idempotency-key)
    (refuse! :invalid-idempotency-key :idempotency-key))
  (drop-nils record))

(defn- mint-payload [plain-record harness]
  (let [validated-harness (act-harness/validate! harness)
        draft (assoc plain-record :id mint-validation-id :act/harness validated-harness)
        edge (record/record->hyperedge draft)]
    (-> edge
        (dissoc :hx/id)
        (assoc :hx/mint-id true)
        (update :hx/props assoc :pattern-card/schema 1))))

(defn selection-payload
  "Build a minted selection payload. Caller-assigned ids and harnesses are refused."
  [request harness]
  (let [plain-record (request-record! request)]
    (when-not (= :pattern-card/selection (:kind plain-record))
      (refuse! :wrong-record-kind :kind))
    (assoc (mint-payload plain-record harness)
           :hx/idempotency-key (:idempotency-key request))))

(defn withdrawal-payload
  "Build a minted withdrawal payload after validating against TARGET-HYPEREDGE."
  [request target-hyperedge harness]
  (let [plain-record (request-record! request)]
    (when (= :interpretation (:kind plain-record))
      (refuse! :interpretation-not-effect :kind))
    (when-not (= :act/withdrawal (:kind plain-record))
      (refuse! :wrong-record-kind :kind))
    (when-not target-hyperedge (refuse! :target-absent :target))
    (when-not (= :pattern-card/selection (:hx/type target-hyperedge))
      (refuse! :target-wrong-type :target))
    (let [target (record/hyperedge->record target-hyperedge)
          draft (assoc plain-record :id mint-validation-id
                       :act/harness (act-harness/validate! harness))]
      (record/validate-withdrawal draft target)
      (assoc (mint-payload plain-record harness)
             :hx/idempotency-key (:idempotency-key request)))))

(defn live-target!
  "Read one withdrawal target from futon1b, translating absence to a typed refusal."
  [base target-id]
  (try
    (store/request! base "GET"
                    (str "/api/alpha/hyperedge/"
                         (URLEncoder/encode (str target-id) "UTF-8")) nil)
    (catch clojure.lang.ExceptionInfo e
      (if (= 404 (:status (ex-data e)))
        (refuse! :target-absent :target)
        (throw e)))))

(defn withdrawal-live-payload [base request harness]
  (let [target-id (get-in request [:record :target])
        target (live-target! base target-id)]
    (withdrawal-payload request target harness)))

(defn- list-path [type system-as-of valid-as-of]
  (str "/api/alpha/hyperedges?type="
       (URLEncoder/encode (subs (str type) 1) "UTF-8")
       "&limit=1000&include-total=false&system-as-of="
       (URLEncoder/encode system-as-of "UTF-8")
       "&valid-as-of=" (URLEncoder/encode valid-as-of "UTF-8")))

(defn- read-stored [hyperedge]
  (try {:record (record/hyperedge->record hyperedge)}
       (catch clojure.lang.ExceptionInfo e
         {:unreadable {:hx/id (:hx/id hyperedge) :reason (:reason (ex-data e))}})))

(defn- list-records!
  "Stored records of TYPE, and the ids of stored documents that do not map to a
   valid record. One malformed record (e.g. act:cb9bff2a…, minted before :at
   was kept in props) must not make the whole seat unreadable."
  [base type system-as-of valid-as-of]
  (let [read (->> (store/request! base "GET" (list-path type system-as-of valid-as-of) nil)
                  :hyperedges
                  (mapv read-stored))]
    {:records (into [] (keep :record) read)
     :unreadable (into [] (keep :unreadable) read)}))

(defn- minted-id! [receipt]
  (let [id (:hx/id receipt)]
    (when-not (and (:ok receipt) (text? id) (str/starts-with? id "act:"))
      (refuse! :missing-minted-receipt :receipt))
    id))

(defn- verified-result! [base payload receipt seat at]
  (let [id (minted-id! receipt)
        system-as-of (str (Instant/now))
        sel (list-records! base :pattern-card/selection system-as-of at)
        wd (list-records! base :act/withdrawal system-as-of at)
        selections (:records sel)
        withdrawals (:records wd)
        stored (some #(when (= id (:id %)) %) (concat selections withdrawals))]
    (when-not stored (refuse! :readback-missing :receipt))
    (when-not (= (dissoc payload :hx/mint-id :hx/idempotency-key)
                 (-> (record/record->hyperedge stored) (dissoc :hx/id)))
      (refuse! :readback-mismatch :receipt))
    {:receipt (assoc receipt :verified? true :system-as-of system-as-of)
     :record stored
     :seat (select-keys seat [:agent :session])
     :card-as-of (acts/card-as-of (concat selections withdrawals)
                                  (:agent seat) (:session seat) at)
     :unreadable (into (:unreadable sel) (:unreadable wd))}))

(defn write-selection! [base request harness]
  (let [payload (selection-payload request harness)
        receipt (store/request! base "POST" "/api/alpha/hyperedge" payload)
        plain (:record request)]
    (verified-result! base payload receipt plain (:at plain))))

(defn write-withdrawal! [base request harness]
  (let [target (live-target! base (get-in request [:record :target]))
        target-record (when (= :pattern-card/selection (:hx/type target))
                        (record/hyperedge->record target))
        payload (withdrawal-payload request target harness)
        receipt (store/request! base "POST" "/api/alpha/hyperedge" payload)]
    (verified-result! base payload receipt target-record (get-in request [:record :at]))))

(defn- usage []
  "Usage: pattern-card-record-cli select|withdraw FILE.edn [--write] [--harness-kind KIND --harness-execution-id ID]")

(defn -main [& args]
  (try
    (let [operation (some-> (first args) keyword)
          _ (when-not (contains? #{:select :withdraw} operation)
              (throw (ex-info (usage) {:reason :invalid-operation})))
          {:keys [write? file harness]}
          (act-harness/parse-cli (rest args)
                                 "cli:futon3c.agency.pattern-card-record-cli"
                                 (usage))
          base (or (System/getenv "FUTON1B_URL") "http://127.0.0.1:7073")
          request (edn/read-string (slurp file))
          result (case operation
                   :select (if write?
                             (write-selection! base request harness)
                             {:ok true :dry-run? true
                              :payload (selection-payload request harness)})
                   :withdraw (if write?
                               (write-withdrawal! base request harness)
                               {:ok true :dry-run? true
                                :payload (withdrawal-live-payload base request harness)}))]
      (prn result)
      (shutdown-agents))
    (catch Exception e
      (binding [*out* *err*]
        (prn {:ok false :message (.getMessage e) :detail (ex-data e)}))
      (shutdown-agents)
      (System/exit 1))))
