(ns futon3c.agency.disclosure-record-cli
  "Minted storage seam and HTTP CLI for disclosed choices."
  (:require [clojure.string :as str]
            [futon3c.agency.disclosure-record :as disclosure]
            [futon3c.agency.pattern-card-record :as withdrawal-record]
            [futon3c.agency.rule-record :as store])
  (:import [java.net URLEncoder]
           [java.time Instant]))

(def ^:private pending-id "act:pending-mint")

(defn- refuse! [reason field]
  (throw (ex-info "Invalid disclosure write" {:reason reason :field field})))

(defn payload [record idempotency-key]
  (when-not (and (string? idempotency-key) (not (str/blank? idempotency-key)))
    (refuse! :invalid-idempotency-key :idempotency-key))
  (when (contains? record :id)
    (refuse! :caller-assigned-storage-field :id))
  (-> (disclosure/->hyperedge (assoc record :id pending-id))
      (dissoc :hx/id)
      (assoc :hx/mint-id true :hx/idempotency-key idempotency-key)))

(defn- list-path [job-id system-as-of valid-as-of]
  (str "/api/alpha/hyperedges?type=disclosure%2Fchoice&end="
       (URLEncoder/encode (str "job:" job-id) "UTF-8")
       "&limit=1000&include-total=false&system-as-of="
       (URLEncoder/encode system-as-of "UTF-8")
       "&valid-as-of=" (URLEncoder/encode valid-as-of "UTF-8")))

(def ^:private identity-keys
  [:author :source-job :unspecified :chosen :affects :inside-request])

(defn- existing-disclosure
  "The stored disclosure on RECORD's job with the same content, if any. The
   idempotency key excludes :at (the server clock), so a replay reaches futon1b
   as the same key with a different payload and gets 409; it is the same choice."
  [base record]
  (let [now (str (Instant/now))]
    (some (fn [hx]
            (let [stored (disclosure/hyperedge->record hx)]
              (when (= (select-keys stored identity-keys)
                       (select-keys record identity-keys))
                stored)))
          (:hyperedges
           (store/request! base "GET" (list-path (:source-job record) now now) nil)))))

(declare write-new!)

(defn write! [base record idempotency-key]
  (let [write-payload (payload record idempotency-key)
        receipt (try
                  (store/request! base "POST" "/api/alpha/hyperedge" write-payload)
                  (catch clojure.lang.ExceptionInfo e
                    (if (= 409 (:status (ex-data e)))
                      (if-let [stored (existing-disclosure base record)]
                        {::existing stored}
                        (refuse! :idempotency-conflict :idempotency-key))
                      (throw e))))]
    (if-let [stored (::existing receipt)]
      {:receipt {:ok true :hx/id (:id stored) :minted? false :existing? true
                 :verified? true}
       :record stored}
      (write-new! base record write-payload receipt))))

(defn- write-new! [base record write-payload receipt]
  (let [id (:hx/id receipt)]
    (when-not (and (:ok receipt) (string? id) (str/starts-with? id "act:"))
      (refuse! :missing-minted-receipt :receipt))
    (let [system-as-of (str (Instant/now))
          edges (:hyperedges
                 (store/request! base "GET"
                                 (list-path (:source-job record) system-as-of
                                            (:at record)) nil))
          stored (some #(when (= id (:hx/id %))
                          (disclosure/hyperedge->record %)) edges)]
      (when-not stored (refuse! :readback-missing :receipt))
      (when-not (= (dissoc write-payload :hx/mint-id :hx/idempotency-key)
                   (-> (disclosure/->hyperedge stored) (dissoc :hx/id)))
        (refuse! :readback-mismatch :receipt))
      {:receipt (assoc receipt :verified? true :system-as-of system-as-of)
       :record stored})))

(defn read-act! [base id]
  (try
    (store/request! base "GET"
                    (str "/api/alpha/hyperedge/"
                         (URLEncoder/encode (str id) "UTF-8")) nil)
    (catch clojure.lang.ExceptionInfo e
      (if (= 404 (:status (ex-data e))) nil (throw e)))))

(defn withdrawals-for! [base target]
  (:hyperedges
   (store/request! base "GET"
                   (str "/api/alpha/hyperedges?type=act%2Fwithdrawal&end="
                        (URLEncoder/encode target "UTF-8")
                        "&limit=1000&include-total=false") nil)))

(defn withdrawal-payload [record idempotency-key]
  (when-not (and (string? idempotency-key) (not (str/blank? idempotency-key)))
    (refuse! :invalid-idempotency-key :idempotency-key))
  (when (contains? record :id)
    (refuse! :caller-assigned-storage-field :id))
  (-> (withdrawal-record/record->hyperedge (assoc record :id pending-id))
      (dissoc :hx/id)
      (assoc :hx/mint-id true :hx/idempotency-key idempotency-key)))

(defn write-withdrawal! [base record idempotency-key]
  (let [write-payload (withdrawal-payload record idempotency-key)
        receipt (store/request! base "POST" "/api/alpha/hyperedge" write-payload)
        id (:hx/id receipt)]
    (when-not (and (:ok receipt) (string? id) (str/starts-with? id "act:"))
      (refuse! :missing-minted-receipt :receipt))
    (let [stored-edge (some #(when (= id (:hx/id %)) %)
                            (withdrawals-for! base (:target record)))
          stored (some-> stored-edge withdrawal-record/hyperedge->record)]
      (when-not stored (refuse! :readback-missing :receipt))
      (when-not (= (dissoc write-payload :hx/mint-id :hx/idempotency-key)
                   (-> (withdrawal-record/record->hyperedge stored) (dissoc :hx/id)))
        (refuse! :readback-mismatch :receipt))
      {:receipt (assoc receipt :verified? true) :record stored})))

(defn- usage []
  (str "Usage: disclosure-record-cli --source-job JOB --unspecified TEXT "
       "--chosen TEXT --affects-kind KIND --affects-id ID "
       "[--affects-path PATH] --quote TEXT\n"
       "Caller is read from AGENCY_AGENT_ID."))

(defn- parse-args [args]
  (loop [args args result {}]
    (if (empty? args)
      result
      (let [[flag value & more] args]
        (when (or (nil? value) (not (str/starts-with? flag "--")))
          (throw (ex-info (usage) {:reason :invalid-arguments})))
        (recur more (assoc result (keyword (subs flag 2)) value))))))

(defn -main [& args]
  (try
    (let [m (parse-args args)
          caller (System/getenv "AGENCY_AGENT_ID")
          required [:source-job :unspecified :chosen :affects-kind :affects-id :quote]]
      (when (str/blank? caller)
        (throw (ex-info (usage) {:reason :missing-caller :field :AGENCY_AGENT_ID})))
      (when-let [missing (first (filter #(str/blank? (get m %)) required))]
        (throw (ex-info (usage) {:reason :missing-argument :field missing})))
      (let [body (cond-> {:caller caller :source-job (:source-job m)
                          :unspecified (:unspecified m) :chosen (:chosen m)
                          :quote (:quote m)
                          :affects {:kind (:affects-kind m) :id (:affects-id m)}}
                   (:affects-path m) (assoc-in [:affects :path] (:affects-path m)))
            base (or (System/getenv "FUTON3C_URL") "http://127.0.0.1:7070")]
        (prn (store/request! base "POST" "/api/alpha/disclosure" body))))
    (catch Exception e
      (binding [*out* *err*]
        (prn {:ok false :message (.getMessage e) :detail (ex-data e)}))
      (System/exit 1))))
