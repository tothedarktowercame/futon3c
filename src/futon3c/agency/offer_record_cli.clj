(ns futon3c.agency.offer-record-cli
  "Dry-run-by-default CLI and verified minted write path for offers."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [futon3c.agency.act-harness :as act-harness]
            [futon3c.agency.act-stamp :as act-stamp]
            [futon3c.agency.offer-record :as offer]
            [futon3c.agency.pattern-card-record-cli :as act-writer]
            [futon3c.agency.rule-record :as store])
  (:import [java.net URLEncoder]
           [java.time Instant]))

(def ^:private mint-validation-id "act:pending-mint")

(defn- refuse! [reason field]
  (throw (ex-info "Invalid offer write" {:reason reason :field field})))

(defn- text? [value]
  (and (string? value) (not (str/blank? value))))

(defn payload
  "Build a minted offer payload. Storage-owned fields are refused in REQUEST."
  [{:keys [record idempotency-key] :as request} harness stamp]
  (when-not (= #{:record :idempotency-key} (set (keys request)))
    (refuse! :invalid-request :request))
  (when-not (and (map? record) (not (contains? record :id))
                 (not (contains? record :act/harness))
                 (not (contains? record :act/stamp)))
    (refuse! :caller-assigned-storage-field :record))
  (when-not (text? idempotency-key)
    (refuse! :invalid-idempotency-key :idempotency-key))
  (let [harness (act-harness/validate! harness)
        stamp (act-stamp/validate! stamp)
        draft (assoc record :id mint-validation-id
                     :act/harness harness :act/stamp stamp)
        edge (offer/record->hyperedge draft)]
    (-> edge
        (dissoc :hx/id)
        (assoc :hx/mint-id true :hx/idempotency-key idempotency-key))))

(defn- list-path [session system-as-of valid-as-of]
  (str "/api/alpha/hyperedges?type=offer%2Frecord&end="
       (URLEncoder/encode (str "session:" session) "UTF-8")
       "&limit=1000&include-total=false&system-as-of="
       (URLEncoder/encode system-as-of "UTF-8")
       "&valid-as-of=" (URLEncoder/encode valid-as-of "UTF-8")))

(defn- minted-id! [receipt]
  (let [id (:hx/id receipt)]
    (when-not (and (:ok receipt) (text? id) (str/starts-with? id "act:"))
      (refuse! :missing-minted-receipt :receipt))
    id))

(defn- verified-result! [base write-payload receipt record]
  (let [id (minted-id! receipt)
        system-as-of (str (Instant/now))
        edges (:hyperedges
               (store/request! base "GET"
                               (list-path (get-in record [:seat :session])
                                          system-as-of (:at record)) nil))
        stored (some (fn [edge]
                       (when (= id (:hx/id edge))
                         (try (offer/hyperedge->record edge)
                              (catch clojure.lang.ExceptionInfo _ nil))))
                     edges)]
    (when-not stored (refuse! :readback-missing :receipt))
    (when-not (= (dissoc write-payload :hx/mint-id :hx/idempotency-key)
                 (-> (offer/record->hyperedge stored) (dissoc :hx/id)))
      (refuse! :readback-mismatch :receipt))
    {:receipt (assoc receipt :verified? true :system-as-of system-as-of)
     :record stored
     :seat (:seat stored)}))

(defn write!
  "Authorize, mint, and verify one offer through a seat-scoped LIST readback."
  [base request harness stamp]
  (let [record (:record request)
        _ (act-writer/authorize! base stamp :offer/record
                                 (get-in record [:seat :agent]) (:at record))
        write-payload (payload request harness stamp)
        receipt (store/request! base "POST" "/api/alpha/hyperedge" write-payload)]
    (verified-result! base write-payload receipt record)))

(defn- usage []
  "Usage: offer-record-cli FILE.edn --act-executor ID --act-signer ID --executor-basis BASIS --act-grant-id ACT [--write] [--harness-kind KIND --harness-execution-id ID]")

(defn -main [& args]
  (try
    (let [{:keys [args stamp]} (act-writer/parse-stamp-flags args)
          {:keys [write? file harness]}
          (act-harness/parse-cli args "cli:futon3c.agency.offer-record-cli" (usage))
          base (or (System/getenv "FUTON1B_URL") "http://127.0.0.1:7073")
          request (edn/read-string (slurp file))
          result (if write? (write! base request harness stamp)
                     {:ok true :dry-run? true :payload (payload request harness stamp)})]
      (prn result)
      (shutdown-agents))
    (catch Exception e
      (binding [*out* *err*]
        (prn {:ok false :message (.getMessage e) :detail (ex-data e)}))
      (shutdown-agents)
      (System/exit 1))))
