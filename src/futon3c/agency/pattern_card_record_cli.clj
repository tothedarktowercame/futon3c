(ns futon3c.agency.pattern-card-record-cli
  "Dry-run-by-default CLI and minted write path for pattern-card acts."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [futon3c.agency.act-harness :as act-harness]
            [futon3c.agency.act-stamp :as act-stamp]
            [futon3c.agency.grant-record :as grant-record]
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
                 (not (contains? record :act/harness))
                 (not (contains? record :act/stamp)))
    (refuse! :caller-assigned-storage-field :record))
  (when-not (text? idempotency-key)
    (refuse! :invalid-idempotency-key :idempotency-key))
  (drop-nils record))

(defn- mint-payload [plain-record harness stamp]
  (let [validated-harness (act-harness/validate! harness)
        validated-stamp (act-stamp/validate! stamp)
        _ (when-not (= (:author plain-record) (:signer validated-stamp))
            (refuse! :stamp-author-mismatch :act/stamp))
        draft (assoc plain-record :id mint-validation-id
                     :act/harness validated-harness :act/stamp validated-stamp)
        edge (record/record->hyperedge draft)]
    (-> edge
        (dissoc :hx/id)
        (assoc :hx/mint-id true)
        (update :hx/props assoc :pattern-card/schema 1))))

(defn selection-payload
  "Build a minted selection payload. Caller-assigned ids and harnesses are refused."
  [request harness stamp]
  (let [plain-record (request-record! request)]
    (when-not (= :pattern-card/selection (:kind plain-record))
      (refuse! :wrong-record-kind :kind))
    (assoc (mint-payload plain-record harness stamp)
           :hx/idempotency-key (:idempotency-key request))))

(defn withdrawal-payload
  "Build a minted withdrawal payload after validating against TARGET-HYPEREDGE."
  [request target-hyperedge harness stamp]
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
                       :act/harness (act-harness/validate! harness)
                       :act/stamp (act-stamp/validate! stamp))]
      (record/validate-withdrawal draft target)
      (assoc (mint-payload plain-record harness stamp)
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

(defn withdrawal-live-payload [base request harness stamp]
  (let [target-id (get-in request [:record :target])
        target (live-target! base target-id)]
    (withdrawal-payload request target harness stamp)))

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

(defn- grant-hyperedge! [base grant-id]
  (try
    (store/request! base "GET"
                    (str "/api/alpha/hyperedge/"
                         (URLEncoder/encode grant-id "UTF-8")) nil)
    (catch clojure.lang.ExceptionInfo e
      (if (= 404 (:status (ex-data e)))
        (refuse! :no-grant :act/stamp)
        (throw e)))))

(defn- grant-records! [base leaf]
  (loop [node leaf records [leaf] seen #{(:hx/id leaf)}]
    (if-let [parent-id (get-in node [:hx/props :grant/parent])]
      (do
        (when (contains? seen parent-id)
          (refuse! :no-grant :act/stamp))
        (let [parent (grant-hyperedge! base parent-id)]
          (recur parent (conj records parent) (conj seen parent-id))))
      records)))

(defn authorize!
  "Check STAMP against live grant records for TARGET at AT. Joe's operator
   authority needs no grant read. Return the validated stamp or refuse with
   :no-grant and the grant query's typed :grant-reason."
  [base stamp target target-signer at]
  (let [validated (act-stamp/validate! stamp)]
    (when-let [grant-id (get-in validated [:authority :grant])]
      (let [leaf (grant-hyperedge! base grant-id)
            answer (grant-record/grant-covers?
                    ;; The grant must cover the party that acts. Checking the
                    ;; signer let an executor sign as another agent and use
                    ;; the own-acts grant on that agent's acts.
                    (grant-records! base leaf) (:executor validated) target at
                    {:leaf-id grant-id :target-signer target-signer})]
        (when-not (= :granted (:status answer))
          (throw (ex-info "No grant covers the pattern-card act"
                          {:reason :no-grant :field :act/stamp
                           :grant-reason (:reason answer)})))))
    validated))

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

(defn write-selection! [base request harness stamp]
  (let [plain (:record request)
        _ (authorize! base stamp :pattern-card/selection (:agent plain) (:at plain))
        payload (selection-payload request harness stamp)
        receipt (store/request! base "POST" "/api/alpha/hyperedge" payload)]
    (verified-result! base payload receipt plain (:at plain))))

(defn write-withdrawal! [base request harness stamp]
  (let [target (live-target! base (get-in request [:record :target]))
        target-record (when (= :pattern-card/selection (:hx/type target))
                        (record/hyperedge->record target))
        payload (withdrawal-payload request target harness stamp)
        target-signer (or (get-in target-record [:act/stamp :signer])
                          (:author target-record))
        _ (authorize! base stamp :act/withdrawal target-signer
                      (get-in request [:record :at]))
        receipt (store/request! base "POST" "/api/alpha/hyperedge" payload)]
    (verified-result! base payload receipt target-record (get-in request [:record :at]))))

(defn- usage []
  "Usage: pattern-card-record-cli select|withdraw FILE.edn --act-executor ID --act-signer ID --executor-basis BASIS [--act-grant-id ACT] [--write] [--harness-kind KIND --harness-execution-id ID]")

(defn parse-stamp-flags
  "Remove the required act-stamp CLI flags from ARGS and return {:args :stamp}."
  [args]
  (loop [remaining (seq args) pass [] values {}]
    (if-let [arg (first remaining)]
      (if (contains? #{"--act-executor" "--act-signer" "--act-grant-id"
                       "--executor-basis"} arg)
        (if-let [value (second remaining)]
          (recur (nnext remaining) pass (assoc values arg value))
          (throw (ex-info (usage) {:reason :missing-option-value :option arg})))
        (recur (next remaining) (conj pass arg) values))
      (let [executor (get values "--act-executor")
            signer (get values "--act-signer")
            basis (some-> (get values "--executor-basis") keyword)
            grant-id (get values "--act-grant-id")
            authority (if grant-id {:grant grant-id}
                          (when (= "joe" signer) {:operator true}))]
        {:args pass
         :stamp (act-stamp/stamp executor signer authority basis)}))))

(defn -main [& args]
  (try
    (let [operation (some-> (first args) keyword)
          _ (when-not (contains? #{:select :withdraw} operation)
              (throw (ex-info (usage) {:reason :invalid-operation})))
          {:keys [args stamp]} (parse-stamp-flags (rest args))
          {:keys [write? file harness]}
          (act-harness/parse-cli args
                                 "cli:futon3c.agency.pattern-card-record-cli"
                                 (usage))
          base (or (System/getenv "FUTON1B_URL") "http://127.0.0.1:7073")
          request (edn/read-string (slurp file))
          result (case operation
                   :select (if write?
                             (write-selection! base request harness stamp)
                             {:ok true :dry-run? true
                              :payload (selection-payload request harness stamp)})
                   :withdraw (if write?
                               (write-withdrawal! base request harness stamp)
                               {:ok true :dry-run? true
                                :payload (withdrawal-live-payload base request harness stamp)}))]
      (prn result)
      (shutdown-agents))
    (catch Exception e
      (binding [*out* *err*]
        (prn {:ok false :message (.getMessage e) :detail (ex-data e)}))
      (shutdown-agents)
      (System/exit 1))))
