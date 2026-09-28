(ns futon3c.agency.overreach-cli
  "Read-only, bitemporally pinned overreach report for pattern-card acts."
  (:require [clojure.string :as str]
            [futon3c.agency.overreach :as overreach]
            [futon3c.agency.pattern-card-record :as card-record]
            [futon3c.agency.rule-record :as store])
  (:import [java.net URLEncoder]
           [java.time Instant]))

(def limit 1000)
(def classes [:authorised :overreach :unverified-executor :outside-coverage])

(defn- refuse! [reason field]
  (throw (ex-info "Invalid overreach report request" {:reason reason :field field})))

(defn- encode [value] (URLEncoder/encode (str value) "UTF-8"))

(defn- list-path [type system-as-of]
  (str "/api/alpha/hyperedges?type=" (encode (subs (str type) 1))
       "&limit=" limit "&include-total=false"
       "&system-as-of=" (encode system-as-of)
       "&valid-as-of=" (encode system-as-of)))

(defn- read-page! [base type system-as-of request-fn]
  (let [response (request-fn base "GET" (list-path type system-as-of) nil)
        documents (vec (:hyperedges response))]
    {:documents documents
     :truncated (boolean (or (>= (count documents) limit)
                             (:next-cursor response)
                             (:incomplete response)))}))

(defn- read-card [document]
  (try {:record (card-record/hyperedge->record document)}
       (catch clojure.lang.ExceptionInfo e
         {:unreadable {:id (:hx/id document)
                       :reason (:reason (ex-data e))}})
       (catch Throwable _
         {:unreadable {:id (:hx/id document) :reason :unreadable-record}})))

(defn- adapt-acts [selections withdrawals]
  (let [by-id (into {} (map (juxt :id identity) selections))]
    {:acts (into (mapv overreach/record->act selections)
                 (map (fn [withdrawal]
                        (let [target (get by-id (:target withdrawal))
                              signer (or (get-in target [:act/stamp :signer])
                                         (:author target))]
                          (overreach/record->act withdrawal signer)))
                      withdrawals))
     :missing-targets
     (into [] (keep (fn [withdrawal]
                      (when-not (contains? by-id (:target withdrawal))
                        {:withdrawal-id (:id withdrawal)
                         :target-id (:target withdrawal)}))) withdrawals)}))

(defn generate-report
  "Read three pinned LIST pages through REQUEST-FN and return the report map.
   REQUEST-FN has the same [base method path body] contract as store/request!."
  [base system-as-of request-fn]
  (try (Instant/parse system-as-of)
       (catch Exception _ (refuse! :invalid-system-as-of :system-as-of)))
  (let [selection-page (read-page! base :pattern-card/selection system-as-of request-fn)
        withdrawal-page (read-page! base :act/withdrawal system-as-of request-fn)
        grant-page (read-page! base :grant/record system-as-of request-fn)
        selection-reads (mapv read-card (:documents selection-page))
        withdrawal-reads (mapv read-card (:documents withdrawal-page))
        selections (into [] (keep :record) selection-reads)
        withdrawals (into [] (keep :record) withdrawal-reads)
        unreadable (into (into [] (keep :unreadable) selection-reads)
                         (keep :unreadable) withdrawal-reads)
        {:keys [acts missing-targets]} (adapt-acts selections withdrawals)
        grants (:documents grant-page)
        classifications (overreach/scan-report acts grants)
        counts (merge (zipmap classes (repeat 0))
                      (frequencies (map :classification classifications)))]
    {:system-as-of system-as-of
     :counts counts
     :findings (into [] (keep :finding) classifications)
     :outside-coverage-ids (into [] (comp (filter #(= :outside-coverage
                                                       (:classification %)))
                                          (map :act/id)) classifications)
     :unreadable unreadable
     :missing-withdrawal-targets missing-targets
     :grant-ids (mapv :hx/id grants)
     :truncated {:pattern-card/selection (:truncated selection-page)
                 :act/withdrawal (:truncated withdrawal-page)
                 :grant/record (:truncated grant-page)}}))

(defn- parse-args [args]
  (loop [remaining (seq args) result {}]
    (if-let [arg (first remaining)]
      (if (contains? #{"--system-as-of" "--out"} arg)
        (if-let [value (second remaining)]
          (recur (nnext remaining) (assoc result arg value))
          (refuse! :missing-option-value arg))
        (refuse! :unknown-option arg))
      (let [system-as-of (get result "--system-as-of")
            out (get result "--out")]
        (when-not (and (not (str/blank? system-as-of)) (not (str/blank? out)))
          (refuse! :missing-required-option :arguments))
        {:system-as-of system-as-of :out out}))))

(defn -main [& args]
  (try
    (let [{:keys [system-as-of out]} (parse-args args)
          base (or (System/getenv "FUTON1B_URL") "http://127.0.0.1:7073")
          report (generate-report base system-as-of store/request!)]
      (spit out (str (pr-str report) "\n"))
      (prn {:ok true :out out :counts (:counts report)
            :truncated (:truncated report)}))
    (catch Exception e
      (binding [*out* *err*]
        (prn {:ok false :message (.getMessage e) :detail (ex-data e)}))
      (System/exit 1))))
