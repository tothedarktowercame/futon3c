(ns xiang2000-p2b
  "Read-only live comparison. No reload or call to snapshot/ensure!: inspect the
   existing authoritative atoms via Drawbridge. HTTP reads use fully qualified
   type strings directly, avoiding the client's namespace-dropping query builder."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [futon3c.agency.promise-replay :as replay]
            [futon3c.social.shapes :as shapes])
  (:import [java.net URI URLEncoder]
           [java.time Instant]))

(defn read-edn-http [method url headers body]
  (let [c (.openConnection (.toURL (URI. url)))]
    (.setRequestMethod c method)
    (.setConnectTimeout c 10000)
    (.setReadTimeout c 60000)
    (doseq [[k v] headers] (.setRequestProperty c k v))
    (when body
      (.setDoOutput c true)
      (with-open [out (.getOutputStream c)] (.write out (.getBytes body "UTF-8"))))
    (let [status (.getResponseCode c)]
      (when-not (<= 200 status 299)
        (throw (ex-info "Read failed" {:url url :status status})))
      (with-open [in (.getInputStream c)] (edn/read-string (slurp in))))))

(def snapshot-form
  "(let [p @(var-get (ns-resolve 'futon3c.agency.parked-on '!parked))
          f @(var-get (ns-resolve 'futon3c.agency.followup-queue '!state))]
      (when (or (nil? p) (nil? f)) (throw (ex-info \"Store not initialized; comparison will not initialize it\" {})))
      {:states {:parked (dissoc p :just-released) :followup f}
       :history-counts (futon3c.agency.promise-history/stats)})")

(defn live-snapshot []
  (let [response (read-edn-http "POST"
                               (str (or (System/getenv "FUTON3C_DRAWBRIDGE_URL") "http://127.0.0.1:6768") "/admin/eval")
                               {"Content-Type" "text/plain" "x-drawbridge-profile" "dev-admin"
                                "x-admin-token" (str/trim (slurp ".admintoken"))} snapshot-form)]
    (when-not (:ok response) (throw (ex-info "Snapshot read refused" response)))
    (:value response)))

(defn query-url [base params]
  (str base "/api/alpha/evidence?"
       (str/join "&" (for [[k v] (sort-by key params)]
                       (str (URLEncoder/encode k "UTF-8") "=" (URLEncoder/encode (str v) "UTF-8"))))))

(defn read-type [base type pin]
  (loop [cursor nil seen #{} entries [] pages 0]
    (when (>= pages 1000) (throw (ex-info "Evidence page budget exhausted; refusing partial comparison" {:type type})))
    (let [q (cond-> {"type" (subs (str type) 1) "limit" 1000 "system-as-of" pin}
              cursor (assoc "cursor-at" (:at cursor) "cursor-id" (:id cursor)))
          page (read-edn-http "GET" (query-url base q) {"Accept" "application/edn"} nil)
          next-cursor (:next-cursor page)]
      (when-not (vector? (:entries page)) (throw (ex-info "Malformed evidence page" {:type type})))
      (when (and (:incomplete page) (nil? next-cursor))
        (throw (ex-info "Incomplete evidence read without continuation" {:type type})))
      (when (and next-cursor (contains? seen next-cursor))
        (throw (ex-info "Repeated evidence cursor" {:type type :cursor next-cursor})))
      (let [rows (into entries (:entries page))]
        (if next-cursor (recur next-cursor (conj seen next-cursor) rows (inc pages)) rows)))))

(try
  (let [before (live-snapshot)
        pin (str (Instant/now))
        types (filter #(and (keyword? %) (= "promise" (namespace %))) shapes/EvidenceType)
        rows (vec (mapcat #(read-type (or (System/getenv "FUTON1B_URL") "http://127.0.0.1:7073") % pin) types))
        after (live-snapshot)
        stable? (= before after)
        pending? (let [{:keys [submitted written failed]} (:history-counts after)]
                   (not= submitted (+ written failed)))
        report (replay/compare-state rows (:states after))
        report (assoc report :equal? (and (:equal? report) stable? (not pending?))
                      :read {:system-as-of pin :types (vec types) :records (count rows)
                             :state-stable? stable? :pending-writes? pending?
                             :history-counts (:history-counts after)
                             :method :qualified-types-over-http :authority :unchanged})]
    (prn report)
    (shutdown-agents)
    (System/exit (if (:equal? report) 0 1)))
  (catch Exception e
    (prn {:equal? false :reason :read-failed :message (.getMessage e) :detail (ex-data e)})
    (shutdown-agents)
    (System/exit 2)))
