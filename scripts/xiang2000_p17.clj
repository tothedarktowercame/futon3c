(ns xiang2000-p17
  "Incremental constraint runner; writes only violation evidence. Cursor is returned
   on stdout, never persisted here. Dry-run is the default. Usage from futon3c:
   clojure -M scripts/xiang2000_p17.clj --dry-run [--since INSTANT] [--cursor FILE]
   --write is explicit and refuses more than 10 matches per run."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [futon3c.agency.history-constraints :as constraints]
            [futon3c.evidence.futon1b-backend :as f1b]
            [futon3c.social.shapes :as shapes])
  (:import [java.net URI URLEncoder]
           [java.time Instant LocalDate ZoneOffset]))

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

(defn query-url [base params]
  (str base "/api/alpha/evidence?"
       (str/join "&" (for [[k v] (sort-by key params)]
                       (str (URLEncoder/encode k "UTF-8") "=" (URLEncoder/encode (str v) "UTF-8"))))))

(defn read-page-set [base params pin]
  (loop [cursor nil seen #{} entries [] pages 0]
    (when (>= pages 1000) (throw (ex-info "Evidence page budget exhausted; refusing partial comparison" {:params params})))
    (let [q (cond-> (assoc params "limit" 1000 "system-as-of" pin)
              cursor (assoc "cursor-at" (:at cursor) "cursor-id" (:id cursor)))
          page (read-edn-http "GET" (query-url base q) {"Accept" "application/edn"} nil)
          next-cursor (:next-cursor page)]
      (when-not (vector? (:entries page)) (throw (ex-info "Malformed evidence page" {:params params})))
      (when (and (:incomplete page) (nil? next-cursor))
        (throw (ex-info "Incomplete evidence read without continuation" {:params params})))
      (when (and next-cursor (contains? seen next-cursor))
        (throw (ex-info "Repeated evidence cursor" {:params params :cursor next-cursor})))
      (let [rows (into entries (:entries page))]
        (if next-cursor (recur next-cursor (conj seen next-cursor) rows (inc pages)) rows)))))

(defn options [args]
  (loop [args args opts {:dry-run? true}]
    (if-let [arg (first args)]
      (case arg
        "--dry-run" (recur (rest args) (assoc opts :dry-run? true))
        "--write" (recur (rest args) (assoc opts :dry-run? false))
        "--since" (do (Instant/parse (second args))
                      (recur (nnext args) (assoc opts :since (second args))))
        "--cursor" (let [data (edn/read-string (slurp (second args)))]
                     (recur (nnext args) (assoc opts :cursor (if (contains? data :cursor) (:cursor data) data))))
        (throw (ex-info "Unknown option" {:option arg}))) opts)))

(try
  (let [opts (options *command-line-args*)
        since (or (:since opts) (get-in opts [:cursor :since])
                  (str (.toInstant (.atStartOfDay (LocalDate/now ZoneOffset/UTC) ZoneOffset/UTC))))
        until (str (Instant/now))
        base (or (System/getenv "FUTON1B_URL") "http://127.0.0.1:7073")
        types (filter #(and (keyword? %) (= "promise" (namespace %))) shapes/EvidenceType)
        read-history (memoize
                      (fn [pin]
                        (vec (concat
                              (read-page-set base {"tags" "invoke-start" "since" since} pin)
                              ;; Full promise context: enqueue may precede today's termination.
                              (mapcat #(read-page-set base {"type" (subs (str %) 1)} pin) types)))))
        run-opts (merge opts {:since since :until until :read-history read-history
                             :backend (f1b/make-futon1b-backend base)})
        preview (constraints/run! (assoc run-opts :dry-run? true))
        _ (when (and (not (:dry-run? opts)) (> (count (:violations preview)) 10))
            (throw (ex-info "Write refused: review more than 10 matches before narrowing the scope"
                            {:violations (count (:violations preview))})))
        result (if (:dry-run? opts) preview (constraints/run! run-opts))]
    (prn (assoc result :counts (frequencies (map :constraint (:violations result)))
                       :read {:system-as-of until :since since :method :pinned-http
                              :cursor-mode :system-time-snapshot-difference}))
    (shutdown-agents)
    (System/exit (if (:ok result) 0 1)))
  (catch Exception e
    (prn {:ok false :reason :constraint-run-failed :message (.getMessage e) :detail (ex-data e)})
    (shutdown-agents)
    (System/exit 2)))
