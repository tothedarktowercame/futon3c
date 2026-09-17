;; Registry ledger audit: does every warrant's log resolve by content?
;;
;; Enumerates via the tagged query (append-record! tags every registry record
;; :test-registry, so this cannot miss a class of them) and parses each payload
;; as EDN. Do NOT pattern-match the payload text: the first version of this
;; sweep used a regex assuming :log-artifact {:path … :sha256 …} key order and
;; would have skipped, silently, any record written with another ordering
;; (zai-1, 2026-09-17).
;;
;;   clojure -M scripts/registry_ledger_audit.clj
(require '[futon3c.test-registry.ledger :as ledger]
         '[clojure.edn :as edn] '[clojure.java.io :as io]
         '[cheshire.core :as json] '[babashka.http-client :as http])
(let [body (:body (http/get "http://127.0.0.1:7070/api/alpha/evidence?tag=test-registry&limit=1000"
                            {:headers {"Accept" "application/json"} :throw false}))
      entries (:entries (json/parse-string body true))
      records (keep (fn [e]
                      (when-let [t (get-in e [:evidence/body :payload-edn])]
                        (let [p (try (edn/read-string t) (catch Exception _ nil))]
                          (when-let [log (:log-artifact p)]
                            {:id (:evidence/id e) :kind (:kind p) :log log
                             :reader (get p :reader-version 0)}))))
                    entries)]
  (println "tagged entries:" (count entries) " records carrying a log:" (count records))
  (let [by (group-by #(cond (nil? (:sha256 (:log %)))            :no-sha
                           (ledger/holds? (:sha256 (:log %)))    :in-ledger
                           :else                                 :MISSING)
                     records)]
    (doseq [[k v] by] (println " " k (count v)))
    (doseq [r (:MISSING by)]
      (println "   MISSING" (subs (:id r) 14 26) (:sha256 (:log r)) (:path (:log r))
               "path-exists:" (.exists (io/file (str (:path (:log r)))))))
    (println "\nledger objects on disk:"
             (count (filter #(.isFile %) (file-seq (io/file ledger/default-root)))))
    (println "records still relying on the path fallback:" (count (:MISSING by)))))
