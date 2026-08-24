(ns futon3c.apm.role-memory-search
  "Authenticated, recorded FTS over the reviewed mathematics memory corpus."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.typed-role-submission :as submission]
            [futon3c.peripheral.memory-recall :as recall])
  (:import (java.nio.file Files StandardCopyOption)
           (java.util UUID)))

(def ^:dynamic *receipt-root* "data/apm-role-memory-searches")
(def ^:dynamic *search-fn* recall/propose-patterns-by-query)
(def search-capable-roles #{:student :scribe :promotion-proctor})
(def max-results 10)

(defn- receipt-path [receipt-id]
  (io/file *receipt-root* "receipts" (str receipt-id ".edn")))

(defn- atomic-write! [file value]
  (io/make-parents file)
  (let [temporary (io/file (.getParentFile file)
                           (str "." (.getName file) "." (UUID/randomUUID) ".tmp"))]
    (spit temporary (str (pr-str value) "\n"))
    (Files/move (.toPath temporary) (.toPath file)
                (into-array StandardCopyOption
                            [StandardCopyOption/ATOMIC_MOVE
                             StandardCopyOption/REPLACE_EXISTING]))))

(defn- result-ids [result]
  (->> (concat (keep :memory/id (:content-matches result))
               (keep :pattern-id (:candidates result)))
       (filter string?) distinct sort vec))

(defn search!
  "Execute one bounded read-only query and persist its exact result receipt.
   Identity comes only from the registered role job authority."
  [job-id token query limit]
  (let [authenticated (submission/authenticate job-id token)
        auth (:authority authenticated)
        bounded-limit (when (integer? limit) (min max-results limit))]
    (cond
      (not (:ok authenticated)) authenticated
      (not (contains? search-capable-roles (:role auth)))
      {:ok false :error/code :role-memory-search-not-authorized
       :role (:role auth)}
      (or (not (string? query)) (empty? query)
          (not (integer? bounded-limit)) (not (pos? bounded-limit)))
      {:ok false :error/code :role-memory-search-request-invalid}
      :else
      (let [trace-id (machine/ledger-digest
                      ["apm-role-memory-search" job-id query bounded-limit])
            result (*search-fn* {:domain :mathematics} query
                                {:limit bounded-limit :trace-id trace-id})
            body {:receipt/type :apm-role-memory-search
                  :receipt/version 1
                  :corpus/scope :reviewed-mathematics
                  :job-id job-id
                  :dispatch/id (:dispatch/id auth)
                  :agent-id (:agent-id auth)
                  :frame-id (:frame-id auth)
                  :problem-id (:problem-id auth)
                  :phase (:phase auth)
                  :role (:role auth)
                  :query query
                  :limit bounded-limit
                  :trace-id trace-id
                  :index-as-of (:index-as-of result)
                  :result-ids (result-ids result)
                  :content-matches (:content-matches result)
                  :candidates (:candidates result)}
            receipt (assoc body :receipt/id (machine/ledger-digest [body]))
            file (receipt-path (:receipt/id receipt))
            existing (when (.isFile file) (edn/read-string (slurp file)))]
        (cond
          (and existing (not= existing receipt))
          {:ok false :error/code :role-memory-search-receipt-conflict}
          existing {:ok true :status :already-recorded :receipt existing}
          :else (do (atomic-write! file receipt)
                    {:ok true :status :recorded :receipt receipt}))))))

(defn receipt [receipt-id]
  (let [file (receipt-path receipt-id)]
    (when (.isFile file) (edn/read-string (slurp file)))))
