(ns futon3c.apm.role-memory-search
  "Authenticated, recorded FTS over the reviewed mathematics memory corpus."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
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

(defn- valid-receipt? [auth value]
  (and (= :apm-role-memory-search (:receipt/type value))
       (= (:job-id auth) (:job-id value))
       (= (:dispatch/id auth) (:dispatch/id value))
       (= (:role auth) (:role value))
       (= :reviewed-mathematics (:corpus/scope value))
       (= (:receipt/id value)
          (machine/ledger-digest [(dissoc value :receipt/id)]))))

(defn validate-claims
  "Validate terminal receipt references against persisted controller records.
   An empty vector honestly records that the role did not search."
  [auth receipt-ids]
  (let [values (when (vector? receipt-ids) (mapv receipt receipt-ids))
        findings (cond-> []
                   (not (vector? receipt-ids))
                   (conj :memory-search-receipt-ids-not-vector)
                   (and (vector? receipt-ids)
                        (not= (count receipt-ids) (count (distinct receipt-ids))))
                   (conj :memory-search-receipt-id-duplicate)
                   (and (contains? #{:scribe :promotion-proctor} (:role auth))
                        (vector? receipt-ids) (empty? receipt-ids))
                   (conj :canonical-pattern-search-required)
                   (and (vector? receipt-ids)
                        (some #(not (string? %)) receipt-ids))
                   (conj :memory-search-receipt-id-invalid)
                   (and values (some nil? values))
                   (conj :memory-search-receipt-missing)
                   (and values (some #(and % (not (valid-receipt? auth %))) values))
                   (conj :memory-search-receipt-authority-invalid))]
    (if (seq findings)
      {:ok false :error/code :role-memory-search-claims-invalid
       :findings findings}
      {:ok true :receipts values})))

(defn- pattern-ids-in [value]
  (->> (tree-seq coll? seq value)
       (filter map?)
       (mapcat #(when (vector? (:pattern-ids %)) (:pattern-ids %)))
       (filter string?) set))

(defn validate-pattern-accounting
  "When FTS discovers canonical pattern ids, require their reuse or an explicit
   rationale for each newly coined id."
  [receipt-values evidence]
  (let [discovered (->> receipt-values (mapcat :candidates)
                        (keep :pattern-id) (filter string?) set)
        proposed (pattern-ids-in evidence)
        rationales (or (:new-pattern-rationales evidence) {})
        unaccounted (->> (set/difference proposed discovered)
                         (remove #(let [reason (get rationales %)]
                                    (and (string? reason) (not-empty reason))))
                         set)]
    (if (and (seq discovered) (seq unaccounted))
      {:ok false :error/code :canonical-pattern-reuse-unaccounted
       :discovered-pattern-ids discovered
       :unaccounted-pattern-ids unaccounted}
      {:ok true :discovered-pattern-ids discovered
       :proposed-pattern-ids proposed})))
