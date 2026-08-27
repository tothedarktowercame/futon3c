(ns futon3c.apm.role-memory-search
  "Authenticated, recorded FTS over the reviewed mathematics memory corpus."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.typed-role-submission :as submission]
            [futon3c.peripheral.memory-recall :as recall])
  (:import (java.nio.file Files StandardCopyOption)
           (java.util UUID)))

(def ^:dynamic *receipt-root* "data/apm-role-memory-searches")
(def ^:dynamic *search-fn* recall/propose-patterns-by-query)
(def search-capable-roles #{:student :scribe :zai-scribe :promotion-proctor})
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

(defn withheld-for-authority
  "The holdout this job must not be served. Empty when the job carries no
   holdout, so attempts that were never held out are unaffected."
  [auth]
  (if (some? (:shelf/holdout auth))
    (set (filter string? (:shelf/withheld-ids auth)))
    #{}))

(defn enforce-holdout
  "Remove withheld memories from a search result before it reaches the role.
   Detection downstream is not enough: a withheld id the channel hands over
   has already been read by the time the terminal is validated."
  [result withheld]
  (if (empty? withheld)
    {:result result :excluded []}
    (let [excluded (->> (concat (keep :memory/id (:content-matches result))
                                (keep :pattern-id (:candidates result)))
                        (filter withheld) distinct sort vec)]
      {:result (-> result
                   (update :content-matches
                           #(vec (remove (comp withheld :memory/id) %)))
                   (update :candidates
                           #(vec (remove (comp withheld :pattern-id) %))))
       :excluded excluded})))

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
            withheld (withheld-for-authority auth)
            searched (*search-fn* {:domain :mathematics} query
                                  {:limit bounded-limit :trace-id trace-id})
            {:keys [result excluded]} (enforce-holdout searched withheld)
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
                  :shelf/holdout (:shelf/holdout auth)
                  :holdout/withheld-count (count withheld)
                  :holdout/excluded-ids excluded
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

(defn recorded-result-ids-for-job
  "Return result ids from content-address-valid receipts recorded for JOB-ID.
   This supports a terminal-repair job whose preserved session may still
   surface results recorded under its explicit :repair/of-job-id predecessor."
  [job-id]
  (let [directory (io/file *receipt-root* "receipts")]
    (if-not (and (string? job-id) (.isDirectory directory))
      #{}
      (->> (.listFiles directory)
           (filter #(.isFile ^java.io.File %))
           (keep (fn [file]
                   (try
                     (let [value (edn/read-string (slurp file))]
                       (when (and (= job-id (:job-id value))
                                  (= (:receipt/id value)
                                     (machine/ledger-digest
                                      [(dissoc value :receipt/id)])))
                         value))
                     (catch Throwable _ nil))))
           (mapcat :result-ids)
           (filter string?)
           set))))

(defn receipt-surfaced-ids
  "Return every reviewed-corpus identifier explicitly surfaced by RECEIPT.

   :result-ids names the primary matches. The returned content also exposes
   typed subject, mission, and pattern identifiers which a role must account
   for if it reports them as surfaced. This does not authorize identifiers
   absent from the content-addressed receipt."
  [receipt]
  (let [matches (:content-matches receipt)
        candidates (:candidates receipt)
        hyperedge-components
        (->> matches
             (map :memory/hyperedge-id)
             (filter string?)
             (mapcat #(str/split % #"\."))
             (filter #(str/starts-with? % "e-")))]
    (->> (concat (:result-ids receipt)
                 (map :memory/id matches)
                 (mapcat :memory/mission-ids matches)
                 (mapcat :memory/subject-ids matches)
                 (mapcat :memory/pattern-ids matches)
                 hyperedge-components
                 (map :pattern-id candidates)
                 (mapcat (fn [candidate]
                           (map :memory-id (:memory-support candidate)))
                         candidates))
         (filter string?)
         set)))

(defn recorded-surfaced-ids-for-job
  "Return identifiers explicitly surfaced by valid receipts for JOB-ID."
  [job-id]
  (let [directory (io/file *receipt-root* "receipts")]
    (if-not (and (string? job-id) (.isDirectory directory))
      #{}
      (->> (.listFiles directory)
           (filter #(.isFile ^java.io.File %))
           (keep (fn [file]
                   (try
                     (let [value (edn/read-string (slurp file))]
                       (when (and (= job-id (:job-id value))
                                  (= (:receipt/id value)
                                     (machine/ledger-digest
                                      [(dissoc value :receipt/id)])))
                         value))
                     (catch Throwable _ nil))))
           (mapcat receipt-surfaced-ids)
           set))))

(defn recorded-receipts-for-job
  "Return every content-address-valid search receipt recorded for JOB-ID.

   This is controller-owned evidence.  Consumers use it instead of asking a
   role to copy receipt ids, result ids, or query strings into its terminal
   submission."
  [job-id]
  (let [directory (io/file *receipt-root* "receipts")]
    (if-not (and (string? job-id) (.isDirectory directory))
      []
      (->> (.listFiles directory)
           (filter #(.isFile ^java.io.File %))
           (keep (fn [file]
                   (try
                     (let [value (edn/read-string (slurp file))]
                       (when (and (= job-id (:job-id value))
                                  (= (:receipt/id value)
                                     (machine/ledger-digest
                                      [(dissoc value :receipt/id)])))
                         value))
                     (catch Throwable _ nil))))
           (sort-by :receipt/id)
           vec))))

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
        ;; The typed JSON boundary keywordizes map keys, so a rationale keyed
        ;; by "math-formalization/x" arrives as :math-formalization/x; the
        ;; proposed ids stay strings. Normalize before the lookup.
        rationales (into {}
                         (map (fn [[k v]]
                                [(if (keyword? k) (subs (str k) 1) k) v]))
                         (or (:new-pattern-rationales evidence) {}))
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
