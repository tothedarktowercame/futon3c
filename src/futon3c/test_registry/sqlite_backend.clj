(ns futon3c.test-registry.sqlite-backend
  "Append-only local SQLite authority for test-registry evidence.

   This backend deliberately shares a database file with warrant_index.py but
   never reads or writes its `warrants` and `files` tables. Registry entries
   retain their exact EDN envelope and payload bytes; indexed columns only
   accelerate protocol queries and latest-run lookup."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [futon3c.evidence.backend :as backend])
  (:import [java.sql Connection DriverManager]
           [java.time Instant]))

(def default-path "/home/joe/code/storage/test-registry/warrant-index.sqlite")
(def ^:private busy-timeout-ms 30000)

(defn- connect ^Connection [path]
  (let [c (DriverManager/getConnection (str "jdbc:sqlite:" path))]
    (doto (.createStatement c)
      (.execute (str "PRAGMA busy_timeout=" busy-timeout-ms))
      (.execute "PRAGMA foreign_keys=ON")
      (.close))
    c))

(defn- execute! [^Connection c sql]
  (doto (.createStatement c) (.execute sql) (.close)))

(defn- initialize! [path]
  (with-open [c (connect path)]
    (execute! c "PRAGMA journal_mode=WAL")
    (execute! c
      "CREATE TABLE IF NOT EXISTS registry_entries (
         id TEXT PRIMARY KEY, payload_text TEXT, payload_sha TEXT,
         envelope_edn TEXT NOT NULL, parent_id TEXT, fork_id TEXT,
         subject_edn TEXT, author TEXT, at TEXT NOT NULL, type_edn TEXT,
         claim_type_edn TEXT, session TEXT, ephemeral INTEGER NOT NULL DEFAULT 0,
         FOREIGN KEY(parent_id) REFERENCES registry_entries(id),
         FOREIGN KEY(fork_id) REFERENCES registry_entries(id))")
    (execute! c
      "CREATE TABLE IF NOT EXISTS registry_entry_tags (
         entry_id TEXT NOT NULL REFERENCES registry_entries(id), tag TEXT NOT NULL,
         PRIMARY KEY(entry_id, tag))")
    (execute! c "CREATE INDEX IF NOT EXISTS registry_tags_tag ON registry_entry_tags(tag, entry_id)")
    (execute! c
      "CREATE TABLE IF NOT EXISTS registry_runs (
         entry_id TEXT PRIMARY KEY REFERENCES registry_entries(id),
         repo_root TEXT, namespace TEXT, command_key TEXT, ran_at TEXT,
         finished_at TEXT, warrant INTEGER, revision TEXT,
         ran_order TEXT NOT NULL, finished_order TEXT NOT NULL)")
    (execute! c "CREATE INDEX IF NOT EXISTS registry_runs_namespace ON registry_runs(namespace, ran_order DESC, finished_order DESC, entry_id DESC)")
    (execute! c "CREATE INDEX IF NOT EXISTS registry_runs_command ON registry_runs(command_key, ran_order DESC, finished_order DESC, entry_id DESC)")
    (execute! c
      "CREATE TABLE IF NOT EXISTS registry_metadata (key TEXT PRIMARY KEY, value TEXT NOT NULL)")
    (execute! c
      "CREATE TABLE IF NOT EXISTS warrant_rerun_requests (
         request_id INTEGER PRIMARY KEY AUTOINCREMENT,
         namespace TEXT NOT NULL,
         repo TEXT NOT NULL,
         reason TEXT NOT NULL CHECK (reason IN ('stale','absent')),
         requested_at TEXT NOT NULL,
         state TEXT NOT NULL CHECK (state IN ('queued','running','done','failed')),
         entry_id TEXT,
         detail TEXT,
         finished_at TEXT)")
    (execute! c
      "CREATE UNIQUE INDEX IF NOT EXISTS warrant_rerun_one_active_namespace
         ON warrant_rerun_requests(namespace) WHERE state IN ('queued','running')")
    (with-open [s (.prepareStatement c "INSERT OR IGNORE INTO registry_metadata(key,value) VALUES('generation','local-v1')")]
      (.executeUpdate s))))

(defn- instant-order [x]
  (try
    (let [i (Instant/parse (str x))]
      (format "%020d:%09d" (.getEpochSecond i) (.getNano i)))
    (catch Exception _ "-0000000000000000001:000000000")))

(defn- decode-payload [entry]
  (let [text (get-in entry [:evidence/body :payload-edn])]
    (when (string? text)
      (try (edn/read-string text) (catch Exception _ nil)))))

(defn- namespace-of [{:keys [namespace command]}]
  (or namespace
      (when (sequential? command)
        (second (drop-while #(not= "-n" %) command)))))

(defn- error [code message & [context]]
  (cond-> {:error/component :test-registry-sqlite
           :error/code code :error/message message :error/at (str (Instant/now))}
    context (assoc :error/context context)))

(defn- exists-sql? [^Connection c id]
  (with-open [s (.prepareStatement c "SELECT 1 FROM registry_entries WHERE id=?")]
    (.setString s 1 id)
    (with-open [r (.executeQuery s)] (.next r))))

(defn- insert-entry! [^Connection c entry]
  (let [id (:evidence/id entry)
        parent (:evidence/in-reply-to entry)
        fork (:evidence/fork-of entry)
        body (:evidence/body entry)
        payload (decode-payload entry)]
    (cond
      (exists-sql? c id) (error :duplicate-id "Evidence id already exists" {:evidence-id id})
      (and parent (not (exists-sql? c parent)))
      (error :reply-not-found "in-reply-to references missing entry" {:evidence-id id :in-reply-to parent})
      (and fork (not (exists-sql? c fork)))
      (error :fork-not-found "fork-of references missing entry" {:evidence-id id :fork-of fork})
      :else
      (do
        (with-open [s (.prepareStatement c
          "INSERT INTO registry_entries VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?)")]
          (doseq [[i value] (map-indexed vector
                              [id (:payload-edn body) (:sha256 body) (pr-str entry)
                               parent fork (pr-str (:evidence/subject entry))
                               (:evidence/author entry) (:evidence/at entry)
                               (pr-str (:evidence/type entry))
                               (pr-str (:evidence/claim-type entry))
                               (:evidence/session-id entry)
                               (if (true? (:evidence/ephemeral? entry)) 1 0)])]
            (.setObject s (inc i) value))
          (.executeUpdate s))
        (with-open [s (.prepareStatement c "INSERT INTO registry_entry_tags(entry_id,tag) VALUES(?,?)")]
          (doseq [tag (distinct (:evidence/tags entry))]
            (.setString s 1 id) (.setString s 2 (pr-str tag)) (.addBatch s))
          (.executeBatch s))
        (when (= :run (:kind payload))
          (with-open [s (.prepareStatement c "INSERT INTO registry_runs VALUES(?,?,?,?,?,?,?,?,?,?)")]
            (doseq [[i value] (map-indexed vector
                                [id (:repo/root payload) (namespace-of payload)
                                 (when (contains? payload :command) (pr-str (:command payload)))
                                 (:ran-at payload) (:finished-at payload)
                                 (if (true? (:warrant? payload)) 1 0)
                                 (or (:git-head payload) (:revision payload))
                                 (instant-order (:ran-at payload))
                                 (instant-order (:finished-at payload))])]
              (.setObject s (inc i) value))
            (.executeUpdate s)))
        {:ok true :entry entry}))))

(defn- append-one! [path entry]
  (with-open [c (connect path)]
    ;; IMMEDIATE serializes concurrent writers before they perform the parent
    ;; and duplicate reads. A deferred transaction can deadlock on lock
    ;; upgrade even with busy_timeout enabled.
    (execute! c "BEGIN IMMEDIATE")
    (try
      (let [result (insert-entry! c entry)]
        (execute! c (if (:ok result) "COMMIT" "ROLLBACK"))
        result)
      (catch Throwable t
        (try (execute! c "ROLLBACK") (catch Throwable _))
        (throw t)))))

(defn- rows [path sql bind]
  (with-open [c (connect path)
              s (.prepareStatement c sql)]
    (doseq [[i value] (map-indexed vector bind)] (.setObject s (inc i) value))
    (with-open [r (.executeQuery s)]
      (loop [out []]
        (if (.next r) (recur (conj out (edn/read-string (.getString r 1)))) out)))))

(defn- latest [path column value]
  (first (rows path
           (str "SELECT e.envelope_edn FROM registry_runs r JOIN registry_entries e ON e.id=r.entry_id "
                "WHERE r." column "=? ORDER BY r.ran_order DESC,r.finished_order DESC,r.entry_id DESC LIMIT 1")
           [value])))

(defrecord SQLiteBackend [path]
  backend/EvidenceBackend
  (-append [_ entry] (append-one! path entry))
  (-get [_ id] (first (rows path "SELECT envelope_edn FROM registry_entries WHERE id=?" [id])))
  (-exists? [_ id] (with-open [c (connect path)] (exists-sql? c id)))
  (-query [_ params]
    (backend/filter-and-sort-entries
      (rows path "SELECT envelope_edn FROM registry_entries" []) params))
  (-count [this params] (count (backend/-query this (dissoc params :query/limit))))
  (-forks-of [_ id]
    (->> (rows path "SELECT envelope_edn FROM registry_entries WHERE fork_id=?" [id])
         (sort-by backend/entry-at) vec))
  (-delete! [_ ids]
    (error :append-only "Registry entries cannot be deleted" {:ids (vec ids) :compacted 0}))
  (-all [_] (rows path "SELECT envelope_edn FROM registry_entries" [])))

(defn sqlite-backend
  ([] (sqlite-backend default-path))
  ([path]
   (let [path (str path)]
     ;; Every schema change is expressed with IF NOT EXISTS, so opening an
     ;; established store also applies additive migrations without rewriting
     ;; registry evidence.
     (initialize! path)
     (->SQLiteBackend path))))

(defn latest-run-for-namespace [backend namespace]
  (latest (:path backend) "namespace" namespace))

(defn latest-run-for-command [backend command]
  (latest (:path backend) "command_key" (pr-str command)))

(defn- rerun-row [r]
  {:request-id (.getLong r "request_id")
   :namespace (.getString r "namespace")
   :repo (.getString r "repo")
   :reason (keyword (.getString r "reason"))
   :requested-at (.getString r "requested_at")
   :state (keyword (.getString r "state"))
   :entry-id (.getString r "entry_id")
   :detail (.getString r "detail")
   :finished-at (.getString r "finished_at")})

(defn- select-reruns [^Connection c where-sql bind]
  (with-open [s (.prepareStatement c
                  (str "SELECT request_id,namespace,repo,reason,requested_at,state,entry_id,detail,finished_at "
                       "FROM warrant_rerun_requests " where-sql
                       " ORDER BY request_id"))]
    (doseq [[i value] (map-indexed vector bind)]
      (.setObject s (inc i) value))
    (with-open [r (.executeQuery s)]
      (loop [out []]
        (if (.next r) (recur (conj out (rerun-row r))) out)))))

(defn- request-result [row created?]
  (assoc (select-keys row [:request-id :namespace :repo :reason :requested-at :state])
         :created? created?))

(defn request-rerun!
  "Return the active rerun request for NAMESPACE, or append one queued request.
  Only an absent or stale current-warrant lookup licenses this operation."
  [backend {:keys [namespace repo reason detail]}]
  (if-not (and (string? namespace) (not (str/blank? namespace))
               (string? repo) (not (str/blank? repo))
               (#{:absent :stale} reason))
    (error :invalid-rerun-request "Reruns require namespace, repo, and reason stale or absent"
           {:namespace namespace :repo repo :reason reason})
    (with-open [c (connect (:path backend))]
      (execute! c "BEGIN IMMEDIATE")
      (try
        (if-let [active (first (select-reruns c
                                 "WHERE namespace=? AND state IN ('queued','running')"
                                 [namespace]))]
          (do (execute! c "COMMIT") (request-result active false))
          (let [requested-at (str (Instant/now))]
            ;; Registration refusals use reason 'stale' because the existing
            ;; SQLite CHECK admits only stale/absent; DETAIL preserves why the
            ;; otherwise-green registration was refused without rebuilding it.
            (with-open [s (.prepareStatement c
                            "INSERT INTO warrant_rerun_requests
                               (namespace,repo,reason,requested_at,state,detail)
                             VALUES(?,?,?,?, 'queued', ?)")]
              (.setString s 1 namespace)
              (.setString s 2 repo)
              (.setString s 3 (name reason))
              (.setString s 4 requested-at)
              (.setString s 5 detail)
              (.executeUpdate s))
            (let [created (first (select-reruns c
                                   "WHERE namespace=? AND state='queued'"
                                   [namespace]))]
              (execute! c "COMMIT")
              (request-result created true))))
        (catch Throwable t
          (try (execute! c "ROLLBACK") (catch Throwable _))
          (throw t))))))

(defn claim-reruns!
  "Atomically move at most N oldest queued requests to running and return them."
  [backend n]
  (if-not (and (integer? n) (pos? n))
    (error :invalid-rerun-claim "Rerun claim count must be a positive integer" {:n n})
    (with-open [c (connect (:path backend))]
      (execute! c "BEGIN IMMEDIATE")
      (try
        (let [claimed (take n (select-reruns c "WHERE state='queued'" []))]
          (with-open [s (.prepareStatement c
                          "UPDATE warrant_rerun_requests SET state='running'
                           WHERE request_id=? AND state='queued'")]
            (doseq [{:keys [request-id]} claimed]
              (.setLong s 1 request-id)
              (.addBatch s))
            (.executeBatch s))
          (execute! c "COMMIT")
          (mapv #(assoc % :state :running) claimed))
        (catch Throwable t
          (try (execute! c "ROLLBACK") (catch Throwable _))
          (throw t))))))

(defn finish-rerun!
  "Finish a running request as done or failed. A done request must name its
  passing registry entry; a failed request may name a non-warrant run."
  [backend request-id {:keys [state entry-id detail]}]
  (cond
    (not (#{:done :failed} state))
    (error :invalid-rerun-finish "Rerun finish state must be done or failed"
           {:request-id request-id :state state})

    (and (= :done state) (or (not (string? entry-id)) (str/blank? entry-id)))
    (error :invalid-rerun-finish "A completed rerun must name its passing registry entry"
           {:request-id request-id :state state :entry-id entry-id})

    :else
    (with-open [c (connect (:path backend))]
      (execute! c "BEGIN IMMEDIATE")
      (try
        (let [row (first (select-reruns c "WHERE request_id=?" [request-id]))]
          (cond
            (nil? row)
            (do (execute! c "ROLLBACK")
                (error :rerun-request-not-found "Rerun request does not exist"
                       {:request-id request-id}))

            (not= :running (:state row))
            (do (execute! c "ROLLBACK")
                (error :rerun-not-running "Only a running rerun request can finish"
                       {:request-id request-id :state (:state row)}))

            :else
            (let [finished-at (str (Instant/now))]
              (with-open [s (.prepareStatement c
                              "UPDATE warrant_rerun_requests
                               SET state=?,entry_id=?,detail=?,finished_at=?
                               WHERE request_id=?")]
                (.setString s 1 (name state))
                (.setString s 2 entry-id)
                (.setString s 3 detail)
                (.setString s 4 finished-at)
                (.setLong s 5 request-id)
                (.executeUpdate s))
              (let [finished (first (select-reruns c "WHERE request_id=?" [request-id]))]
                (execute! c "COMMIT")
                finished))))
        (catch Throwable t
          (try (execute! c "ROLLBACK") (catch Throwable _))
          (throw t))))))

(defn rerun-requests
  "Read rerun request history, optionally filtered by state and namespace."
  [backend {:keys [state namespace]}]
  (let [[clauses bind] (reduce (fn [[clauses bind] [clause value]]
                                 (if (nil? value)
                                   [clauses bind]
                                   [(conj clauses clause) (conj bind value)]))
                               [[] []]
                               [["state=?" (some-> state name)]
                                ["namespace=?" namespace]])
        where-sql (if (seq clauses)
                    (str "WHERE " (str/join " AND " clauses))
                    "")]
    (with-open [c (connect (:path backend))]
      (select-reruns c where-sql bind))))

(defn append-batch!
  "Append ENTRIES in one transaction. Used by rebuild tooling and contention tests."
  [backend entries]
  (with-open [c (connect (:path backend))]
    (execute! c "BEGIN IMMEDIATE")
    (try
      (let [results (mapv #(insert-entry! c %) entries)]
        (execute! c (if (every? :ok results) "COMMIT" "ROLLBACK"))
        results)
      (catch Throwable t
        (try (execute! c "ROLLBACK") (catch Throwable _))
        (throw t)))))
