(ns futon3c.test-registry.sqlite-backend
  "Append-only local SQLite authority for test-registry evidence.

   This backend deliberately shares a database file with warrant_index.py but
   never reads or writes its `warrants` and `files` tables. Registry entries
   retain their exact EDN envelope and payload bytes; indexed columns only
   accelerate protocol queries and latest-run lookup."
  (:require [clojure.edn :as edn]
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
    (with-open [s (.prepareStatement c "INSERT OR IGNORE INTO registry_metadata(key,value) VALUES('generation','local-v1')")]
      (.executeUpdate s))))

(defn- initialized? [path]
  (try
    (with-open [c (connect path)
                s (.prepareStatement c
                    "SELECT 1 FROM registry_metadata WHERE key='generation'")
                r (.executeQuery s)]
      (.next r))
    (catch java.sql.SQLException e
      ;; An unopened database has no metadata table. Other storage failures
      ;; must remain failures rather than being mistaken for initialization.
      (if (re-find #"no such table: registry_metadata" (or (.getMessage e) ""))
        false
        (throw e)))))

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
     (when-not (initialized? path) (initialize! path))
     (->SQLiteBackend path))))

(defn latest-run-for-namespace [backend namespace]
  (latest (:path backend) "namespace" namespace))

(defn latest-run-for-command [backend command]
  (latest (:path backend) "command_key" (pr-str command)))

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
