(ns futon3c.giraffe.reread-queue
  "Durable, authority-reconstructible queue for stale Giraffe readings.

  This namespace does not dispatch readers, mutate a corpus, or mint a pass.
  Reconciliation derives desired queue state from the pure reading-drift
  classifier; SQLite only preserves claims and makes pending work visible."
  (:require [clojure.java.io :as io]
            [futon3c.giraffe.excursions :as excursions]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(def default-path
  (str (io/file (System/getProperty "user.home") ".local" "state" "futon3c"
                "giraffe-reread" "requests.sqlite")))

(def ^:private query-opts {:builder-fn rs/as-unqualified-lower-maps})

(defn- datasource [path]
  (let [file (io/file path)]
    (some-> file .getParentFile .mkdirs)
    (jdbc/get-datasource {:dbtype "sqlite" :dbname (.getAbsolutePath file)})))

(defn- ensure-column! [store column definition]
  (let [columns (set (map :name
                          (jdbc/execute! (:datasource store)
                                         ["PRAGMA table_info(reread_requests)"]
                                         query-opts)))]
    (when-not (contains? columns column)
      (jdbc/execute! (:datasource store)
                     [(str "ALTER TABLE reread_requests ADD COLUMN " definition)]))))

(defn open-store
  "Open PATH and ensure the queue schema. Tests must pass a temporary PATH."
  ([] (open-store default-path))
  ([path]
   (let [store {:path (str path) :datasource (datasource path)}]
     (jdbc/execute! (:datasource store) ["PRAGMA journal_mode=WAL"])
     (jdbc/execute!
      (:datasource store)
      [(str "CREATE TABLE IF NOT EXISTS reread_requests ("
            "target_id TEXT NOT NULL, source_sha TEXT NOT NULL, "
            "old_sha TEXT, source_path TEXT NOT NULL, state TEXT NOT NULL, "
            "requested_at_ms INTEGER NOT NULL, updated_at_ms INTEGER NOT NULL, "
            "lease_until_ms INTEGER, superseded_by_sha TEXT, superseded_reason TEXT, "
            "attempts INTEGER NOT NULL DEFAULT 0, last_error TEXT, landed_digest TEXT, "
            "PRIMARY KEY(target_id, source_sha))")])
     ;; REREAD-1 databases predate worker outcomes. Keep the durable queue
     ;; forward-compatible rather than requiring deletion/reconciliation.
     (ensure-column! store "attempts" "attempts INTEGER NOT NULL DEFAULT 0")
     (ensure-column! store "last_error" "last_error TEXT")
     (ensure-column! store "landed_digest" "landed_digest TEXT")
     (jdbc/execute! (:datasource store)
                    ["CREATE INDEX IF NOT EXISTS reread_requests_state_idx ON reread_requests(state, requested_at_ms)"])
     store)))

(defn- kw-state [row]
  (cond-> row
    (:state row) (update :state keyword)
    (:superseded_reason row) (update :superseded_reason keyword)
    (:last_error row) (update :last_error keyword)))

(defn- keyword-text [k]
  (if-let [ns (namespace k)] (str ns "/" (name k)) (name k)))

(defn requests
  "Return queue rows, optionally filtered by :target-id or :state."
  ([store] (requests store {}))
  ([store {:keys [target-id state]}]
   (let [[where params]
         (cond
           (and target-id state) [" WHERE target_id=? AND state=?" [target-id (name state)]]
           target-id [" WHERE target_id=?" [target-id]]
           state [" WHERE state=?" [(name state)]]
           :else ["" []])]
     (mapv kw-state
           (jdbc/execute! (:datasource store)
                          (into [(str "SELECT * FROM reread_requests" where
                                      " ORDER BY requested_at_ms, target_id, source_sha")]
                                params)
                          query-opts)))))

(defn- expire-leases! [tx now-ms]
  (jdbc/execute! tx
                 ["UPDATE reread_requests SET state='pending', lease_until_ms=NULL, updated_at_ms=? WHERE state='claimed' AND lease_until_ms<=?"
                  now-ms now-ms]))

(defn- supersede-other! [tx target-id source-sha now-ms]
  (jdbc/execute! tx
                 ["UPDATE reread_requests SET state='superseded', superseded_by_sha=?, superseded_reason='newer-source', lease_until_ms=NULL, updated_at_ms=? WHERE target_id=? AND source_sha<>? AND state IN ('pending','claimed')"
                  source-sha now-ms target-id source-sha]))

(defn- ensure-pending! [tx {:keys [target-id source-sha old-sha source-path]} now-ms]
  (let [row (jdbc/execute-one! tx
                               ["SELECT state FROM reread_requests WHERE target_id=? AND source_sha=?"
                                target-id source-sha]
                               query-opts)]
    (cond
      (nil? row)
      (jdbc/execute! tx
                     ["INSERT INTO reread_requests(target_id,source_sha,old_sha,source_path,state,requested_at_ms,updated_at_ms) VALUES(?,?,?,?, 'pending', ?, ?)"
                      target-id source-sha old-sha source-path now-ms now-ms])

      (= "superseded" (:state row))
      (jdbc/execute! tx
                     ["UPDATE reread_requests SET old_sha=?,source_path=?,state='pending',requested_at_ms=?,updated_at_ms=?,lease_until_ms=NULL,superseded_by_sha=NULL,superseded_reason=NULL WHERE target_id=? AND source_sha=?"
                      old-sha source-path now-ms now-ms target-id source-sha])

      :else nil)))

(defn reconcile!
  "Reconcile OPEN-ITEMS and checked COVERAGE at NOW-MS.

  Calls the pure `reading-drift` classifier. Changed targets become pending;
  a newer source supersedes an older request; returning to the corpus SHA
  supersedes outstanding work. Expired leases are released on every pass."
  [store open-items coverage now-ms]
  {:pre [(integer? now-ms) (not (neg? now-ms))]}
  (let [drift (excursions/reading-drift open-items coverage)
        changed (set (:reading-source-changed drift))
        by-id (into {} (map (juxt (comp keyword :id) identity)) (:members coverage))
        open-by-id (into {} (map (juxt :id identity)) open-items)]
    (jdbc/with-transaction [tx (:datasource store)]
      (expire-leases! tx now-ms)
      (doseq [target-id changed
              :let [item (get open-by-id target-id)
                    old (get by-id (keyword (name target-id)))
                    source (:source-pin item)
                    row {:target-id (name target-id)
                         :source-sha (:sha256 source)
                         :old-sha (get-in old [:source :sha256])
                         :source-path (:path source)}]]
        (when-not (and (:source-sha row) (:source-path row))
          (throw (ex-info "Changed reading lacks a pinned source"
                          {:kind :giraffe-reread/source-pin-missing :row row})))
        (supersede-other! tx (:target-id row) (:source-sha row) now-ms)
        (ensure-pending! tx row now-ms))
      ;; A source that again equals its admitted reading no longer needs work.
      (doseq [target-id (:read drift)
              :let [sha (get-in open-by-id [target-id :source-pin :sha256])]]
        (jdbc/execute! tx
                       ["UPDATE reread_requests SET state='superseded',superseded_by_sha=?,superseded_reason='source-returned-to-read',lease_until_ms=NULL,updated_at_ms=? WHERE target_id=? AND state IN ('pending','claimed')"
                        sha now-ms (name target-id)])))
    {:drift drift :requests (requests store)}))

(defn claim!
  "Claim the oldest pending request through NOW-MS+LEASE-MS."
  [store now-ms lease-ms]
  {:pre [(integer? now-ms) (pos-int? lease-ms)]}
  (jdbc/with-transaction [tx (:datasource store)]
    (expire-leases! tx now-ms)
    (when-let [row (jdbc/execute-one!
                    tx
                    ["SELECT target_id,source_sha FROM reread_requests WHERE state='pending' ORDER BY requested_at_ms,target_id,source_sha LIMIT 1"]
                    query-opts)]
      (jdbc/execute! tx
                     ["UPDATE reread_requests SET state='claimed',lease_until_ms=?,updated_at_ms=? WHERE target_id=? AND source_sha=? AND state='pending'"
                      (+ now-ms lease-ms) now-ms (:target_id row) (:source_sha row)])
      (some-> (jdbc/execute-one!
               tx ["SELECT * FROM reread_requests WHERE target_id=? AND source_sha=?"
                   (:target_id row) (:source_sha row)] query-opts)
              kw-state))))

(defn active-claim?
  "Is REQUEST still the live claimed row? Reconciliation may supersede it
  while an Agency job is running."
  [store request]
  (= "claimed"
     (:state (jdbc/execute-one!
              (:datasource store)
              ["SELECT state FROM reread_requests WHERE target_id=? AND source_sha=?"
               (:target_id request) (:source_sha request)]
              query-opts))))

(defn settle!
  "Record a terminal attempt OUTCOME for claimed REQUEST.

  :landed and :superseded are terminal. :failed increments attempts and
  returns to pending until MAX-ATTEMPTS, then remains visibly failed."
  [store request outcome now-ms max-attempts]
  {:pre [(contains? #{:landed :failed :superseded} (:status outcome))
         (pos-int? max-attempts)]}
  (jdbc/with-transaction [tx (:datasource store)]
    (let [current (jdbc/execute-one!
                   tx ["SELECT * FROM reread_requests WHERE target_id=? AND source_sha=?"
                       (:target_id request) (:source_sha request)] query-opts)]
      (when current
        (case (:status outcome)
          :landed
          (jdbc/execute! tx
                         ["UPDATE reread_requests SET state='landed',landed_digest=?,lease_until_ms=NULL,updated_at_ms=? WHERE target_id=? AND source_sha=? AND state='claimed'"
                          (:digest outcome) now-ms (:target_id request) (:source_sha request)])

          :superseded
          (jdbc/execute! tx
                         ["UPDATE reread_requests SET state='superseded',superseded_reason=?,lease_until_ms=NULL,updated_at_ms=? WHERE target_id=? AND source_sha=? AND state='claimed'"
                          (keyword-text (:reason outcome)) now-ms (:target_id request) (:source_sha request)])

          :failed
          (let [attempts (inc (long (or (:attempts current) 0)))
                state (if (>= attempts max-attempts) "failed" "pending")]
            (jdbc/execute! tx
                           ["UPDATE reread_requests SET state=?,attempts=?,last_error=?,lease_until_ms=NULL,updated_at_ms=? WHERE target_id=? AND source_sha=? AND state='claimed'"
                            state attempts (keyword-text (:reason outcome)) now-ms
                            (:target_id request) (:source_sha request)]))))
      (some-> (jdbc/execute-one!
               tx ["SELECT * FROM reread_requests WHERE target_id=? AND source_sha=?"
                   (:target_id request) (:source_sha request)] query-opts)
              kw-state))))
