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
            "lease_until_ms INTEGER, claimed_by TEXT, superseded_by_sha TEXT, superseded_reason TEXT, "
            "attempts INTEGER NOT NULL DEFAULT 0, last_error TEXT, landed_digest TEXT, "
            "PRIMARY KEY(target_id, source_sha))")])
     ;; REREAD-1 databases predate worker outcomes. Keep the durable queue
     ;; forward-compatible rather than requiring deletion/reconciliation.
     (ensure-column! store "attempts" "attempts INTEGER NOT NULL DEFAULT 0")
     (ensure-column! store "retries" "retries INTEGER NOT NULL DEFAULT 0")
     (ensure-column! store "last_error" "last_error TEXT")
     (ensure-column! store "landed_digest" "landed_digest TEXT")
     (ensure-column! store "claimed_by" "claimed_by TEXT")
    ;; S2/GIRAFFE-RECOVERY (2026-11-15): a parse or landing failure must keep
    ;; the reader's raw answer so it can be re-parsed after a json-record fix,
    ;; instead of being silently discarded (29 valid readings were lost to a
    ;; prose-prefix bug before this column existed).
    (ensure-column! store "last_answer" "last_answer TEXT")
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
                 ["UPDATE reread_requests SET state='pending', lease_until_ms=NULL, claimed_by=NULL, updated_at_ms=? WHERE state='claimed' AND lease_until_ms<=?"
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

      :else
      ;; Reconciliation is authoritative for whether this is never-read.
      ;; Normalize rows created by an older classifier without resetting
      ;; their age or attempt history.
      (jdbc/execute! tx
                     ["UPDATE reread_requests SET old_sha=?,source_path=? WHERE target_id=? AND source_sha=?"
                      old-sha source-path target-id source-sha]))))

(defn reconcile!
  "Reconcile OPEN-ITEMS and checked COVERAGE at NOW-MS.

  Calls the pure `reading-drift` classifier. Changed and never-read targets
  become pending;
  a newer source supersedes an older request; returning to the corpus SHA
  supersedes outstanding work. Expired leases are released on every pass."
  [store open-items coverage now-ms]
  {:pre [(integer? now-ms) (not (neg? now-ms))]}
  (let [drift (excursions/reading-drift open-items coverage)
        changed (set (concat (:reading-source-changed drift)
                             (:unread-new drift)))
        unread (set (:unread-new drift))
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
                         :old-sha (when-not (contains? unread target-id)
                                    (get-in old [:source :sha256]))
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

(defn reconcile-item!
  "Reconcile only TARGET-ID using a freshly parsed item and checked coverage.
   Nil ITEM means the document is no longer in the open census. Other items'
   claims, leases and requests are untouched."
  [store target-id item coverage now-ms]
  (let [id (name target-id)]
    (jdbc/with-transaction [tx (:datasource store)]
      (jdbc/execute! tx
                     ["UPDATE reread_requests SET state='pending',lease_until_ms=NULL,claimed_by=NULL,updated_at_ms=? WHERE target_id=? AND state='claimed' AND lease_until_ms<=?"
                      now-ms id now-ms])
      (if-not item
        (jdbc/execute! tx
                       ["UPDATE reread_requests SET state='superseded',superseded_reason='item-not-open',lease_until_ms=NULL,updated_at_ms=? WHERE target_id=? AND state IN ('pending','claimed')"
                        now-ms id])
        (let [drift (excursions/reading-drift [item] coverage)
              source (:source-pin item)
              old (some #(when (= id (:id %)) %) (:members coverage))]
          (when-not (and (= id (name (:id item))) (:sha256 source) (:path source))
            (throw (ex-info "Item reconciliation requires matching identity and source"
                            {:kind :giraffe-reread/source-pin-missing :target-id id})))
          (if (seq (:read drift))
            (jdbc/execute! tx
                           ["UPDATE reread_requests SET state='superseded',superseded_by_sha=?,superseded_reason='source-returned-to-read',lease_until_ms=NULL,updated_at_ms=? WHERE target_id=? AND state IN ('pending','claimed')"
                            (:sha256 source) now-ms id])
            (do (supersede-other! tx id (:sha256 source) now-ms)
                (ensure-pending! tx {:target-id id :source-sha (:sha256 source)
                                     :source-path (:path source)
                                     :old-sha (when (= :read (:status old))
                                                (get-in old [:source :sha256]))} now-ms))))))
    {:requests (requests store {:target-id id})}))

(defn claim!
  "Claim the oldest pending request through NOW-MS+LEASE-MS.
  NEW-ITEM-CAP is the per-cycle allowance already remaining for rows whose
  old_sha is nil. This worker claims at most one row per cycle, so positive
  values admit one and zero admits none."
  [store now-ms lease-ms new-item-cap reader-id]
  {:pre [(integer? now-ms) (pos-int? lease-ms)]}
  (jdbc/with-transaction [tx (:datasource store)]
    (expire-leases! tx now-ms)
    (when-let [row (jdbc/execute-one!
                    tx
                    [(str "SELECT target_id,source_sha FROM reread_requests "
                          "WHERE state='pending' AND (old_sha IS NOT NULL OR ? > 0) "
                          "ORDER BY CASE WHEN old_sha IS NULL THEN 1 ELSE 0 END, "
                          "requested_at_ms,target_id,source_sha LIMIT 1")
                     new-item-cap]
                    query-opts)]
      (jdbc/execute! tx
                     ["UPDATE reread_requests SET state='claimed',lease_until_ms=?,claimed_by=?,updated_at_ms=? WHERE target_id=? AND source_sha=? AND state='pending'"
                      (+ now-ms lease-ms) reader-id now-ms (:target_id row) (:source_sha row)])
      (some-> (jdbc/execute-one!
               tx ["SELECT * FROM reread_requests WHERE target_id=? AND source_sha=?"
                   (:target_id row) (:source_sha row)] query-opts)
              kw-state))))

(defn active-claim?
  "Is REQUEST still the live claimed row? Reconciliation may supersede it
  while an Agency job is running."
  [store request]
  ;; Owner AND lease identify the claim: after a lease expires and another
  ;; reader reclaims the row, the former owner's claim is no longer live
  ;; (codex-7 review, 2026-11-16).
  (= "claimed"
     (:state (jdbc/execute-one!
              (:datasource store)
              ["SELECT state FROM reread_requests WHERE target_id=? AND source_sha=? AND claimed_by IS ? AND lease_until_ms IS ?"
               (:target_id request) (:source_sha request)
               (:claimed_by request) (:lease_until_ms request)]
              query-opts))))

(defn release-usage-claim!
  "Release only this claim after provider refusal, preserving its attempt count.
   Owner and lease guards prevent a late reply releasing a newer claim."
  [store request now-ms]
  (jdbc/execute! (:datasource store)
                 ["UPDATE reread_requests SET state='pending',last_error='reader-usage-exhausted',lease_until_ms=NULL,claimed_by=NULL,updated_at_ms=? WHERE target_id=? AND source_sha=? AND state='claimed' AND claimed_by IS ? AND lease_until_ms=?"
                  now-ms (:target_id request) (:source_sha request)
                  (:claimed_by request) (:lease_until_ms request)])
  (some-> (jdbc/execute-one! (:datasource store)
                           ["SELECT * FROM reread_requests WHERE target_id=? AND source_sha=?"
                            (:target_id request) (:source_sha request)] query-opts)
          kw-state))

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
                         ["UPDATE reread_requests SET state='landed',landed_digest=?,lease_until_ms=NULL,updated_at_ms=? WHERE target_id=? AND source_sha=? AND state='claimed' AND claimed_by IS ? AND lease_until_ms IS ?"
                          (:digest outcome) now-ms (:target_id request) (:source_sha request)
                          (:claimed_by request) (:lease_until_ms request)])

          :superseded
          (jdbc/execute! tx
                         ["UPDATE reread_requests SET state='superseded',superseded_reason=?,lease_until_ms=NULL,updated_at_ms=? WHERE target_id=? AND source_sha=? AND state='claimed' AND claimed_by IS ? AND lease_until_ms IS ?"
                          (keyword-text (:reason outcome)) now-ms (:target_id request) (:source_sha request)
                          (:claimed_by request) (:lease_until_ms request)])

          :failed
          (let [attempts (inc (long (or (:attempts current) 0)))
                state (if (>= attempts max-attempts) "failed" "pending")]
            (jdbc/execute! tx
                           ["UPDATE reread_requests SET state=?,attempts=?,last_error=?,last_answer=?,lease_until_ms=NULL,updated_at_ms=? WHERE target_id=? AND source_sha=? AND state='claimed' AND claimed_by IS ? AND lease_until_ms IS ?"
                            state attempts (keyword-text (:reason outcome))
                            (or (:answer-text outcome) (:last_answer current))
                            now-ms
                            (:target_id request) (:source_sha request)
                          (:claimed_by request) (:lease_until_ms request)]))))
      (some-> (jdbc/execute-one!
               tx ["SELECT * FROM reread_requests WHERE target_id=? AND source_sha=?"
                   (:target_id request) (:source_sha request)] query-opts)
              kw-state))))

(defn retry-failed!
  "Retry a cooled-off failed row without erasing its last error/answer.
   Compare the observed update time too: a concurrent newer failure must cool
   off independently. Returns true only when this call requeued the row."
  [store request now-ms retry-after-ms retry-limit]
  {:pre [(pos-int? retry-after-ms) (nat-int? retry-limit)]}
  (= 1 (:next.jdbc/update-count
        (jdbc/execute-one!
         (:datasource store)
         ["UPDATE reread_requests SET state='pending',attempts=0,retries=retries+1,lease_until_ms=NULL,claimed_by=NULL,updated_at_ms=? WHERE target_id=? AND source_sha=? AND state='failed' AND updated_at_ms=? AND updated_at_ms<? AND retries<?"
          now-ms (:target_id request) (:source_sha request) (:updated_at_ms request)
          (- now-ms retry-after-ms) retry-limit]))))

(defn requeue!
  "Return a terminally FAILED request to pending with a clean attempt
  history (a refusal that did no work is not a spent attempt). last_answer
  survives for the record; the stale last_error is cleared so a pending row
  never carries a misleading reason."
  [store request now-ms]
  {:pre [(contains? #{:failed} (:state request))]}
  (jdbc/execute! (:datasource store)
                 ["UPDATE reread_requests SET state='pending',attempts=0,last_error=NULL,lease_until_ms=NULL,claimed_by=NULL,updated_at_ms=? WHERE target_id=? AND source_sha=? AND state='failed'"
                  now-ms (:target_id request) (:source_sha request)])
  (some-> (jdbc/execute-one! (:datasource store)
                             ["SELECT * FROM reread_requests WHERE target_id=? AND source_sha=?"
                              (:target_id request) (:source_sha request)]
                             query-opts)
          kw-state))

(defn reclaim-for-retry!
  "Move a terminally FAILED request back to 'claimed' for READER-ID so a
  maintenance pass can settle it (settle! only writes claimed rows). Returns
  the reclaimed row, or nil when another writer got there first."
  [store request reader-id now-ms lease-ms]
  (let [updated (first (jdbc/execute! (:datasource store)
                                      ["UPDATE reread_requests SET state='claimed',claimed_by=?,lease_until_ms=?,updated_at_ms=? WHERE target_id=? AND source_sha=? AND state='failed'"
                                       reader-id (+ (long now-ms) (long lease-ms)) now-ms
                                       (:target_id request) (:source_sha request)]))
        n (long (or (:next.jdbc/update-count updated) 0))]
    (when (pos? n)
      ;; return the NEW lease: settle! guards on owner and lease
      (assoc request :state "claimed" :claimed_by reader-id
             :lease_until_ms (+ (long now-ms) (long lease-ms))))))

(def default-seat-failure-threshold 0.25)
(def default-seat-min-outcomes 5)

(defn seat-health
  "Per reader seat, terminal outcomes inside the window ending NOW-MS:
  landings vs failures by reason, the failure share, and a stop-dispatch
  verdict once a seat's share passes THRESHOLD over at least MIN-OUTCOMES
  outcomes. Pure over ROWS (recent-outcomes); the window makes the stop
  self-lifting: failures age out and dispatch resumes without state."
  ([rows now-ms window-ms]
   (seat-health rows now-ms window-ms {}))
  ([rows now-ms window-ms {:keys [threshold min-outcomes]
                           :or {threshold default-seat-failure-threshold
                                min-outcomes default-seat-min-outcomes}}]
   (let [cutoff (- (long now-ms) (long window-ms))
         acc (reduce (fn [acc {:keys [claimed_by state last_error updated_at_ms]}]
                       (if (or (nil? claimed_by)
                               (< (long updated_at_ms) cutoff))
                         acc
                         (if (= :landed state)
                           (update-in acc [claimed_by :landed] (fnil inc 0))
                           (update-in acc [claimed_by :failed (or last_error :unknown)]
                                      (fnil inc 0)))))
                     {} rows)]
     (into {} (map (fn [[seat {:keys [landed failed] :or {landed 0 failed {}}}]]
                     (let [failures (apply + (vals failed))
                           total (+ landed failures)
                           share (if (pos? total) (/ failures total) 0.0)]
                       [seat {:landed landed
                              :failed failed
                              :total total
                              :failure-share share
                              :stop-dispatch? (and (>= total min-outcomes)
                                                   (>= share threshold))}])))
                   acc))))

(defn recent-outcomes
  "Terminal outcome rows (state landed/failed) updated inside the window
  ending NOW-MS, for seat-health."
  [store now-ms window-ms]
  (mapv kw-state
        (jdbc/execute! (:datasource store)
                       ["SELECT claimed_by,state,last_error,updated_at_ms FROM reread_requests WHERE state IN ('landed','failed') AND updated_at_ms>=?"
                        (- (long now-ms) (long window-ms))]
                       query-opts)))
