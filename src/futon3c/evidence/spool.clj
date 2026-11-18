(ns futon3c.evidence.spool
  "Durable write-ahead queue. Success is remote delivery, never local enqueue.
  One daemon owns replay per database in this JVM. HTTP runs outside the queue
  lock; SQLite commits and ownership changes are serialized, not network I/O."
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn])
  (:import [java.sql DriverManager Connection]))

(def ^:dynamic *db-path* nil)
(defonce ^:private test-dir
  (delay (str (java.nio.file.Files/createTempDirectory
               "futon3c-spool-test-" (make-array java.nio.file.attribute.FileAttribute 0)))))
(def response-retry-limit 5)

(defn db-path []
  (or *db-path* (System/getenv "FUTON3C_EVIDENCE_SPOOL_DB")
      (when (= "true" (System/getProperty "futon3c.test-run"))
        (str @test-dir "/evidence-spool.db"))
      (str (System/getProperty "user.home") "/storage/futon3c/evidence-spool.db")))
(defonce ^:private managers (atom {}))

(defn- connect ^Connection [path]
  (let [c (DriverManager/getConnection (str "jdbc:sqlite:" path))]
    (with-open [s (.createStatement c)]
      (.execute s "PRAGMA busy_timeout=30000")
      (.execute s "PRAGMA synchronous=FULL"))
    c))

(defn- execute! [^Connection c sql params]
  (with-open [s (.prepareStatement c sql)]
    (doseq [[i v] (map-indexed vector params)] (.setObject s (inc i) v))
    (.executeUpdate s)))

(defn- rows [^Connection c sql params]
  (with-open [s (.prepareStatement c sql)]
    (doseq [[i v] (map-indexed vector params)] (.setObject s (inc i) v))
    (with-open [r (.executeQuery s)]
      (let [meta (.getMetaData r)
            keys (mapv #(keyword (.getColumnLabel meta %))
                       (range 1 (inc (.getColumnCount meta))))]
        (loop [out []]
          (if (.next r)
            (recur (conj out (into {} (map-indexed
                                      (fn [i k] [k (.getObject r (inc i))]) keys))))
            out))))))

(defn- initialize! [path]
  (io/make-parents path)
  (with-open [c (connect path)]
    (with-open [s (.createStatement c)] (.execute s "PRAGMA journal_mode=WAL"))
    (execute! c "CREATE TABLE IF NOT EXISTS pending (
      seq INTEGER PRIMARY KEY AUTOINCREMENT, evidence_id TEXT NOT NULL,
      endpoint TEXT NOT NULL, body TEXT NOT NULL, attempts INTEGER NOT NULL DEFAULT 0,
      last_error TEXT, created_ms INTEGER NOT NULL, updated_ms INTEGER NOT NULL,
      UNIQUE(endpoint,evidence_id))" [])
    (execute! c "CREATE TABLE IF NOT EXISTS dead_letter (
      seq INTEGER PRIMARY KEY, evidence_id TEXT NOT NULL, endpoint TEXT NOT NULL,
      body TEXT NOT NULL, attempts INTEGER NOT NULL, last_error TEXT,
      created_ms INTEGER NOT NULL, updated_ms INTEGER NOT NULL)" [])
    ;; Upgrade pre-review WALs in place. Dependencies are extracted only once
    ;; here; the scheduler never reparses a payload.
    (doseq [table ["pending" "dead_letter"]]
      (let [columns (set (map :name (rows c (str "PRAGMA table_info(" table ")") [])))]
        (doseq [[column type] [["dep_a" "TEXT"] ["dep_b" "TEXT"]
                               ["next_attempt_ms" "INTEGER NOT NULL DEFAULT 0"]
                               ["failure_kind" "TEXT"]
                               ["response_failures" "INTEGER NOT NULL DEFAULT 0"]]]
          (when-not (contains? columns column)
            (execute! c (str "ALTER TABLE " table " ADD COLUMN " column " " type) [])))
        (when-not (contains? columns "dep_a")
          (doseq [row (rows c (str "SELECT seq,body FROM " table) [])]
            (let [e (edn/read-string (:body row))]
              (execute! c (str "UPDATE " table " SET dep_a=?,dep_b=? WHERE seq=?")
                        [(:evidence/in-reply-to e) (:evidence/fork-of e) (:seq row)]))))))))

(defn spool-status
  ([] (spool-status (db-path)))
  ([path]
   (if-not (.exists (io/file path))
     {:pending 0 :oldest-age-ms nil :dead-letter 0}
     (with-open [c (connect path)]
       (let [{:keys [n oldest]} (first (rows c "SELECT count(*) AS n,min(created_ms) AS oldest FROM pending" []))]
         {:pending n :oldest-age-ms (when oldest (max 0 (- (System/currentTimeMillis) oldest)))
          :dead-letter (:n (first (rows c "SELECT count(*) AS n FROM dead_letter" [])))})))))

(defn- head-row [m]
  (with-open [c (connect (:path m))]
    (first
     (remove #(contains? @(:active m) (:seq %))
             (rows c "SELECT p.* FROM pending p WHERE p.next_attempt_ms<=?
               AND NOT EXISTS (SELECT 1 FROM pending d WHERE d.endpoint=p.endpoint
                 AND (d.evidence_id=p.dep_a OR d.evidence_id=p.dep_b))
               ORDER BY p.seq ASC" [(System/currentTimeMillis)])))))

(defn- finish! [{:keys [lock] :as m} row outcome]
  (locking lock
    (let [failures (if (:bounded-response? outcome) (inc (:response_failures row)) 0)
          outcome (cond-> outcome
                    (>= failures response-retry-limit) (assoc :disposition :dead))
          now (System/currentTimeMillis)]
      (with-open [c (connect (:path m))]
        (.setAutoCommit c false)
        (try
          (execute! c "UPDATE pending SET last_error=?,updated_ms=?,failure_kind=?,
                        response_failures=?,next_attempt_ms=? WHERE seq=?"
                    [(pr-str (:result outcome)) now (some-> (:failure-kind outcome) name)
                     failures (+ now (if (zero? (:attempts row)) 0
                                         (long ((:backoff m) (:attempts row))))) (:seq row)])
          (when (contains? #{:delivered :dead} (:disposition outcome))
            (when (= :dead (:disposition outcome))
              (execute! c "INSERT INTO dead_letter SELECT * FROM pending WHERE seq=?" [(:seq row)]))
            (execute! c "DELETE FROM pending WHERE seq=?" [(:seq row)]))
          (.commit c)
          (catch Throwable t (.rollback c) (throw t))))
      (when (contains? #{:delivered :dead} (:disposition outcome))
        (when-let [p (get @(:waiters m) (:seq row))] (deliver p (:result outcome)))
        (swap! (:waiters m) dissoc (:seq row)))
      (swap! (:active m) disj (:seq row))
      outcome)))

(defn- attempt! [m row replay?]
  (try
    ;; Commit the attempt BEFORE POST: a crash after acceptance is a replay.
    (with-open [c (connect (:path m))]
      (execute! c "UPDATE pending SET attempts=attempts+1,updated_ms=? WHERE seq=?"
                [(System/currentTimeMillis) (:seq row)]))
    (let [outcome ((:send m) row replay?)]
      (finish! m row outcome))
    (catch Throwable t
      (swap! (:active m) disj (:seq row))
      (throw t))))

(defn- drain-loop [{:keys [lock] :as m}]
  (while (not @(:stopped m))
    (try
      (let [row (locking lock
                  (when-let [row (head-row m)]
                    (swap! (:active m) conj (:seq row)) row))]
        (if row
          (attempt! m row true)
          (Thread/sleep 10)))
      (catch InterruptedException _ nil)
      (catch Throwable t
        ;; SQLite failure leaves the durable row in place. Surface on stderr;
        ;; never delete a row merely because the worker encountered an error.
        (binding [*out* *err*] (println "[evidence-spool] drain failed:" (.getMessage t)))
        (Thread/sleep 100)))))

(defn start!
  "Return the single manager for PATH; start recovery of persisted rows. SEND
  returns {:disposition :delivered/:dead/:retry :result receipt}."
  [path send backoff]
  (let [path (.getCanonicalPath (io/file path))]
    (locking managers
      (or (get @managers path)
          (do
            (initialize! path)
            (let [m {:path path :send send :backoff backoff :lock (Object.)
                     :active (atom #{}) :waiters (atom {}) :stopped (atom false)}
                  t (Thread. ^Runnable #(drain-loop m) "evidence-spool-drainer")
                  m (assoc m :thread t)]
              (.setDaemon t true)
              (swap! managers assoc path m)
              (.start t)
              m))))))

(defn stop!
  "Stop this manager (shutdown/tests); pending rows stay durable for recovery."
  [path]
  (let [path (.getCanonicalPath (io/file path))]
    (locking managers
      (when-let [m (get @managers path)]
        (reset! (:stopped m) true)
        (.interrupt ^Thread (:thread m))
        (.join ^Thread (:thread m))
        (swap! managers dissoc path)))))

(defn append!
  "Commit BODY before any POST. Only pending dependencies block the fast path.
  Replay selects the oldest due dependency-ready row; locks never cover HTTP.
  UNREACHABLE constructs the existing timeout-window receipt."
  [{:keys [lock] :as m} endpoint eid body dependencies window-ms short-wait-ms unreachable]
  (let [started (System/nanoTime)
        elapsed #(quot (- (System/nanoTime) started) 1000000)
        [row p fast?]
        (locking lock
          (with-open [c (connect (:path m))]
            (let [prior (first (rows c "SELECT * FROM pending WHERE endpoint=? AND evidence_id=?" [endpoint eid]))
                  pending (rows c "SELECT seq,evidence_id,endpoint FROM pending" [])
                  parent-pending? (some #(and (= endpoint (:endpoint %))
                                              (contains? dependencies (:evidence_id %))) pending)
                  now (System/currentTimeMillis)]
              (when (and prior (not= body (:body prior)))
                (throw (ex-info "Conflicting payload for pending evidence id" {:evidence-id eid})))
              (when-not prior
                (execute! c "INSERT INTO pending(evidence_id,endpoint,body,created_ms,updated_ms,dep_a,dep_b) VALUES(?,?,?,?,?,?,?)"
                          [eid endpoint body now now (first dependencies) (second dependencies)]))
              (let [row (or prior (first (rows c "SELECT * FROM pending WHERE endpoint=? AND evidence_id=?" [endpoint eid])))
                    p (or (get @(:waiters m) (:seq row)) (promise))
                    fast? (and (nil? prior) (not parent-pending?))]
                (swap! (:waiters m) assoc (:seq row) p)
                (when fast? (swap! (:active m) conj (:seq row)))
                [row p fast?]))))
        first-out (when fast? (attempt! m row false))]
    (if (and first-out (:return-now? first-out))
      (assoc (:result first-out) :spooled true)
      (loop []
        (let [result (deref p 10 ::pending)]
          (if (not= ::pending result)
            result
            (let [answer
                  (with-open [c (connect (:path m))]
                    (let [current (first (rows c "SELECT * FROM pending WHERE seq=?" [(:seq row)]))
                          blockers (rows c "SELECT * FROM pending WHERE endpoint=?
                                            AND (evidence_id=? OR evidence_id=?)"
                                         [endpoint (:dep_a row) (:dep_b row)])
                          failures (if (seq blockers) blockers [current])
                          restart? (some #(= "connection" (:failure_kind %)) failures)
                          budget (if restart? window-ms (min window-ms short-wait-ms))]
                      (if (< (elapsed) budget)
                        ::again
                        (assoc (cond-> (unreachable (:last_error current)
                                                   (or (:attempts current) 0) (elapsed))
                                 (not restart?)
                                 (assoc :error/code :store-unavailable
                                        :error/message "Evidence delivery pending behind an unconfirmed write"))
                               :spooled true))))]
              ;; Poll outside the JDBC scope; a dependency can change from a
              ;; refused connection to a load response while this caller waits.
              (if (= ::again answer) (recur) answer))))))))
