(ns futon3c.evidence.spool
  "Durable write-ahead queue. Success is remote delivery, never local enqueue.
  One daemon owns replay per database in this JVM. HTTP runs outside the queue
  lock; SQLite commits and ownership changes are serialized, not network I/O."
  (:require [clojure.java.io :as io])
  (:import [java.sql DriverManager Connection]))

(def ^:dynamic *db-path* nil)
(defn db-path []
  (or *db-path* (System/getenv "FUTON3C_EVIDENCE_SPOOL_DB")
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
      created_ms INTEGER NOT NULL, updated_ms INTEGER NOT NULL)" [])))

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
    (first (rows c "SELECT * FROM pending ORDER BY seq ASC LIMIT 1" []))))

(defn- finish! [{:keys [lock] :as m} row outcome]
  (locking lock
    (with-open [c (connect (:path m))]
      (.setAutoCommit c false)
      (try
        (if (contains? #{:delivered :dead} (:disposition outcome))
          (do
            (when (= :dead (:disposition outcome))
              (execute! c "UPDATE pending SET last_error=?,updated_ms=? WHERE seq=?"
                        [(pr-str (:result outcome)) (System/currentTimeMillis) (:seq row)])
              (execute! c "INSERT INTO dead_letter SELECT * FROM pending WHERE seq=?" [(:seq row)]))
            (execute! c "DELETE FROM pending WHERE seq=?" [(:seq row)]))
          (execute! c "UPDATE pending SET last_error=?,updated_ms=? WHERE seq=?"
                    [(pr-str (:result outcome)) (System/currentTimeMillis) (:seq row)]))
        (.commit c)
        (catch Throwable t (.rollback c) (throw t))))
    (when (contains? #{:delivered :dead} (:disposition outcome))
      (when-let [p (get @(:waiters m) (:seq row))] (deliver p (:result outcome)))
      (swap! (:waiters m) dissoc (:seq row)))
    (swap! (:active m) disj (:seq row))
    (when (= (:seq row) @(:replaying m)) (reset! (:replaying m) nil))
    outcome))

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
      (when (= (:seq row) @(:replaying m)) (reset! (:replaying m) nil))
      (throw t))))

(defn- drain-loop [{:keys [lock] :as m}]
  (while (not @(:stopped m))
    (try
      (let [row (locking lock
                  (when (empty? @(:active m))
                    (when-let [row (head-row m)]
                      (reset! (:replaying m) (:seq row))
                      (swap! (:active m) conj (:seq row)) row)))]
        (if row
          (let [out (attempt! m row true)]
            (when (= :retry (:disposition out))
              (Thread/sleep (long ((:backoff m) (:attempts row))))))
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
                     :active (atom #{}) :replaying (atom nil) :waiters (atom {}) :stopped (atom false)}
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
  "Commit BODY before any POST. Only a queue with no backlog uses the caller's fast
  path. Unrelated in-flight writes may coexist; pending parents block children. Otherwise wait on the daemon's FIFO delivery; locks never cover HTTP.
  UNREACHABLE constructs the existing timeout-window receipt."
  [{:keys [lock] :as m} endpoint eid body dependencies window-ms unreachable]
  (let [started (System/nanoTime)
        elapsed #(quot (- (System/nanoTime) started) 1000000)
        [row p fast?]
        (locking lock
          (with-open [c (connect (:path m))]
            (let [prior (first (rows c "SELECT * FROM pending WHERE endpoint=? AND evidence_id=?" [endpoint eid]))
                  pending (rows c "SELECT seq,evidence_id,endpoint FROM pending" [])
                  no-backlog? (every? #(contains? @(:active m) (:seq %)) pending)
                  parent-pending? (some #(and (= endpoint (:endpoint %))
                                              (contains? dependencies (:evidence_id %))) pending)
                  now (System/currentTimeMillis)]
              (when (and prior (not= body (:body prior)))
                (throw (ex-info "Conflicting payload for pending evidence id" {:evidence-id eid})))
              (when-not prior
                (execute! c "INSERT INTO pending(evidence_id,endpoint,body,created_ms,updated_ms) VALUES(?,?,?,?,?)"
                          [eid endpoint body now now]))
              (let [row (or prior (first (rows c "SELECT * FROM pending WHERE endpoint=? AND evidence_id=?" [endpoint eid])))
                    p (or (get @(:waiters m) (:seq row)) (promise))
                    fast? (and (nil? prior) (nil? @(:replaying m)) no-backlog? (not parent-pending?))]
                (swap! (:waiters m) assoc (:seq row) p)
                (when fast? (swap! (:active m) conj (:seq row)))
                [row p fast?]))))
        first-out (when fast? (attempt! m row false))]
    (if (and first-out (:return-now? first-out))
      (assoc (:result first-out) :spooled true)
      (let [result (deref p (max 0 (- (long window-ms) (elapsed))) ::pending)]
        (if (= ::pending result)
          (with-open [c (connect (:path m))]
            (let [current (first (rows c "SELECT * FROM pending WHERE seq=?" [(:seq row)]))]
              (assoc (unreachable (:last_error current) (or (:attempts current) 1) (elapsed))
                     :spooled true)))
          result)))))
