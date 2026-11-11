(ns futon3c.agency.pattern-index
  "Freshness gate for the pattern-index sidecar (M-repl-circus packet 2),
   in the style of futon1b's ent/reads-usable?: readers call `usable?` per
   query and fall back (keyword search) when it answers false.

   Reads the sidecar's newest checkpoint DIRECTLY from SQLite, read-only
   (org.sqlite read-only connection) — not the `pattern_index.py status`
   subprocess: a per-query gate cannot pay a python startup, and SQLite
   readers do not block the builder's single transaction (WAL not needed;
   busy_timeout covers the brief writer window).

   Usable when: a checkpoint exists, its drift is empty, no embedding row is
   stale (its file_sha256 differs from the pattern row's — packet 1's drift
   counts pattern rows only, so the gate checks this itself; the same figure
   is in `pattern_index.py status`), and the last watcher build (when the
   watcher is running and has run) succeeded after that checkpoint.

   Cheap: the result is cached by the sidecar file's mtime plus a short TTL
   (default 1 s; the TTL is what lets a library-side deletion surface without
   a build — the sidecar's mtime does not move when the library loses a file)."
  (:require [clojure.java.io :as io]
            [cheshire.core :as json])
  (:import [org.sqlite SQLiteConfig]
           [java.sql DriverManager]))

(def default-db-path
  (or (System/getenv "FUTON3A_PATTERN_INDEX_DB")
      (str (System/getProperty "user.home")
           "/code/storage/futon3a/pattern-index.sqlite")))

(def futon3-root
  (or (System/getenv "FUTON3_ROOT")
      (str (System/getProperty "user.home") "/code/futon3")))

(def ^:dynamic *cache-ttl-ms* 1000)

(defonce ^:private !cache (atom nil)) ;; {:key [path mtime lb] :at ms :result {}}

(defn- read-only-conn ^java.sql.Connection [path]
  (let [config (doto (SQLiteConfig.) (.setReadOnly true))]
    (DriverManager/getConnection (str "jdbc:sqlite:" path)
                                 (.toProperties config))))

(defn- query [conn sql]
  (with-open [st (.createStatement conn)
              rs (.executeQuery st sql)]
    (let [md (.getMetaData rs)
          cols (mapv #(keyword (.getColumnLabel md (inc %)))
                     (range (.getColumnCount md)))]
      (loop [rows []]
        (if (.next rs)
          (recur (conj rows (zipmap cols (map #(.getObject rs (inc %))
                                              (range (count cols))))))
          rows)))))

(defn- check
  "The uncached freshness check against the sidecar at PATH.  OPTS:
   :last-build — the watcher's last-result map; :library-root — the root the
   pattern paths are relative to (default futon3-root)."
  [path {:keys [last-build library-root] :or {library-root futon3-root}}]
  (let [f (io/file path)]
    (if-not (.exists f)
      {:usable? false :why :no-sidecar :path path}
      (try
        (with-open [conn (read-only-conn path)]
          (let [ckpts (query conn (str "SELECT model, library_head,"
                                       " patterns_indexed, drift, finished_at,"
                                       " seq FROM checkpoint"
                                       " ORDER BY seq DESC"))
                by-model (reduce (fn [m r] (if (contains? m (:model r)) m
                                               (assoc m (:model r) r)))
                                 {} ckpts)
                stale (or (first (query conn (str "SELECT count(*) AS n"
                                                  " FROM embedding e"
                                                  " JOIN pattern p"
                                                  " ON p.id = e.pattern_id"
                                                  " WHERE e.file_sha256 <>"
                                                  " p.file_sha256")))
                          {:n 0})
                n-stale (long (:n stale))
                ;; library-side deletions: indexed rows whose file is gone
                missing-files (vec (keep (fn [r]
                                           (when-not (.exists
                                                      (io/file library-root
                                                               (:path r)))
                                             (:id r)))
                                         (query conn "SELECT id, path FROM pattern")))]
            (cond
              (empty? ckpts)
              {:usable? false :why :no-checkpoint}

              (pos? n-stale)
              {:usable? false :why :stale-embeddings :stale-embeddings n-stale}

              (seq missing-files)
              {:usable? false :why :drift
               :drift {:stale missing-files}}

              (some #(let [d (json/parse-string (:drift %) true)]
                       (or (seq (:missing d)) (seq (:stale d))))
                    (vals by-model))
              {:usable? false :why :drift
               :drift (into {} (map (fn [[m r]]
                                      [m (json/parse-string (:drift r) true)])
                                    by-model))}

              (and last-build (not (zero? (:exit last-build))))
              {:usable? false :why :last-build-failed
               :last-build last-build}

              :else
              {:usable? true :why :ok
               :checkpoint-at (:finished_at (first (vals by-model)))
               :patterns-indexed (:patterns_indexed (first (vals by-model)))})))
        (catch Throwable t
          {:usable? false :why :unreadable :error (.getMessage t)})))))

(defn usable?
  "Freshness gate for the pattern-index sidecar.  Arities:
   (usable?)              the shared sidecar
   (usable? path)         an explicit sidecar path
   (usable? path opts)    opts: :last-build (the watcher's last-result),
                          :library-root (default futon3-root)
   Cached on [path, sidecar mtime, opts fingerprint] for *cache-ttl-ms*;
   the TTL is what lets a library-side deletion surface without a build."
  ([] (usable? default-db-path {}))
  ([path] (usable? path {}))
  ([path opts]
   (let [f (io/file path)
         mtime (if (.exists f) (.lastModified f) -1)
         key [path mtime (:finished-at (:last-build opts))
              (:library-root opts)]
         now (System/currentTimeMillis)
         hit @!cache]
     (if (and hit (= (:key hit) key)
              (< (- now (:at hit)) *cache-ttl-ms*))
       (:result hit)
       (let [result (check path opts)]
         (reset! !cache {:key key :at now :result result})
         result)))))
