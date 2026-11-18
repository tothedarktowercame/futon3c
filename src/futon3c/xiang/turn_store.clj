(ns futon3c.xiang.turn-store
  "The turn-record store, file-compatible with what Emacs writes.

   One directory (session-mode-turn-analysis-directory, by default
   ~/.emacs-graph/session-turn-analysis) holds one JSON file per operator
   turn, named turn-XXXXXX.json by make-temp-file, with the delegate's
   reading published beside it as turn-XXXXXX.json.analysis.json and any
   pattern proposals as turn-XXXXXX.json.candidates.json. Every downstream
   reader -- turn_frames.py, xlate.py census, feed.html, turn_dispatch_reap.py,
   the Emacs stepper and painter -- reads that layout, so the server-side
   store keeps it exactly rather than inventing a second one. A record this
   store writes is a record Emacs could have written, and the seam's
   conformance.py is the test of that claim.

   Writes are atomic (temp file + move) and the analysis is published with
   exclusive creation, as session_turn_analysis.py complete does: an earlier
   interpretation is never replaced by accident. Session-end transitions use
   FUTON3C_SESSION_END_OUTBOX when set, otherwise session-ends.jsonl here."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.nio.file Files StandardCopyOption StandardOpenOption Path]
           [java.nio.file.attribute PosixFilePermissions]
           [java.security SecureRandom]))

(def default-directory
  (str (System/getProperty "user.home") "/.emacs-graph/session-turn-analysis"))

(defn store
  "A store over DIRECTORY (default: the Emacs directory, or
   FUTON3C_TURN_ANALYSIS_DIR when set)."
  ([] (store (or (System/getenv "FUTON3C_TURN_ANALYSIS_DIR") default-directory)))
  ([directory] {:dir (str directory)}))

(def ^:private id-alphabet "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
(def ^:private rng (SecureRandom.))

(defn- fresh-id []
  (str "turn-" (apply str (repeatedly 6 #(nth id-alphabet (.nextInt rng (count id-alphabet)))))))

(defn record-id?
  "A record id as the store names them; also what a path's base name is."
  [id]
  (boolean (and (string? id) (re-matches #"turn-[A-Za-z0-9_-]{1,32}" id))))

(defn record-path ^String [{:keys [dir]} id]
  (when-not (record-id? id) (throw (ex-info "Invalid record id" {:reason :invalid-record-id :id id})))
  (str dir "/" id ".json"))

(defn analysis-path ^String [store id] (str (record-path store id) ".analysis.json"))
(defn candidates-path ^String [store id] (str (record-path store id) ".candidates.json"))
(defn draft-path ^String [store id] (str (record-path store id) ".draft.json"))
(defn pattern-candidates-path ^String [store id] (str (record-path store id) ".patterns.json"))
(defn quotes-path
  "Joe's >>> blocks for ID, kept beside the record for display only. No
   brief names this file and 象 is never pointed at it."
  ^String [store id] (str (record-path store id) ".quotes.json"))

(defn- ensure-dir! [{:keys [dir]}]
  (let [f (io/file dir)]
    (.mkdirs f)
    (try (Files/setPosixFilePermissions (.toPath f) (PosixFilePermissions/fromString "rwx------"))
         (catch Exception _ nil))
    f))

(defn- write-json!
  "Write VALUE as JSON to PATH atomically via a sibling temp file."
  [^String path value]
  (let [target (.toPath (io/file path))
        tmp (Files/createTempFile (.getParent target) ".turn-write-" ".tmp"
                                  (make-array java.nio.file.attribute.FileAttribute 0))]
    (try
      (Files/write tmp (.getBytes (str (json/generate-string value {:pretty true}) "\n") "UTF-8")
                   (into-array StandardOpenOption [StandardOpenOption/WRITE StandardOpenOption/TRUNCATE_EXISTING]))
      (Files/move tmp target (into-array java.nio.file.CopyOption
                                         [StandardCopyOption/ATOMIC_MOVE StandardCopyOption/REPLACE_EXISTING]))
      (finally (Files/deleteIfExists tmp)))
    path))

(defn- read-json [^String path]
  (when (.exists (io/file path))
    (json/parse-string (slurp path :encoding "UTF-8") true)))

(declare list-records)

;; ------------------------------------------------- operator-turn strand links
;; Joe 2026-11-18: "shouldn't sessions simply maintain a linked-list of
;; backpointers?" Finding a turn's predecessor used to list the whole store
;; (51,703 files, ~0.3 s per call). Each new operator turn now carries
;; :prev_operator_turn_id (key present, nil = first in its session) and its
;; predecessor gets :next_operator_turn_id; a per-session head file names the
;; newest. A record without the key predates this and is read by scan.

(defn- operator-record? [record]
  (and (some? (:session_id record))
       (= "operator" (or (:origin record) "operator"))))

(defn- session-head-path ^String [{:keys [dir]} session-id]
  (str (io/file dir "session-heads"
                (str (java.net.URLEncoder/encode (str session-id) "UTF-8") ".json"))))

(defonce ^:private ^java.util.concurrent.ConcurrentHashMap session-locks
  (java.util.concurrent.ConcurrentHashMap.))

(defn- session-lock [store session-id]
  (.computeIfAbsent session-locks (str (:dir store) "\u0000" session-id)
                    (reify java.util.function.Function (apply [_ _] (Object.)))))

(defn- scan-session-tail
  "Newest operator turn of SESSION-ID by scan: only when no head file exists
   (the first new turn of a session that began before the links)."
  [store session-id]
  (->> (list-records store :session-id session-id :limit Long/MAX_VALUE)
       (filter #(operator-record? (:record %)))
       (sort-by (juxt #(str (get-in % [:record :created_at])) :id))
       last :id))

(defn- session-tail
  "The session's newest operator turn: the head file, advanced along any
   :next_operator_turn_id a crash left unrecorded there."
  [store session-id]
  (loop [id (let [f (io/file (session-head-path store session-id))]
              (if (.isFile f)
                (:head (json/parse-string (slurp f :encoding "UTF-8") true))
                (scan-session-tail store session-id)))
         seen #{}]
    (let [next-id (when id (:next_operator_turn_id
                           (json/parse-string
                            (or (try (slurp (record-path store id) :encoding "UTF-8")
                                     (catch Exception _ nil)) "null") true)))]
      (if (and next-id (not (seen next-id)))
        (recur next-id (conj seen id))
        id))))

(declare update-record!)

(defn- create-record! [store record]
  (loop [attempt 0]
    (let [id (fresh-id)
          path (record-path store id)
          ^Path p (.toPath (io/file path))]
      (if (try (Files/createFile p (make-array java.nio.file.attribute.FileAttribute 0)) true
               (catch java.nio.file.FileAlreadyExistsException _ false))
        (do (write-json! path record)
            {:id id :path path :record record})
        (if (< attempt 20)
          (recur (inc attempt))
          (throw (ex-info "Could not allocate a record id" {:reason :store-failure})))))))

(defn write-record!
  "Create a new record file for RECORD and return {:id :path :record}.
   Exclusive creation: a colliding id is retried, never overwritten.
   An operator turn is linked into its session's strand (see above)."
  [store record]
  (ensure-dir! store)
  (if-not (operator-record? record)
    (create-record! store record)
    (let [session-id (:session_id record)]
      #_{:clj-kondo/ignore [:locking-suspicious-lock]}
      (locking (session-lock store session-id)
        (let [prev (session-tail store session-id)
              written (create-record! store (assoc record :prev_operator_turn_id prev))]
          (when prev
            (try (update-record! store prev #(assoc % :next_operator_turn_id (:id written)))
                 (catch clojure.lang.ExceptionInfo _ nil)))
          (let [head (session-head-path store session-id)]
            (.mkdirs (.getParentFile (io/file head)))
            (write-json! head {:head (:id written)}))
          written)))))

(defn write-record-id!
  "Create RECORD at deterministic ID. Returns {:created? false ...} when ID
   already exists; never replaces an existing record."
  [store id record]
  (ensure-dir! store)
  (let [path (record-path store id)
        ^Path p (.toPath (io/file path))]
    (if (try
          (Files/createFile p (make-array java.nio.file.attribute.FileAttribute 0))
          true
          (catch java.nio.file.FileAlreadyExistsException _ false))
      (do (write-json! path record)
          {:id id :path path :record record :created? true})
      {:id id :path path :record (read-json path) :created? false})))

(defn read-record
  "The record for ID, or nil."
  [store id]
  (read-json (record-path store id)))

(defonce ^:private ^java.util.concurrent.ConcurrentHashMap record-locks
  (java.util.concurrent.ConcurrentHashMap.))

(defn- record-lock [^String path]
  (.computeIfAbsent record-locks path
                    (reify java.util.function.Function (apply [_ _] (Object.)))))

(defn update-record!
  "Replace ID's record with (F record) atomically; returns the new record.
   Throws {:reason :record-not-found} when there is none.

   The read-modify-write holds a per-record lock, so two updates in this
   JVM never interleave. Without it, two animals updating the same turn
   at once each wrote back the record they had read, and one field was
   lost (狗's :dog_buried on 2026-11-16, overwritten by its own GCC
   outcome write on another thread). F runs inside the lock: keep it a
   pure function of the record."
  [store id f]
  (let [path (record-path store id)]
    #_{:clj-kondo/ignore [:locking-suspicious-lock]} ; the lock is shared, from record-locks
    (locking (record-lock path)
      (let [record (or (read-json path)
                       (throw (ex-info "Record not found" {:reason :record-not-found :id id})))
            updated (f record)]
        (write-json! path updated)
        updated))))

(defn read-analysis
  "The published analysis for ID, or nil."
  [store id]
  (read-json (analysis-path store id)))

(defn analysis-published?
  [store id]
  (.exists (io/file (analysis-path store id))))

(defn publish-analysis!
  "Publish ANALYSIS for ID with exclusive creation, then flag the record
   analyzed and name the file (the analysis is the authority; the flag makes
   it findable). Throws {:reason :analysis-exists} on a second publication."
  [store id analysis]
  (let [path (analysis-path store id)
        ^Path p (.toPath (io/file path))
        bytes (.getBytes (str (json/generate-string analysis {:pretty true}) "\n") "UTF-8")]
    (try
      (Files/write p bytes (into-array StandardOpenOption [StandardOpenOption/CREATE_NEW StandardOpenOption/WRITE]))
      (catch java.nio.file.FileAlreadyExistsException _
        (throw (ex-info "An interpretation is already published for this record"
                        {:reason :analysis-exists :id id}))))
    (try
      (update-record! store id #(assoc % :analysis_status "analyzed"
                                       :analysis_file (.toString (.toAbsolutePath p))))
      (catch Exception _ nil)) ; the analysis is published; a flag is not worth losing it over
    {:id id :path path}))

(defn read-candidates [store id] (read-json (candidates-path store id)))

(defn read-pattern-candidates
  "The precomputed pattern candidates for ID ({query [hit ...]}), or nil."
  [store id]
  (read-json (pattern-candidates-path store id)))

(defn write-pattern-candidates!
  "Keep the candidates xlate.py found for ID's fragments beside the record.
   Replaceable: a later dispatch may recompute them."
  [store id candidates]
  (let [path (pattern-candidates-path store id)]
    (write-json! path candidates)
    {:id id :path path}))

(defn write-quotes!
  "Write ID's quoted blocks to their display-only sidecar."
  [store id quotes]
  (write-json! (quotes-path store id) (vec quotes)))

(defn read-draft
  "小象's draft for ID, or nil."
  [store id]
  (read-json (draft-path store id)))

(defn write-draft!
  "Write (or replace) the classical draft beside the record and flag the
   record. A draft is cheap and reproducible, so unlike the analysis it may
   be replaced by a later one."
  [store id draft]
  (let [path (draft-path store id)]
    (write-json! path draft)
    (try (update-record! store id #(assoc % :draft_status "drafted" :draft_file path))
         (catch Exception _ nil))
    {:id id :path path}))

(def ^:private record-index
  "Parsed records by path, each with the file's mtime (ms) and length at the
   time it was read. list-records used to consult the directory by parsing
   every turn-*.json in it (6920 files, 9 MB on 2026-10-15) to find the few
   dozen of one session, about 600 ms a call, and the scopes route paid that
   on every request. A file whose mtime and length are unchanged is served
   from here; one that changed, or is new, is read again; one that has gone
   is forgotten. Keyed by store directory so test stores never meet."
  (atom {}))

(defn- indexed-record
  "The index entry for F: the one on hand when the file is unchanged, else a
   fresh read."
  [index ^java.io.File f]
  (let [path (.getPath f)
        mtime (.lastModified f)
        length (.length f)
        hit (get index path)]
    (if (and hit (= mtime (:mtime hit)) (= length (:length hit)))
      hit
      {:mtime mtime :length length
       :record (try (read-json path) (catch Exception _ nil))})))

(defn list-records
  "Records in the store, newest first by created_at, optionally for one
   SESSION-ID, AGENT-ID and/or SURFACE (exact, e.g. \"matrix (!room:server)\",
   which is how a bridge names the room), as [{:id :record}]. LIMIT caps the
   result."
  [store & {:keys [session-id agent-id surface limit] :or {limit 200}}]
  (let [dir (io/file (:dir store))]
    (if-not (.isDirectory dir)
      []
      (let [files (filterv (fn [^java.io.File f]
                             (and (.isFile f)
                                  (re-matches #"turn-[A-Za-z0-9_-]+\.json" (.getName f))))
                           (.listFiles dir))
            index (get @record-index (:dir store) {})
            fresh (into {} (map (fn [^java.io.File f] [(.getPath f) (indexed-record index f)]))
                        files)]
        (swap! record-index assoc (:dir store) fresh)
        (->> files
             (keep (fn [^java.io.File f]
                     (let [id (str/replace (.getName f) #"\.json$" "")
                           record (:record (get fresh (.getPath f)))]
                       (when (and (map? record)
                                  (or (nil? session-id) (= session-id (:session_id record)))
                                  (or (nil? agent-id) (= agent-id (:agent_id record)))
                                  (or (nil? surface) (= surface (:surface record))))
                         {:id id :record record}))))
             (sort-by #(str (get-in % [:record :created_at])))
             reverse
             (take limit)
             vec)))))
