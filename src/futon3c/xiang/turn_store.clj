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
   interpretation is never replaced by accident."
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

(defn write-record!
  "Create a new record file for RECORD and return {:id :path :record}.
   Exclusive creation: a colliding id is retried, never overwritten."
  [store record]
  (ensure-dir! store)
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

(defn read-record
  "The record for ID, or nil."
  [store id]
  (read-json (record-path store id)))

(defn update-record!
  "Replace ID's record with (F record) atomically; returns the new record.
   Throws {:reason :record-not-found} when there is none."
  [store id f]
  (let [path (record-path store id)
        record (or (read-json path)
                   (throw (ex-info "Record not found" {:reason :record-not-found :id id})))
        updated (f record)]
    (write-json! path updated)
    updated))

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

(defn list-records
  "Records in the store, newest first by created_at, optionally for one
   SESSION-ID, AGENT-ID and/or SURFACE (exact, e.g. \"matrix (!room:server)\",
   which is how a bridge names the room), as [{:id :record}]. LIMIT caps the
   result."
  [store & {:keys [session-id agent-id surface limit] :or {limit 200}}]
  (let [dir (io/file (:dir store))]
    (if-not (.isDirectory dir)
      []
      (->> (.listFiles dir)
           (filter (fn [^java.io.File f]
                     (and (.isFile f)
                          (re-matches #"turn-[A-Za-z0-9_-]+\.json" (.getName f)))))
           (keep (fn [^java.io.File f]
                   (let [id (str/replace (.getName f) #"\.json$" "")
                         record (try (read-json (.getPath f)) (catch Exception _ nil))]
                     (when (and (map? record)
                                (or (nil? session-id) (= session-id (:session_id record)))
                                (or (nil? agent-id) (= agent-id (:agent_id record)))
                                (or (nil? surface) (= surface (:surface record))))
                       {:id id :record record}))))
           (sort-by #(str (get-in % [:record :created_at])))
           reverse
           (take limit)
           vec))))
