(ns futon3c.wm.meta-latest
  "GET /api/alpha/wm/meta-latest — serve the META field from the newest
  outer-task-selection receipt a real click retained on disk, without
  recomputing anything.

  Where the receipts live: the full-loop machinery persists each attempt's
  selection checkpoint as EDN under
    <root>/wm-contract-machinery-*-v1/attempt-*/002-selection.edn
  and the receipt is retained at [:payload :judgment :outer-task-selection]
  (futon2 full_loop_runner retain-outer-task-selection; the same file also
  carries typed absences {:status :absent :reason ...}, which are NOT
  receipts).  The files are large (tens of MB), so this namespace never
  parses a whole file: it scans the raw text for the :outer-task-selection
  key, extracts the balanced map that follows, and reads only that substring
  as EDN.  Candidates are visited newest-mtime first across the configured
  roots; the first real receipt wins.  The receipt is projected with
  futon2.aif.meta-live-outer-selector/browser-receipt so the shape matches
  GET /api/alpha/wm/meta-preview's :receipt exactly."
  (:require [clojure.edn :as edn]
            [clojure.string :as str])
  (:import [java.nio.file Files]))

(def default-roots
  "The machinery attempt trees, newest cohort style first (the pre-machinery
  wm-full-loop trees keep the same checkpoint file and key)."
  ["/home/joe/code/futon2/data/wm-full-loop-machinery"
   "/home/joe/code/futon2/data/wm-full-loop"])

(defn- selection-files
  "Every attempt's 002-selection.edn under ROOT (one level of cohort dirs,
  then one level of wm-*-v1 dirs, then attempt dirs)."
  [^java.io.File root]
  (vec
   (for [cohort (sort-by #(.getName ^java.io.File %) (.listFiles root))
         :when (.isDirectory ^java.io.File cohort)
         tree (.listFiles ^java.io.File cohort)
         :when (.isDirectory ^java.io.File tree)
         attempt (sort-by #(.getName ^java.io.File %) (.listFiles ^java.io.File tree))
         :when (.isDirectory ^java.io.File attempt)
         :let [f (java.io.File. ^java.io.File attempt "002-selection.edn")]
         :when (.isFile f)]
     f)))

(defn- newest-first
  "Files ordered by last-modified, newest first."
  [files]
  (sort-by (fn [^java.io.File f] (- (.lastModified f))) files))

(defn- match-delimiter
  "The index just past the balanced closing brace of the map that starts at
  OPEN in TEXT, skipping braces inside strings and \\char literals.  Nil when
  the file ends first (a truncated write)."
  [^String text open]
  (let [n (.length text)]
    (loop [i (inc open) depth 1 in-string false]
      (when (< i n)
        (let [c (.charAt text i)]
          (cond
            in-string (if (= c \\)
                        (recur (+ i 2) depth true)
                        (recur (inc i) depth (not= c \")))
            (= c \") (recur (inc i) depth true)
            (= c \\) (recur (+ i 2) depth false)
            (= c \{) (recur (inc i) (inc depth) false)
            (= c \}) (if (= 1 depth)
                       (inc i)
                       (recur (inc i) (dec depth) false))
            :else (recur (inc i) depth false)))))))

(defn- extract-receipt-at
  "The balanced EDN map that KEY maps to starting at offset START in TEXT
  (START points at the key), or nil when its value is not a map."
  [^String text start key-len]
  (let [k (+ start key-len)]
    (loop [k k]
      (when (< k (.length text))
        (let [c (.charAt text k)]
          (cond
            (Character/isWhitespace c) (recur (inc k))
            (= c \{) (when-let [end (match-delimiter text k)]
                       (subs text k end))
            :else nil))))))

(defn- key-occurrences
  "Every offset of KEY in TEXT."
  [^String text key]
  (loop [offsets [] from 0]
    (if-let [i (str/index-of text key from)]
      (recur (conj offsets i) (+ i (.length key)))
      offsets)))

(defn receipt-in
  "The retained receipt inside one selection checkpoint FILE's text, or nil.
  The file may mention the key in other positions (paths, pins) before the
  receipt itself, so every occurrence is tried.  A typed absence
  ({:status :absent ...}) is not a receipt."
  [^String text]
  (some (fn [start]
          (when-let [raw (extract-receipt-at text start (.length ":outer-task-selection"))]
            (try
              (let [v (edn/read-string raw)]
                (when (and (map? v)
                           (= :wm/outer-task-selection-v1 (:schema v))
                           (not= :absent (:status v)))
                  v))
              (catch Throwable _ nil))))
        (key-occurrences text ":outer-task-selection")))

(defn- attempt-of
  "The attempt directory name of FILE (attempt-042) for the :source block."
  [^java.io.File f]
  (.getName (.getParentFile f)))

(defn latest-retained-receipt
  "The newest retained outer-task-selection receipt on disk: a map
  {:receipt <raw v1 receipt> :text <its EDN text>
   :source {:path .. :attempt .. :tree .. :written-at ..}}
  or a typed absence {:absent ::no-retained-receipt :reason ... :scanned n}.
  Roots default to default-roots.  No computation, no selection, one file
  read per candidate newest-first until a receipt is found."
  ([] (latest-retained-receipt default-roots))
  ([roots]
   (let [files (->> (mapv #(java.io.File. (str %)) roots)
                    (filter #(.isDirectory ^java.io.File %))
                    (mapcat selection-files)
                    newest-first)]
     (loop [[^java.io.File f & more] files]
       (if (nil? f)
         {:absent ::no-retained-receipt
          :reason :no-retained-receipt
          :scanned (count files)}
         (let [text (String. (Files/readAllBytes (.toPath f)) "UTF-8")]
           (if-let [receipt (receipt-in text)]
             {:receipt receipt
              :source {:path (.getAbsolutePath f)
                       :attempt (attempt-of f)
                       :tree (.getName (.getParentFile (.getParentFile f)))
                       :written-at (str (java.time.Instant/ofEpochMilli
                                         (.lastModified f)))}}
             (recur more))))))))

(defn browser-projection
  "The raw receipt in meta-preview's :receipt shape (browser-receipt), or a
  typed refusal when futon2's projection cannot be resolved."
  [receipt]
  (if-let [project (requiring-resolve
                    'futon2.aif.meta-live-outer-selector/browser-receipt)]
    (project receipt)
    {:schema (:schema receipt)
     :status :refused :reason :browser-receipt-unavailable}))

(defn response
  "The meta-latest HTTP response body and status for ROOTS:
  200 {:ok true :receipt <browser shape> :source {...}}, or
  404 {:ok false :error :no-retained-receipt :scanned n}."
  ([] (response default-roots))
  ([roots]
   (let [found (latest-retained-receipt roots)]
     (if (:absent found)
       [404 {:ok false :error :no-retained-receipt
             :scanned (:scanned found)}]
       [200 {:ok true
             :receipt (browser-projection (:receipt found))
             :source (:source found)}]))))
