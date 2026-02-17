(ns futon3c.transport.encyclopedia
  "PlanetMath/encyclopedia data access for transport-layer demos.

   This adapter reads corpus EDN files from a local directory and serves
   list/entry views for HTTP handlers. It intentionally keeps the same
   response shape as the legacy Futon3 encyclopedia endpoints so clients can
   point at refactored Futon3c without UI changes."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def ^:private default-corpus-root
  "/home/joe/code/planetmath")

(defonce ^:private corpus-cache (atom {}))

(defn clear-cache!
  "Clear cached corpus data."
  []
  (reset! corpus-cache {}))

(defn corpus-root
  "Resolve corpus root from options and environment."
  [{:keys [corpus-root]}]
  (or corpus-root
      (System/getenv "FUTON3C_PLANETMATH_ROOT")
      default-corpus-root))

(defn parse-int
  "Parse string S into integer, returning DEFAULT on parse failure."
  [s default]
  (try
    (if (or (nil? s) (str/blank? (str s)))
      default
      (Integer/parseInt (str s)))
    (catch Exception _
      default)))

(defn decode-uri-component
  "URL-decode string S (UTF-8). Returns nil for nil input."
  [s]
  (when (some? s)
    (java.net.URLDecoder/decode (str s) "UTF-8")))

(defn- corpus-file
  [root corpus-name]
  (io/file root (str corpus-name ".edn")))

(defn- load-corpus-from-file
  [^java.io.File f]
  (let [content (slurp f)
        parsed (edn/read-string content)]
    (if (vector? parsed) parsed (vec parsed))))

(defn load-corpus
  "Load corpus entries for CORPUS-NAME or return nil when missing."
  [opts corpus-name]
  (let [root (corpus-root opts)
        f (corpus-file root corpus-name)]
    (when (.exists f)
      (let [path (.getAbsolutePath f)
            mtime (.lastModified f)
            cached (get @corpus-cache path)]
        (if (and cached (= mtime (:mtime cached)))
          (:entries cached)
          (let [entries (load-corpus-from-file f)]
            (swap! corpus-cache assoc path {:mtime mtime :entries entries})
            entries))))))

(defn list-corpuses
  "List local corpus descriptors."
  [opts]
  (let [root (io/file (corpus-root opts))]
    (if (not (.exists root))
      []
      (->> (.listFiles root)
           (filter #(.isFile ^java.io.File %))
           (filter #(str/ends-with? (.getName ^java.io.File %) ".edn"))
           (map (fn [^java.io.File f]
                  (let [name (str/replace (.getName f) #"\.edn$" "")
                        entries (or (load-corpus opts name) [])]
                    {:corpus/id name
                     :corpus/count (count entries)
                     :corpus/source "planetmath"})))
           (sort-by :corpus/id)
           vec))))

(defn page-entries
  "Return paginated entry summaries for CORPUS-NAME or nil if missing."
  [opts corpus-name limit offset]
  (when-let [entries (load-corpus opts corpus-name)]
    (let [safe-limit (max 1 (min 2000 limit))
          safe-offset (max 0 offset)
          page (->> entries
                    (drop safe-offset)
                    (take safe-limit)
                    (map #(select-keys % [:entry/id :entry/title :entry/type
                                          :entry/msc-codes :entry/related]))
                    vec)]
      {:corpus corpus-name
       :total (count entries)
       :offset safe-offset
       :limit safe-limit
       :entries page})))

(defn find-entry
  "Return full entry map for ENTRY-ID in CORPUS-NAME or nil."
  [opts corpus-name entry-id]
  (when-let [entries (load-corpus opts corpus-name)]
    (first (filter #(= (:entry/id %) entry-id) entries))))

