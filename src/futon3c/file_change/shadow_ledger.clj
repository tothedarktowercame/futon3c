(ns futon3c.file-change.shadow-ledger
  "Durable observations and offline scoring for Giraffe shadow demands."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon3c.giraffe.run :as giraffe-run]))

(def default-path
  "/home/joe/code/futon3c/data/file-change-hygiene/giraffe-shadow-demands.jsonl")

(def ^:private append-lock (Object.))

(declare score-intervals)

(defn append!
  "Durably append one demand observation. Returns the record."
  [path record]
  (locking append-lock
    (let [file (io/file path)]
      (.mkdirs (.getParentFile file))
      (with-open [stream (java.io.FileOutputStream. file true)
                  writer (java.io.OutputStreamWriter. stream "UTF-8")]
        (.write writer (json/generate-string record))
        (.write writer "\n")
        (.flush writer)
        (.sync (.getFD stream)))))
  record)

(defn read-records [path]
  (if (.isFile (io/file path))
    (with-open [reader (io/reader path)]
      (mapv #(json/parse-string % true) (line-seq reader)))
    []))

(defn sealed-field-runs
  "Read verified field-run history from Giraffe storage, oldest first."
  [root]
  (->> (.listFiles (io/file root))
       (filter #(.isDirectory ^java.io.File %))
       (keep (fn [dir]
               (try
                 (let [manifest (edn/read-string
                                 (slurp (io/file dir "manifest.edn")))]
                   (when (and (not (contains? (:sources manifest) :tickets-index))
                              (:ok? (giraffe-run/verify-run dir)))
                     {:run-id (.getName ^java.io.File dir)
                      :at-ms (.lastModified (io/file dir "manifest.edn"))
                      :input-sha256 (giraffe-run/sha256
                                     (io/file dir "input.edn"))}))
                 (catch Exception _ nil))))
       (sort-by :at-ms)
       vec))

(defn score-log
  "Score the durable shadow ledger against verified Giraffe field history."
  ([path] (score-log path giraffe-run/default-root))
  ([path root]
   (score-intervals (read-records path) (sealed-field-runs root))))

(defn score-intervals
  "Score demand coverage between consecutive sealed runs.

  RUNS are ordered maps with :at-ms and :input-sha256. A changed interval with
  no demand is a miss; an unchanged interval with demands is a spurious wake."
  [demands runs]
  (let [intervals
        (mapv (fn [[before after]]
                (let [seen (filterv #(let [t (:emitted-at-ms %)]
                                       (and (< (:at-ms before) t)
                                            (<= t (:at-ms after))))
                                    demands)
                      changed? (not= (:input-sha256 before)
                                     (:input-sha256 after))]
                  {:from (:run-id before) :to (:run-id after)
                   :changed? changed? :demand-count (count seen)
                   :miss? (and changed? (empty? seen))
                   :spurious? (and (not changed?) (seq seen))}))
              (partition 2 1 runs))]
    {:intervals intervals
     :misses (count (filter :miss? intervals))
     :spurious-wakes (count (filter :spurious? intervals))
     :covered-changes (count (filter #(and (:changed? %)
                                           (pos? (:demand-count %))) intervals))}))
