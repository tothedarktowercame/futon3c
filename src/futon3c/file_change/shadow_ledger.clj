(ns futon3c.file-change.shadow-ledger
  "Durable observations and offline scoring for Giraffe shadow demands."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]))

(def default-path
  (or (System/getenv "FUTON3C_FILE_CHANGE_SHADOW_LEDGER")
      "/home/joe/code/futon3c/data/file-change-hygiene/giraffe-shadow-demands.jsonl"))

(def ^:private append-lock (Object.))

(declare score-probes)

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

(defn score-log
  "Score the durable shadow ledger against chronological per-click freshness
  probes. Probe maps carry :at-ms and :fresh?."
  [path probes]
  (score-probes (read-records path) probes))

(defn score-probes
  "Score demands against every click's freshness verdict.

  Attribution uses :first-at-ms, so debounce emission after a stale probe does
  not move the triggering change into the following fresh interval."
  [demands probes]
  (let [windows
        (mapv (fn [[before probe]]
                (let [seen (filterv #(let [t (:first-at-ms %)]
                                       (and (< (:at-ms before) t)
                                            (<= t (:at-ms probe))))
                                    demands)
                      stale? (false? (:fresh? probe))]
                  {:at-ms (:at-ms probe) :fresh? (:fresh? probe)
                   :demand-count (count seen)
                   :miss? (and stale? (empty? seen))
                   :spurious? (and (not stale?) (seq seen))}))
              (partition 2 1 probes))]
    {:windows windows
     :misses (count (filter :miss? windows))
     :spurious-wakes (count (filter :spurious? windows))
     :covered-stale (count (filter #(and (false? (:fresh? %))
                                         (pos? (:demand-count %))) windows))}))
