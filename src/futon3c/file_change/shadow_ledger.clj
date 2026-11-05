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
  (let [{:keys [windows]}
        (reduce
         (fn [{:keys [baseline-at windows accounted stale-covered?] :as acc} probe]
           (let [known? (boolean? (:fresh? probe))
                 candidates (if known?
                              (filterv #(let [t (:first-at-ms %)]
                                          (and (< baseline-at t)
                                               (<= t (:at-ms probe))))
                                       demands)
                              [])
                 seen (filterv #(not (contains? accounted (:first-at-ms %)))
                               candidates)
                 stale? (false? (:fresh? probe))
                 covered? (and stale? (or stale-covered? (seq seen)))
                 window {:at-ms (:at-ms probe) :fresh? (:fresh? probe)
                         :minted? (boolean (:minted? probe))
                         :demand-count (count seen)
                         :unscored? (not known?)
                         :covered? (boolean covered?)
                         :miss? (and stale? (not covered?))
                         :spurious? (and (true? (:fresh? probe)) (seq seen))}
                 closes? (or (not known?) (true? (:fresh? probe))
                             (and stale? (:minted? probe)))]
             (cond-> (assoc acc :windows (conj windows window))
               stale? (update :accounted into (map :first-at-ms seen))
               (and stale? (not closes?)) (assoc :stale-covered? (boolean covered?))
               closes? (assoc :baseline-at (:at-ms probe)
                               :accounted #{} :stale-covered? false))))
         {:baseline-at (:at-ms (first probes)) :windows []
          :accounted #{} :stale-covered? false}
         (rest probes))]
    {:windows windows
     :misses (count (filter :miss? windows))
     :spurious-wakes (count (filter :spurious? windows))
     :unscored (count (filter :unscored? windows))
     :covered-stale (count (filter :covered? windows))}))
