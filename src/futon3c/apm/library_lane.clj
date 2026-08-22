(ns futon3c.apm.library-lane
  "Live derivation of APM work lanes from each problem's status.json."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def done-classifications
  #{"complete" "proved" "solved" "complete-lean-proof" "closed"})

(defn- normalized [x]
  (some-> x name str/lower-case (str/replace "_" "-")))

(defn- status-files [corpus-root]
  (let [root (io/file corpus-root "problems")]
    (if (.isDirectory root)
      (->> (.listFiles root)
           (filter #(.isDirectory %))
           (map #(io/file % "status.json"))
           (filter #(.isFile %)))
      [])))

(defn- read-status [file]
  (json/parse-string (slurp file) true))

(defn- note-text [status]
  (->> (tree-seq coll? seq (select-keys status [:note :notes :reason :blocker
                                                 :obstruction :finding]))
       (filter string?)
       (str/join " ")
       str/lower-case))

(defn lane-for [status lean-file?]
  (let [classification (normalized (or (:classification status) (:status status)))
        notes (note-text status)]
    (cond
      (contains? done-classifications classification) :done
      (or (str/includes? (or classification "") "statement-defect")
          (str/includes? (or classification "") "invalid-statement")
          (and (str/includes? notes "statement")
               (or (str/includes? notes "invalid")
                   (str/includes? notes "defect")))) :repair
      (not lean-file?) :formalize
      (and (or (str/includes? notes "infrastructure")
               (str/includes? notes "library"))
           (or (str/includes? notes "absent")
               (str/includes? notes "missing")
               (str/includes? notes "not in mathlib")
               (str/includes? notes "not available"))) :library
      :else :standard)))

(defn- records [corpus-root]
  (for [file (status-files corpus-root)
        :let [problem-dir (.getParentFile file)
              problem-id (.getName problem-dir)
              status (read-status file)
              lean-file (io/file problem-dir "lean" "Main.lean")]]
    {:problem-id problem-id
     :lane (lane-for status (.isFile lean-file))
     :sorry-count (long (or (:sorry_count_total status)
                            (:sorry-count status) 0))}))

(defn lanes [corpus-root]
  (into (sorted-map) (map (juxt :problem-id :lane)) (records corpus-root)))

(defn queue [corpus-root lane]
  (->> (records corpus-root)
       (filter #(= lane (:lane %)))
       (sort-by (juxt :sorry-count :problem-id))
       (mapv :problem-id)))
