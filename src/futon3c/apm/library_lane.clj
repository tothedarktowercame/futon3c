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

(def note-keys
  "status.json fields that actually carry obstruction prose.

  Verified against the corpus on 2026-08-22: :obstructions appears in 134
  bundles, :closer_note 57, :boundary 55, :classification_detail 42, :note 23,
  :notes 20.  Earlier drafts read :reason, :blocker, :obstruction (singular)
  and :finding, none of which occur even once, which silently emptied the
  :library lane."
  [:obstructions :closer_note :boundary :classification_detail :note :notes])

(defn- note-text [status]
  (->> (tree-seq coll? seq (select-keys status note-keys))
       (filter string?)
       (str/join " ")
       str/lower-case))

(defn areas
  "Return the sorted distinct :areas named by this problem's obstruction records.

  status.json carries obstructions as a LIST of maps, each with a controlled
  `:areas` vocabulary naming the missing mathematical infrastructure --
  singular-homology, mayer-vietoris, van-kampen, covering-space, transversality
  and so on.  58 bundles carry a non-empty set; 76 carry an obstruction record
  with none, meaning the blocker was not classified.

  This is the library signal, and it is structured, so it does not need prose
  matching.  An earlier draft grepped notes for the words \"infrastructure\" or
  \"library\"; no real obstruction note uses either, which emptied the lane."
  [status]
  (->> (:obstructions status)
       (filter map?)
       (mapcat :areas)
       (filter string?)
       distinct sort vec))

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
      (seq (areas status)) :library
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
     :areas (areas status)
     :sorry-count (long (or (get-in status [:lean :sorry_count_total])
                            (:sorry_count_total status)
                            (:sorry-count status) 0))}))

(defn lanes [corpus-root]
  (into (sorted-map) (map (juxt :problem-id :lane)) (records corpus-root)))

(defn queue
  "Problem ids in LANE, deterministically ordered.
  With AREA, restrict to problems whose obstruction records name it."
  ([corpus-root lane] (queue corpus-root lane nil))
  ([corpus-root lane area]
   (->> (records corpus-root)
        (filter #(= lane (:lane %)))
        (filter #(or (nil? area) (some #{area} (:areas %))))
        (sort-by (juxt :sorry-count :problem-id))
        (mapv :problem-id))))
