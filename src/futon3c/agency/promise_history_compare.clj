(ns futon3c.agency.promise-history-compare
  "Pure migration comparison; evidence remains the caller-visible authority."
  (:require [clojure.set :as set]
            [futon3c.agency.promise-history :as history])
  (:import [java.time Instant]))

(def dual-write-live-at
  "Rows before this instant predate the production dual writer."
  "2026-11-05T17:10:00Z")

(defn- before-dual-write? [row]
  (try
    (.isBefore (Instant/parse (:evidence/at row))
               (Instant/parse dual-write-live-at))
    (catch Throwable _ false)))

(defn- row-signature [row]
  (let [body (:evidence/body row)]
    {:evidence/type (:evidence/type row)
     :evidence/author (:evidence/author row)
     :evidence/at (:evidence/at row)
     :history/payload-edn (:history/payload-edn body)
     :history/promise-id (:history/promise-id body)
     :history/promise-sequence (:history/promise-sequence body)
     :history/predecessor (:history/predecessor body)}))

(defn compare-history
  "Compare operational LOG-ROWS with EVIDENCE-ROWS without choosing a new read
   authority. Pre-dual-write evidence-only rows are counted by event time and
   excluded from :only-evidence; all common rows compare their lossless link."
  [evidence-rows log-rows]
  (let [evidence-by-id (into {} (map (juxt :evidence/id identity)) evidence-rows)
        log-by-id (into {} (map (juxt :evidence/id identity)) log-rows)
        evidence-ids (set (keys evidence-by-id))
        log-ids (set (keys log-by-id))
        evidence-only (sort (set/difference evidence-ids log-ids))
        legacy-ids (->> evidence-only
                        (filter #(before-dual-write? (evidence-by-id %)))
                        set)
        only-evidence (vec (remove legacy-ids evidence-only))
        only-log (vec (sort (set/difference log-ids evidence-ids)))
        common (sort (set/intersection evidence-ids log-ids))
        mismatched (->> common
                        (remove #(= (row-signature (evidence-by-id %))
                                    (row-signature (log-by-id %))))
                        vec)
        legacy-by-time (->> legacy-ids
                            (map #(get-in evidence-by-id [% :evidence/at]))
                            frequencies
                            (into (sorted-map)))]
    {:only-evidence only-evidence
     :only-log only-log
     :mismatched mismatched
     :pre-dual-write-evidence-by-event-time legacy-by-time
     :pre-dual-write-evidence-count (count legacy-ids)
     :chain-check-evidence (history/check-chains evidence-rows)
     :chain-check-log (history/check-chains log-rows)}))
