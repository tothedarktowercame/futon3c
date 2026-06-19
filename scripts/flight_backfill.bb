#!/usr/bin/env bb
;; M-first-flights build-order step 4: backfill pre-schema γ frames as
;; derivation-thin flight records (R6 — representable, flagged, never
;; trainable). Idempotent: skips run-ids that already have a .flight.edn.
;;
;;   bb --classpath src scripts/flight_backfill.bb [--dry-run] [--skip RUN-ID]
;;
;; Each emitted record is immediately re-read and run through the F1-F9
;; verifier; a backfill that does not conform-as-thin is reported and the
;; file still written (the report is the work item, not a silent drop).
(require '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[futon3c.aif.flight-record :as fr])
(load-file "scripts/flight_spec_verify.clj")

(def traces-dir "data/repl-traces")

(let [args (set *command-line-args*)
      dry? (contains? args "--dry-run")
      skips (->> *command-line-args*
                 (partition 2 1)
                 (keep (fn [[a b]] (when (= a "--skip") b)))
                 set)
      frames (->> (.listFiles (io/file traces-dir))
                  (map #(.getName %))
                  (filter #(re-matches #"live-[0-9a-f-]+\.edn" %))
                  sort)
      results
      (for [fname frames
            :let [run-id (subs fname 0 (- (count fname) 4))
                  flight-file (str traces-dir "/" run-id ".flight.edn")]]
        (cond
          (contains? skips run-id)
          {:run-id run-id :status :skipped-by-flag}

          (.exists (io/file flight-file))
          {:run-id run-id :status :already-has-record}

          :else
          (let [frame (edn/read-string (slurp (str traces-dir "/" fname)))]
            (if-not (seq (or (:trace frame) (:gamma frame)))
              {:run-id run-id :status :no-trace-skipped}
              (let [rec (fr/backfill-record frame)]
                (if dry?
                  {:run-id run-id :status :would-backfill}
                  (let [path (fr/write-flight-record! rec traces-dir)
                        report ((resolve 'scripts.flight-spec-verify/verify)
                                (edn/read-string (slurp path)))]
                    {:run-id run-id
                     :status (if (:conforms? report) :backfilled-conforms :backfilled-NONCONFORMING)
                     :mask (get-in report [:projection :validity-mask])
                     :violations (when-not (:conforms? report)
                                   (into {} (for [[k v] (:invariants report)
                                                  :when (not (:pass v))]
                                              [k (:violations v)])))})))))))]
  (doseq [r results]
    (println (format "%-46s %s%s" (:run-id r) (name (:status r))
                     (if (:mask r) (str " mask:" (name (:mask r))) ""))))
  (let [by-status (frequencies (map :status results))]
    (println)
    (println "summary:" (pr-str by-status))
    (doseq [r results :when (= :backfilled-NONCONFORMING (:status r))]
      (println "NONCONFORMING" (:run-id r) "->" (pr-str (:violations r))))
    (System/exit (if (pos? (get by-status :backfilled-NONCONFORMING 0)) 1 0))))
