#!/usr/bin/env bb
;; P3 / V2-5: scale the frozen D_state operator to one current-graph case per
;; problem with a recorded dispatch query.  This script deliberately reuses
;; damage_state_sweep.bb without editing it.  Capture reads the store; replay
;; is offline and write-once.

(ns damage-state-scale-sweep
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.io PushbackReader]
           [java.time Instant]))

(def root "/home/joe/code/futon3c/holes/labs/M-memory-retrieval")
(def harness-path (str root "/damage_state_sweep.bb"))
(def receipts-path (str root "/receipts-export-20260731-all-authors.edn"))
(def fixture-path (str root "/damage-state-scale-fixture-20260801.edn"))
(def result-path (str root "/damage-state-scale-results-20260801.edn"))
(def fixed-seed 20260731)

(def mandatory-caption
  "CURRENT-GRAPH STRUCTURAL SENSITIVITY, NOT HISTORICAL REPLAY: without dispatch-time snapshots this sweep measures the reviewed memory graph at capture time, not the graph as it stood at dispatch.")

(defn load-frozen-harness! []
  (binding [*ns* *ns*]
    (with-open [reader (PushbackReader. (io/reader harness-path))]
      (loop []
        (let [form (read {:eof ::eof} reader)]
          (when-not (= ::eof form)
            ;; The only non-definition top-level form runs the old two-case
            ;; experiment.  Skip it; all operator definitions remain frozen.
            (when-not (and (seq? form) (= 'let (first form)))
              (eval form))
            (recur)))))))

(defn harness-fn [sym]
  (or (ns-resolve 'damage-state-sweep sym)
      (throw (ex-info "frozen harness symbol unavailable" {:symbol sym}))))

(defn write-once! [path value]
  (let [file (io/file path)
        rendered (str (pr-str value) "\n")]
    (if (.exists file)
      (do
        (when-not (= rendered (slurp file))
          (throw (ex-info "refusing to overwrite nonidentical frozen artifact"
                          {:path path})))
        :existing)
      (do (spit file rendered) :written))))

(defn offered-receipt? [entry]
  (= :offered (get-in entry [:evidence/body :phase])))

(defn recorded-query [entry]
  (let [query (get-in entry [:evidence/body :recall-query :query])]
    (when (and (string? query) (not (str/blank? query))) query)))

(defn problem-id [entry]
  (get-in entry [:evidence/body :problem]))

(defn latest-entry [entries]
  (last (sort-by (juxt :evidence/at :evidence/id) entries)))

(defn receipt-census []
  (let [entries (:entries (edn/read-string (slurp receipts-path)))
        offered (filter offered-receipt? entries)
        by-problem (group-by problem-id offered)
        selected
        (->> by-problem
             (keep
              (fn [[problem rows]]
                (let [query-rows (filter recorded-query rows)]
                  (when (seq query-rows)
                    (let [entry (latest-entry query-rows)]
                      {:case-id problem
                       :query (recorded-query entry)
                       :query-provenance
                       {:problem problem
                        :job-id (get-in entry [:evidence/body :job-id])
                        :offered-evidence-id (:evidence/id entry)
                        :receipt-at (:evidence/at entry)
                        :selection-rule :latest-recorded-query-per-problem}})))))
             (sort-by :case-id)
             vec)
        missing
        (->> by-problem
             (keep
              (fn [[problem rows]]
                (when-not (some recorded-query rows)
                  {:problem problem
                   :reason :no-recorded-recall-query
                   :offered-evidence-ids
                   (->> rows (map :evidence/id) sort vec)})))
             (sort-by :problem)
             vec)]
    {:export-entry-count (count entries)
     :offered-row-count (count offered)
     :distinct-offered-problem-count (count by-problem)
     :selected-case-count (count selected)
     :pre-capture-exclusions missing
     :cases selected}))

(defn exception-data-safe [throwable]
  {:class (str (class throwable))
   :message (.getMessage throwable)
   :data (ex-data throwable)})

(defn capture-case-safe [capture-case! case]
  (try
    {:status :captured
     :case (capture-case! case)}
    (catch Throwable throwable
      {:status :capture-failed
       :case-id (:case-id case)
       :query (:query case)
       :query-provenance (:query-provenance case)
       :failure (exception-data-safe throwable)})))

(defn capture-fixture! []
  (when (.exists (io/file fixture-path))
    (throw (ex-info "scale fixture already exists; capture is write-once"
                    {:path fixture-path})))
  (let [census (receipt-census)
        capture-case! (harness-fn 'capture-case!)
        captured (mapv #(capture-case-safe capture-case! %) (:cases census))
        fixture
        {:fixture/version 2
         :experiment :P3-V2-5-D-state-scale
         :captured-at (str (Instant/now))
         :seed fixed-seed
         :determinism :sorted-complete-enumeration-no-random-sampling
         :source
         {:receipts-file (.getName (io/file receipts-path))
          :graph :current-reviewed-memory-graph
          :store-read-only? true}
         :claim-boundary {:caption mandatory-caption}
         :census (dissoc census :cases)
         :captures captured}]
    (write-once! fixture-path fixture)
    fixture))

(defn nonempty-baseline? [case-result]
  (seq (get-in case-result [:baseline :candidate-ids])))

(defn perturbation-count [case-result]
  (+ (get-in case-result [:edge-removals :summary :perturbation-count] 0)
     (get-in case-result [:pattern-role-removals :summary :perturbation-count] 0)))

(defn usable-case? [case-result]
  (and (nonempty-baseline? case-result)
       (pos? (perturbation-count case-result))))

(defn sum-path [cases path]
  (reduce + 0 (map #(long (get-in % path 0)) cases)))

(defn mean [xs]
  (if (seq xs) (/ (reduce + 0.0 xs) (double (count xs))) 0.0))

(defn maximum [xs]
  (reduce max 0.0 xs))

(defn aggregate [usable]
  (let [edge-n (sum-path usable [:edge-removals :summary :perturbation-count])
        edge-changed (sum-path usable [:edge-removals :summary :changed-count])
        role-n (sum-path usable [:pattern-role-removals :summary :perturbation-count])
        role-changed (sum-path usable [:pattern-role-removals :summary :changed-count])
        all-edge-rows (mapcat #(get-in % [:edge-removals :rows]) usable)
        all-role-rows (mapcat #(get-in % [:pattern-role-removals :rows]) usable)]
    {:caption mandatory-caption
     :usable-problem-count (count usable)
     :baseline
     {:mean-candidate-count
      (mean (map #(count (get-in % [:baseline :candidate-ids])) usable))}
     :edge-removal
     {:perturbation-count edge-n
      :changed-count edge-changed
      :changed-fraction (if (pos? edge-n) (/ (double edge-changed) edge-n) 0.0)
      :mean-jaccard-distance
      (mean (map #(get-in % [:divergence :jaccard-distance]) all-edge-rows))
      :maximum-jaccard-distance
      (maximum (map #(get-in % [:divergence :jaccard-distance]) all-edge-rows))}
     :pattern-role-removal
     {:perturbation-count role-n
      :changed-count role-changed
      :changed-fraction (if (pos? role-n) (/ (double role-changed) role-n) 0.0)
      :mean-jaccard-distance
      (mean (map #(get-in % [:divergence :jaccard-distance]) all-role-rows))
      :maximum-jaccard-distance
      (maximum (map #(get-in % [:divergence :jaccard-distance]) all-role-rows))}
     :arm-ablation
     {:without-content-changed-problems
      (count (filter #(get-in % [:arm-ablations :without-content :divergence :changed?]) usable))
      :without-pattern-changed-problems
      (count (filter #(get-in % [:arm-ablations :without-pattern :divergence :changed?]) usable))}}))

(defn run-sweep [fixture]
  (let [run-case (harness-fn 'run-case)
        captured (keep #(when (= :captured (:status %)) (:case %))
                       (:captures fixture))
        capture-failures (filterv #(= :capture-failed (:status %))
                                  (:captures fixture))
        replayed (mapv #(assoc (run-case %)
                               :execution-mode :offline-frozen-snapshot)
                       captured)
        usable (filterv usable-case? replayed)
        replay-exclusions
        (->> replayed
             (remove usable-case?)
             (mapv
              (fn [case]
                {:case-id (:case-id case)
                 :reason (cond
                           (not (nonempty-baseline? case)) :empty-baseline
                           (zero? (perturbation-count case)) :no-perturbable-reviewed-edge
                           :else :unknown)
                 :baseline-candidate-count
                 (count (get-in case [:baseline :candidate-ids]))
                 :perturbation-count (perturbation-count case)})))]
    {:experiment/version 2
     :experiment :P3-V2-5-D-state-scale
     :seed (:seed fixture)
     :measured-from
     {:fixture-file (.getName (io/file fixture-path))
      :captured-at (:captured-at fixture)
      :graph-state :current-at-capture}
     :claim-boundary
     {:caption mandatory-caption
      :measures :D-state
      :does-not-measure [:historical-dispatch-state :D-functional
                         :memory-usefulness :outcome-lift]}
     :sample-accounting
     {:distinct-receipt-problems
      (get-in fixture [:census :distinct-offered-problem-count])
      :selected-with-recorded-query
      (get-in fixture [:census :selected-case-count])
      :missing-query-count
      (count (get-in fixture [:census :pre-capture-exclusions]))
      :capture-failure-count (count capture-failures)
      :replay-exclusion-count (count replay-exclusions)
      :usable-problem-count (count usable)}
     :pre-capture-exclusions
     (get-in fixture [:census :pre-capture-exclusions])
     :capture-failures capture-failures
     :replay-exclusions replay-exclusions
     :table (aggregate usable)
     :cases replayed}))

(defn print-summary [fixture-status result-status result]
  (println "P3 D_state scale sweep complete")
  (println "fixture" fixture-status fixture-path)
  (println "result" result-status result-path)
  (println "caption" mandatory-caption)
  (println "sample" (pr-str (:sample-accounting result)))
  (println "table" (pr-str (:table result))))

(defn validate-result! [fixture result]
  (let [accounting (:sample-accounting result)
        selected (:selected-with-recorded-query accounting)
        classified (+ (:capture-failure-count accounting)
                      (:replay-exclusion-count accounting)
                      (:usable-problem-count accounting))]
    (assert (= selected (count (:captures fixture)))
            "every selected problem must have a capture record")
    (assert (= selected (count (:cases result)))
            "every successful capture must be replayed")
    (assert (= selected classified)
            "usable, replay-excluded, and capture-failed cases must exhaust selection")
    (assert (= (:distinct-receipt-problems accounting)
               (+ (:missing-query-count accounting) selected))
            "missing-query and selected cases must exhaust receipt problems")
    (assert (= mandatory-caption (get-in result [:table :caption]))
            "mandatory current-graph caveat must be attached to the table")
    (assert (every? #(= :offline-frozen-snapshot (:execution-mode %))
                    (:cases result))
            "all replay cases must declare offline execution")
    true))

(load-frozen-harness!)
(let [capture? (some #{"--capture"} *command-line-args*)
      fixture (if capture?
                (capture-fixture!)
                (edn/read-string (slurp fixture-path)))
      result (run-sweep fixture)
      result-status (write-once! result-path result)]
  (validate-result! fixture result)
  (print-summary (if capture? :written :existing) result-status result))
