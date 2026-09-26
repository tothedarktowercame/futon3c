(ns futon3c.peripheral.memory-trials
  "Resource-bounded Phase 3 trial runner.

   A trial row is an audit record, not a success-rate statistic. The caller
   supplies the real recall/use/check/correction operation so mathematics and
   later WM trials traverse the same seam."
  (:import [java.util.concurrent Callable Executors TimeUnit]))

(def default-max-trials 20)
(def default-max-concurrency 4)

(def required-audit-keys
  #{:trial/id :trial/query :trial/recall :trial/use
    :trial/external-outcome :trial/corrections})

(defn- validate-row
  [case row]
  (let [row (merge {:trial/id (:trial/id case)
                    :trial/query (:trial/query case)
                    :trial/corrections []}
                   row)
        missing (remove #(contains? row %) required-audit-keys)]
    (when (seq missing)
      (throw (ex-info "trial result is not auditable"
                      {:trial/id (:trial/id case) :missing (vec missing)})))
    row))

(defn run-bounded!
  "Run CASES with fixed concurrency and return an ordered audit table.

   RUN-TRIAL receives one case and must return recall, use, independently
   witnessed outcome, and correction fields. More than MAX-TRIALS is refused;
   inputs are never silently truncated."
  [cases {:keys [run-trial max-trials concurrency]
          :or {max-trials default-max-trials
               concurrency 1}}]
  (let [cases (vec cases)]
    (when-not (fn? run-trial)
      (throw (ex-info "bounded trials require run-trial" {})))
    (when (> (count cases) max-trials)
      (throw (ex-info "trial request exceeds resource bound"
                      {:requested (count cases) :max-trials max-trials})))
    (when-not (and (integer? concurrency)
                   (pos? concurrency)
                   (<= concurrency default-max-concurrency))
      (throw (ex-info "invalid trial concurrency"
                      {:concurrency concurrency
                       :max-concurrency default-max-concurrency})))
    (let [pool (Executors/newFixedThreadPool concurrency)]
      (try
        (let [futures
              (mapv (fn [case]
                      (.submit
                       pool
                       ^Callable
                       (reify Callable
                         (call [_]
                           (validate-row case (run-trial case))))))
                    cases)]
          {:ok true
           :requested-count (count cases)
           :concurrency concurrency
           :rows (mapv #(.get %) futures)
           :claim :audit-only-no-success-rate})
        (finally
          (.shutdownNow pool)
          (.awaitTermination pool 5 TimeUnit/SECONDS))))))

(defn retrieval-case-row
  "Make the retrieval portion of a trial row from ordered candidate ids."
  [{:trial/keys [id query expected-pattern]} candidate-ids]
  (let [candidate-ids (vec candidate-ids)
        rank (some (fn [[idx pattern-id]]
                     (when (= expected-pattern pattern-id) (inc idx)))
                   (map-indexed vector candidate-ids))]
    {:trial/id id
     :trial/query query
     :trial/recall {:candidate-ids candidate-ids
                    :expected-pattern expected-pattern
                    :expected-rank rank
                    :retrieved? (some? rank)}}))
