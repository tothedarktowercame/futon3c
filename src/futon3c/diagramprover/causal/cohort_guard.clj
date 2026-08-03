(ns futon3c.diagramprover.causal.cohort-guard
  "Dispatch-time guard for cohort experiments (the live-guard pattern
  applied to analysis-integrity preconditions).

  Refuses a dispatch BEFORE a problem is spent when the dispatch record
  could not support the analysis it is meant to feed: arm assignment not
  recoverable from the record, join key failing to identify the declared
  unit, or an arm's estimand denominator unrecorded (the :push+pull
  case: uses countable, offers not). Requirements are DATA, supplied by
  the cohort owner - this namespace is pure and knows nothing about the
  store; the caller passes the dispatch record and (for uniqueness
  checks) the batch of prior key values.")

(defn arm-recoverable?
  "Every axis named in `axes` must have its assignment recorded."
  [record axes]
  (let [missing (into (sorted-set) (remove #(contains? record %)) axes)]
    {:check :arm-recoverable
     :ok? (empty? missing)
     :missing-fields (vec missing)}))

(defn join-key-check
  "The declared join key must exist in the record and, when
  `prior-key-values` are supplied, must not collide with them (a
  collision means the key does not identify the declared unit - the
  session-id-is-per-seat failure)."
  [record join-key & [prior-key-values]]
  (let [value (get record join-key ::absent)
        absent? (= ::absent value)
        batch (or prior-key-values [])
        collision? (boolean (and (not absent?) (some #{value} batch)))]
    {:check :join-key-identifies-unit
     :join-key join-key
     :ok? (and (not absent?) (not collision?))
     :absent? absent?
     :collision? collision?
     ;; an empty batch makes this check vacuous - the field makes the
     ;; caller's batch-supply discipline auditable from the receipt
     :prior-batch-size (count batch)
     :vacuous? (zero? (count batch))}))

(defn denominator-check
  "Arm-conditional required fields: for each estimand the arm feeds,
  the field supplying its denominator must be present in the record.
  `requirements` maps arm-value -> {estimand-id -> denominator-field}."
  [record arm-field requirements]
  (let [arm (get record arm-field)
        needed (get requirements arm {})
        missing (into (sorted-map)
                      (remove (fn [[_ field]] (contains? record field)))
                      needed)]
    {:check :denominator-recorded
     :arm arm
     :ok? (empty? missing)
     :unrecorded-denominators missing}))

(defn dispatch-verdict
  "Run all three checks; returns {:licensed? bool :checks [...]}."
  [record {:keys [axes join-key prior-key-values arm-field
                  denominator-requirements]}]
  (let [checks [(arm-recoverable? record axes)
                (join-key-check record join-key prior-key-values)
                (denominator-check record arm-field
                                   denominator-requirements)]]
    {:licensed? (every? :ok? checks)
     :checks checks}))

(defn guard!
  "Failing-loudly form for the dispatch wrapper: returns the verdict
  when licensed, throws with the failed checks named when not."
  [record config]
  (let [{:keys [licensed?] :as verdict} (dispatch-verdict record config)]
    (if licensed?
      verdict
      (throw (ex-info
              (str "Cohort dispatch refused: the record cannot support "
                   "the analysis it feeds; spend nothing until the "
                   "named fields are recorded")
              (assoc verdict :failed
                     (mapv :check (remove :ok? (:checks verdict)))))))))
