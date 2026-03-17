(ns futon3c.logic.structural-law
  "Shared structural-law query helpers for domain-specific logic layers.

   This namespace only contains shapes that already recur in live domains:
   paired-edge symmetry, dangling references, enum validity, and phase-output
   completeness. Domain namespaces still own their vocabulary and fact schema."
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.pldb :as pldb]
            [clojure.set :as set]))

(defn phase-index
  [phase-order]
  (into {} (map-indexed (fn [idx phase] [phase idx]) phase-order)))

(defn query-paired-edge-mismatches
  "Return mismatches for a bidirectional edge pair.

   `forward-rel` and `backward-rel` are binary relations. If `forward-rel`
   contains [a b], then `backward-rel` is expected to contain [b a]."
  [db {:keys [forward-rel backward-rel forward-label backward-label]}]
  (let [missing-backward
        (pldb/with-db db
          (l/run* [q]
            (l/fresh [a b]
              (forward-rel a b)
              (l/nafc backward-rel b a)
              (l/== q [a b forward-label]))))
        missing-forward
        (pldb/with-db db
          (l/run* [q]
            (l/fresh [a b]
              (backward-rel a b)
              (l/nafc forward-rel b a)
              (l/== q [a b backward-label]))))]
    (vec (concat missing-backward missing-forward))))

(defn query-dangling-targets
  "Return [source target direction] triples where `ref-rel` points at a target
   that has no corresponding `entity-rel` fact."
  [db {:keys [entity-rel ref-rel direction]}]
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [source-id target-id]
        (ref-rel source-id target-id)
        (l/nafc entity-rel target-id)
        (l/== q [source-id target-id direction])))))

(defn query-invalid-enum-values
  "Return [entity value] pairs where `value-rel` holds a value outside
   `allowed-values`."
  [db {:keys [value-rel allowed-values]}]
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [entity-id value]
        (value-rel entity-id value)
        (l/project [value]
          (if (contains? allowed-values value)
            l/fail
            l/succeed))
        (l/== q [entity-id value])))))

(defn query-missing-phase-outputs
  "Return maps describing phases whose required outputs are absent."
  [db {:keys [cycle-phase-rel cycle-output-rel phase-order phase-required-outputs]}]
  (let [phase->idx (phase-index phase-order)
        cycles (pldb/with-db db
                 (l/run* [q]
                   (l/fresh [cycle-id phase]
                     (cycle-phase-rel cycle-id phase)
                     (l/== q [cycle-id phase]))))]
    (vec
     (for [[cycle-id current-phase] cycles
           :let [current-idx (get phase->idx current-phase -1)]
           [required-phase required-keys] phase-required-outputs
           :let [required-idx (get phase->idx required-phase -1)]
           :when (> current-idx required-idx)
           :let [present (set (pldb/with-db db
                              (l/run* [output-key]
                                (cycle-output-rel cycle-id required-phase output-key))))
                 missing (set/difference required-keys present)]
           :when (seq missing)]
       {:cycle cycle-id :phase required-phase :missing missing}))))

(defn query-phase-prefix-mismatches
  "Return cycles whose completed-phase prefix does not match the current phase."
  [db {:keys [cycle-phase-rel completed-phase-rel phase-order]}]
  (let [phase->idx (phase-index phase-order)
        cycles (pldb/with-db db
                 (l/run* [q]
                   (l/fresh [cycle-id phase]
                     (cycle-phase-rel cycle-id phase)
                     (l/== q [cycle-id phase]))))]
    (vec
     (keep (fn [[cycle-id current-phase]]
             (let [current-idx (get phase->idx current-phase -1)
                   expected (->> phase-order
                                 (take (max current-idx 0))
                                 set)
                   actual (set (pldb/with-db db
                                (l/run* [phase]
                                  (completed-phase-rel cycle-id phase))))]
               (when (not= expected actual)
                 {:cycle cycle-id
                  :current-phase current-phase
                  :expected expected
                  :actual actual})))
           cycles))))
