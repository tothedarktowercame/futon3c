(ns futon3c.agents.zaif-actand
  "The provenance-bearing arm-session instance of Q_actand."
  (:require [clojure.edn :as edn]))

(def query-id :q-actand/calibration-v1)
(def calibration-source :zaif/calibration-sessions)
(def grains #{:arm-session :mission})
(def gold-observables #{:gold-judged :not-gold-judged})

(def q-actand-record
  "The shared record contract for both current Q_actand grains."
  {:required #{:grain :actand :action :observables :density :counts :provenance}
   :grains grains
   :provenance-required #{:source :query :record-ids}
   :construction-rule :v1-kin-pool})

(defn no-typed-source
  [grain actand action]
  {:q-actand/refusal :q-actand/no-typed-source
   :grain grain
   :actand actand
   :action action})

(defn missing-input
  [field]
  {:q-actand/refusal :q-actand/missing-input
   :field field})

(defn- probability?
  [x]
  (and (number? x) (<= 0.0 (double x) 1.0)))

(defn q-actand-record?
  "True only for a complete Q_actand density record; no coercion is performed."
  [record]
  (let [{:keys [grain actand action observables density counts provenance]} record
        record-ids (:record-ids provenance)
        constructed? (contains? record :constructed)
        derivation (:derivation record)]
    (and (map? record)
         (every? #(contains? record %) (:required q-actand-record))
         (contains? grains grain)
         (some? actand)
         (keyword? action)
         (set? observables)
         (seq observables)
         (= observables (set (keys density)) (set (keys counts)))
         (every? probability? (vals density))
         (< (Math/abs (- 1.0 (reduce + 0.0 (map double (vals density))))) 1.0e-12)
         (every? #(and (integer? %) (not (neg? %))) (vals counts))
         (pos? (reduce + 0 (vals counts)))
         (map? provenance)
         (every? #(contains? provenance %) (:provenance-required q-actand-record))
         (keyword? (:source provenance))
         (keyword? (:query provenance))
         (vector? record-ids)
         (seq record-ids)
         (every? string? record-ids)
         (or (not constructed?)
             (and (= true (:constructed record))
                  (= (:construction-rule q-actand-record) (:rule derivation))
                  (vector? (:kin derivation))
                  (seq (:kin derivation))
                  (every? vector? (:kin derivation)))))))

(defn calibration-actand
  "Derive the calibration actand class without interpreting context text."
  [session]
  {:route (:route session)
   :correction-label (:is_correction session)})

(defn calibration-arm
  "Return the arm warranted by the calibration session's correction label.

  DECLARED MAPPING (U11 design section 4 discipline; review finding, U11b):
  this route->arm warrant is AUTHORED for v1, not derived from any record --
  no calibration session carries an arm, so the grouping needs a declared
  bridge: a correction routed to :gamma or :c-channel warrants :ask, one
  routed to :actand warrants :retrieve, a non-correction warrants :act.
  Its effect on the table is measurable and revisable at the U11e comparison;
  changing it changes group keys, never densities within a group."
  [{:keys [is_correction route]}]
  (if is_correction
    (case route
      :actand :retrieve
      :gamma :ask
      :c-channel :ask
      :ask)
    :act))

(defn- outcome
  [session]
  (if (:gold_judged session) :gold-judged :not-gold-judged))

(defn- missing-session-field
  [session]
  (some #(when-not (contains? session %) %)
        [:id :route :is_correction :gold_judged]))

(defn- density-record
  [[[actand action] sessions]]
  (let [record-ids (->> sessions (map :id) sort vec)
        counts (merge (zipmap gold-observables (repeat 0))
                      (frequencies (map outcome sessions)))
        support (count sessions)]
    {:grain :arm-session
     :actand actand
     :action action
     :observables gold-observables
     :density (into {} (map (fn [[observable n]]
                              [observable (/ n support)])
                            counts))
     :counts counts
     :provenance {:source calibration-source
                  :query query-id
                  :record-ids record-ids}}))

(defn calibration-table
  "Materialise :q-actand/calibration-v1 from parsed calibration sessions."
  [sessions]
  (if-let [field (some missing-session-field sessions)]
    (missing-input field)
    (->> sessions
         (group-by (juxt calibration-actand calibration-arm))
         (sort-by (comp pr-str key))
         (mapv density-record))))

(defn calibration-density
  "Query one (actand, arm) group, refusing a group with no observations."
  [sessions actand action]
  (let [result (calibration-table sessions)]
    (if (map? result)
      result
      (or (some #(when (and (= actand (:actand %))
                            (= action (:action %)))
                   %)
                result)
          (no-typed-source :arm-session actand action)))))

(defn- positive-target-support?
  [record]
  (pos? (get-in record [:counts :gold-judged] 0)))

(defn- same-v1-kin-class?
  [target candidate]
  (and (= (:action target) (:action candidate))
       (= (get-in target [:actand :correction-label])
          (get-in candidate [:actand :correction-label]))))

(defn- pool-density-records
  [target kin]
  (let [pool (into [target] kin)
        counts (apply merge-with + (map :counts pool))
        support (reduce + (vals counts))]
    (assoc target
           :constructed true
           :derivation {:rule :v1-kin-pool
                        :kin (mapv (juxt :actand :action) kin)}
           :density (update-vals counts #(/ % support))
           :counts counts
           :provenance (assoc (:provenance target)
                              :record-ids (->> pool
                                               (mapcat #(get-in % [:provenance :record-ids]))
                                               distinct
                                               sort
                                               vec)))))

(defn calibration-density-with-kin
  "Apply rungs 1 and 2 of the declared v1 zero-support ladder.

  A directly supported row is returned unchanged. A zero-support row pools
  with the other rows sharing its arm and correction label. If no such kin
  exists, or the pooled target support remains zero, return a typed trigger
  for rung 3 rather than manufacturing a finite value."
  [sessions actand action]
  (let [table (calibration-table sessions)]
    (if (map? table)
      table
      (if-let [target (some #(when (and (= actand (:actand %))
                                         (= action (:action %)))
                                %)
                            table)]
        (if (positive-target-support? target)
          target
          (let [kin (->> table
                         (remove #(= [actand action] [(:actand %) (:action %)]))
                         (filter #(same-v1-kin-class? target %))
                         (sort-by (comp pr-str (juxt :actand :action)))
                         vec)
                pooled (when (seq kin) (pool-density-records target kin))]
            (if (and pooled (positive-target-support? pooled))
              pooled
              {:q-actand/refusal :q-actand/zero-support-after-kin-pool
               :grain :arm-session
               :actand actand
               :action action
               :target :gold-judged
               :kin (mapv (juxt :actand :action) kin)})))
        (no-typed-source :arm-session actand action)))))

(defn demo-bridge
  "Apply U11 design amendment [A4]'s demo-scoped density-to-scalar bridge.

  The declared target is :gold-judged and the declared baseline is the
  uniform two-observable density, 1/2. U11e may replace this bridge; the U11c
  and U11d adapters do not inherit it. Provenance is forwarded unchanged."
  [record]
  (if-not (contains? (:density record) :gold-judged)
    (missing-input [:density :gold-judged])
    (if-not (q-actand-record? record)
      (no-typed-source (:grain record) (:actand record) (:action record))
      {:act-value (- (Math/log (double (get-in record [:density :gold-judged])))
                     (Math/log 0.5))
       :bridge :q-actand/demo-bridge-a4
       :provenance (:provenance record)})))

(defn read-calibration-sessions
  "Read tracked calibration bytes. The materialised table itself remains pure."
  [path]
  (edn/read-string (slurp path)))
