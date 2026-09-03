(ns futon3c.agents.zaif-arm-comparison
  "Pure U11e comparison of declared Q_actand candidate sources."
  (:require [futon3c.agents.zaif-actand :as q-actand]
            [futon3c.agents.zaif-arm-adapters :as adapters])
  (:import [java.math BigInteger]
           [java.security MessageDigest]))

(def baselines
  {:wm-u12-status-quo
   {:distinct-risk-values 1 :actions 133 :channels 14}
   :zaif-status-quo
   {:distinct-act-values 1 :act-value 0.0 :sessions 114 :supported-sessions 114}})

(defn- sha256
  [value]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256")
                        (.getBytes (pr-str value) "UTF-8"))]
    (format "%064x" (BigInteger. 1 digest))))

(defn- result-value
  [column result]
  (case column
    (:A :B) (get-in result [:value :pragmatic-value])
    :calibration-table-a4 (:act-value result)
    nil))

(defn- source-result
  [column corpus record calibration-sessions]
  (case column
    :A (adapters/adapt-arm-a record)
    :B (adapters/adapt-arm-b record)
    :C (q-actand/no-typed-source
        (if (= corpus :calibration-sessions) :arm-session :mission)
        (or (:id record) (:run/id record))
        :C)
    :calibration-table-a4
    (if (= corpus :calibration-sessions)
      (q-actand/demo-bridge
       (q-actand/calibration-density
        calibration-sessions
        (q-actand/calibration-actand record)
        (q-actand/calibration-arm record)))
      (let [table (q-actand/calibration-table [record])]
        (if (map? table)
          table
          (q-actand/no-typed-source :mission (:run/id record)
                                    :calibration-table-a4))))))

(defn- evaluation
  [column corpus record calibration-sessions]
  (let [consumed-inputs (if (and (= column :calibration-table-a4)
                                 (= corpus :calibration-sessions))
                          {:record record
                           :calibration-sessions calibration-sessions}
                          record)
        digest (sha256 consumed-inputs)
        first-result (source-result column corpus record calibration-sessions)
        second-result (source-result column corpus record calibration-sessions)]
    {:inputs-digest digest
     :result first-result
     :repeat-result second-result
     :value (result-value column first-result)
     :repeat-value (result-value column second-result)}))

(defn- provenance-bearing-value?
  [{:keys [value result]}]
  (and (some? value)
       (seq (get-in result [:provenance :record-ids]))))

(defn- ordering-probe-a
  []
  ;; portfolio/policy.clj:54-63: with fixed mu-sens, raising observed gap
  ;; raises both the absolute gap error and the direct observed-gap term.
  (let [base {:action :work-on
              :observation {:gap-count 0.2 :stall-count 0.2 :review-age 0.5
                            :spinoff-pressure 0.5 :coverage-pct 0.5}
              :mu-sens {:gap-count 0.0 :stall-count 0.2 :review-age 0.5
                        :spinoff-pressure 0.5 :coverage-pct 0.5}
              :adjacent-missions []}
        higher (assoc-in base [:observation :gap-count] 0.8)
        low-value (get-in (adapters/adapt-arm-a base) [:value :pragmatic-value])
        high-value (get-in (adapters/adapt-arm-a higher) [:value :pragmatic-value])]
    {:status (if (> high-value low-value) :pass :fail)
     :varied-field [:observation :gap-count]
     :direction :higher-input-higher-value
     :values [low-value high-value]}))

(defn- ordering-probe-b
  []
  ;; aif/mission_head.clj:139: :advance-phase is 1 - :phase-progress.
  (let [base {:mission "M-ordering-probe" :action :advance-phase :tau 1.0
              :channels {:phase-progress 0.2 :prediction-divergence 0.4
                         :gate-readiness 0.5 :obligation-satisfaction 0.6}}
        higher (assoc-in base [:channels :phase-progress] 0.8)
        low-input-value (get-in (adapters/adapt-arm-b base)
                                [:value :pragmatic-value])
        high-input-value (get-in (adapters/adapt-arm-b higher)
                                 [:value :pragmatic-value])]
    {:status (if (> low-input-value high-input-value) :pass :fail)
     :varied-field [:channels :phase-progress]
     :direction :higher-input-lower-value
     :values [low-input-value high-input-value]}))

(defn- planted-density-row
  [gold-count]
  {:grain :arm-session
   :actand {:route :planted :correction-label true}
   :action :ask
   :observables q-actand/gold-observables
   :density {:gold-judged (/ gold-count 14)
             :not-gold-judged (/ (- 14 gold-count) 14)}
   :counts {:gold-judged gold-count
            :not-gold-judged (- 14 gold-count)}
   :provenance {:source :u11e/ordering-plant
                :query q-actand/query-id
                :record-ids [(str "plant-" gold-count)]}})

(defn- ordering-probe-calibration
  []
  (let [low (q-actand/demo-bridge (planted-density-row 6))
        high (q-actand/demo-bridge (planted-density-row 12))
        low-value (:act-value low)
        high-value (:act-value high)]
    {:status (if (> high-value low-value) :pass :fail)
     :varied-field [:density :gold-judged]
     :direction :higher-density-higher-value
     :values [low-value high-value]}))

(def ordering-probes
  {:A (ordering-probe-a)
   :B (ordering-probe-b)
   :C {:status :not-applicable-typed-refusal}
   :calibration-table-a4 (ordering-probe-calibration)})

(defn- floor-plant
  [column]
  (case column
    :A (let [record {:action :work-on
                     :observation {:gap-count 0.0 :stall-count 0.0
                                   :review-age 1.0 :spinoff-pressure 0.0
                                   :coverage-pct 1.0}
                     :mu-sens {:gap-count 0.0 :stall-count 0.0
                               :review-age 1.0 :spinoff-pressure 0.0
                               :coverage-pct 1.0}
                     :adjacent-missions []}
             value (get-in (adapters/adapt-arm-a record)
                           [:value :pragmatic-value])]
         {:status (if (zero? value) :pass :fail) :value value :floor 0.0})
    :B (let [record {:mission "M-floor" :action :advance-phase :tau 1.0
                     :channels {:phase-progress 1.0 :prediction-divergence 0.0
                                :gate-readiness 1.0
                                :obligation-satisfaction 1.0}}
             value (get-in (adapters/adapt-arm-b record)
                           [:value :pragmatic-value])]
         {:status (if (zero? value) :pass :fail) :value value :floor 0.0})
    :C {:status :not-applicable-typed-refusal}
    :calibration-table-a4
    (let [value (:act-value (q-actand/demo-bridge (planted-density-row 0)))]
      {:status (if (= Double/NEGATIVE_INFINITY value) :pass :fail)
       :value value :floor Double/NEGATIVE_INFINITY})))

(defn- cell-report
  [column corpus records calibration-sessions]
  (let [evaluations (mapv #(evaluation column corpus % calibration-sessions)
                          records)
        values (keep :value evaluations)
        produced (count values)
        refusal-counts (frequencies
                        (keep #(get-in % [:result :q-actand/refusal])
                              evaluations))
        digest-violations (count
                           (filter #(not= (:value %) (:repeat-value %))
                                   evaluations))]
    {:rows (count records)
     :produced-values produced
     :distinct-values (count (set values))
     :refusals refusal-counts
     :provenance-coverage (if (zero? produced)
                            0
                            (/ (count (filter provenance-bearing-value?
                                              evaluations))
                               produced))
     :digest-stability {:comparisons (count evaluations)
                        :violations digest-violations}
     :ordering-probe (get ordering-probes column)
     :floor-plant (floor-plant column)}))

(defn comparison-report
  "Run U11e over supplied, already-parsed tracked corpora. No I/O is performed."
  [{:keys [calibration-sessions mission-fixtures]}]
  (let [corpora {:calibration-sessions calibration-sessions
                 :mission-node-fixtures mission-fixtures}
        columns [:A :B :C :calibration-table-a4]
        cells (into (sorted-map)
                    (for [column columns]
                      [column
                       (into (sorted-map)
                             (for [[corpus records] corpora]
                               [corpus (cell-report column corpus records
                                                    calibration-sessions)]))]))
        pinned (first (filter #(= "e-0cae94f2-9ca8-4863-9251-44278445a5f7"
                                  (:id %))
                              calibration-sessions))]
    {:query :q-actand/comparison-v1
     :baselines baselines
     :corpus-counts {:calibration-sessions (count calibration-sessions)
                     :mission-node-fixtures (count mission-fixtures)}
     :cells cells
     :live-pin {:record-id (:id pinned)
                :column :calibration-table-a4
                :result (source-result :calibration-table-a4
                                       :calibration-sessions pinned
                                       calibration-sessions)}}))
