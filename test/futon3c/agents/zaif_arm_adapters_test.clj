(ns futon3c.agents.zaif-arm-adapters-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3c.agents.zaif-actand :as q-actand]
            [futon3c.agents.zaif-arm-adapters :as adapters]))

(def calibration-path
  "../futon2/holes/labs/M-zaif-harness/calibration-sessions.edn")

(defn- sessions []
  (q-actand/read-calibration-sessions calibration-path))

(def r2-fixture-dir
  "../futon2/holes/labs/wm-contract/runs/U12-c-mis-falsifier/node-fixtures")

(defn- r2-fixtures []
  (->> (file-seq (io/file r2-fixture-dir))
       (filter #(re-find #"-R2\.edn$" (.getName %)))
       (sort-by #(.getName %))
       (mapv #(q-actand/read-calibration-sessions (.getPath %)))))

(deftest arm-a-mapping-is-complete-and-declared
  (is (= (set adapters/arm-a-inputs) (set (keys adapters/arm-a-mapping))))
  (is (every? #(and (:source-field %)
                    (= #{:calibration :zaif-decisions}
                       (set (keys (:corpora %))))
                    (every? (fn [{:keys [source basis]}]
                              (and (= :q-actand/missing-input
                                      (:q-actand/refusal source))
                                   (keyword? basis)))
                            (vals (:corpora %))))
              (vals adapters/arm-a-mapping))))

(deftest calibration-session-refuses-a-real-missing-channel
  (let [session (first (filter #(= "e-0cae94f2-9ca8-4863-9251-44278445a5f7"
                                    (:id %))
                               (sessions)))]
    ;; Live calibration pin: e-0cae94f2-9ca8-4863-9251-44278445a5f7.
    (is (= {:id "e-0cae94f2-9ca8-4863-9251-44278445a5f7"
            :route :gamma
            :is_correction true
            :gold_judged true}
           (select-keys session [:id :route :is_correction :gold_judged])))
    (is (= {:q-actand/refusal :q-actand/missing-input
            :field [:observation :gap-count]}
           (adapters/adapt-arm-a session)))))

(deftest complete-arm-a-record-produces-typed-finite-scalar
  (let [result (adapters/adapt-arm-a
                {:action :work-on
                 :observation {:gap-count 0.9
                               :stall-count 0.8
                               :review-age 0.4
                               :spinoff-pressure 0.3
                               :coverage-pct 0.7}
                 :mu-sens {:gap-count 0.2
                           :stall-count 0.1
                           :review-age 0.2
                           :spinoff-pressure 0.1
                           :coverage-pct 0.6}
                 :adjacent-missions [{:adjacent? true}
                                     {:adjacent? false}]})
        scalar (get-in result [:value :pragmatic-value])]
    (is (= :scalar-awaiting-density (get-in result [:value :type])))
    (is (number? scalar))
    (is (Double/isFinite (double scalar)))))

(deftest calibration-corpus-refusal-count-is-measured
  (let [results (map adapters/adapt-arm-a (sessions))
        refusals (filter :q-actand/refusal results)]
    (is (= 114 (count results)))
    (is (= 114 (count refusals)))
    (is (= {[:observation :gap-count] 114}
           (frequencies (map :field refusals))))))

(deftest arm-b-mapping-is-complete-and-declared
  (is (= (set adapters/arm-b-channels)
         (set (keys adapters/arm-b-mapping))))
  (is (every? #(and (= :q-actand/missing-input
                       (get-in % [:source :q-actand/refusal]))
                    (= :plausible-but-undeclared (:basis %))
                    (vector? (:candidate-field %))
                    (= :u12-c-mis-falsifier/r2-fixtures
                       (:candidate-corpus %)))
              (vals adapters/arm-b-mapping))))

(deftest tracked-r2-fixture-refuses-undeclared-channel-mapping
  (let [fixture (first (filter #(= "801976e7-01c6-4e39-aada-27f620f7c2f1"
                                   (:run/id %))
                              (r2-fixtures)))]
    ;; Tracked live-derived pin: node-fixtures/801976e7-R2.edn,
    ;; run id 801976e7-01c6-4e39-aada-27f620f7c2f1.
    (is (= {:node :R2
            :run/id "801976e7-01c6-4e39-aada-27f620f7c2f1"
            :field [:observation]
            :status :present}
           (select-keys fixture [:node :run/id :field :status])))
    (is (= 0.023376623376623377 (get-in fixture [:value :mission-health])))
    (is (= 0.6 (get-in fixture [:value :support-coverage])))
    (is (= {:q-actand/refusal :q-actand/missing-input
            :field :phase-progress}
           (adapters/adapt-arm-b fixture)))))

(deftest complete-arm-b-record-produces-exact-typed-scalar
  (let [result (adapters/adapt-arm-b
                {:mission "M-synthetic"
                 :action :advance-phase
                 :tau 1.0
                 :channels {:phase-progress 0.25
                            :prediction-divergence 0.4
                            :gate-readiness 0.75
                            :obligation-satisfaction 0.6}})]
    (is (= :scalar-awaiting-density (get-in result [:value :type])))
    (is (= 0.75 (get-in result [:value :pragmatic-value])))
    (is (Double/isFinite
         (double (get-in result [:value :pragmatic-value]))))))

(deftest tracked-r2-fixture-refusal-count-is-measured
  (let [fixtures (r2-fixtures)
        results (map adapters/adapt-arm-b fixtures)]
    (is (= 3 (count fixtures)))
    (is (= 3 (count (filter :q-actand/refusal results))))
    (is (= {:phase-progress 3}
           (frequencies (map :field results))))))
