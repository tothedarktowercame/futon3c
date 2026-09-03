(ns futon3c.agents.zaif-arm-adapters-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.agents.zaif-actand :as q-actand]
            [futon3c.agents.zaif-arm-adapters :as adapters]))

(def calibration-path
  "../futon2/holes/labs/M-zaif-harness/calibration-sessions.edn")

(defn- sessions []
  (q-actand/read-calibration-sessions calibration-path))

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
