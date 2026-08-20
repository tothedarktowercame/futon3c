(ns futon3c.aif.two-layer-calibration-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.aif.two-layer-calibration :as two-layer]))

(def layer-1-pass
  #:layer-1{:prediction {:action :open-mission :expected-discharge -4.0}
            :realisation {:action :open-mission :measured-discharge -3.5}
            :verdict :pass
            :provenance #:provenance{:producer-id :campaign-controller
                                     :trace-id "campaign-12/tick-a"}})

(def independent-layer-2-pass
  #:layer-2{:verdict :pass
            :independent? true
            :observation {:grounded-action :open-mission :status :witnessed}
            :provenance #:provenance{:witness-id :substrate-observer
                                     :control :independent
                                     :evidence-id "evidence/tick-a/witness"}})

(deftest independent-witness-can-clear-the-gate
  (let [report (two-layer/two-layer-report
                {:layer-1/evidence layer-1-pass
                 :layer-2/witness independent-layer-2-pass})]
    (is (= {:gate/clear? true
            :gate/verdict :pass
            :gate/reason :independently-confirmed}
           (select-keys report [:gate/clear? :gate/verdict :gate/reason])))
    (is (= layer-1-pass (:report/layer-1 report)))
    (is (= independent-layer-2-pass (:report/layer-2 report)))
    (is (= {:layer-1/provenance (:layer-1/provenance layer-1-pass)
            :layer-2/provenance (:layer-2/provenance independent-layer-2-pass)}
           (:report/provenance report)))))

(deftest layer-1-success-cannot-clear-a-missing-layer-2
  (let [report (two-layer/two-layer-report
                {:layer-1/evidence layer-1-pass})]
    (is (false? (:gate/clear? report)))
    (is (= :fail (:gate/verdict report)))
    (is (= :layer-2-missing (:gate/reason report)))))

(deftest layer-1-success-cannot-clear-a-failed-layer-2
  (let [report (two-layer/two-layer-report
                {:layer-1/evidence layer-1-pass
                 :layer-2/witness (assoc independent-layer-2-pass
                                           :layer-2/verdict :fail)})]
    (is (false? (:gate/clear? report)))
    (is (= :layer-2-not-passed (:gate/reason report)))))

(deftest author-controlled-or-self-witnessed-layer-2-is-ineligible
  (doseq [[description witness]
          [["author-controlled"
            (assoc-in independent-layer-2-pass
                      [:layer-2/provenance :provenance/control]
                      :author)]
           ["same producer and witness"
            (assoc-in independent-layer-2-pass
                      [:layer-2/provenance :provenance/witness-id]
                      :campaign-controller)]
           ["independence not asserted"
            (assoc independent-layer-2-pass :layer-2/independent? false)]]]
    (testing description
      (let [report (two-layer/two-layer-report
                    {:layer-1/evidence layer-1-pass
                     :layer-2/witness witness})]
        (is (false? (:gate/clear? report)))
        (is (= :layer-2-independence-unproven
               (:gate/reason report)))))))

(deftest deterministic-replay
  (let [evidence {:layer-1/evidence layer-1-pass
                  :layer-2/witness independent-layer-2-pass}
        first-report (two-layer/two-layer-report evidence)]
    (is (= first-report (two-layer/two-layer-report evidence)))
    (is (= two-layer/report-schema (:report/schema first-report)))))
