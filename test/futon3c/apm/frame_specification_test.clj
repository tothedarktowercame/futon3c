(ns futon3c.apm.frame-specification-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [futon3c.apm.campaign-gates :as gates]
            [futon3c.apm.campaign-qualification :as qualification]
            [futon3c.apm.frame-specification :as specification]))

(def control-path
  "holes/labs/M-apm-demonstration/frame-18-control.edn")

(deftest frame-18-specification-ingests-to-the-gate-shape
  (let [ingested (specification/ingest control-path "f18" nil)
        facts (qualification/derive-facts {:specification-check ingested})]
    (is (:valid? ingested))
    (is (= #{:valid? :digest :frame-matches? :registration-matches?}
           (set (keys (:specification facts)))))
    (is (= {:valid? true :digest (:digest ingested) :frame-matches? true
            :registration-matches? true}
           (:specification facts)))))

(deftest role-cards-are-resolved-at-the-pinned-apparatus-revision
  (let [spec (edn/read-string (slurp control-path))]
    (is (:valid? (specification/validate spec "f18" nil)))
    (testing "a plausible but incorrect blob fails closed"
      (let [changed (assoc-in spec [:frame/apparatus :role-cards :solver :blob]
                              "0000000000000000000000000000000000000000")
            result (specification/validate changed "f18" nil)]
        (is (false? (:valid? result)))
        (is (some #{:apparatus-revision-mismatch} (:errors result)))))))

(deftest specification-gate-passes-and-mismatch-fails-closed
  (let [plan (:plan (qualification/read-plan
                     "holes/labs/M-apm-demonstration/frame-18-step-plan.edn"))
        spec-gate (fn [facts]
                    (->> (gates/evaluate-obligation
                          (:qualification/gates plan) facts
                          {:obligation/action {:kind :open-frame}})
                         (filter #(= :frame-specification (:gate/id %))) first))
        digest (:digest (specification/ingest control-path "f18" nil))
        accepted (specification/ingest control-path "f18" digest)
        rejected (specification/ingest control-path "f19" digest)]
    (is (= :pass (:gate/status
                  (spec-gate {:specification accepted}))))
    (is (= :fail (:gate/status
                  (spec-gate {:specification rejected}))))))
