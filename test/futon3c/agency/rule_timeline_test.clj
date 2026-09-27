(ns futon3c.agency.rule-timeline-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.edn :as edn]
            [futon3c.agency.rule-record :as record]
            [futon3c.agency.rule-timeline :as timeline]))
(def fixtures (edn/read-string (slurp "holes/labs/M-象-2000/P13b-requisition-versions.edn")))
(def records (mapv record/payload fixtures))
(def family "kimi-requisition-20260924")

(deftest source-backed-as-of-answers
  (is (= "committed, not yet live" (:answer (timeline/as-of records family "2026-09-24T17:00:00Z"))))
  (is (= "requisition rule applied" (:answer (timeline/as-of records family "2026-09-25T20:00:00Z"))))
  (is (= "followup half withdrawn" (:answer (timeline/as-of records family "2026-09-25T21:00:00Z"))))
  (is (= "2026-09-25T20:05:51.002Z" (:until (first (timeline/intervals records family)))))
  (is (= 2 (:version (timeline/as-of records family "2026-09-25T20:05:51.002Z")))))

(deftest bad-case-commit-time-would-give-the-wrong-answer
  (let [wrong (assoc-in records [0 :hx/props :rule/timeline :live :at] "2026-09-24T16:34:08Z")]
    (is (= "requisition rule applied" (:answer (timeline/as-of wrong family "2026-09-24T17:00:00Z"))))
    (is (not= (:answer (timeline/as-of records family "2026-09-24T17:00:00Z"))
              (:answer (timeline/as-of wrong family "2026-09-24T17:00:00Z"))))))

(deftest promulgation-is-queryable-without-inventing-a-grant
  (let [p (first (timeline/promulgated-as-of records family "2026-09-24T17:00:00Z"))]
    (is (= 3 (count (:adopted p))))
    (is (= 2 (count (:committed p))))
    (is (every? #(= :unrecorded (:grant-status %)) (:adopted p)))))

(deftest missing-or-negative-live-witness-never-establishes-application
  (let [one [(first records)]
        missing (update-in one [0 :hx/props :rule/timeline] dissoc :live)
        stale (-> one
                  (assoc-in [0 :hx/props :rule/timeline :kind] :code)
                  (assoc-in [0 :hx/props :rule/timeline :live :status] :not-loaded)
                  (assoc-in [0 :hx/props :rule/timeline :live :source :kind] :stale-runner-source))]
    (doseq [r [missing stale]]
      (is (= "committed, not yet live" (:answer (timeline/as-of r family "2026-09-25T21:00:00Z")))))))

(deftest source-kind-and-ambiguous-intervals-refused
  (is (thrown? clojure.lang.ExceptionInfo
               (timeline/validate! (assoc (get-in records [0 :hx/props :rule/timeline]) :kind :code))))
  (is (thrown? clojure.lang.ExceptionInfo
               (timeline/intervals (conj records (first records)) family)))
  (let [same-time (assoc-in records [1 :hx/props :rule/timeline :live :at]
                            (get-in records [0 :hx/props :rule/timeline :live :at]))]
    (is (thrown? clojure.lang.ExceptionInfo (timeline/intervals same-time family)))))
