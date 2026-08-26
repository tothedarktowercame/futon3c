(ns futon3c.apm.qualification-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [futon3c.apm.qualification :as sut]))

(def plan (edn/read-string
           (slurp "holes/labs/M-apm-demonstration/apm-qualification-v1.edn")))

(def report-path "data/apm-validation/qualification-report-v1.edn")

(deftest committed-contract-and-qualification-are-one-coherent-state
  (let [report (edn/read-string (slurp report-path))
        contract (:generated-contract plan)]
    (is (:ok (sut/validate-report report contract)))
    (is (= :apm-qualification-report-invalid
           (:error/code
            (sut/validate-report
             (assoc-in report [:generated-contract :observed-digest] "stale")
             contract))))))

(deftest six-part-plan-is-non-vacuous
  (is (:ok (sut/validate-plan plan)))
  (let [report (sut/qualify plan (constantly {:exit 0}))]
    (is (:ok report))
    (is (true? (get-in report [:non-vacuity :witnessed?])))
    (is (every? pos? (vals (:mutation-coverage report))))
    (is (every? :pass? (:gates report)))))

(deftest qualification-plan-mutations-fail-closed
  (doseq [[finding mutant]
          [[:vacuous-positive-set (assoc plan :positive-fixtures [])]
           [:mutation-class-coverage-incomplete
            (update plan :mutation-classes dissoc :memory)]
           [:residual-hole-test-missing
            (assoc-in plan [:residual-holes 0 :test-id] nil)]
           [:numeric-bounds-not-executable
            (assoc plan :bounds-executable-test "docstring")]
           [:generated-artifact-stale
            (assoc plan :generated-contract-digest "stale")]]]
    (testing (name finding)
      (is (some #{finding} (:findings (sut/validate-plan mutant)))))))

(deftest command-own-failure-cannot-qualify
  (let [calls (atom 0)
        report (sut/qualify
                plan (fn [_] {:exit (if (= 2 (swap! calls inc)) 1 0)}))]
    (is (false? (:ok report)))
    (is (some (comp false? :pass?) (:gates report)))))
