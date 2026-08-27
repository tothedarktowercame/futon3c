(ns futon3c.apm.qualification-test
  (:require [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.apm.qualification :as sut]))

(def plan (edn/read-string
           (slurp "holes/labs/M-apm-demonstration/apm-qualification-v1.edn")))

(def report-path "data/apm-validation/qualification-report-v1.edn")

(deftest clojure-qualification-covers-derived-inventory-with-explicit-exclusions
  (let [discovered (set (sut/qualification-test-namespaces))
        exclusions (set (keys (:clojure-qualification/exclusions plan)))
        command (some #(when (= :clojure-qualification (:id %)) %)
                      (:commands plan))
        declared (->> (:argv command) (drop 2) (partition 2)
                      (map second) set)]
    (is (= (set/difference discovered exclusions) declared))
    (is (= #{"futon3c.apm.disruption-soak-test"} exclusions))
    (is (every? #(not (str/blank? %))
                (vals (:clojure-qualification/exclusions plan))))
    (is (some #{:qualification-namespace-coverage-incomplete}
              (:findings
               (sut/validate-plan
                (update-in plan [:commands]
                           (fn [commands]
                             (mapv #(if (= :clojure-qualification (:id %))
                                      (update % :argv pop)
                                      %)
                                   commands)))))))))

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
