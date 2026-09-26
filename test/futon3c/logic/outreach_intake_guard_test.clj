(ns futon3c.logic.outreach-intake-guard-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.logic.outreach-intake-guard :as guard]))

(deftest outreach-intake-verify-green
  (let [report (guard/run-verify)]
    (is (:verified? report))
    (is (get-in report [:witness :clean?]))
    (is (get-in report [:summary :all-adversarial-caught?]))
    (is (= 2 (get-in report [:summary :n-invariants])))))

(deftest conforming-witness-trace-is-legal
  (let [report (guard/run-verify guard/conforming-trace)]
    (is (:verified? report))
    (is (every? empty? (vals (get-in report [:witness :violations]))))))

(deftest hedge-known-event-does-not-camp-cold-thesis
  (let [report (guard/run-verify [guard/conforming-hedge-known-event])]
    (is (:verified? report))
    (is (empty? (get-in report [:witness :violations :lead-class-matches-thesis])))
    (is (= :hedge-anthropic (:plants-thesis guard/conforming-hedge-known-event)))
    (is (= :known (:lead-class guard/conforming-hedge-known-event)))))

(deftest adversarial-lead-class-laundering-fails-only-invariant-1
  (let [trace (:lead-class-matches-thesis guard/adversarial-traces)
        report (guard/run-verify trace)
        violations (get-in report [:witness :violations])]
    (is (not (:verified? report)))
    (is (seq (:lead-class-matches-thesis violations)))
    (is (empty? (:send-witness-present violations)))))

(deftest adversarial-claimed-send-without-witness-fails-only-invariant-2
  (let [trace (:send-witness-present guard/adversarial-traces)
        report (guard/run-verify trace)
        violations (get-in report [:witness :violations])]
    (is (not (:verified? report)))
    (is (seq (:send-witness-present violations)))
    (is (empty? (:lead-class-matches-thesis violations)))))

(deftest adversarial-traces-caught-by-own-category
  (let [report (guard/run-verify)]
    (doseq [[cat result] (:adversarial report)]
      (is (:caught? result) (str cat " should be caught"))
      (is (seq (:hits result)) (str cat " should report hits")))))
