(ns futon3c.logic.business-coupling-invariants-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.logic.business-coupling-invariants :as inv]))

(deftest business-coupling-verify-green
  (let [report (inv/run-verify)]
    (is (:verified? report))
    (is (get-in report [:witness :clean?]))
    (is (get-in report [:summary :all-adversarial-caught?]))
    (is (= 4 (get-in report [:summary :n-invariants])))))

(deftest conforming-trace-has-zero-violations
  (let [report (inv/run-verify inv/conforming-trace)]
    (is (:verified? report))
    (is (every? empty? (vals (get-in report [:witness :violations]))))))

(deftest conforming-inbound-click-models-interest-event
  (let [{:keys [evidence-anchor posterior-delta]} inv/conforming-inbound]
    (is (seq evidence-anchor))
    (is (= :posterior-state (:field posterior-delta)))
    (is (contains? inv/interest-event-posterior-states (:to posterior-delta)))))

(deftest adversarial-traces-caught-by-own-category
  (let [report (inv/run-verify)]
    (doseq [[cat result] (:adversarial report)]
      (is (:caught? result) (str cat " should be caught"))
      (is (seq (:hits result)) (str cat " should report hits")))))

(deftest each-adversarial-trace-fails-verification
  (doseq [[cat trace] inv/adversarial-traces]
    (let [report (inv/run-verify trace)]
      (is (not (:verified? report)) (str cat " adversarial trace should fail"))
      (is (seq (get-in report [:witness :violations cat]))
          (str cat " should be the flagged category"))
      (is (every? empty? (vals (dissoc (get-in report [:witness :violations]) cat)))
          (str cat " adversarial trace should violate only its category")))))
