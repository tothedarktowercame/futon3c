(ns futon3c.logic.substrate-metric-e1-invariants-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.logic.substrate-metric-e1-invariants :as inv]))

(deftest e1-cut-verify-green
  (let [report (inv/run-verify)]
    (is (:verified? report))
    (is (get-in report [:witness :clean?]))
    (is (get-in report [:summary :all-adversarial-caught?]))
    (is (= 6 (get-in report [:summary :n-invariants])))))

(deftest adversarial-traces-caught-by-own-category
  (let [report (inv/run-verify)]
    (doseq [[cat result] (:adversarial report)]
      (is (:caught? result) (str cat " should be caught"))
      (is (seq (:hits result)) (str cat " should report hits")))))
