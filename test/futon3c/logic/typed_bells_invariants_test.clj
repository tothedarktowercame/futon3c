(ns futon3c.logic.typed-bells-invariants-test
  "VERIFY gate for M-typed-bells: the TB-1..7 logic model must pass before any
   protocol / ArSE-bridge code is written. Conforming witness clean; each
   adversarial trace caught by its own category."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.logic.typed-bells-invariants :as tb]))

(deftest typed-bells-verify-green
  (let [report (tb/run-verify)]
    (is (:verified? report))
    (is (get-in report [:witness :clean?]) "conforming witness has zero violations")
    (is (get-in report [:summary :all-adversarial-caught?]))
    (is (= 7 (get-in report [:summary :n-invariants])))))

(deftest adversarial-traces-caught-by-own-category
  (let [report (tb/run-verify)]
    (doseq [[cat result] (:adversarial report)]
      (is (:caught? result) (str cat " should be caught"))
      (is (seq (:hits result)) (str cat " should report hits")))))

(deftest witness-is-byte-clean-per-category
  ;; every category empty on the conforming trace — no false positives
  (let [v (tb/query-violations (tb/build-db tb/witness-trace))]
    (doseq [[cat hits] v]
      (is (empty? hits) (str cat " must be empty on the witness")))))
