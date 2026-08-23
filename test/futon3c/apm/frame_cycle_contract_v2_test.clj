(ns futon3c.apm.frame-cycle-contract-v2-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.frame-cycle-contract :as sut]))

(def contract
  (edn/read-string
   (slurp "holes/labs/M-apm-demonstration/frame-cycle-contract-v2.edn")))

(deftest solver-promotion-is-before-every-student
  (let [order (:phase-order contract)
        promotion (.indexOf order :promote-solver)]
    (is (:ok (sut/validate-contract contract)))
    (is (every? #(< promotion (.indexOf order %))
                [:student-attempt-1 :student-attempt-2 :student-attempt-3]))
    (is (every? #(contains? (get-in contract [:phases % :requires])
                            :solver-memory-snapshot)
                [:student-attempt-1 :student-attempt-2 :student-attempt-3]))))

(deftest promotion-and-student-receipts-carry-snapshot-identity
  (is (every? (get-in contract [:receipt/schemas :solver-promotion :required])
              [:receipt/snapshot-id :receipt/snapshot-digest
               :receipt/reviewed-memory-ids :receipt/independent-review?]))
  (is (contains? (get-in contract [:receipt/schemas :student-attempt :required])
                 :receipt/memory-snapshot)))
