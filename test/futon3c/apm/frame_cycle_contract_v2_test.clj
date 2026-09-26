(ns futon3c.apm.frame-cycle-contract-v2-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.campaign-machine :as machine]
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

(deftest controller-observation-is-the-only-student-alternate
  (let [body {:receipt/type :student-observation-missing
              :receipt/frame-id "fixture-f25" :receipt/problem-id "m94A02"
              :receipt/attempt-ordinal 1 :receipt/job-id "job-f25-shaped"
              :receipt/author :controller
              :receipt/reason :typed-submission-missing
              :receipt/repair-attempts 1
              :receipt/memory-snapshot {:snapshot-id "snap"}
              :receipt/harness-observed {:job {:state :done}}}
        receipt (assoc body :receipt/id (machine/ledger-digest [body]))]
    (is (:ok (sut/validate-receipt contract :student-attempt-1 receipt)))
    (is (= :frame-cycle-receipt-type-mismatch
           (:error/code (sut/validate-receipt
                         contract :student-attempt-1
                         (let [forged (assoc body :receipt/type :frame-close)]
                           (assoc forged :receipt/id
                                  (machine/ledger-digest [forged])))))))))
