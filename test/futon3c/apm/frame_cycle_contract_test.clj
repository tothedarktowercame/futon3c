(ns futon3c.apm.frame-cycle-contract-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.frame-cycle-contract :as contract]))

(def path
  "holes/labs/M-apm-demonstration/frame-cycle-contract-v1.edn")

(def cycle-contract (:contract (contract/read-contract path)))

(deftest complete-cycle-is-linear-and-data-ready
  (let [validated (contract/validate-contract cycle-contract)]
    (is (:ok validated) (pr-str validated))
    (is (= 10 (count (:phase-order validated))))
    (is (= [:student-attempt-1 :student-attempt-2 :student-attempt-3]
           (filterv #(= :student-attempt
                        (get-in cycle-contract [:phases % :kind]))
                    (:phase-order validated))))
    (is (every? true?
                (map #(get-in cycle-contract [:phases % :fresh-session?])
                     [:student-attempt-1 :student-attempt-2
                      :student-attempt-3])))))

(deftest close-requires-the-whole-map-not-the-analyst
  (let [required (get-in cycle-contract [:phases :close-frame :requires])]
    (is (contains? required :verify-receipt))
    (is (contains? required :memory-use-3-receipt))
    (is (contains? required :guide-intervention-2-receipt))
    (is (contains? required :promotion-review-receipt))
    (is (not-any? #{:analyst-receipt} required))))

(deftest typed-receipts-fail-closed
  (let [body {:receipt/type :student-attempt
              :receipt/frame-id "f18" :receipt/problem-id "a97J07"
              :receipt/attempt-ordinal 1 :receipt/fresh-session-id "fresh-1"
              :receipt/job-id "job-1" :receipt/outcome :stuck
              :receipt/failure-account {:tried "x" :expected "y" :actual "z"}
              :receipt/memory-use {:surfaced-ids []}}
        receipt (assoc body :receipt/id (machine/ledger-digest [body]))]
    (is (:ok (contract/validate-receipt cycle-contract
                                        :student-attempt-1 receipt)))
    (is (= :frame-cycle-receipt-fields-missing
           (:error/code
            (contract/validate-receipt cycle-contract :student-attempt-1
                                       (dissoc receipt :receipt/job-id)))))
    (is (= :frame-cycle-receipt-content-invalid
           (:error/code
            (contract/validate-receipt cycle-contract :student-attempt-1
                                       (assoc receipt :receipt/outcome
                                              :solved)))))))

(deftest dependency-ordering-fails-closed
  (let [bad (assoc-in cycle-contract [:phases :student-attempt-1 :requires]
                      #{:guide-intervention-2-receipt})]
    (is (= :frame-cycle-input-before-production
           (:error/code (contract/validate-contract bad))))))
