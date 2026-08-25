(ns futon3c.apm.generated-contract-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.generated-contract :as sut]))

(def generated-path
  "holes/labs/M-apm-demonstration/generated/apm-cycle-contract-v4.json")

(def candidate-generated-path
  "holes/labs/M-apm-demonstration/generated/apm-cycle-contract-v4.json")

(def clojure-contract
  (edn/read-string
   (slurp "holes/labs/M-apm-demonstration/frame-cycle-contract-v2.edn")))

(deftest lean-emitted-contract-round-trips-through-clojure
  (let [result (sut/validate-round-trip generated-path clojure-contract)]
    (is (:ok result) (pr-str result))
    (is (= "promote-solver"
           (get-in result [:contract :transitions 2 :to])))))

(deftest candidate-strengthened-contract-round-trips
  (let [result (sut/validate-round-trip candidate-generated-path
                                        clojure-contract)]
    (is (:ok result) (pr-str result))
    (is (= 1800000 (get-in result [:contract :bounds
                                   :student-turn-timeout-ms])))
    (is (= sut/required-student-candidate-policy
           (select-keys (get-in result [:contract :terminal-policy])
                        (keys sut/required-student-candidate-policy))))))

(deftest partial-student-candidate-policy-is-refused
  (let [contract (:contract (sut/read-contract candidate-generated-path))
        result (sut/validate
                (update contract :terminal-policy
                        dissoc :student-candidate-persisted-before-receipt))]
    (is (= [:generated-contract-student-candidate-policy-invalid]
           (:findings result)))))

(deftest rejected-candidate-and-retirement-order-mutations-are-refused
  (let [contract (:contract (sut/read-contract candidate-generated-path))]
    (doseq [[path bad-value finding]
            [[[:terminal-policy :rejected-student-candidate-evidence-only]
              false :generated-contract-student-candidate-policy-invalid]
             [[:terminal-policy :missing-observation-records-certified-candidate]
              true :generated-contract-student-candidate-policy-invalid]
             [[:terminal-policy :missing-observation-controller-memory-use-required]
              false :generated-contract-student-candidate-policy-invalid]
             [[:dispatch-policy :frame-terminal-persisted-before-retirement]
              false :generated-contract-dispatch-policy-invalid]
             [[:dispatch-policy :retirement-replay-uses-persisted-terminal]
              false :generated-contract-dispatch-policy-invalid]]]
      (let [result (sut/validate (assoc-in contract path bad-value))]
        (is (some #{finding} (:findings result)) (pr-str path))))))

(deftest complete-cycle-is-non-vacuous
  (let [contract (:contract (sut/read-contract generated-path))]
    (is (= 11 (count (:phase-order contract))))
    (is (= "preflight" (first (:phase-order contract))))
    (is (= "close-frame" (last (:phase-order contract))))
    (is (nil? (get-in contract [:transitions 10 :to])))))

(deftest missing-promotion-mutation-is-killed
  (let [contract (:contract (sut/read-contract generated-path))
        mutated (-> contract
                    (update :phase-order #(vec (remove #{"promote-solver"} %)))
                    (update :transitions #(vec (remove
                                                (fn [edge]
                                                  (or (= "promote-solver" (:from edge))
                                                      (= "promote-solver" (:to edge))))
                                                %))))
        result (sut/validate mutated)]
    (is (false? (:ok result)))
    (is (some #{:generated-contract-verify-bypasses-promotion}
              (:findings result)))))

(deftest timeout-as-success-policy-mutation-is-killed
  (let [contract (:contract (sut/read-contract generated-path))
        result (sut/validate
                (assoc-in contract [:dispatch-policy :client-timeout-is-success]
                          true))]
    (is (false? (:ok result)))
    (is (some #{:generated-contract-dispatch-policy-invalid}
              (:findings result)))))

(deftest blocking-preflight-warning-mutation-is-killed
  (let [contract (:contract (sut/read-contract generated-path))
        result (sut/validate
                (assoc-in contract
                          [:dispatch-policy :preflight-blocking-warning-count]
                          1))]
    (is (false? (:ok result)))
    (is (some #{:generated-contract-dispatch-policy-invalid}
              (:findings result)))))

(deftest coordinator-retry-policy-mutations-are-killed
  (let [contract (:contract (sut/read-contract generated-path))]
    (doseq [field [:coordinator-one-registration-per-problem
                   :coordinator-retries-increment-same-entry
                   :coordinator-retry-beyond-maximum-refused]]
      (let [result (sut/validate
                    (assoc-in contract [:dispatch-policy field] false))]
        (is (false? (:ok result)) (str field))
        (is (some #{:generated-contract-dispatch-policy-invalid}
                  (:findings result)) (str field))))))

(deftest memory-and-isolation-policy-mutations-are-killed
  (let [contract (:contract (sut/read-contract generated-path))
        memory-result (sut/validate
                       (assoc-in contract [:memory-policy :student-attempts] 2))
        isolation-result
        (sut/validate
         (assoc-in contract [:isolation-policy :campaign-scoped-regulator]
                   false))]
    (is (some #{:generated-contract-memory-policy-invalid}
              (:findings memory-result)))
    (is (some #{:generated-contract-isolation-policy-invalid}
              (:findings isolation-result)))))

(deftest f29-open-search-policy-mutations-are-killed
  (let [contract (:contract (sut/read-contract generated-path))
        mutations [[:open-reviewed-corpus-search false]
                   [:search-query-trace-persisted false]
                   [:search-results-content-addressed false]
                   [:student-open-search-distinct-from-proactive-snapshot false]
                   [:self-reported-query-is-search-evidence true]
                   [:search-capable-roles ["student" "scribe"]]]]
    (doseq [[field value] mutations]
      (let [result (sut/validate
                    (assoc-in contract [:memory-policy field] value))]
        (is (false? (:ok result)) (str field))
        (is (some #{:generated-contract-memory-policy-invalid}
                  (:findings result)) (str field))))))

(deftest generated-receipt-and-submission-schema-mutations-are-killed
  (let [contract (:contract (sut/read-contract generated-path))
        submission-mutant
        (assoc-in contract
                  [:submission-schemas :student-memory-use
                   :role-authored-fields]
                  ["used-ids" "surfaced-ids"])
        receipt-mutant
        (update-in contract [:receipt-schemas :student-attempt :required]
                   pop)
        phase-mutant
        (update-in contract [:phases :student-attempt-2 :requires] pop)]
    (is (some #{:generated-contract-submission-schemas-invalid}
              (:findings (sut/validate submission-mutant))))
    (is (some #{:generated-contract-receipt-schemas-invalid}
              (:findings (sut/validate receipt-mutant))))
    (is (some #{:generated-contract-phase-io-invalid}
              (:findings (sut/validate phase-mutant))))))

(deftest wire-result-and-session-rotation-policy-mutations-are-killed
  (let [contract (:contract (sut/read-contract generated-path))]
    (is (some #{:generated-contract-terminal-policy-invalid}
              (:findings
               (sut/validate
                (assoc-in contract
                          [:terminal-policy :close-result-wire-canonicalization]
                          false)))))
    (is (some #{:generated-contract-memory-policy-invalid}
              (:findings
               (sut/validate
                (assoc-in contract
                          [:memory-policy :fresh-session-rotation-mints-new-id]
                          false)))))))

(deftest residual-fit-promotion-policy-mutation-is-killed
  (let [contract (:contract (sut/read-contract generated-path))
        result (sut/validate
                (assoc-in contract
                          [:promotion-policy :persisted-review-residual-required]
                          false))]
    (is (false? (:ok result)))
    (is (some #{:generated-contract-promotion-policy-invalid}
              (:findings result)))))

(deftest terminal-and-analyst-policy-mutations-are-killed
  (let [contract (:contract (sut/read-contract generated-path))]
    (is (some #{:generated-contract-terminal-policy-invalid}
              (:findings
               (sut/validate
                (assoc-in contract [:terminal-policy :certified-phase-receipts]
                          10)))))
    (is (some #{:generated-contract-analyst-policy-invalid}
              (:findings
               (sut/validate
                (assoc-in contract [:analyst-policy :in-flight-mutation]
                          true)))))))

(deftest f25-derived-terminal-policy-mutations-are-killed
  (let [contract (:contract (sut/read-contract generated-path))
        mutations [[:solved-partial-bankable false]
                   [:bankable-solved-successor-eligible false]
                   [:missing-observation-author "student"]
                   [:missing-observation-may-impersonate-student true]
                   [:learning-outcome-required false]]]
    (doseq [[field value] mutations]
      (let [result (sut/validate
                    (assoc-in contract [:terminal-policy field] value))]
        (is (false? (:ok result)) (str field))
        (is (some #{:generated-contract-terminal-policy-invalid}
                  (:findings result)) (str field))))))

(deftest deterministic-collection-policy-mutations-are-killed
  (let [contract (:contract (sut/read-contract generated-path))
        mutations [[:terminal-collection-required false]
                   [:terminal-collection-persisted false]
                   [:terminal-collection-before-missing-observation false]
                   [:terminal-collection-attempts-per-role 0]
                   [:terminal-repair-attempts-per-role 2]
                   [:terminal-collection-covered-role-count 6]
                   [:role-terminal-budgets :missing]
                   [:missing-observation-student-only false]
                   [:unbounded-conversational-retries true]]]
    (doseq [[field value] mutations]
      (let [result (sut/validate
                    (assoc-in contract [:dispatch-policy field] value))]
        (is (false? (:ok result)) (str field))
        (is (some #{:generated-contract-dispatch-policy-invalid}
                  (:findings result)) (str field))))))
