(ns futon3c.apm.generated-contract-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.generated-contract :as sut]))

(def generated-path
  "holes/labs/M-apm-demonstration/generated/apm-cycle-contract-v3.json")

(def clojure-contract
  (edn/read-string
   (slurp "holes/labs/M-apm-demonstration/frame-cycle-contract-v2.edn")))

(deftest lean-emitted-contract-round-trips-through-clojure
  (let [result (sut/validate-round-trip generated-path clojure-contract)]
    (is (:ok result) (pr-str result))
    (is (= "promote-solver"
           (get-in result [:contract :transitions 2 :to])))))

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
