(ns futon3c.agency.pattern-card-record-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.agency.pattern-card-record :as record]))

(def at "2026-09-28T11:00:00Z")
(def harness {:kind :none :basis :producer-context :source-ref "test:p10-2a-2"})
(def selection
  {:id "act:card-a" :kind :pattern-card/selection :author "agent-a"
   :agent "agent-a" :session "session-1" :at at :pattern-id "pattern/a"
   :act/harness harness})
(def withdrawal
  {:id "act:withdraw-a" :kind :act/withdrawal :author "agent-a" :at at
   :target "act:card-a" :status :effective :basis {:kind :self}
   :reverses nil :act/harness harness})

(defn refusal [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn reason [f]
  (:reason (refusal f)))

(deftest valid-records-pass
  (is (= selection (record/validate-selection selection)))
  (is (= withdrawal (record/validate-withdrawal withdrawal selection))))

(deftest selection-refusals-are-typed
  (is (= :missing-at (reason #(record/validate-selection (dissoc selection :at)))))
  (is (= :invalid-at (reason #(record/validate-selection (assoc selection :at "later")))))
  (doseq [field [:agent :session :pattern-id]]
    (let [data (refusal #(record/validate-selection (dissoc selection field)))]
      (is (= :missing-selection-field (:reason data)))
      (is (= field (:field data)))))
  (is (= :missing-author
         (reason #(record/validate-selection (dissoc selection :author))))))

(deftest withdrawal-refusals-are-typed
  (is (= :missing-at (reason #(record/validate-withdrawal (dissoc withdrawal :at)))))
  (is (= :invalid-at
         (reason #(record/validate-withdrawal (assoc withdrawal :at "not-an-instant")))))
  (is (= :missing-target
         (reason #(record/validate-withdrawal (dissoc withdrawal :target)))))
  (is (= :invalid-status
         (reason #(record/validate-withdrawal (assoc withdrawal :status :pending)))))
  (is (= :invalid-basis
         (reason #(record/validate-withdrawal
                   (assoc withdrawal :basis {:kind :inferred})))))
  (is (= :not-author
         (reason #(record/validate-withdrawal
                   (assoc withdrawal :author "agent-b") selection))))
  (is (= :reversal-missing-target
         (reason #(record/validate-withdrawal
                   (-> withdrawal (dissoc :target) (assoc :reverses "act:provisional"))))))
  (is (= :interpretation-not-effect
         (reason #(record/validate-withdrawal
                   {:id "act:reading" :kind :interpretation :author "xiang"
                    :at at :target "act:card-a"})))))

(deftest hyperedge-round-trips-preserve-record-and-harness
  (testing "selection"
    (let [edge (record/record->hyperedge selection)]
      (is (= :pattern-card/selection (:hx/type edge)))
      (is (= at (:hx/valid-time edge)))
      (is (= harness (get-in edge [:hx/props :act/harness])))
      (is (= selection (record/hyperedge->record edge)))))
  (testing "withdrawal"
    (let [edge (record/record->hyperedge withdrawal)]
      (is (= :act/withdrawal (:hx/type edge)))
      (is (= at (:hx/valid-time edge)))
      (is (= harness (get-in edge [:hx/props :act/harness])))
      (is (= withdrawal (record/hyperedge->record edge))))))

(deftest reverses-must-name-an-act
  (is (= :invalid-reverses
         (reason #(record/validate-withdrawal (assoc withdrawal :reverses "")))))
  (is (= withdrawal (record/validate-withdrawal withdrawal))))
