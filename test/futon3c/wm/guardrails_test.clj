(ns futon3c.wm.guardrails-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.wm.guardrails :as guardrails]))

(defn- mission-status [open? holes]
  (fn [_target] {:open? open? :open-hole-count holes}))

(deftest autonomous-admissible-action-classes-test
  (let [ctx {:mission-status-fn (mission-status true 1)}]
    (is (true? (guardrails/autonomous-admissible?
                {:type :address-sorry :target "sorry/foo"} ctx)))
    (is (true? (guardrails/autonomous-admissible?
                {:type :fire-pattern :target "pattern/foo"} ctx)))
    (is (false? (guardrails/autonomous-admissible?
                 {:type :learn-action-class :target :open-mission} ctx)))))

(deftest open-mission-requires-open-mission-with-holes-test
  (testing "open with holes is autonomous"
    (is (true? (guardrails/autonomous-admissible?
                {:type :open-mission :target "M-live"}
                {:mission-status-fn (mission-status true 2)}))))
  (testing "net-new, closed, and hole-free missions need the operator"
    (is (false? (guardrails/autonomous-admissible?
                 {:type :open-mission :target "M-new"}
                 {:mission-status-fn (mission-status false 2)})))
    (is (false? (guardrails/autonomous-admissible?
                 {:type :open-mission :target "M-hole-free"}
                 {:mission-status-fn (mission-status true 0)})))
    (is (false? (guardrails/autonomous-admissible?
                 {:type :open-mission :target "M-unknown-holes"}
                 {:mission-status-fn (mission-status true nil)})))))

(deftest open-mission-default-status-uses-live-mission-registry-test
  (testing "live mission with parsed holes is autonomous without ctx injection"
    (is (true? (guardrails/open-mission-with-holes?
                "M-reflective-discipline" {}))))
  (testing "done mission stays operator-gated even if historical text mentions work"
    (is (false? (guardrails/open-mission-with-holes?
                 "M-forum-refactor" {})))))

(deftest forbidden-paths-and-markers-need-operator-test
  (let [ctx {:mission-status-fn (mission-status true 1)}]
    (is (false? (guardrails/autonomous-admissible?
                 {:type :address-sorry :target "futon3c/.state/frame/checkout/foo.clj"}
                 ctx)))
    (is (false? (guardrails/autonomous-admissible?
                 {:type :address-sorry :target "futon7b/holes/public.md"}
                 ctx)))
    (is (false? (guardrails/autonomous-admissible?
                 {:type :fire-pattern :target "pattern/foo" :protocol-defining? true}
                 ctx)))
    (is (false? (guardrails/autonomous-admissible?
                 {:type :address-sorry :target "sorry/foo" :rationale "send EOI"}
                 ctx)))))

(deftest classify-action-test
  (let [ctx {:mission-status-fn (mission-status true 1)}]
    (is (= :autonomous
           (guardrails/classify-action {:type :address-sorry :target "sorry/foo"} ctx)))
    (is (= :needs-operator
           (guardrails/classify-action {:type :learn-action-class :target :open-mission} ctx)))))
