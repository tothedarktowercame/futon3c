(ns futon3c.diagramprover.rule-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.graph :as graph]
            [futon3c.diagramprover.rule :as rule]))

(deftest rule-boundaries-must-agree
  (testing "compatible boundaries form a rule"
    (is (map? (rule/make-rule (graph/generator :f [:wire] [:wire])
                              (graph/generator :g [:wire] [:wire])))))
  (testing "different boundaries are rejected"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"Inputs must match"
         (rule/make-rule (graph/generator :f [:wire] [:wire])
                         (graph/generator :g [:bit] [:wire]))))))
