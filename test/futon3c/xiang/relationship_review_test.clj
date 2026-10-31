(ns futon3c.xiang.relationship-review-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.xiang.relationship-review :as sut]))

(deftest clock-context-diff-produces-review-not-a-relationship
  (let [text "IF prove memory useful is clocked HOWEVER operator text concerns Giraffe THEN relationship between memory and giraffe may need formalisation"
        reviews (sut/relationship-reviews
                 {:clocked-target "E-prove-memory-useful"
                  :operator-evidence-id "e-operator-example"
                  :operator-text text
                  :resolved-references [{:id "M-giraffe" :quote "Giraffe"
                                         :basis :registry-alias}]})]
    (is (= [{:event :relationship-review
             :status :proposed
             :relation :unresolved
             :clocked-work "E-prove-memory-useful"
             :discussed-work "M-giraffe"
             :evidence {:operator-evidence-id "e-operator-example"
                        :quote "Giraffe"
                        :reference-basis :registry-alias}}]
           reviews))
    (is (not-any? #(contains? % :reclock-to) reviews))))

(deftest exact-work-ids-need-no-alias-resolver
  (is (= ["M-giraffe" "T-one"]
         (mapv :discussed-work
               (sut/relationship-reviews
                {:clocked-target "E-prove-memory-useful"
                 :operator-evidence-id "e-1"
                 :operator-text "compare M-giraffe with T-one"})))))

(deftest fail-closed-without-evidence-or-verbatim-support
  (testing "no durable operator address"
    (is (= [] (sut/relationship-reviews
               {:clocked-target "E-prove-memory-useful"
                :operator-text "M-giraffe"}))))
  (testing "resolver quote must occur in the operator text"
    (is (= [] (sut/relationship-reviews
               {:clocked-target "E-prove-memory-useful"
                :operator-evidence-id "e-1"
                :operator-text "some unrelated text"
                :resolved-references [{:id "M-giraffe" :quote "Giraffe"
                                       :basis :registry-alias}]}))))
  (testing "mention of the clock target is not a relationship candidate"
    (is (= [] (sut/relationship-reviews
               {:clocked-target "E-prove-memory-useful"
                :operator-evidence-id "e-1"
                :operator-text "continue E-prove-memory-useful"})))))
