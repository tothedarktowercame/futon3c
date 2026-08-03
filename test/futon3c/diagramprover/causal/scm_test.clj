(ns futon3c.diagramprover.causal.scm-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.causal.scm :as scm]))

(defn tiny-dag
  ([] (tiny-dag "A or B"))
  ([equation]
   {:variables (sorted-map
                :A {:id :A :kind :observed}
                :B {:id :B :kind :observed}
                :death {:id :death :kind :observed})
    :arrows [{:from :A :to :death} {:from :B :to :death}]
    :leak-edges [] :interventions [] :sensors []
    :metadata {:structural_equations {:death equation}}}))

(deftest fixed-equation-grammar
  (is (= {:op :variable :input :A} (scm/parse-equation "A")))
  (is (= {:op :not :input :A} (scm/parse-equation "not A")))
  (is (= {:op :and :inputs [:A :B]} (scm/parse-equation "A and B")))
  (is (= {:op :or :inputs [:A :B]} (scm/parse-equation "A or B"))))

(deftest underdetermined-counterfactual-carries-both-worlds
  (let [receipt (scm/counterfactual
                 (tiny-dag)
                 {:evidence {:death true}
                  :intervention {:A false}
                  :outcome :death})]
    (is (= :counterfactual-underdetermined (:reason receipt)))
    (is (= #{false true} (set (keys (:witness-worlds receipt)))))
    (is (= 3 (get-in receipt [:abduction :consistent-count])))
    (is (false? (get-in receipt [:prediction :all-agree?])))))

(deftest inconsistent-evidence-is-refused
  (let [receipt (scm/counterfactual
                 (tiny-dag)
                 {:evidence {:A true :death false}
                  :intervention {:A false}
                  :outcome :death})]
    (is (= :evidence-inconsistent (:reason receipt)))
    (is (zero? (get-in receipt [:abduction :consistent-count])))))

(deftest equation-parent-mismatch-is-rejected
  (testing "all and only DAG parents must occur in the equation"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Structural equations do not match causal DAG"
         (scm/validate (tiny-dag "A"))))))

(deftest unspecified-scm-preserves-capability-boundary
  (let [dag (assoc-in (tiny-dag) [:metadata :structural_equations] {})
        receipt (scm/counterfactual dag {:outcome :death})]
    (is (= :counterfactual-identification (:missing-capability receipt)))))

(deftest deterministic-counterfactual
  (let [query {:evidence {:death true}
               :intervention {:A false}
               :outcome :death}]
    (is (= (scm/counterfactual (tiny-dag) query)
           (scm/counterfactual (tiny-dag) query)))))
