(ns futon3c.apm.promotion-pipeline-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.promotion-pipeline :as sut]))

(def candidate {:memory-id "m1" :content-digest "d1" :pattern-ids ["p1"]
                :source-attempts [1]})
(def lanes [{:lane :solve :status :ran}
            {:lane :arc :status :ran-empty :reason "no errors"}
            {:lane :trajectory :status :ran}
            {:lane :challenge :status :not-run :reason "no prior claim"}])

(deftest three-student-reductions-deduplicate-with-provenance
  (let [result (sut/dedupe-candidates
                [candidate (assoc candidate :memory-id "m2" :source-attempts [2 3])])]
    (is (= 1 (count result)))
    (is (= [1 2 3] (:source-attempts (first result))))))

(deftest review-must-be-independent-complete-and-persisted
  (let [deposit {:depositor "f22-scribe" :candidates [candidate] :lanes lanes}
        review {:memory-id "m1" :reviewer "f22-proctor" :verdict :approve
                :review-evidence-id "e1" :attachment-status :reviewed
                :pattern-ids ["p1"] :reason "actionable fact"
                :residual "Main.lean:12"}]
    (let [validated (sut/validate-review deposit "f22-proctor" [review])]
      (is (:ok validated))
      (is (= "f22-scribe" (get-in validated [:candidates 0 :depositor]))
          "approved snapshot candidates retain the deposit attribution"))
    (is (some #{:reviewer-is-depositor}
              (:findings (sut/validate-review deposit "f22-scribe" [review]))))
    (is (some #{:review-set-mismatch}
              (:findings (sut/validate-review deposit "f22-proctor" []))))))

(deftest deposit-requires-all-four-typed-lanes
  (is (:ok (sut/validate-deposit {:depositor "scribe" :candidates [candidate]
                                  :lanes lanes})))
  (is (some #{:lane-report-invalid}
            (:findings (sut/validate-deposit
                        {:depositor "scribe" :candidates [candidate]
                         :lanes []})))))
