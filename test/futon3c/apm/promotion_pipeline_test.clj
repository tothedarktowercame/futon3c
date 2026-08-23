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

(deftest deposit-requires-a-bound-pattern-per-candidate
  ;; f27: three candidates with :pattern-ids [] reached the proctor and were
  ;; all rejected pattern-attachment-missing; the solver arm deposited nothing.
  (let [findings (:findings (sut/validate-deposit
                             {:depositor "scribe"
                              :candidates [candidate
                                           (assoc candidate :memory-id "m2"
                                                  :pattern-ids [])]
                              :lanes lanes}))]
    (is (some #{:candidate-patterns-missing} findings))
    (is (not (some #{:candidate-shape-invalid} findings))
        "an empty vector is well-shaped; it is the binding that is missing")))

(deftest guide-deposit-is-gated-without-a-lane-report
  (let [ok (sut/validate-guide-deposit {:depositor "f27-guide"
                                        :candidates [candidate]})]
    (is (:ok ok))
    (is (= [candidate] (:candidates ok))))
  (is (= [:candidate-patterns-missing]
         (:findings (sut/validate-guide-deposit
                     {:depositor "f27-guide"
                      :candidates [(assoc candidate :pattern-ids [])]}))))
  (is (some #{:candidate-shape-invalid}
            (:findings (sut/validate-guide-deposit
                        {:depositor "f27-guide"
                         :candidates [(dissoc candidate :source-attempts)]}))))
  (is (some #{:candidates-missing}
            (:findings (sut/validate-guide-deposit {:depositor "g" :candidates []})))))

(deftest review-core-matches-the-deposit-entry-point
  (let [deposit {:depositor "f22-scribe" :candidates [candidate] :lanes lanes}
        review {:memory-id "m1" :reviewer "f22-proctor" :verdict :approve
                :review-evidence-id "e1" :attachment-status :reviewed
                :pattern-ids ["p1"] :reason "actionable fact"
                :residual "Main.lean:12"}]
    (is (= (sut/validate-review deposit "f22-proctor" [review])
           (sut/validate-review* [candidate] "f22-scribe" "f22-proctor" [review])))))
