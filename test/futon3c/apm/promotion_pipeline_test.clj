(ns futon3c.apm.promotion-pipeline-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.promotion-pipeline :as sut]))

(def candidate {:memory-id "m1" :content-digest "d1" :pattern-ids ["p1"]
                :source-attempts [1]})
(def lanes [{:lane :solve :status :ran}
            {:lane :arc :status :ran-empty :reason "no errors"}
            {:lane :trajectory :status :ran}
            {:lane :challenge :status :not-run :reason "no prior claim"}])

(defn materialization [id digest]
  {:artifact-id id :content-digest digest
   :persisted-content-digest digest :read-back-content-digest digest
   :persistence-receipt-id id})

(deftest completed-pass-requires-one-materialized-disposition-per-candidate
  (let [candidate (assoc candidate :materialization (materialization "m1" "d1"))
        review {:memory-id "m1" :verdict :reject
                :attachment-status :proposed :pattern-ids ["p1"]
                :review-materialization (materialization "r1" "rd1")}
        valid (sut/validate-complete-dispositions [candidate] [review])]
    (is (:ok valid) valid)
    (is (= [{:memory-id "m1" :verdict :reject
             :candidate-materialization (materialization "m1" "d1")
             :review-materialization (materialization "r1" "rd1")
             :attachment-status :proposed :pattern-ids ["p1"]
             :publishing? false}]
           (:dispositions valid)))
    (is (= :promotion-pass-incomplete
           (:error/code
            (sut/validate-complete-dispositions
             [candidate] [(assoc review :verdict :cannot-judge)]))))
    (is (= :promotion-pass-incomplete
           (:error/code
            (sut/validate-complete-dispositions [candidate] []))))))

(deftest projection-failure-is-not-a-completed-disposition
  (let [candidate (assoc candidate :materialization
                         (materialization "m1" "d1"))
        review {:memory-id "m1" :verdict :approve
                :attachment-status :proposed :pattern-ids ["p1"]
                :projection/valid? false
                :projection/finding {:failure :edge-write-failed}
                :review-materialization (materialization "r1" "rd1")}
        result (sut/validate-complete-dispositions [candidate] [review])]
    (is (false? (:ok result)))
    (is (= :promotion-pass-incomplete (:error/code result)))
    (is (= :promotion-review-projection-failed
           (get-in result [:findings 0 :finding])))
    (is (nil? (:dispositions result)))))

(deftest certification-binds-exact-publishing-subset-to-materialized-snapshot
  (let [dispositions [{:memory-id "approved" :publishing? true}
                      {:memory-id "rejected" :publishing? false}]
        snapshot {:snapshot/id "snapshot-digest"
                  :snapshot/digest "snapshot-digest"}
        valid (sut/validate-certified-promotion-pass
               dispositions snapshot "/snapshots/f42.edn"
               [{:memory-id "approved"}])]
    (is (:ok valid) valid)
    (is (= ["approved"]
           (get-in valid [:witness :published-memory-ids])))
    (is (= :certified-promotion-pass-invalid
           (:error/code
            (sut/validate-certified-promotion-pass
             dispositions snapshot "/snapshots/f42.edn"
             [{:memory-id "rejected"}]))))))

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
              (:findings (sut/validate-review deposit "f22-proctor" []))))
    (is (some #{:review-verdict-invalid}
              (:findings
               (sut/validate-review deposit "f22-proctor"
                                    [(assoc review :verdict :challenge)]))))))

(deftest deposit-requires-all-four-typed-lanes
  (is (:ok (sut/validate-deposit {:depositor "scribe" :candidates [candidate]
                                  :lanes lanes})))
  (is (some #{:lane-report-invalid}
            (:findings (sut/validate-deposit
                        {:depositor "scribe" :candidates [candidate]
                         :lanes []})))))

(deftest deposit-mechanically-rejects-an-unbound-candidate
  ;; f27: three candidates with :pattern-ids [] reached the proctor and were
  ;; all rejected pattern-attachment-missing; the solver arm deposited nothing.
  (let [result (sut/validate-deposit
                {:depositor "scribe"
                 :candidates [candidate
                              (assoc candidate :memory-id "m2"
                                     :pattern-ids [])]
                 :lanes lanes})]
    (is (:ok result))
    (is (= [candidate] (:candidates result)))
    (is (= [:no-parent-pattern]
           (:finding-codes (first (:mechanical-reviews result)))))))

(deftest mechanical-review-guards-reject-f29-proof-block-and-identifiers
  (let [proof-body (get-in (edn/read-string
                            (slurp "test/fixtures/apm/f29-proof-text-memory.edn"))
                           [:evidence/body :body])
        proof (assoc candidate :hook "verbatim certified proof"
                     :body proof-body)
        prose (assoc candidate :hook "signature mismatch"
                     :body (apply str (repeat 80 "explain the reusable move ")))
        named (assoc candidate :hook "a01j05-local route" :body "short")]
    (is (some #{:proof-text-not-memory}
              (get-in (sut/validate-deposit
                       {:depositor "scribe" :candidates [proof] :lanes lanes}
                       {:problem-id "a01J05"})
                      [:mechanical-reviews 0 :finding-codes])))
    (is (:ok (sut/validate-deposit
              {:depositor "scribe" :candidates [prose] :lanes lanes}
              {:problem-id "a01J05"})))
    (is (some #{:problem-identifier-in-body}
              (get-in (sut/validate-deposit
                       {:depositor "scribe" :candidates [named] :lanes lanes}
                       {:problem-id "a01J05"})
                      [:mechanical-reviews 0 :finding-codes])))))

(deftest guide-deposit-is-gated-without-a-lane-report
  (let [ok (sut/validate-guide-deposit {:depositor "f27-guide"
                                        :candidates [candidate]})]
    (is (:ok ok))
    (is (= [candidate] (:candidates ok))))
  (is (= [:no-parent-pattern]
         (get-in (sut/validate-guide-deposit
                  {:depositor "f27-guide"
                   :candidates [(assoc candidate :pattern-ids [])]})
                 [:mechanical-reviews 0 :finding-codes])))
  (is (some #{:candidate-shape-invalid}
            (:findings (sut/validate-guide-deposit
                        {:depositor "f27-guide"
                         :candidates [(dissoc candidate :source-attempts)]}))))
  (is (some #{:candidates-missing}
            (:findings (sut/validate-guide-deposit {:depositor "g" :candidates []})))))

(deftest guide-deposits-pass-through-the-shared-mechanical-guards
  (let [proof-body (get-in (edn/read-string
                            (slurp "test/fixtures/apm/f30-guide-proof-text-memory.edn"))
                           [:evidence/body :body])
        proof (assoc candidate :body proof-body)
        prose (assoc candidate :body (apply str (repeat 40 "reusable prose move ")))
        rejected (sut/validate-guide-deposit
                  {:depositor "f30-guide" :candidates [proof]}
                  {:problem-id "a01J06"})
        accepted (sut/validate-guide-deposit
                  {:depositor "f30-guide" :candidates [prose]}
                  {:problem-id "a01J06"})]
    (is (some #{:proof-text-not-memory}
              (get-in rejected [:mechanical-reviews 0 :finding-codes])))
    (is (empty? (:candidates rejected)))
    (is (:ok accepted))
    (is (= [prose] (:candidates accepted)))
    (is (empty? (:mechanical-reviews accepted)))))

(deftest review-core-matches-the-deposit-entry-point
  (let [deposit {:depositor "f22-scribe" :candidates [candidate] :lanes lanes}
        review {:memory-id "m1" :reviewer "f22-proctor" :verdict :approve
                :review-evidence-id "e1" :attachment-status :reviewed
                :pattern-ids ["p1"] :reason "actionable fact"
                :residual "Main.lean:12"}]
    (is (= (sut/validate-review deposit "f22-proctor" [review])
           (sut/validate-review* [candidate] "f22-scribe" "f22-proctor" [review])))))

(deftest f29-guide-publication-accounts-for-preserved-prior-snapshot
  (let [prior-ids #{"e-81a44d2c-5f32-4587-8cc9-f7f62a1eb8dd"
                    "e-1866fc8e-aa5a-426c-aa30-d8d57c224238"
                    "e-aa4210cf-5ba3-49ed-8e40-96ace9aa6d8a"}
        approved-ids #{"e-93b083ba-2a5c-4492-8120-9d48ab25a2de"
                       "e-f72e5ece-2a26-48aa-a47c-2b6b310caf69"
                       "e-d2563094-59b1-45c1-902a-c28b5ad3ada3"}
        memories (fn [ids] (mapv (fn [id] {:memory-id id}) ids))
        reviews (mapv (fn [id] {:memory-id id :verdict :approve}) approved-ids)
        prior (memories prior-ids)
        union (memories (into prior-ids approved-ids))]
    (is (= :promotion-publication-accounting-invalid
           (:error/code (sut/validate-publication-accounting reviews union)))
        "first-publication accounting must reject the F29 extension shape")
    (is (:ok (sut/validate-extension-publication-accounting
              reviews prior union)))
    (is (= #{"e-81a44d2c-5f32-4587-8cc9-f7f62a1eb8dd"}
           (:missing-prior-memory-ids
            (sut/validate-extension-publication-accounting
             reviews prior (memories (disj (into prior-ids approved-ids)
                                           "e-81a44d2c-5f32-4587-8cc9-f7f62a1eb8dd"))))))
    (is (= #{"unreviewed"}
           (:unapproved-new-memory-ids
            (sut/validate-extension-publication-accounting
             reviews prior (conj union {:memory-id "unreviewed"})))))))
