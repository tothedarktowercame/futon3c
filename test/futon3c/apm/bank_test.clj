(ns futon3c.apm.bank-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.apm.bank :as bank]))

(def digest (apply str (repeat 64 "a")))
(def merge-sha (apply str (repeat 40 "b")))

(def base
  {:receipt/type :frame-bank
   :receipt/frame-id "f21"
   :receipt/problem-id "a94A02"
   :receipt/verify-receipt-id digest
   :receipt/lane-transition {:from :proof :to :done}})

(def landed
  {:receipt/trunk-branch "master"
   :receipt/merge-sha merge-sha
   :receipt/post-merge-axioms '[propext Classical.choice Quot.sound]
   :receipt/rollup-sorry-warnings 0
   :receipt/status-recomputed
   {:previous-classification "partial"
    :classification "solved"
    :previous-sorry-count 1
    :sorry-count 0
    :method :elaboration}
   :receipt/branch-deleted true})

(defn built [body]
  (:receipt (bank/build-receipt body)))

(deftest all-four-rulings-produce-content-addressed-receipts
  (let [closed (built (merge base landed {:receipt/ruling :closed}))
        partial (built (merge base landed
                              {:receipt/ruling :partial-banked
                               :receipt/lane-transition {:from :proof :to :library}
                               :receipt/boundary "Mathlib lacks the chain-map seam"
                               :receipt/status-recomputed
                               (assoc (:receipt/status-recomputed landed)
                                      :classification "partial-banked"
                                      :sorry-count 1)}))
        defective (built (merge base
                                {:receipt/ruling :statement-defective
                                 :receipt/lane-transition {:from :proof :to :repair}
                                 :receipt/defect-witness
                                 "f(z)=(1-z)^-1+(z-1)^-2 has coefficients n+2"
                                 :receipt/refuted-statement-sha digest}))
        blocked (built (merge base
                              {:receipt/ruling :blocked
                               :receipt/lane-transition {:from :proof :to :library}
                               :receipt/seam "Missing producer for the spectral lemma"}))]
    (is (every? some? [closed partial defective blocked]))
    (is (every? #(true? (:ok (bank/validate-receipt %)))
                [closed partial defective blocked]))
    (is (every? #(= #{digest} (:receipt/input-receipt-ids %))
                [closed partial defective blocked]))
    (is (= [:done :library :repair :library]
           (mapv #(get-in % [:receipt/lane-transition :to])
                 [closed partial defective blocked])))
    (is (= "partial-banked" (:receipt/classification partial)))
    (is (= "statement-defective" (:receipt/classification defective)))))

(deftest statement-defective-requires-an-exact-witness
  (let [result (bank/build-receipt
                (merge base
                       {:receipt/ruling :statement-defective
                        :receipt/lane-transition {:from :proof :to :repair}
                        :receipt/refuted-statement-sha digest}))]
    (is (false? (:ok result)))
    (is (some #{:defect-witness-required} (:findings result)))))

(deftest closed-refuses-a-sorry-bearing-rollup
  (let [result (bank/build-receipt
                (merge base landed
                       {:receipt/ruling :closed
                        :receipt/rollup-sorry-warnings 1}))]
    (is (false? (:ok result)))
    (is (some #{:rollup-carries-sorry} (:findings result)))))

(deftest landed-rulings-require-post-merge-evidence
  (testing "pre-merge axioms cannot substitute for the post-merge rerun"
    (let [result (bank/build-receipt
                  (dissoc (merge base landed {:receipt/ruling :closed})
                          :receipt/post-merge-axioms))]
      (is (false? (:ok result)))
      (is (some #(and (map? %) (= :bank-fields-missing (:finding %)))
                (:findings result)))))
  (testing "status must be recomputed by elaboration"
    (let [result (bank/build-receipt
                  (assoc-in (merge base landed {:receipt/ruling :closed})
                            [:receipt/status-recomputed :method] :grep))]
      (is (false? (:ok result)))
      (is (some #{:status-not-recomputed-by-elaboration}
                (:findings result))))))

(deftest canonical-new-classifications-are-authoritative
  (is (= "statement-defective" bank/statement-defective-classification))
  (is (= "partial-banked" bank/banked-seam-classification)))

(deftest closed-pins-its-classification
  (testing "a closed ruling may not record a classification other than solved"
    (let [result (bank/build-receipt
                  (assoc-in (merge base landed {:receipt/ruling :closed})
                            [:receipt/status-recomputed :classification]
                            "complete"))]
      (is (false? (:ok result)))
      (is (some #{:closed-classification-invalid} (:findings result))))))
