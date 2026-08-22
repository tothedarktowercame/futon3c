(ns futon3c.apm.bank
  "Pure construction and validation of durable frame-bank rulings.

  This namespace does not merge branches or recompute status.  Those effects
  are performed by the future bank driver; this boundary only certifies their
  receipts.  In particular, proof verification remains the responsibility of
  the existing verify phase."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [futon3c.apm.campaign-machine :as machine]))

(def rulings
  #{:closed :partial-banked :statement-defective :blocked})

(def ruling->lane
  {:closed :done
   :partial-banked :library
   :statement-defective :repair
   :blocked :library})

(def statement-defective-classification "statement-defective")
(def banked-seam-classification "partial-banked")
(def solved-classification "solved")

(def ruling->classification
  {:closed solved-classification
   :partial-banked banked-seam-classification
   :statement-defective statement-defective-classification})

(def required-fields
  #{:receipt/id :receipt/type :receipt/frame-id :receipt/problem-id
    :receipt/verify-receipt-id :receipt/ruling :receipt/lane-transition})

(def landed-fields
  #{:receipt/trunk-branch :receipt/merge-sha :receipt/post-merge-axioms
    :receipt/rollup-sorry-warnings :receipt/status-recomputed
    :receipt/branch-deleted})

(def ruling-fields
  {:closed (conj landed-fields :receipt/classification)
   :partial-banked (conj landed-fields :receipt/boundary
                         :receipt/classification)
   :statement-defective #{:receipt/classification :receipt/defect-witness
                          :receipt/refuted-statement-sha}
   :blocked #{:receipt/seam}})

(def permitted-axioms '[propext Classical.choice Quot.sound])

(defn- nonblank? [value]
  (and (string? value) (not (str/blank? value))))

(defn- sha256? [value]
  (and (string? value) (boolean (re-matches #"[0-9a-f]{64}" value))))

(defn- commit-sha? [value]
  (and (string? value) (boolean (re-matches #"[0-9a-f]{40}" value))))

(defn- status-findings [ruling status]
  (cond-> []
    (not (map? status))
    (conj :status-recomputed-invalid)

    (and (map? status)
         (not= #{:previous-classification :classification
                 :previous-sorry-count :sorry-count :method}
               (set (keys status))))
    (conj :status-recomputed-shape-invalid)

    (and (map? status) (not= :elaboration (:method status)))
    (conj :status-not-recomputed-by-elaboration)

    (and (map? status)
         (not (nat-int? (:previous-sorry-count status))))
    (conj :previous-sorry-count-invalid)

    (and (map? status) (not (nat-int? (:sorry-count status))))
    (conj :sorry-count-invalid)

    (and (map? status) (not (nonblank? (:classification status))))
    (conj :status-classification-invalid)

    (and (= :closed ruling) (map? status)
         (not= 0 (:sorry-count status)))
    (conj :closed-sorry-count-nonzero)

    (and (= :closed ruling) (map? status)
         (not= solved-classification (:classification status)))
    (conj :closed-classification-invalid)

    (and (= :partial-banked ruling) (map? status)
         (not= banked-seam-classification (:classification status)))
    (conj :banked-seam-classification-invalid)))

(defn findings
  "Return all semantic defects in a proposed :frame-bank receipt."
  [receipt]
  (let [ruling (:receipt/ruling receipt)
        required (set/union required-fields (get ruling-fields ruling #{}))
        missing (set/difference required (set (keys receipt)))
        lane (:receipt/lane-transition receipt)
        landed? (contains? #{:closed :partial-banked} ruling)]
    (cond-> []
      (seq missing) (conj {:finding :bank-fields-missing :fields missing})
      (not= :frame-bank (:receipt/type receipt))
      (conj :bank-receipt-type-invalid)
      (not (nonblank? (:receipt/frame-id receipt)))
      (conj :bank-frame-id-invalid)
      (not (nonblank? (:receipt/problem-id receipt)))
      (conj :bank-problem-id-invalid)
      (not (sha256? (:receipt/verify-receipt-id receipt)))
      (conj :verify-receipt-id-invalid)
      (not (contains? rulings ruling))
      (conj :bank-ruling-invalid)
      (and (contains? ruling->classification ruling)
           (not= (get ruling->classification ruling)
                 (:receipt/classification receipt)))
      (conj :bank-classification-invalid)
      (not (and (map? lane)
                (= #{:from :to} (set (keys lane)))
                (keyword? (:from lane))
                (= (get ruling->lane ruling) (:to lane))))
      (conj :lane-transition-invalid)
      (and landed? (not (nonblank? (:receipt/trunk-branch receipt))))
      (conj :trunk-branch-invalid)
      (and landed? (not (commit-sha? (:receipt/merge-sha receipt))))
      (conj :merge-sha-invalid)
      (and landed? (not= permitted-axioms
                         (:receipt/post-merge-axioms receipt)))
      (conj :post-merge-axioms-invalid)
      (and landed? (not= 0 (:receipt/rollup-sorry-warnings receipt)))
      (conj :rollup-carries-sorry)
      (and landed? (not (true? (:receipt/branch-deleted receipt))))
      (conj :source-branch-not-deleted)
      landed?
      (into (status-findings ruling (:receipt/status-recomputed receipt)))
      (and (= :partial-banked ruling)
           (not (nonblank? (:receipt/boundary receipt))))
      (conj :banked-boundary-required)
      (and (= :statement-defective ruling)
           (not (nonblank? (:receipt/defect-witness receipt))))
      (conj :defect-witness-required)
      (and (= :statement-defective ruling)
           (not (sha256? (:receipt/refuted-statement-sha receipt))))
      (conj :refuted-statement-sha-invalid)
      (and (= :blocked ruling) (not (nonblank? (:receipt/seam receipt))))
      (conj :blocked-seam-required))))

(defn validate-receipt
  "Validate content addressing and ruling-specific bank evidence."
  [receipt]
  (let [semantic-findings (findings receipt)
        expected-id (machine/ledger-digest [(dissoc receipt :receipt/id)])]
    (cond
      (seq semantic-findings)
      {:ok false :error/code :frame-bank-invalid
       :findings semantic-findings}

      (not= expected-id (:receipt/id receipt))
      {:ok false :error/code :frame-bank-content-invalid}

      :else
      {:ok true :receipt receipt})))

(defn build-receipt
  "Content-address and validate a bank receipt body.

  The body must omit :receipt/id.  Refusing a supplied id prevents callers
  from accidentally certifying a body under stale content authority."
  [body]
  (if (contains? body :receipt/id)
    {:ok false :error/code :frame-bank-body-contains-id}
    (let [body (cond-> body
                 (contains? ruling->classification (:receipt/ruling body))
                 (assoc :receipt/classification
                        (get ruling->classification (:receipt/ruling body))))
          receipt (assoc body :receipt/id (machine/ledger-digest [body]))]
      (validate-receipt receipt))))
