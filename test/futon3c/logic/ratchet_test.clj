(ns futon3c.logic.ratchet-test
  "Tests for futon3c.logic.ratchet — the I-coverage-ratchet meta-invariant.

   The ratchet's job: catch attempts to reduce an operational-family entry's
   :status without recording a corresponding :family-demoted evidence entry.
   Decreases require evidence; increases (graduations) and additions are
   unrestricted.

   Mission: M-invariant-queue-unstuck (futon3c/holes/missions/)."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [xtdb.api :as xtdb]
            [futon3c.evidence.xtdb-backend :as xb]
            [futon3c.logic.ratchet :as ratchet]))

(def ^:dynamic *xtdb-backend* nil)

(use-fixtures
  :each
  (fn [f]
    (let [node (xtdb/start-node {})]
      (try
        (binding [*xtdb-backend* (xb/make-xtdb-backend node)]
          (f))
        (finally
          (.close node))))))

;; -----------------------------------------------------------------------------
;; status ordering + decreased?
;; -----------------------------------------------------------------------------

(deftest status-decreased-respects-ordering
  (testing "operational → candidate is a decrease"
    (is (true? (ratchet/status-decreased? :operational :candidate))))
  (testing "candidate → operational is NOT a decrease (graduation)"
    (is (false? (ratchet/status-decreased? :candidate :operational))))
  (testing "operational → operational-but-bypassable is a decrease"
    (is (true? (ratchet/status-decreased? :operational :operational-but-bypassable))))
  (testing "operational-but-bypassable → operational is a graduation"
    (is (false? (ratchet/status-decreased? :operational-but-bypassable :operational))))
  (testing "operational → violated is a decrease"
    (is (true? (ratchet/status-decreased? :operational :violated))))
  (testing "same status is NOT a decrease"
    (is (false? (ratchet/status-decreased? :operational :operational))))
  (testing "missing values are NOT decreases (caller must handle nil)"
    (is (false? (ratchet/status-decreased? nil :candidate)))
    (is (false? (ratchet/status-decreased? :operational nil)))))

;; -----------------------------------------------------------------------------
;; diff-family-statuses
;; -----------------------------------------------------------------------------

(deftest diff-detects-demotions-promotions-additions-removals
  (let [old {:graph-symmetry :operational
             :phase-ordering :operational
             :pattern-language :candidate
             :stale-family :operational-but-bypassable}
        new {:graph-symmetry :operational           ;; unchanged
             :phase-ordering :operational-but-bypassable ;; demoted
             :pattern-language :operational         ;; promoted
             ;; :stale-family removed
             :new-family :candidate}                ;; added
        diff (ratchet/diff-family-statuses old new)]
    (testing "demotions detected"
      (is (= [{:family-id :phase-ordering
               :from :operational
               :to :operational-but-bypassable}]
             (:demotions diff))))
    (testing "promotions detected"
      (is (= [{:family-id :pattern-language
               :from :candidate
               :to :operational}]
             (:promotions diff))))
    (testing "additions detected"
      (is (= [{:family-id :new-family :status :candidate}]
             (:added diff))))
    (testing "removals detected"
      (is (= [{:family-id :stale-family
               :status :operational-but-bypassable}]
             (:removed diff))))
    (testing "unchanged listed"
      (is (= [:graph-symmetry] (:unchanged diff))))))

;; -----------------------------------------------------------------------------
;; emit-demotion-event! + matches-demotion? + unreconciled-demotions
;; -----------------------------------------------------------------------------

(deftest emit-then-match-then-reconcile
  (testing "after emitting a demotion event, the matching demotion reconciles"
    (let [emit-result (ratchet/emit-demotion-event!
                       *xtdb-backend*
                       {:family-id :test-family
                        :from :operational
                        :to :candidate
                        :reason "test scenario"
                        :commit "test-commit-sha"
                        :authored-by "ratchet-test"})]
      (is (:ok emit-result) (str "expected emit success, got " (pr-str emit-result)))
      (let [demotion {:family-id :test-family
                      :from :operational
                      :to :candidate}
            unreconciled (ratchet/unreconciled-demotions *xtdb-backend* [demotion])]
        (is (empty? unreconciled)
            "matching evidence entry reconciles the demotion")))))

(deftest unreconciled-when-no-evidence
  (testing "demotions without evidence stay unreconciled"
    (let [demotion {:family-id :no-evidence-family
                    :from :operational
                    :to :candidate}
          unreconciled (ratchet/unreconciled-demotions *xtdb-backend* [demotion])]
      (is (= [demotion] unreconciled)
          "demotion without matching evidence is unreconciled"))))

(deftest unreconciled-when-evidence-mismatches
  (testing "evidence with different from/to does not reconcile"
    (let [_ (ratchet/emit-demotion-event!
             *xtdb-backend*
             {:family-id :mismatch-family
              :from :operational
              :to :violated      ;; different :to than the demotion below
              :reason "different to"})
          demotion {:family-id :mismatch-family
                    :from :operational
                    :to :candidate}
          unreconciled (ratchet/unreconciled-demotions *xtdb-backend* [demotion])]
      (is (= [demotion] unreconciled)
          "evidence with different to-status does not match"))))

;; -----------------------------------------------------------------------------
;; validate-against-baseline — end-to-end
;; -----------------------------------------------------------------------------

(deftest validate-passes-when-no-changes
  (testing "identical baseline + current → ok"
    (let [statuses {:fa :operational :fb :candidate}
          result (ratchet/validate-against-baseline *xtdb-backend* statuses statuses)]
      (is (true? (:ok? result)))
      (is (empty? (:demotions result)))
      (is (empty? (:unreconciled result))))))

(deftest validate-passes-on-graduations-and-additions
  (testing "promotions and additions don't trigger ratchet"
    (let [old {:fa :candidate :fb :operational}
          new {:fa :operational :fb :operational :fc :candidate}
          result (ratchet/validate-against-baseline *xtdb-backend* old new)]
      (is (true? (:ok? result)))
      (is (empty? (:demotions result)))
      (is (= 1 (count (:promotions result))))
      (is (= 1 (count (:added result)))))))

(deftest validate-fails-on-unreconciled-demotion
  (testing "demotion without evidence → :ok? false"
    (let [old {:fa :operational}
          new {:fa :candidate}
          result (ratchet/validate-against-baseline *xtdb-backend* old new)]
      (is (false? (:ok? result)))
      (is (= 1 (count (:demotions result))))
      (is (= 1 (count (:unreconciled result))))
      (is (= :fa (-> result :unreconciled first :family-id))))))

(deftest validate-passes-when-demotion-has-evidence
  (testing "demotion with matching evidence → :ok? true"
    (let [_ (ratchet/emit-demotion-event!
             *xtdb-backend*
             {:family-id :fa
              :from :operational
              :to :candidate
              :reason "test"})
          old {:fa :operational}
          new {:fa :candidate}
          result (ratchet/validate-against-baseline *xtdb-backend* old new)]
      (is (true? (:ok? result)) (str "expected ok, got " (pr-str result)))
      (is (empty? (:unreconciled result))))))

;; -----------------------------------------------------------------------------
;; check-on-load! — emits :family-fired evidence for the load-time check
;; -----------------------------------------------------------------------------

(deftest check-on-load-emits-family-fired-evidence
  (testing "check-on-load! emits a :family-fired entry for :coverage-ratchet"
    (let [result (ratchet/check-on-load!
                  *xtdb-backend*
                  {:print? false})]
      (is (contains? result :outcome)
          "result carries an :outcome (:ok / :violation / :inactive)")
      (is (#{:ok :violation :inactive} (:outcome result))
          (str "expected one of #{:ok :violation :inactive}, got "
               (pr-str (:outcome result))))
      (is (:emit-receipt result) "emit-receipt populated by default")
      (is (:ok (:emit-receipt result))
          (str "expected emit success, got " (pr-str (:emit-receipt result)))))))

(deftest check-on-load-respects-emit-flag
  (testing "emit?=false skips evidence emission"
    (let [result (ratchet/check-on-load!
                  *xtdb-backend*
                  {:print? false :emit? false})]
      (is (nil? (:emit-receipt result))
          "emit-receipt absent when emit? is false"))))

;; -----------------------------------------------------------------------------
;; I-coverage-ratchet canonical statement is grep-verifiable
;; -----------------------------------------------------------------------------

(deftest canonical-statement-is-grep-verifiable
  (is (string? ratchet/I-coverage-ratchet))
  (is (re-find #"I-coverage-ratchet" ratchet/I-coverage-ratchet)))
