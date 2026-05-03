(ns futon3c.evidence.boundary-test
  "Tests for futon3c.evidence.boundary/append!.

   The boundary's job is:
     (a) Coerce string-shaped fields to keyword-shaped fields before
         shape validation (the dominant cause of silent EvidenceEntry
         rejections in the existing 30-call-site landscape).
     (b) Surface shape failures, append failures, and not-readable-back
         failures as structured receipts (no silent loss).
     (c) Verify durable persistence via verify-persisted before declaring
         success (I-evidence-per-turn binding).

   Tests cover the three kinds of receipts (success / shape-fail /
   coerce-fail) against an XTDB-backed store. Mission:
   M-invariant-queue-unstuck (futon3c/holes/missions/)."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [xtdb.api :as xtdb]
            [futon3c.evidence.boundary :as boundary]
            [futon3c.evidence.store :as store]
            [futon3c.evidence.xtdb-backend :as xb]))

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
;; Round-trip: well-shaped input persists and is readable back.
;; -----------------------------------------------------------------------------

(deftest well-shaped-entry-round-trips
  (testing "fully namespaced + correctly-typed entry succeeds"
    (let [result (boundary/append!
                  *xtdb-backend*
                  {:subject {:ref/type :agent :ref/id "claude-test"}
                   :type :coordination
                   :claim-type :step
                   :author "claude-test"
                   :body {:event "boundary-test"}
                   :tags [:boundary :test]})]
      (is (:ok result) (str "expected success, got " (pr-str result)))
      (is (string? (:evidence/id result)))
      (is (= "claude-test" (get-in result [:entry :evidence/author])))
      (is (= [:boundary :test] (get-in result [:entry :evidence/tags])))
      (let [readback (store/get-entry* *xtdb-backend* (:evidence/id result))]
        (is (some? readback) "entry must be readable back through the same backend")
        (is (= (:evidence/id result) (:evidence/id readback)))))))

;; -----------------------------------------------------------------------------
;; Coercion: string-shaped fields are coerced to keywords before validation.
;; This is the failure-mode the bot-evidence bug exhibited.
;; -----------------------------------------------------------------------------

(deftest string-tags-are-coerced-to-keywords
  (testing "string tags coerced + entry persists"
    (let [result (boundary/append!
                  *xtdb-backend*
                  {:subject {:ref/type :agent :ref/id "claude-test"}
                   :type :coordination
                   :claim-type :step
                   :author "claude-test"
                   :body {:event "string-tag-coercion"}
                   :tags ["coordination" "test" :already-keyword]})]
      (is (:ok result) (str "expected success, got " (pr-str result)))
      (is (= [:coordination :test :already-keyword]
             (get-in result [:entry :evidence/tags]))
          "string tags coerced to keywords; existing keyword passes through"))))

(deftest string-subject-ref-type-is-coerced
  (testing "string :ref/type coerced to keyword"
    (let [result (boundary/append!
                  *xtdb-backend*
                  {:subject {:ref/type "agent" :ref/id "claude-test"}
                   :type :coordination
                   :claim-type :step
                   :author "claude-test"
                   :body {}
                   :tags [:test]})]
      (is (:ok result) (str "expected success, got " (pr-str result)))
      (is (= :agent (get-in result [:entry :evidence/subject :ref/type]))
          ":ref/type string coerced to keyword"))))

(deftest string-evidence-type-is-coerced
  (testing "string :type coerced to enum keyword"
    (let [result (boundary/append!
                  *xtdb-backend*
                  {:subject {:ref/type :agent :ref/id "claude-test"}
                   :type "coordination"
                   :claim-type "step"
                   :author "claude-test"
                   :body {}
                   :tags [:test]})]
      (is (:ok result) (str "expected success, got " (pr-str result)))
      (is (= :coordination (get-in result [:entry :evidence/type])))
      (is (= :step (get-in result [:entry :evidence/claim-type]))))))

(deftest fully-namespaced-string-tags-coerced
  (testing "namespaced :evidence/tags accepts strings and coerces them"
    (let [result (boundary/append!
                  *xtdb-backend*
                  {:evidence/subject {:ref/type :agent :ref/id "claude-test"}
                   :evidence/type :coordination
                   :evidence/claim-type :step
                   :evidence/author "claude-test"
                   :evidence/body {}
                   :evidence/tags ["a" "b" :c]})]
      (is (:ok result))
      (is (= [:a :b :c] (get-in result [:entry :evidence/tags]))))))

;; -----------------------------------------------------------------------------
;; Loud failure: bad shapes return structured violations.
;; -----------------------------------------------------------------------------

(deftest unrecoverable-tag-throws-structured-violation
  (testing "non-string non-keyword tag triggers exception → structured violation"
    (let [result (boundary/append!
                  *xtdb-backend*
                  {:subject {:ref/type :agent :ref/id "claude-test"}
                   :type :coordination
                   :claim-type :step
                   :author "claude-test"
                   :body {}
                   :tags [42 :test]})]
      (is (false? (:ok result)))
      (is (= :exception (:error/code result)))
      (is (= :exception (get-in result [:invariant/violation :kind])))
      (let [readback (store/query* *xtdb-backend* {:query/limit 100})]
        (is (zero? (count readback))
            "no entry persisted when coercion threw")))))

(deftest blank-string-tag-fails-loudly
  (testing "blank string tag throws coercion exception"
    (let [result (boundary/append!
                  *xtdb-backend*
                  {:subject {:ref/type :agent :ref/id "claude-test"}
                   :type :coordination
                   :claim-type :step
                   :author "claude-test"
                   :body {}
                   :tags ["" :test]})]
      (is (false? (:ok result)))
      (is (= :exception (:error/code result))))))

(deftest invalid-evidence-type-fails-shape
  (testing "unknown evidence-type keyword fails shape validation, not coercion"
    (let [result (boundary/append!
                  *xtdb-backend*
                  {:subject {:ref/type :agent :ref/id "claude-test"}
                   :type :nonsense-type
                   :claim-type :step
                   :author "claude-test"
                   :body {}
                   :tags [:test]})]
      (is (false? (:ok result)))
      (is (= :invalid-entry (:error/code result)))
      (is (= :shape (get-in result [:invariant/violation :kind]))))))

;; -----------------------------------------------------------------------------
;; Receipt-shape contract: success returns evidence/id; failure does not
;; have a successful entry but does have invariant/violation.
;; -----------------------------------------------------------------------------

(deftest success-receipt-has-id-failure-receipt-has-violation
  (testing "success vs failure receipt shapes are distinguishable by :ok"
    (let [success (boundary/append!
                   *xtdb-backend*
                   {:subject {:ref/type :agent :ref/id "claude-test"}
                    :type :coordination
                    :claim-type :step
                    :author "claude-test"
                    :body {}
                    :tags [:test]})
          failure (boundary/append!
                   *xtdb-backend*
                   {:subject {:ref/type :agent :ref/id "claude-test"}
                    :type :nonsense-type
                    :claim-type :step
                    :author "claude-test"
                    :body {}
                    :tags [:test]})]
      (is (:ok success))
      (is (string? (:evidence/id success)))
      (is (false? (:ok failure)))
      (is (some? (:invariant/violation failure)))
      (is (nil? (:evidence/id failure))))))

;; -----------------------------------------------------------------------------
;; I-single-boundary canonical statement is grep-verifiable.
;; -----------------------------------------------------------------------------

(deftest i-single-boundary-canonical-statement-is-a-non-empty-string
  (is (string? boundary/I-single-boundary))
  (is (re-find #"I-single-boundary" boundary/I-single-boundary)
      "canonical statement contains its own name (grep-verifiable)"))
