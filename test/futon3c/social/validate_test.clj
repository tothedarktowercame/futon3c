(ns futon3c.social.validate-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.evidence.store :as store]
            [futon3c.social.shapes :as shapes]
            [futon3c.social.test-fixtures :as fix]
            [futon3c.social.validate :as v]))

(use-fixtures
  :each
  (fn [f]
    (store/reset-store!)
    (f)))

(deftest validate-outcome-happy-path
  (testing "validate-outcome returns CoordinationOutcome valid? true when evidence contains a conclusion"
    (let [patterns (fix/mock-patterns)
          msg-id "msg-1"
          subject (fix/make-artifact-ref :evidence msg-id)
          _ (store/append! {:evidence-id "g" :subject subject :type :coordination :claim-type :goal :author "a" :body {} :tags [:t]})
          _ (store/append! {:evidence-id "c" :subject subject :type :coordination :claim-type :conclusion :author "a" :body {} :tags [:t]
                            :in-reply-to "g"})
          receipt (fix/make-dispatch-receipt {:receipt/msg-id msg-id
                                              :receipt/to (fix/make-agent-id "claude-1" :continuity)})
          result (v/validate-outcome receipt patterns store/!store)]
      (fix/assert-valid! shapes/CoordinationOutcome result)
      (is (true? (:outcome/valid? result)))
      (is (= :coordination-complete (:outcome/type result))))))

(deftest validate-outcome-missing-evidence
  (testing "validate-outcome returns SocialError :missing-evidence when no evidence exists"
    (let [patterns (fix/mock-patterns)
          receipt (fix/make-dispatch-receipt {:receipt/msg-id "msg-none"
                                              :receipt/to (fix/make-agent-id "claude-1" :continuity)})
          result (v/validate-outcome receipt patterns store/!store)]
      (fix/assert-valid! shapes/SocialError result)
      (is (= :E-validate (:error/component result)))
      (is (= :missing-evidence (:error/code result))))))

(deftest validate-outcome-incomplete-evidence
  (testing "validate-outcome returns CoordinationOutcome valid? false when evidence exists but has no conclusion"
    (let [patterns (fix/mock-patterns)
          msg-id "msg-2"
          subject (fix/make-artifact-ref :evidence msg-id)
          _ (store/append! {:evidence-id "g" :subject subject :type :coordination :claim-type :goal :author "a" :body {} :tags [:t]})
          receipt (fix/make-dispatch-receipt {:receipt/msg-id msg-id
                                              :receipt/to (fix/make-agent-id "claude-1" :continuity)})
          result (v/validate-outcome receipt patterns store/!store)]
      (fix/assert-valid! shapes/CoordinationOutcome result)
      (is (false? (:outcome/valid? result)))
      (is (= :coordination-failed (:outcome/type result))))))

(deftest validate-outcome-invalid-patterns
  (testing "invalid patterns input returns SocialError"
    (let [msg-id "msg-3"
          receipt (fix/make-dispatch-receipt {:receipt/msg-id msg-id
                                              :receipt/to (fix/make-agent-id "claude-1" :continuity)})
          result (v/validate-outcome receipt {:patterns/ids "nope"} store/!store)]
      (fix/assert-valid! shapes/SocialError result)
      (is (= :E-validate (:error/component result)))
      (is (= :invalid-patterns (:error/code result))))))

(deftest validate-outcome-output-shape
  (testing "validate-outcome returns CoordinationOutcome or SocialError"
    (let [patterns (fix/mock-patterns)
          receipt (fix/make-dispatch-receipt {:receipt/msg-id "msg-4"
                                              :receipt/to (fix/make-agent-id "claude-1" :continuity)})
          r1 (v/validate-outcome receipt patterns store/!store)
          _ (store/append! {:evidence-id "g" :subject (fix/make-artifact-ref :evidence "msg-4")
                            :type :coordination :claim-type :goal :author "a" :body {} :tags [:t]})
          r2 (v/validate-outcome receipt patterns store/!store)]
      (is (or (shapes/valid? shapes/SocialError r1)
              (shapes/valid? shapes/CoordinationOutcome r1)))
      (is (shapes/valid? shapes/CoordinationOutcome r2)))))

