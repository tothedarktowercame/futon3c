(ns futon3c.agency.invariant-test
  "Invariant tests for the social pipeline — R1-R11 compliance.

   Ported from futon3/agency/invariants/ (A0-A5 → R1-R11).
   The old tests had EXPECTED FAIL markers. These tests MUST PASS by design.

   | Old Invariant | New Requirement | What It Tests |
   |---------------|-----------------|---------------|
   | A0 (delivery) | R1              | Every message produces receipt or explicit failure |
   | A1 (identity) | R2 + R6         | Single routing authority + typed identifiers |
   | A2 (atomicity)| R3              | State transitions are atomic |
   | A3 (loud fail)| R4              | Errors surface at causing layer |
   | A5 (bounded)  | R5              | Transient resources have lifecycle bounds |"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.agency.registry :as reg]
            [futon3c.social.shapes :as shapes]
            [futon3c.social.test-fixtures :as fix]))

;; =============================================================================
;; Fixture: clean registry before each test
;; =============================================================================

(use-fixtures
  :each
  (fn [f]
    (reg/reset-registry!)
    (f)))

;; =============================================================================
;; R1: Delivery Receipt — every operation returns typed result or SocialError
;; (was A0)
;; =============================================================================

(deftest r1-register-returns-typed-result
  (testing "register returns typed agent record, not boolean"
    (let [result (reg/register-agent!
                  {:agent-id (fix/make-agent-id "r1-agent")
                   :type :codex
                   :invoke-fn (fn [_prompt _session-id] {:result "ok"})
                   :capabilities [:edit]})]
      (is (map? result))
      (is (contains? result :agent/id))
      (is (contains? result :agent/registered-at)))))

(deftest r1-unregister-returns-typed-result
  (testing "unregister returns typed result, not boolean"
    (reg/register-agent!
     {:agent-id (fix/make-agent-id "r1-unreg")
      :type :codex
      :invoke-fn (fn [_p _s] {:result "ok"})
      :capabilities [:edit]})
    (let [result (reg/unregister-agent! (fix/make-agent-id "r1-unreg"))]
      (is (map? result))
      (is (contains? result :ok)))))

(deftest r1-invoke-missing-returns-social-error
  (testing "invoking missing agent returns SocialError, not string"
    (let [result (reg/invoke-agent! (fix/make-agent-id "nonexistent") "hi")]
      (is (map? result))
      (is (= false (:ok result)))
      (is (shapes/valid? shapes/SocialError (:error result))))))

;; =============================================================================
;; R2 + R6: Single Routing Authority + Typed Identifiers
;; (was A1)
;; =============================================================================

(deftest r2-single-routing-authority
  (testing "registering same agent-id twice returns error, not overwrite"
    (let [aid (fix/make-agent-id "r2-agent")
          inv-fn (fn [_p _s] {:result "ok"})]
      (reg/register-agent! {:agent-id aid :type :codex :invoke-fn inv-fn :capabilities [:edit]})
      (let [result (reg/register-agent! {:agent-id aid :type :codex :invoke-fn inv-fn :capabilities [:edit]})]
        (is (= false (:ok result)) "duplicate registration should fail")
        (is (shapes/valid? shapes/SocialError (:error result)))))))

(deftest r6-typed-identifiers
  (testing "agent-id must be a typed map (TypedAgentId)"
    (let [aid (fix/make-agent-id "r6-agent" :continuity)
          result (reg/register-agent!
                  {:agent-id aid :type :claude
                   :invoke-fn (fn [_p _s] {:result "ok"})
                   :capabilities [:explore :edit]})]
      (is (shapes/valid? shapes/TypedAgentId (:agent/id result))))))

;; =============================================================================
;; R3: Atomic State Transitions
;; (was A2)
;; =============================================================================

(deftest r3-concurrent-register-unregister
  (testing "concurrent register/unregister does not corrupt registry"
    (let [aids (mapv #(fix/make-agent-id (str "r3-" %)) (range 10))
          inv-fn (fn [_p _s] {:result "ok"})
          ;; Register all in parallel
          futs (mapv (fn [aid]
                       (future
                         (reg/register-agent!
                          {:agent-id aid :type :mock
                           :invoke-fn inv-fn :capabilities []})))
                     aids)]
      (doseq [f futs] @f)
      ;; Unregister half in parallel
      (let [unreg-futs (mapv (fn [aid] (future (reg/unregister-agent! aid)))
                             (take 5 aids))]
        (doseq [f unreg-futs] @f))
      ;; Registry should be consistent: exactly 5 remaining
      (let [status (reg/registry-status)]
        (is (= 5 (:count status)))))))

;; =============================================================================
;; R4: Loud Failure — errors surface at causing layer
;; (was A3)
;; =============================================================================

(deftest r4-invoke-missing-agent-returns-social-error
  (testing "invoke of missing agent produces SocialError with component attribution"
    (let [result (reg/invoke-agent! (fix/make-agent-id "ghost") "hello")]
      (is (= false (:ok result)))
      (let [err (:error result)]
        (is (= :registry (:error/component err)))
        (is (keyword? (:error/code err)))
        (is (string? (:error/message err)))))))

(deftest r4-no-silent-catch-nil
  (testing "invoke-fn exceptions produce SocialError, not nil"
    (reg/register-agent!
     {:agent-id (fix/make-agent-id "r4-boom")
      :type :mock
      :invoke-fn (fn [_p _s] (throw (ex-info "kaboom" {})))
      :capabilities []})
    (let [result (reg/invoke-agent! (fix/make-agent-id "r4-boom") "hello")]
      (is (= false (:ok result)))
      (is (map? (:error result)))
      (is (= :registry (:error/component (:error result)))))))

;; =============================================================================
;; R5: Bounded Lifecycle — transient resources have deterministic bounds
;; (was A5)
;; =============================================================================

(deftest r5-agent-with-ttl-gets-reaped
  (testing "agent registered with TTL is removed after expiry"
    (reg/register-agent!
     {:agent-id (fix/make-agent-id "r5-ttl")
      :type :mock
      :invoke-fn (fn [_p _s] {:result "ok"})
      :capabilities []
      :ttl-ms 100})
    (is (some? (reg/get-agent (fix/make-agent-id "r5-ttl"))))
    (Thread/sleep 200)
    (reg/reap-expired!)
    (is (nil? (reg/get-agent (fix/make-agent-id "r5-ttl"))))))

(deftest r5-invoke-does-not-resurrect-after-unregister
  (testing "invoke does not re-add agent record if unregistered mid-call"
    (reg/register-agent!
     {:agent-id (fix/make-agent-id "r5-race")
      :type :codex
      :invoke-fn (fn [_p _s]
                   (Thread/sleep 200)
                   {:result "done"})
      :capabilities [:edit]})
    (let [f (future (reg/invoke-agent! (fix/make-agent-id "r5-race") "hi" 2000))]
      (Thread/sleep 50)
      (reg/unregister-agent! (fix/make-agent-id "r5-race"))
      (deref f 3000 :timeout)
      (is (nil? (reg/get-agent (fix/make-agent-id "r5-race")))))))
