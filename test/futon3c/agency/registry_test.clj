(ns futon3c.agency.registry-test
  "Registry unit tests — ported from futon3 + new R1-R11 compliance tests.

   Original tests from futon3: timeout enforcement, no-resurrect-after-unregister.
   New tests: single-routing-authority (R2), concurrent register/unregister (R3),
   invoke-missing → SocialError (R4), bounded-lifecycle/TTL (R5),
   typed-identifiers (R6)."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.agency.registry :as reg]
            [futon3c.social.shapes :as shapes]
            [futon3c.social.test-fixtures :as fix]))

(use-fixtures
  :each
  (fn [f]
    (reg/reset-registry!)
    (f)))

;; =============================================================================
;; Ported from futon3: timeout enforcement
;; =============================================================================

(deftest invoke-timeout-is-enforced
  (testing "registry-level timeout returns promptly and reports timeout"
    (reg/register-agent!
     {:agent-id (fix/make-agent-id "t-timeout")
      :type :codex
      :invoke-fn (fn [_prompt _session-id]
                   (Thread/sleep 200)
                   {:result "late" :session-id nil :exit-code 0})
      :capabilities [:edit]})
    (let [resp (reg/invoke-agent! (fix/make-agent-id "t-timeout") "hi" 50)]
      (is (false? (:ok resp)))
      (is (= :invoke-error (:error/code (:error resp)))))))

;; =============================================================================
;; Ported from futon3: no resurrect after unregister
;; =============================================================================

(deftest invoke-does-not-resurrect-after-unregister
  (testing "invoke does not re-add agent record if it is unregistered mid-call"
    (reg/register-agent!
     {:agent-id (fix/make-agent-id "t-race")
      :type :codex
      :invoke-fn (fn [_prompt _session-id]
                   (Thread/sleep 200)
                   {:result "done" :session-id nil :exit-code 0})
      :capabilities [:edit]})
    (let [f (future (reg/invoke-agent! (fix/make-agent-id "t-race") "hi" 2000))]
      (Thread/sleep 50)
      (is (true? (:ok (reg/unregister-agent! (fix/make-agent-id "t-race")))))
      (deref f 3000 :timeout)
      (is (false? (reg/agent-registered? (fix/make-agent-id "t-race")))))))

;; =============================================================================
;; R2: Single routing authority
;; =============================================================================

(deftest single-routing-authority
  (testing "register same agent-id twice → error, not overwrite"
    (let [aid (fix/make-agent-id "dual")
          inv-fn (fn [_p _s] {:result "ok"})]
      (let [r1 (reg/register-agent! {:agent-id aid :type :codex :invoke-fn inv-fn :capabilities [:edit]})]
        (is (contains? r1 :agent/id) "first registration succeeds"))
      (let [r2 (reg/register-agent! {:agent-id aid :type :codex :invoke-fn inv-fn :capabilities [:edit]})]
        (is (= false (:ok r2)) "second registration fails")
        (is (= :duplicate-registration (:error/code (:error r2))))))))

;; =============================================================================
;; R3: Atomic state transitions
;; =============================================================================

(deftest concurrent-register-unregister
  (testing "concurrent register/unregister race doesn't corrupt registry"
    (let [aids (mapv #(fix/make-agent-id (str "race-" %) :continuity) (range 20))
          inv-fn (fn [_p _s] {:result "ok"})
          reg-futs (mapv (fn [aid]
                           (future
                             (reg/register-agent!
                              {:agent-id aid :type :mock :invoke-fn inv-fn :capabilities []})))
                         aids)]
      (doseq [f reg-futs] @f)
      (is (= 20 (:count (reg/registry-status))))
      ;; Unregister odd-indexed in parallel
      (let [unreg-futs (mapv (fn [i]
                               (future
                                 (reg/unregister-agent! (nth aids i))))
                             (filter odd? (range 20)))]
        (doseq [f unreg-futs] @f))
      (is (= 10 (:count (reg/registry-status)))))))

;; =============================================================================
;; R4: Loud failure
;; =============================================================================

(deftest invoke-missing-agent-returns-social-error
  (testing "invoke of missing agent returns SocialError map"
    (let [result (reg/invoke-agent! (fix/make-agent-id "ghost") "hello")]
      (is (= false (:ok result)))
      (is (= :registry (:error/component (:error result))))
      (is (= :agent-not-found (:error/code (:error result))))
      (is (shapes/valid? shapes/SocialError (:error result))))))

(deftest invoke-exception-returns-social-error
  (testing "invoke-fn that throws returns SocialError, not nil"
    (reg/register-agent!
     {:agent-id (fix/make-agent-id "boom")
      :type :mock
      :invoke-fn (fn [_p _s] (throw (ex-info "kaboom" {:reason :test})))
      :capabilities []})
    (let [result (reg/invoke-agent! (fix/make-agent-id "boom") "hello")]
      (is (= false (:ok result)))
      (is (= :invoke-exception (:error/code (:error result))))
      (is (shapes/valid? shapes/SocialError (:error result))))))

(deftest unregister-missing-returns-social-error
  (testing "unregister of missing agent returns SocialError"
    (let [result (reg/unregister-agent! (fix/make-agent-id "nobody"))]
      (is (= false (:ok result)))
      (is (= :agent-not-found (:error/code (:error result))))
      (is (shapes/valid? shapes/SocialError (:error result))))))

;; =============================================================================
;; R5: Bounded lifecycle
;; =============================================================================

(deftest agent-with-ttl-gets-reaped
  (testing "agent registered with TTL is removed after expiry"
    (reg/register-agent!
     {:agent-id (fix/make-agent-id "ephemeral")
      :type :mock
      :invoke-fn (fn [_p _s] {:result "ok"})
      :capabilities []
      :ttl-ms 100})
    (is (some? (reg/get-agent (fix/make-agent-id "ephemeral"))))
    (Thread/sleep 200)
    (let [reaped (reg/reap-expired!)]
      (is (= 1 (count reaped)))
      (is (nil? (reg/get-agent (fix/make-agent-id "ephemeral")))))))

(deftest agent-without-ttl-not-reaped
  (testing "agent without TTL is not affected by reap"
    (reg/register-agent!
     {:agent-id (fix/make-agent-id "permanent")
      :type :claude
      :invoke-fn (fn [_p _s] {:result "ok"})
      :capabilities [:explore]})
    (Thread/sleep 50)
    (let [reaped (reg/reap-expired!)]
      (is (empty? reaped))
      (is (some? (reg/get-agent (fix/make-agent-id "permanent")))))))

;; =============================================================================
;; R6: Typed identifiers
;; =============================================================================

(deftest agent-id-is-typed-map
  (testing "registered agent has TypedAgentId"
    (let [aid (fix/make-agent-id "typed-agent" :continuity)
          result (reg/register-agent!
                  {:agent-id aid :type :claude
                   :invoke-fn (fn [_p _s] {:result "ok"})
                   :capabilities [:explore :edit]})]
      (is (shapes/valid? shapes/TypedAgentId (:agent/id result)))
      (is (= :continuity (:id/type (:agent/id result)))))))

;; =============================================================================
;; Introspection
;; =============================================================================

(deftest registry-status-reports-count
  (testing "registry-status reflects actual agent count"
    (is (= 0 (:count (reg/registry-status))))
    (reg/register-agent!
     {:agent-id (fix/make-agent-id "stat-1")
      :type :mock
      :invoke-fn (fn [_p _s] {:result "ok"})
      :capabilities []})
    (is (= 1 (:count (reg/registry-status))))))

(deftest shutdown-all-clears-registry
  (testing "shutdown-all! removes all agents"
    (doseq [i (range 5)]
      (reg/register-agent!
       {:agent-id (fix/make-agent-id (str "shutdown-" i))
        :type :mock
        :invoke-fn (fn [_p _s] {:result "ok"})
        :capabilities []}))
    (is (= 5 (:count (reg/registry-status))))
    (let [n (reg/shutdown-all!)]
      (is (= 5 n))
      (is (= 0 (:count (reg/registry-status)))))))
