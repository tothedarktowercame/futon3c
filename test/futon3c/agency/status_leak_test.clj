(ns futon3c.agency.status-leak-test
  "Tests for the agent-status leak fix: failed/thrown invoke jobs must reset
   the agent's roster status to :idle, and stale :invoking agents must be
   reconciled on roster reads."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.agency.registry :as reg]
            [futon3c.transport.http :as http]
            [futon3c.social.persist :as persist]
            [futon3c.evidence.store :as estore]
            [futon3c.agency.clock-store :as clock-store]
            [futon3c.portfolio.core :as portfolio]
            [futon3c.portfolio.perceive :as perceive]
            [futon3c.transport.encyclopedia :as enc]))

;; =============================================================================
;; Fixtures
;; =============================================================================

(use-fixtures
  :each
  (fn [f]
    (reg/reset-registry!)
    (clock-store/reset-store!)
    (persist/reset-sessions!)
    (estore/reset-store!)
    (reset! portfolio/!state {:mu perceive/default-mu
                              :prec perceive/default-precision
                              :pending nil
                              :recent []
                              :step-count 0})
    (enc/clear-cache!)
    (http/reset-invoke-jobs!)
    (f)))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- register-mock-agent!
  ([agent-id-str] (register-mock-agent! agent-id-str :codex))
  ([agent-id-str type]
   (reg/register-agent!
    {:agent-id {:id/value agent-id-str :id/type :continuity}
     :type type
     :invoke-fn (fn [_prompt _session-id] {:result "ok" :session-id nil})
     :capabilities [:explore :edit]})))

(defn- agent-status
  "Read the raw :agent/status from the registry atom (defaults to :idle)."
  [agent-id]
  (or (:agent/status (reg/get-agent agent-id)) :idle))

;; =============================================================================
;; Unit tests: mark-agent-idle!
;; =============================================================================

(deftest mark-agent-idle-resets-invoking
  (testing "mark-agent-idle! resets :invoking to :idle"
    (register-mock-agent! "test-idle-1")
    (reg/update-agent! "test-idle-1" :agent/status :invoking)
    (is (= :invoking (agent-status "test-idle-1")))
    (is (true? (reg/mark-agent-idle! "test-idle-1")))
    (is (= :idle (agent-status "test-idle-1")))))

(deftest mark-agent-idle-idempotent
  (testing "mark-agent-idle! is safe when already idle"
    (register-mock-agent! "test-idle-2")
    (is (= :idle (agent-status "test-idle-2")))
    (is (false? (reg/mark-agent-idle! "test-idle-2")))
    (is (= :idle (agent-status "test-idle-2")))))

(deftest mark-agent-idle-nonexistent-noop
  (testing "mark-agent-idle! on unregistered agent is a no-op"
    (is (false? (reg/mark-agent-idle! "ghost-agent")))))

;; =============================================================================
;; Unit tests: reconcile-stale-invoking!
;; =============================================================================

(deftest reconcile-resets-stale-invoking
  (testing "(c) reconcile resets a stale :invoking agent with no job"
    (register-mock-agent! "stale-1")
    ;; Simulate stuck :invoking with an old timestamp
    (reg/update-agent! "stale-1"
                       :agent/status :invoking
                       :agent/invoke-started-at (.minusSeconds (java.time.Instant/now) 300))
    (is (= :invoking (agent-status "stale-1")))
    (let [repaired (reg/reconcile-stale-invoking! 0)]
      (is (= ["stale-1"] repaired))
      (is (= :idle (agent-status "stale-1"))))))

(deftest reconcile-does-not-touch-recent-invoking
  (testing "reconcile does not reset an agent that started invoking recently"
    (register-mock-agent! "recent-1")
    (reg/update-agent! "recent-1"
                       :agent/status :invoking
                       :agent/invoke-started-at (java.time.Instant/now))
    (let [repaired (reg/reconcile-stale-invoking! 120000)]
      (is (empty? repaired))
      (is (= :invoking (agent-status "recent-1"))))))

(deftest reconcile-does-not-touch-idle
  (testing "reconcile ignores idle agents"
    (register-mock-agent! "idle-1")
    (let [repaired (reg/reconcile-stale-invoking! 0)]
      (is (empty? repaired))
      (is (= :idle (agent-status "idle-1"))))))

(deftest reconcile-skips-agent-with-running-job
  (testing "(d) reconcile does NOT touch an agent with a running job"
    (register-mock-agent! "running-1")
    ;; Set up a fake running job in the ledger
    (let [job-id "invoke-test-running-job"]
      (swap! @#'http/!invoke-jobs-ledger
             (fn [ledger]
               (-> (or ledger {:version 1 :next-seq 1 :job-order [] :jobs {} :trace->job {}})
                   (assoc-in [:jobs job-id]
                             {:job-id job-id
                              :agent-id "running-1"
                              :state "running"})
                   (update :job-order (fnil conj []) job-id))))
      ;; Mark agent as invoking with old timestamp
      (reg/update-agent! "running-1"
                         :agent/status :invoking
                         :agent/invoke-started-at (.minusSeconds (java.time.Instant/now) 300))
      (let [repaired (reg/reconcile-stale-invoking! 0)]
        (is (empty? repaired)
            "agent with a running job must not be reconciled")
        (is (= :invoking (agent-status "running-1")))))))

;; =============================================================================
;; Integration tests: run-invoke-job! status guarantee
;; =============================================================================

(defn- setup-queued-job!
  "Create a queued job in the ledger for AGENT-ID."
  [job-id agent-id]
  (swap! @#'http/!invoke-jobs-ledger
         (fn [ledger]
           (-> (or ledger {:version 1 :next-seq 0 :job-order [] :jobs {} :trace->job {}})
               (assoc-in [:jobs job-id]
                         {:job-id job-id
                          :agent-id agent-id
                          :state "queued"})
               (update :job-order (fnil conj []) job-id)))))

(deftest run-invoke-job-failed-resets-agent-status
  (testing "(a) invoke path whose job fails -> agent status is :idle afterward.
             Simulates the leak: redefed invoke-agent! marks :invoking but
             returns an error (skipping the registry's internal mark-idle!).
             The HTTP layer's finally block must reset to :idle."
    (register-mock-agent! "codex-leak-1")
    ;; Manually set :invoking (simulating what the registry wrapper does
        ;; at the start of invoke-agent!). The mock invoke-fn returns {:ok false}.
    (reg/update-agent! "codex-leak-1" :agent/status :invoking)
    (with-redefs [reg/invoke-agent! (fn [_aid _prompt _timeout]
                                      {:ok false
                                       :error {:error/component :registry
                                               :error/code :invoke-error
                                               :error/message "simulated failure"}
                                       :session-id nil})]
      (let [job-id (str "invoke-leak-" (System/currentTimeMillis))
            _ (setup-queued-job! job-id "codex-leak-1")
            result (#'http/run-invoke-job! {:job-id job-id
                                            :agent-id "codex-leak-1"
                                            :prompt "do the thing"
                                            :caller "test"
                                            :surface "bell"
                                            :timeout-ms nil
                                            :mission-id nil
                                            :evidence-store nil})]
        (is (true? (:ok result)) "job itself completes")
        (is (false? (:ok (:result result))) "inner invoke result failed")
        (is (= :idle (agent-status "codex-leak-1"))
            "agent must be :idle after failed invoke — finally resets")))))

(deftest run-invoke-job-throwing-resets-agent-status
  (testing "(b) invoke path that throws -> :idle afterward"
    (register-mock-agent! "codex-leak-2")
    (reg/update-agent! "codex-leak-2" :agent/status :invoking)
    (with-redefs [reg/invoke-agent! (fn [_aid _prompt _timeout]
                                      (throw (RuntimeException. "boom")))]
      (let [job-id (str "invoke-leak-" (System/currentTimeMillis))
            _ (setup-queued-job! job-id "codex-leak-2")
            result (#'http/run-invoke-job! {:job-id job-id
                                            :agent-id "codex-leak-2"
                                            :prompt "do the thing"
                                            :caller "test"
                                            :surface "bell"
                                            :timeout-ms nil
                                            :mission-id nil
                                            :evidence-store nil})]
        (is (false? (:ok result)) "job-level ok is false when invoke throws")
        (is (= :idle (agent-status "codex-leak-2"))
            "agent must be :idle even when invoke throws")))))

(deftest run-invoke-job-successful-resets-agent-status
  (testing "(e) normal successful invoke: idle after"
    (register-mock-agent! "codex-leak-3")
    (reg/update-agent! "codex-leak-3" :agent/status :invoking)
    (with-redefs [reg/invoke-agent! (fn [_aid _prompt _timeout]
                                      {:ok true
                                       :result "all done"
                                       :session-id "s-leak-3"})]
      (let [job-id (str "invoke-leak-" (System/currentTimeMillis))
            _ (setup-queued-job! job-id "codex-leak-3")
            result (#'http/run-invoke-job! {:job-id job-id
                                            :agent-id "codex-leak-3"
                                            :prompt "hello there"
                                            :caller "test"
                                            :surface "bell"
                                            :timeout-ms nil
                                            :mission-id nil
                                            :evidence-store nil})]
        (is (true? (:ok result)) "job-level ok is true on success")
        (is (= :idle (agent-status "codex-leak-3"))
            "agent must be :idle after successful invoke")))))
