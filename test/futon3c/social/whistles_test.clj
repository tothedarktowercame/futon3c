(ns futon3c.social.whistles-test
  "Tests for whistle dispatcher — synchronous request-response coordination."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.social.whistles :as whistles]
            [futon3c.agency.registry :as registry]
            [futon3c.evidence.store :as estore]))

;; =============================================================================
;; Fixtures
;; =============================================================================

(use-fixtures
  :each
  (fn [f]
    (registry/reset-registry!)
    (estore/reset-store!)
    (f)))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- register-mock-agent!
  "Register a mock agent that returns a canned response."
  [id response]
  (registry/register-agent!
   {:agent-id {:id/value id :id/type :continuity}
    :type :mock
    :invoke-fn (fn [prompt _session-id]
                 {:result response :session-id (str "s-" id)})
    :capabilities [:chat]}))

(defn- register-slow-agent!
  "Register a mock agent that sleeps before responding."
  [id delay-ms]
  (registry/register-agent!
   {:agent-id {:id/value id :id/type :continuity}
    :type :mock
    :invoke-fn (fn [_prompt _session-id]
                 (Thread/sleep delay-ms)
                 {:result "slow response" :session-id nil})
    :capabilities [:chat]}))

;; =============================================================================
;; Whistle roundtrip
;; =============================================================================

(deftest whistle-roundtrip-success
  (testing "whistle! sends prompt to agent and returns response"
    (register-mock-agent! "codex-1" "The answer is 42")
    (let [result (whistles/whistle!
                  {:agent-id "codex-1"
                   :prompt "What is the meaning of life?"
                   :author "joe"})]
      (is (true? (:whistle/ok result)))
      (is (= "The answer is 42" (:whistle/response result)))
      (is (= "codex-1" (:whistle/agent-id result)))
      (is (= "s-codex-1" (:whistle/session-id result)))
      (is (string? (:whistle/at result))))))

(deftest whistle-unregistered-agent-error
  (testing "whistle! to unregistered agent returns error"
    (let [result (whistles/whistle!
                  {:agent-id "ghost-agent"
                   :prompt "hello"
                   :author "joe"})]
      (is (false? (:whistle/ok result)))
      (is (re-find #"not registered" (:whistle/error result)))
      (is (= "ghost-agent" (:whistle/agent-id result))))))

;; =============================================================================
;; Evidence emission
;; =============================================================================

(deftest whistle-emits-evidence
  (testing "whistle! emits coordination evidence with :whistle tag"
    (register-mock-agent! "codex-1" "pong")
    (whistles/whistle!
     {:agent-id "codex-1"
      :prompt "ping"
      :author "claude-1"
      :evidence-store estore/!store})
    (let [entries (estore/query {:query/type :coordination})
          whistle-ev (first (filter #(some #{:whistle} (:evidence/tags %)) entries))]
      (is (some? whistle-ev))
      (is (= :step (:evidence/claim-type whistle-ev)))
      (is (= "claude-1" (:evidence/author whistle-ev)))
      (is (= "codex-1" (get-in whistle-ev [:evidence/body :agent-id])))
      (is (= "ping" (get-in whistle-ev [:evidence/body :prompt])))
      (is (= "pong" (get-in whistle-ev [:evidence/body :response])))
      (is (= :completed (get-in whistle-ev [:evidence/body :status]))))))

(deftest whistle-evidence-records-error
  (testing "whistle evidence records error status for failed invocation"
    (whistles/whistle!
     {:agent-id "ghost"
      :prompt "hello"
      :author "joe"
      :evidence-store estore/!store})
    (let [entries (estore/query {:query/type :coordination})
          whistle-ev (first (filter #(some #{:whistle} (:evidence/tags %)) entries))]
      (is (some? whistle-ev))
      (is (= :error (get-in whistle-ev [:evidence/body :status]))))))

(deftest whistle-without-evidence-store
  (testing "whistle! works without evidence-store (no evidence emitted)"
    (register-mock-agent! "codex-1" "works fine")
    (let [result (whistles/whistle!
                  {:agent-id "codex-1"
                   :prompt "test"
                   :author "joe"})]
      (is (true? (:whistle/ok result)))
      (is (= "works fine" (:whistle/response result)))
      ;; No evidence emitted to global store
      (is (empty? (estore/query {:query/type :coordination}))))))

;; =============================================================================
;; Timeout handling
;; =============================================================================

(deftest whistle-timeout-handling
  (testing "whistle! returns error on timeout with slow agent"
    (register-slow-agent! "slow-agent" 2000)
    (let [result (whistles/whistle!
                  {:agent-id "slow-agent"
                   :prompt "hurry up"
                   :author "joe"
                   :timeout-ms 100
                   :evidence-store estore/!store})]
      (is (false? (:whistle/ok result)))
      (is (re-find #"timeout" (:whistle/error result)))
      ;; Evidence records timeout status
      (let [entries (estore/query {:query/type :coordination})
            whistle-ev (first (filter #(some #{:whistle} (:evidence/tags %)) entries))]
        (is (some? whistle-ev))
        (is (= :timeout (get-in whistle-ev [:evidence/body :status])))))))

;; =============================================================================
;; Validation guards
;; =============================================================================

(deftest whistle-missing-agent-id-errors
  (testing "whistle! with nil agent-id returns error without touching registry"
    (let [result (whistles/whistle! {:prompt "hello" :author "joe"})]
      (is (false? (:whistle/ok result)))
      (is (re-find #"agent-id" (:whistle/error result))))))

(deftest whistle-missing-prompt-errors
  (testing "whistle! with nil prompt returns error"
    (let [result (whistles/whistle! {:agent-id "codex-1" :author "joe"})]
      (is (false? (:whistle/ok result)))
      (is (re-find #"prompt" (:whistle/error result))))))
