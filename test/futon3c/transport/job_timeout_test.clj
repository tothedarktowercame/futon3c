(ns futon3c.transport.job-timeout-test
  "Tests for honest job timeout enforcement: cap -> :overrun, ceiling -> :timeout."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.transport.http :as http]
            [futon3c.agency.registry :as reg]
            [futon3c.social.persist :as persist]
            [futon3c.evidence.store :as estore]
            [futon3c.agency.clock-store :as clock-store]
            [futon3c.portfolio.core :as portfolio]
            [futon3c.portfolio.perceive :as perceive]
            [futon3c.transport.encyclopedia :as enc])
  (:import [java.time Instant]))

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
    (reset! @#'http/!job-workers {})
    (f)))

;; =============================================================================
;; Helpers
;; =============================================================================

(def job-cap-ms #'http/job-cap-ms)
(def job-ceiling-ms #'http/job-ceiling-ms)
(def reap-stale-invoke-jobs! #'http/reap-stale-invoke-jobs!)

(defn- setup-running-job!
  "Create a running job in the ledger with a started-at offset from now."
  [job-id agent-id started-ms-ago]
  (let [started-at (.toString (.minusMillis (Instant/now) (long started-ms-ago)))]
    (swap! @#'http/!invoke-jobs-ledger
           (fn [ledger]
             (-> (or ledger {:version 1 :next-seq 0 :job-order [] :jobs {} :trace->job {}})
                 (assoc-in [:jobs job-id]
                           {:job-id job-id
                            :agent-id agent-id
                            :state "running"
                            :started-at started-at
                            :events []
                            :event-seq 0})
                 (update :job-order (fnil conj []) job-id))))))

(defn- setup-overrun-job!
  "Create an overrun job in the ledger with a started-at offset from now."
  [job-id agent-id started-ms-ago]
  (let [started-at (.toString (.minusMillis (Instant/now) (long started-ms-ago)))]
    (swap! @#'http/!invoke-jobs-ledger
           (fn [ledger]
             (-> (or ledger {:version 1 :next-seq 0 :job-order [] :jobs {} :trace->job {}})
                 (assoc-in [:jobs job-id]
                           {:job-id job-id
                            :agent-id agent-id
                            :state "overrun"
                            :started-at started-at
                            :overrun-at (.toString (Instant/now))
                            :events []
                            :event-seq 0})
                 (update :job-order (fnil conj []) job-id))))))

(defn- job-state
  [job-id]
  (let [job (get-in @@#'http/!invoke-jobs-ledger [:jobs job-id])]
    (:state job)))

(defn- job-terminal-code
  [job-id]
  (let [job (get-in @@#'http/!invoke-jobs-ledger [:jobs job-id])]
    (:terminal-code job)))

(defn- job-execution
  [job-id]
  (let [job (get-in @@#'http/!invoke-jobs-ledger [:jobs job-id])]
    (:execution job)))

(defn- register-mock-agent!
  [agent-id-str]
  (reg/register-agent!
   {:agent-id {:id/value agent-id-str :id/type :continuity}
    :type :codex
    :invoke-fn (fn [_ _] {:result "ok" :session-id nil})
    :capabilities [:explore :edit]}))

(defn- agent-status
  [agent-id]
  (or (:agent/status (reg/get-agent agent-id)) :idle))

;; =============================================================================
;; Config tests (e)
;; =============================================================================

(deftest env-overrides-parse-with-fallback
  (testing "(e) env overrides parse + fallback"
    ;; With no env var set, defaults apply
    (is (pos? (job-cap-ms)))
    (is (pos? (job-ceiling-ms)))
    (is (<= (job-cap-ms) (job-ceiling-ms))
        "ceiling must be >= cap")
    ;; ceiling defaults to 2x cap
    (is (= (* 2 (job-cap-ms)) (job-ceiling-ms)))))

;; =============================================================================
;; Cap -> overrun tests (a)
;; =============================================================================

(deftest job-past-cap-becomes-overrun
  (testing "(a) job passing cap -> state 'overrun', events still recorded"
    (setup-running-job! "job-cap-1" "agent-cap-1" (* 40 60 1000)) ;; 40 min ago
    (let [n (reap-stale-invoke-jobs!)]
      (is (pos? n) "at least one job transitioned")
      (is (= "overrun" (job-state "job-cap-1"))
          "running job past cap becomes overrun, not failed"))))

(deftest job-before-cap-stays-running
  (testing "job under cap stays running"
    (setup-running-job! "job-cap-2" "agent-cap-2" (* 10 60 1000)) ;; 10 min ago
    (reap-stale-invoke-jobs!)
    (is (= "running" (job-state "job-cap-2")))))

;; =============================================================================
;; Ceiling -> timeout tests (b)
;; =============================================================================

(deftest overrun-past-ceiling-becomes-timeout
  (testing "(b) job passing ceiling -> state 'timeout', terminal-code set"
    (setup-overrun-job! "job-ceil-1" "agent-ceil-1" (* 80 60 1000)) ;; 80 min ago
    (let [n (reap-stale-invoke-jobs!)]
      (is (pos? n))
      (is (= "timeout" (job-state "job-ceil-1")))
      (is (= "job-ceiling-exceeded" (job-terminal-code "job-ceil-1"))))))

(deftest ceiling-finalize-marks-agent-idle
  (testing "ceiling timeout marks the agent idle"
    (register-mock-agent! "agent-ceil-2")
    (reg/update-agent! "agent-ceil-2" :agent/status :invoking)
    (setup-overrun-job! "job-ceil-2" "agent-ceil-2" (* 80 60 1000))
    (reap-stale-invoke-jobs!)
    (is (= :idle (agent-status "agent-ceil-2"))
        "agent must be :idle after ceiling timeout")))

(deftest ceiling-finalize-computes-execution-evidence
  (testing "ceiling timeout path computes execution evidence from ledger"
    (setup-overrun-job! "job-ceil-3" "agent-ceil-3" (* 80 60 1000))
    ;; Inject a tool_use event into the job
    (swap! @#'http/!invoke-jobs-ledger
           (fn [ledger]
             (update-in ledger [:jobs "job-ceil-3"]
                        #(assoc % :events [{:type "tool_use" :tools ["Bash"] :previews ["Bash ls"]}]
                                  :event-seq 1))))
    (reap-stale-invoke-jobs!)
    (is (= "timeout" (job-state "job-ceil-3")))
    (let [execution (job-execution "job-ceil-3")]
      (is (true? (:executed execution))
          "execution evidence must be computed from ledger events")
      (is (pos? (:tool-events execution))))))

;; =============================================================================
;; Overrun -> done when finishing normally (c)
;; =============================================================================

(deftest overrun-job-can-finish-as-done
  (testing "(c) overrun job finishing before ceiling -> 'done' (no flip through failed)"
    ;; This tests that finalize-invoke-job! can transition an overrun job to done.
    (setup-overrun-job! "job-done-1" "agent-done-1" (* 40 60 1000))
    ;; Simulate finalize-invoke-job! being called with done state
    (#'http/finalize-invoke-job! "job-done-1" "done" nil nil {:ok true :result "finished"} "sid-1")
    (is (= "done" (job-state "job-done-1"))
        "overrun job can finish as done — no false failed state")))

;; =============================================================================
;; Reconcile treats overrun as live (d)
;; =============================================================================

(deftest reconcile-does-not-sweep-overrun-agent
  (testing "(d) reconcile treats an overrun-job agent as live (not swept at 120s)"
    (register-mock-agent! "agent-overrun-1")
    (reg/update-agent! "agent-overrun-1"
                       :agent/status :invoking
                       :agent/invoke-started-at (.minusSeconds (Instant/now) 300))
    ;; Set up an overrun job (non-terminal) for this agent
    (setup-overrun-job! "job-overrun-recon" "agent-overrun-1" (* 40 60 1000))
    ;; Reconcile with 120s threshold — should NOT sweep this agent
    (let [repaired (reg/reconcile-stale-invoking! 120000)]
      (is (not (some #{"agent-overrun-1"} repaired))
          "agent with an overrun (live) job must not be reconciled to idle")
      (is (= :invoking (agent-status "agent-overrun-1"))))))

(deftest reconcile-sweeps-stale-agent-without-job
  (testing "reconcile still sweeps genuinely stale agents (no job at all)"
    (register-mock-agent! "agent-stale-1")
    (reg/update-agent! "agent-stale-1"
                       :agent/status :invoking
                       :agent/invoke-started-at (.minusSeconds (Instant/now) 300))
    (let [repaired (reg/reconcile-stale-invoking! 120000)]
      (is (some #{"agent-stale-1"} repaired))
      (is (= :idle (agent-status "agent-stale-1"))))))
