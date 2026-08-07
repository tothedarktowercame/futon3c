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

(defn- register-invoke-agent!
  [agent-id-str type invoke-fn]
  (reg/register-agent!
   {:agent-id {:id/value agent-id-str :id/type :continuity}
    :type type
    :invoke-fn invoke-fn
    :capabilities [:explore :edit]}))

(defn- create-job!
  [job-id agent-id caller]
  (#'http/create-invoke-job! {:requested-job-id job-id
                              :agent-id agent-id
                              :prompt "test turn"
                              :caller caller
                              :surface "bell"}))

(defn- run-job!
  [job-id agent-id timeout-ms]
  (#'http/run-invoke-job! {:job-id job-id
                           :agent-id agent-id
                           :prompt "test turn"
                           :caller "caller-1"
                           :surface "bell"
                           :timeout-ms timeout-ms}))

(defn- job-events
  [job-id event-type]
  (->> (get-in @@#'http/!invoke-jobs-ledger [:jobs job-id :events])
       (filter #(= event-type (:type %)))))

(defn- agent-status
  [agent-id]
  (or (:agent/status (reg/get-agent agent-id)) :idle))

;; =============================================================================
;; Config tests (e)
;; =============================================================================

(deftest env-overrides-parse-with-fallback
  (testing "(e) settings parse + fallback"
    ;; The soft cap still has a positive default — it drives the observational
    ;; 'overrun' transition.
    (is (pos? (job-cap-ms)))
    ;; The hard ceiling does NOT. Unset means no wall-clock termination:
    ;; wall clock is an SLA signal, not evidence of stuckness.
    (is (nil? (job-ceiling-ms))
        "no ceiling is configured by default — turns end by cancel, not clock")))

(deftest ceiling-is-opt-in-via-setting
  (testing "an operator can restore a hard ceiling, and 0 means 'none'"
    (try
      (System/setProperty "FUTON3C_JOB_CEILING_MS" "90000")
      (is (= 90000 (job-ceiling-ms)))
      (System/setProperty "FUTON3C_JOB_CEILING_MS" "0")
      (is (nil? (job-ceiling-ms))
          "0 reads as 'no ceiling', not as 'terminate immediately'")
      (finally
        (System/clearProperty "FUTON3C_JOB_CEILING_MS")))
    (is (nil? (job-ceiling-ms)))))

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
  (testing "(b) with a ceiling configured, an overrun job past it -> 'timeout'"
    (setup-overrun-job! "job-ceil-1" "agent-ceil-1" (* 80 60 1000)) ;; 80 min ago
    (with-redefs [http/job-ceiling-ms (constantly (* 70 60 1000))]
      (let [n (reap-stale-invoke-jobs!)]
        (is (pos? n))
        (is (= "timeout" (job-state "job-ceil-1")))
        (is (= "job-ceiling-exceeded" (job-terminal-code "job-ceil-1")))))))

(deftest overrun-past-old-ceiling-survives-by-default
  (testing "with no ceiling configured, a long overrun job is NOT reaped"
    ;; The regression this whole change exists to prevent: a turn that was
    ;; still working got force-terminated with state=failed and an empty
    ;; result, losing work already written to disk (README-agency-cap.md).
    (setup-overrun-job! "job-ceil-live" "agent-ceil-live" (* 600 60 1000)) ;; 10h
    (reap-stale-invoke-jobs!)
    (is (= "overrun" (job-state "job-ceil-live"))
        "an overrun job stays live and pollable; only cancel ends it")
    (is (nil? (job-terminal-code "job-ceil-live")))))

(deftest ceiling-finalize-marks-agent-idle
  (testing "ceiling timeout marks the agent idle"
    (register-mock-agent! "agent-ceil-2")
    (reg/update-agent! "agent-ceil-2" :agent/status :invoking)
    (setup-overrun-job! "job-ceil-2" "agent-ceil-2" (* 80 60 1000))
    (with-redefs [http/job-ceiling-ms (constantly (* 70 60 1000))]
      (reap-stale-invoke-jobs!))
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
    (with-redefs [http/job-ceiling-ms (constantly (* 70 60 1000))]
      (reap-stale-invoke-jobs!))
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

(deftest on-time-supervised-turn-is-unchanged
  (testing "an on-time async turn finalizes normally and returns the agent idle"
    (register-invoke-agent! "agent-fast" :codex
                            (fn [_ _] {:result "on time" :session-id "s-fast"}))
    (create-job! "job-fast" "agent-fast" "caller-1")
    ;; Budgets are generous on purpose. The assertion is about semantics — an
    ;; on-time turn records no overrun — and at a 100ms budget it was really
    ;; measuring scheduler latency, so it failed whenever test ordering put a
    ;; heavier namespace first.
    (with-redefs [http/job-ceiling-ms (constantly 10000)]
      (let [result (run-job! "job-fast" "agent-fast" 5000)]
        (is (= "done" (:terminal-state result)))
        (is (= "done" (job-state "job-fast")))
        (is (empty? (job-events "job-fast" "overrun")))
        (is (= :idle (agent-status "agent-fast")))))))

(deftest overrun-late-result-finalizes-done-and-bells-once
  (testing "the same future may finish after timeout and still takes the normal path"
    (register-invoke-agent! "agent-late" :codex
                            (fn [_ _]
                              (Thread/sleep 70)
                              {:result "late but valid" :session-id "s-late"}))
    (register-invoke-agent! "caller-1" :claude
                            (fn [_ _] {:result "ack" :session-id nil}))
    (create-job! "job-late" "agent-late" "caller-1")
    (let [bellbacks (atom [])]
      (with-redefs [http/job-ceiling-ms (constantly 250)
                    http/*enqueue-auto-bellback!* #(swap! bellbacks conj %)]
        (run-job! "job-late" "agent-late" 15))
      (is (= "done" (job-state "job-late")))
      (is (= 1 (count (job-events "job-late" "overrun"))))
      (is (= 1 (count @bellbacks)))
      (is (= :idle (agent-status "agent-late"))))))

(deftest overrun-ceiling-finalizes-timeout-and-bells-once
  (testing "the ceiling wins once, interrupts the turn, and releases the agent"
    (register-invoke-agent! "agent-ceiling" :codex
                            (fn [_ _]
                              (Thread/sleep 500)
                              {:result "too late" :session-id nil}))
    (register-invoke-agent! "caller-1" :claude
                            (fn [_ _] {:result "ack" :session-id nil}))
    (create-job! "job-ceiling" "agent-ceiling" "caller-1")
    (let [bellbacks (atom [])]
      (with-redefs [http/job-ceiling-ms (constantly 60)
                    http/*enqueue-auto-bellback!* #(swap! bellbacks conj %)]
        (run-job! "job-ceiling" "agent-ceiling" 15))
      (is (= "timeout" (job-state "job-ceiling")))
      (is (= "job-ceiling-exceeded" (job-terminal-code "job-ceiling")))
      (is (= 1 (count (job-events "job-ceiling" "timeout"))))
      (is (= 1 (count @bellbacks)))
      (is (= :idle (agent-status "agent-ceiling"))))))

(deftest reaper-and-supervisor-race-finalizes-once
  (testing "the reaper and supervisor share the same atomic completion guard"
    (register-invoke-agent! "agent-race" :codex
                            (fn [_ _] {:result "unused" :session-id nil}))
    (register-invoke-agent! "caller-1" :claude
                            (fn [_ _] {:result "ack" :session-id nil}))
    (setup-overrun-job! "job-finalize-race" "agent-race" 1000)
    (swap! @#'http/!invoke-jobs-ledger assoc-in
           [:jobs "job-finalize-race" :caller] "caller-1")
    (let [start (promise)
          bellbacks (atom [])]
      (with-redefs [http/job-ceiling-ms (constantly 1)
                    http/*enqueue-auto-bellback!* #(swap! bellbacks conj %)]
        (let [supervisor (future @start
                                 (#'http/finalize-job-at-ceiling!
                                  "job-finalize-race" 1))
              reaper (future @start (reap-stale-invoke-jobs! 1))]
          (deliver start true)
          @supervisor
          @reaper))
      (is (= "timeout" (job-state "job-finalize-race")))
      (is (= 1 (count (job-events "job-finalize-race" "timeout"))))
      (is (= 1 (count @bellbacks))))))

;; =============================================================================
;; No ceiling: the worker stays authoritative
;; =============================================================================

(deftest overrun-turn-without-ceiling-still-finalizes-done
  (testing "past the soft cap with no ceiling, the turn keeps running and wins"
    (register-invoke-agent! "agent-slow" :codex
                            (fn [_ _]
                              (Thread/sleep 200)
                              {:result "slow but real" :session-id "s-slow"}))
    (register-invoke-agent! "caller-1" :claude
                            (fn [_ _] {:result "ack" :session-id nil}))
    (create-job! "job-slow" "agent-slow" "caller-1")
    (let [bellbacks (atom [])]
      ;; job-ceiling-ms is nil by default — no with-redefs, that IS the policy.
      (with-redefs [http/*enqueue-auto-bellback!* #(swap! bellbacks conj %)]
        (let [result (run-job! "job-slow" "agent-slow" 15)]
          (is (= "done" (:terminal-state result))
              "the result is harvested, not discarded at the cap")))
      (is (= "done" (job-state "job-slow")))
      (is (= 1 (count (job-events "job-slow" "overrun")))
          "the cap still records an observational overrun checkpoint")
      (is (= 1 (count @bellbacks)))
      (is (= :idle (agent-status "agent-slow"))))))

;; =============================================================================
;; Explicit cancellation is the replacement for the clock
;; =============================================================================

(defn- cancel-job!
  [job-id body]
  (#'http/handle-cancel-invoke-job
   job-id {:request-method :post
           :uri (str "/api/alpha/invoke/jobs/" job-id "/cancel")
           :body body}))

(deftest cancel-finalizes-a-running-job
  (testing "an operator can end a live job explicitly"
    (register-mock-agent! "agent-cancel-1")
    (reg/update-agent! "agent-cancel-1" :agent/status :invoking)
    (setup-running-job! "job-cancel-1" "agent-cancel-1" 1000)
    (let [response (cancel-job! "job-cancel-1" "{\"caller\":\"joe\",\"reason\":\"wedged\"}")]
      (is (= 200 (:status response)))
      (is (= "cancelled" (job-state "job-cancel-1")))
      (is (= "operator-cancelled" (job-terminal-code "job-cancel-1")))
      (is (= :idle (agent-status "agent-cancel-1"))))))

(deftest cancel-records-caller-and-reason
  (testing "the terminal message names who cancelled and why"
    (setup-running-job! "job-cancel-2" "agent-cancel-2" 1000)
    (cancel-job! "job-cancel-2" "{\"caller\":\"joe\",\"reason\":\"superseded\"}")
    (let [msg (:terminal-message (get-in @@#'http/!invoke-jobs-ledger [:jobs "job-cancel-2"]))]
      (is (re-find #"joe" msg))
      (is (re-find #"superseded" msg)))))

(deftest cancel-tolerates-empty-body
  (testing "cancel works without a JSON body"
    (setup-running-job! "job-cancel-3" "agent-cancel-3" 1000)
    (is (= 200 (:status (cancel-job! "job-cancel-3" nil))))
    (is (= "cancelled" (job-state "job-cancel-3")))))

(deftest cancel-is-404-and-409-on-bad-targets
  (testing "unknown job -> 404; already-terminal job -> 409"
    (is (= 404 (:status (cancel-job! "job-does-not-exist" nil))))
    (setup-running-job! "job-cancel-4" "agent-cancel-4" 1000)
    (#'http/finalize-invoke-job! "job-cancel-4" "done" nil nil {:ok true :result "r"} nil)
    (let [response (cancel-job! "job-cancel-4" nil)]
      (is (= 409 (:status response)))
      (is (= "done" (job-state "job-cancel-4"))
          "cancelling a finished job must not rewrite its outcome"))))

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
