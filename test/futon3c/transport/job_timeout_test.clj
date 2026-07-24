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

(deftest on-time-supervised-turn-is-unchanged
  (testing "an on-time async turn finalizes normally and returns the agent idle"
    (register-invoke-agent! "agent-fast" :codex
                            (fn [_ _] {:result "on time" :session-id "s-fast"}))
    (create-job! "job-fast" "agent-fast" "caller-1")
    (with-redefs [http/job-ceiling-ms (constantly 200)]
      (let [result (run-job! "job-fast" "agent-fast" 100)]
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
