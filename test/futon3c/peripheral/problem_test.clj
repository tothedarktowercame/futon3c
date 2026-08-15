(ns futon3c.peripheral.problem-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.dispatch-with-recall :as dispatch-with-recall]
            [futon3c.peripheral.problem :as problem]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]
            [mmca.apm-demonstration-preregistration :as prereg]))

(def dispatch-packet
  (apply str (repeat 220 "p")))

(defn dispatch-state [phase]
  (let [calls (atom [])
        dispatch-fn (fn [opts packet]
                      (swap! calls conj {:opts opts :packet packet})
                      {:job-id "job-1"
                       :assembled-packet packet
                       :evidence {:body {:memory-channel (:memory-channel opts)}}})
        p (problem/make-problem (tools/make-mock-backend) dispatch-fn)
        result (runner/start p {:session-id "s" :problem-id "p"
                                :cycle/mode :store-mode})]
    [p (assoc (:state result) :current-phase phase) calls]))

(deftest solver-dispatch-uses-push-plus-pull-and-returns-dispatcher-receipt
  (let [[p state calls] (dispatch-state :guided-solve)
        result (runner/step p state
                            {:tool :dispatch-solver
                             :args [{:problem "p" :to "codex-4"}
                                    dispatch-packet]})
        receipt (get-in result [:result :memory-offers 0])]
    (is (:ok result))
    (is (= :push+pull (get-in @calls [0 :opts :memory-channel])))
    (is (= {:body {:memory-channel :push+pull}} receipt))))

(deftest student-dispatch-is-pull-only-and-pushes-no-memories
  (let [recall-result {:status :ok
                       :memories [{:memory/id "must-not-reach-student"
                                   :memory/body {:content "a hint"}}]}
        calls (atom [])
        dispatch-fn
        (fn [opts packet]
          (swap! calls conj opts)
          {:assembled-packet
           (dispatch-with-recall/assemble-packet
            packet recall-result (:memory-channel opts))
           :evidence
           (dispatch-with-recall/offered-evidence
            opts (assoc recall-result :memories []) "job-1" "session-1")})
        p (problem/make-problem (tools/make-mock-backend) dispatch-fn)
        start (runner/start p {:session-id "s" :problem-id "p"
                               :cycle/mode :store-mode})
        result (runner/step
                p (assoc (:state start) :current-phase :student-attempts)
                {:tool :dispatch-student-fresh
                 :args [{:problem "p" :to "zai-1"} dispatch-packet]})]
    (is (:ok result))
    (is (= :pull-only (:memory-channel (first @calls))))
    (is (not (.contains ^String (get-in result [:result :assembled-packet])
                        "must-not-reach-student")))
    (is (empty? (get-in result
                        [:result :memory-offers 0 :body :memory-use
                         :memory-use/surfaced-memory-ids])))))

(deftest role-channel-cannot-be-overridden-by-the-caller
  ;; The role fixes the channel. A caller-supplied :push+pull reaching the
  ;; student would be a containment breach of the same family as I.38's
  ;; environment pass-note, so the precedence is asserted, not just written.
  (let [calls (atom [])
        dispatch-fn (fn [opts _] (swap! calls conj (:memory-channel opts))
                      {:evidence {}})
        backend (problem/make-ground-control-backend
                 (tools/make-mock-backend) dispatch-fn)]
    (tools/execute-tool backend :dispatch-student-fresh
                        [{:memory-channel :push+pull} "packet"])
    (tools/execute-tool backend :dispatch-solver
                        [{:memory-channel :none} "packet"])
    (is (= [:pull-only :push+pull] @calls))))

(deftest failed-bell-is-a-tool-failure-not-an-escaping-exception
  ;; ToolBackend promises {:ok true :result} | {:ok false :error}. run-dispatch!
  ;; throws when Agency returns no job-id, so an unguarded call breaks that
  ;; contract and the cycle cannot record its own failure.
  (let [boom (fn [_ _] (throw (ex-info "Agency bell returned no job-id" {})))
        backend (problem/make-ground-control-backend
                 (tools/make-mock-backend) boom)
        result (tools/execute-tool backend :dispatch-solver
                                   [{:problem "p"} "packet"])]
    (is (false? (:ok result)))
    (is (re-find #"no job-id" (:error result)))))

(deftest failed-recall-still-records-an-empty-offer
  (let [backend (problem/make-ground-control-backend
                 (tools/make-mock-backend)
                 dispatch-with-recall/run-dispatch!)
        dispatch-result (atom nil)
        result
        (with-out-str
          (with-redefs [dispatch-with-recall/safe-recall
                        (fn [_ _]
                          {:status :recall-failed
                           :reason :probe-failure
                           :error "store unavailable"
                           :memories []})]
            (reset! dispatch-result
                    (tools/execute-tool
                     backend :dispatch-solver
                     [{:problem "p" :to "codex-4" :dry-run? true}
                      dispatch-packet]))))]
    (is (string? result))
    (is (:ok @dispatch-result))
    (is (= :recall-failed
           (get-in @dispatch-result
                   [:result :memory-offers 0 :body :recall-status])))
    (is (empty? (get-in @dispatch-result
                        [:result :memory-offers 0 :body :memory-use
                         :memory-use/surfaced-memory-ids])))))

(defn started [mode]
  (let [p (problem/make-problem (tools/make-mock-backend))
        result (runner/start p {:session-id "s" :problem-id "p"
                                :cycle/mode mode})]
    [p (:state result)]))

(deftest student-phase-has-no-substrate-write
  (let [[p state] (started :store-mode)
        result (runner/step p (assoc state :current-phase :student-attempts)
                            {:tool :write-substrate :args []})]
    (is (= :phase-tool-not-allowed (:error/code result)))))

(deftest intervention-tools-are-mutually-exclusive
  (let [[store-p store-state] (started :store-mode)
        [harness-p harness-state] (started :harness-mode)]
    (is (= :phase-tool-not-allowed
           (:error/code (runner/step store-p
                                     (assoc store-state :current-phase :intervene)
                                     {:tool :tune-harness :args []}))))
    (is (= :phase-tool-not-allowed
           (:error/code (runner/step harness-p
                                     (assoc harness-state :current-phase :intervene)
                                     {:tool :write-substrate :args []}))))))

(deftest later-phase-refuses-missing-earlier-output
  (let [[p state] (started :store-mode)
        state (assoc state :current-phase :frame
                           :cycle/outputs {:frame :f :containment-probe :c})
        result (runner/step p state
                            {:tool :advance-problem-phase
                             :args ["M" "C" {:frame :f
                                               :containment-probe :c}]})]
    (is (= :missing-required-outputs (:error/code result)))
    (is (some #{:registration} (get-in result [:error/context :missing])))))

(deftest register-refuses-missing-resource-pin
  (let [[p state] (started :store-mode)
        state (assoc state :current-phase :register :cycle/outputs {})
        result (runner/step
                p state
                {:tool :advance-problem-phase
                 :args ["M" "C" {:registration :r :store-snapshot :s
                                    :stratum-frozen-at 1
                                    :environment-revision "env"}]})]
    (is (= :missing-required-outputs (:error/code result)))
    (is (some #{:harness-revision}
              (get-in result [:error/context :missing])))))

(def outputs-through-student
  {:registration :r :store-snapshot :round-open :stratum-frozen-at 1
   :environment-revision "env-a" :harness-revision "harness-a"
   :frame :f :containment-probe :c
   :solver-attempt {:cycle/environment-revision "env-a"
                    :cycle/store-snapshot :solver-store}
   :ground-control-events [] :memory-offers [] :intervention :i})

(deftest environment-mismatch-fails-at-first-complete-advance
  (let [[p state] (started :store-mode)
        state (assoc state :current-phase :student-attempts
                           :cycle/outputs outputs-through-student)
        result (runner/step
                p state
                {:tool :advance-problem-phase
                 :args ["M" "C"
                        {:student-attempts
                         [{:cycle/environment-revision "env-b"}]
                         :memory-uses []}]})]
    (is (= :environment-mismatch-between-arms (:error/code result)))
    (is (= :environment-arms-match
           (get-in result [:error/context :invariant])))))

(deftest malformed-operand-is-a-failure-not-a-crash-and-not-a-pass
  ;; Invariant operands are supplied by TOOLS, so a malformed one must be
  ;; rejected as a structured error. Crashing loses the cycle; passing would let
  ;; a bad tool defeat the gate by emitting garbage instead of a mismatch.
  (let [[p state] (started :store-mode)
        state (assoc state :current-phase :student-attempts
                           :cycle/outputs outputs-through-student)
        result (runner/step
                p state
                {:tool :advance-problem-phase
                 :args ["M" "C" {:student-attempts :not-a-sequence
                                 :memory-uses []}]})]
    (is (= :invariant-check-threw (:error/code result)))
    (is (= :environment-arms-match
           (get-in result [:error/context :invariant])))
    (is (not (:ok result)))))

(deftest differing-store-snapshots-are-explicitly-accepted
  (let [[p state] (started :store-mode)
        state (assoc state :current-phase :student-attempts
                           :cycle/outputs outputs-through-student)
        result (runner/step
                p state
                {:tool :advance-problem-phase
                 :args ["M" "C"
                        {:student-attempts
                         [{:cycle/environment-revision "env-a"
                           :cycle/store-snapshot :student-store}]
                         :memory-uses []}]})]
    ;; Store snapshots are intentionally not invariant operands: the store is
    ;; the transfer channel and is permitted to differ between arms.
    (is (:ok result))))

(def synthetic-trace
  {:problem {:problem-id "p" :difficulty-stratum "s" :regime "r"
             :locked-lemma-exposure []}
   :frame {:scaffold-hash "a" :closing-hash "b"}
   :launch-gate-refused-without-witness? true :cycle-closed? true
   :disposition-ids ["d"] :memory-offers []
   :memory-disposition-offer-ids [] :stratum-frozen-at 1 :assigned-at 2
   :cycle/attempts [{:cycle/regime "r"
                     :cycle/store-revision "1111111111111111111111111111111111111111"
                     :cycle/harness-revision "2222222222222222222222222222222222222222"
                     :cycle/runner-freshness true}]
   :cycle/mode :store-mode :cycle/deposit-state :n/a :cycle/paired-with nil
   :cycle/store-snapshot-id "snap/1" :cycle/store-snapshot-memory-ids []
   :cycle/window {:opened-at "2026-08-15T00:00:00Z"
                  :closed-at "2026-08-15T01:00:00Z"}
   :denominator-declared? true :denominator-inferred-from-corpus? false
   :available-artifact-ids [] :need-probe-retrieved-ids []
   :containment-claimed? true :containment-probe-recorded? true
   :containment-probe-passed? true :capability-probes []
   :required-measurement-fields ["x"]
   :measurement {:meas/values {"x" 1} :meas/unset {}}
   :promoted-artifact-ids [] :importable-promoted-artifact-ids []
   :need-tagged-promoted-artifact-ids []})

(deftest fruit-is-validator-trace-shape
  (let [[p state] (started :store-mode)
        stop (runner/stop p (assoc state :steps [{:tool :emit-trace
                                                   :result synthetic-trace}])
                          "synthetic")]
    (is (empty? (prereg/trace-shape-failures (:fruit stop))))))
