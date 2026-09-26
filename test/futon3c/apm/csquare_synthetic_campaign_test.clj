(ns futon3c.apm.csquare-synthetic-campaign-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.campaign-trace :as campaign-trace]
            [futon3c.apm.csquare-synthetic-campaign :as sut]))

(deftest campaign-shape-is-ten-sequential-real-gated-frames
  (is (= 10 sut/problem-count))
  (is (= [:solve :verify :close] sut/phases))
  (let [events (#'sut/append-frame-events
                (:events (#'sut/initial-campaign)) "c01"
                {:trace/combined {"schemaVersion" 1}
                 :trace/digest "not-a-real-receipt"})]
    ;; The production model, not C□, rejects an unbound closure receipt.
    (is (= :frame-close-combined-trace-required
           (:error/code
            (machine/projection events))))))

(deftest adapter-reports-only-real-postconditions
  (with-redefs [sut/process-one! (constantly {:ok true :status :phase-advanced})]
    (is (= :queue-tick-complete
           (:status ((:reconcile-fn (sut/adapter-constructor {})) nil nil)))))
  (with-redefs [sut/process-one! (constantly {:ok true :status :batch-complete})]
    (is (= :frame-complete
           (:status ((:reconcile-fn (sut/adapter-constructor {})) nil nil))))))

(deftest absent-campaign-state-starts-at-frame-one
  (with-redefs [clojure.core/slurp
                (fn [path & _]
                  (throw (AssertionError.
                          (str "absent state must not be read: " path))))
                io/file
                (fn [& _]
                  (proxy [java.io.File] ["/definitely/absent"]
                    (exists [] false)))]
    (is (= 1 (:next-frame (#'sut/read-campaign-state))))))

(deftest ^:slow harness-emits-every-lean-declared-observation-kind
  ;; The rest of this namespace stubs process-one!, so it stayed green for
  ;; eight days while the harness itself could not run: the axiom gate made
  ;; :proof-standard a required observation kind and C-square emitted only
  ;; three, so every run died in two ticks with
  ;; :campaign-trace-observation-absent. A harness whose test cannot notice
  ;; that it is broken is worse than no harness -- it is why changes were
  ;; being verified by restarting the live campaign.
  (let [declared (set (map (comp keyword :kind)
                           (campaign-trace/observation-schemas)))
        terminal (#'sut/write-proof! 0)
        documents (#'sut/durable-observations! 0 terminal "test-predecessor")
        emitted (set (mapcat keys documents))
        ;; the assembly the harness actually performs
        assembled (campaign-trace/assemble-combined-operational-trace documents)]
    (is (zero? (:exit terminal))
        "the synthetic solver's Lean source must really elaborate")
    (is (contains? emitted :trace/proof-standard-observation)
        "the axiom gate's observation kind must be emitted")
    ;; The assembly is where :campaign-trace-observation-absent was thrown.
    ;; Reaching a trace at all IS the assertion; every Lean-declared kind must
    ;; have a producer, whatever that set becomes.
    (is (map? assembled)
        "assembly must not throw :campaign-trace-observation-absent")
    (is (= 1 (get assembled "schemaVersion")))
    (is (= "apm-combined-operational" (get assembled "traceKind")))
    (is (seq (get assembled "proofStandardObservations"))
        (str "the axiom gate's collection must be populated; declared kinds="
             declared))))
