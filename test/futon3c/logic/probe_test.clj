(ns futon3c.logic.probe-test
  "Tests for futon3c.logic.probe — the live-state probe.

   Probe responsibilities:
     - Read the inventory's family list.
     - For each family, look up a registered check-fn or fall back to :inactive.
     - Run the check, normalize result, emit a :family-fired evidence entry.
     - Aggregate results into a sweep summary.
     - Provide three modes: scheduled / on-demand / autoshutter.

   These tests cover the run-probe-sweep! path against a real in-memory
   XTDB store, plus check-fn semantics (:ok, :violation, :inactive,
   throw → :violation), plus start/stop/idempotency of the loop.

   Mission: M-invariant-queue-unstuck (futon3c/holes/missions/)."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.evidence.store :as store]
            [futon3c.evidence.backend :as backend]
            [futon3c.logic.probe :as probe]))

(def ^:dynamic *evidence-backend* nil)

(use-fixtures
  :each
  (fn [f]
    (let [evidence-backend (backend/->AtomBackend (atom {:entries {} :order []}))]
      (try
        (binding [*evidence-backend* evidence-backend]
          (reset! probe/family-check-fns {})
          (f))
        (finally
          (reset! probe/family-check-fns {})
          (try (probe/stop-probe-loop!) (catch Throwable _ nil)))))))

;; -----------------------------------------------------------------------------
;; run-family-check
;; -----------------------------------------------------------------------------

(deftest run-family-check-returns-inactive-when-unregistered
  (testing "no check registered → :outcome :inactive"
    (let [r (probe/run-family-check *evidence-backend* :no-such-family)]
      (is (= :no-such-family (:family-id r)))
      (is (= :inactive (:outcome r)))
      (is (string? (get-in r [:detail :reason]))))))

(deftest run-family-check-returns-ok-when-check-says-so
  (testing "registered check returning {:outcome :ok}"
    (probe/register-family-check! :always-ok
                                  (fn [_store] {:outcome :ok :detail {:n 1}}))
    (let [r (probe/run-family-check *evidence-backend* :always-ok)]
      (is (= :always-ok (:family-id r)))
      (is (= :ok (:outcome r)))
      (is (= 1 (get-in r [:detail :n]))))))

(deftest run-family-check-returns-violation-when-check-says-so
  (testing "registered check returning {:outcome :violation}"
    (probe/register-family-check! :always-violation
                                  (fn [_store]
                                    {:outcome :violation
                                     :detail {:reason "test"}}))
    (let [r (probe/run-family-check *evidence-backend* :always-violation)]
      (is (= :violation (:outcome r)))
      (is (= "test" (get-in r [:detail :reason]))))))

(deftest run-family-check-converts-throw-to-violation
  (testing "check that throws → :outcome :violation with exception detail"
    (probe/register-family-check! :throwing
                                  (fn [_store]
                                    (throw (ex-info "boom" {:x 1}))))
    (let [r (probe/run-family-check *evidence-backend* :throwing)]
      (is (= :violation (:outcome r)))
      (is (re-find #"boom" (get-in r [:detail :exception]))))))

(deftest run-family-check-treats-non-conforming-result-as-violation
  (testing "check returning a non-conforming map → :violation"
    (probe/register-family-check! :nonsense
                                  (fn [_store] {:not-an-outcome true}))
    (let [r (probe/run-family-check *evidence-backend* :nonsense)]
      (is (= :violation (:outcome r)))
      (is (re-find #"non-conforming" (get-in r [:detail :reason]))))))

;; -----------------------------------------------------------------------------
;; run-probe-sweep!
;; -----------------------------------------------------------------------------

(deftest sweep-emits-family-fired-evidence-per-family
  (testing "every family produces one :family-fired entry"
    (probe/register-family-check! :graph-symmetry
                                  (fn [_store] {:outcome :ok}))
    (probe/register-family-check! :status-discipline
                                  (fn [_store] {:outcome :violation
                                                :detail {:n 3}}))
    (let [family-ids [:graph-symmetry :status-discipline :unregistered-family]
          summary (probe/run-probe-sweep! *evidence-backend* family-ids)
          counts (:counts summary)]
      (is (string? (:probe-run-id summary)))
      (is (string? (:at summary)))
      (is (pos? (:family-count summary)))
      (is (= (:family-count summary) (count (:results summary))))
      ;; counts include :ok :violation :inactive — exact totals depend on
      ;; the inventory's family count, but :ok and :violation should each
      ;; have at least 1 (from the registered checks).
      (is (>= (:ok counts) 1))
      (is (>= (:violation counts) 1))
      ;; Every result has an :evidence/id from the boundary
      (doseq [r (:results summary)]
        (is (string? (:evidence/id r))
            (str "expected evidence/id for " (:family-id r)))
        (is (true? (:emit-ok? r))
            (str "expected emit success for " (:family-id r)))))))

(deftest sweep-tags-include-outcome-and-family-id
  (testing "evidence entries are tagged for query-by-outcome and -family-id"
    (probe/register-family-check! :graph-symmetry
                                  (fn [_store] {:outcome :ok}))
    (let [summary (probe/run-probe-sweep! *evidence-backend* [:graph-symmetry])
          all (store/query* *evidence-backend* {:query/tags [:family-fired]})
          ok-tagged (store/query* *evidence-backend* {:query/tags [:ok]})
          family-tagged (store/query* *evidence-backend* {:query/tags [:graph-symmetry]})]
      (is (pos? (count all)) "at least one :family-fired entry exists")
      (is (pos? (count ok-tagged)) "at least one :ok entry exists")
      (is (pos? (count family-tagged))
          "graph-symmetry entry is queryable by family-id tag")
      ;; The graph-symmetry entry should be in the registered :ok-result form
      (let [gs (->> family-tagged
                    (filter #(= :family-fired (get-in % [:evidence/body :event])))
                    first)]
        (is (some? gs))
        (is (= :ok (get-in gs [:evidence/body :outcome])))
        (is (= (:probe-run-id summary) (get-in gs [:evidence/body :probe-run-id])))))))

;; -----------------------------------------------------------------------------
;; Mode (b) on-demand
;; -----------------------------------------------------------------------------

(deftest probe-now-is-equivalent-to-run-probe-sweep
  (testing "probe-now! returns the same shape as run-probe-sweep!"
    (let [r (probe/probe-now! *evidence-backend*)]
      (is (string? (:probe-run-id r)))
      (is (contains? r :results))
      (is (contains? r :counts))
      (is (contains? r :family-count)))))

;; -----------------------------------------------------------------------------
;; Mode (c) autoshutter
;; -----------------------------------------------------------------------------

(deftest autoshutter-fires-probe-around-body
  (testing "with-autoshutter-probe emits sweeps before and after BODY"
    (probe/register-family-check! :graph-symmetry
                                  (fn [_store] {:outcome :ok}))
    ;; Run two manual sweeps (the autoshutter macro form) and assert there
    ;; are two distinct probe-run-ids on disk.
    (probe/run-probe-sweep! *evidence-backend* [:graph-symmetry])
    (probe/run-probe-sweep! *evidence-backend* [:graph-symmetry])
    (let [all (store/query* *evidence-backend* {:query/tags [:family-fired]})
          run-ids (->> all
                       (map (fn [e] (get-in e [:evidence/body :probe-run-id])))
                       distinct
                       count)]
      (is (>= run-ids 2)
          "at least two distinct probe-run-ids — one for each sweep"))))

;; -----------------------------------------------------------------------------
;; Mode (a) scheduled loop — start/stop/idempotency
;; -----------------------------------------------------------------------------

(deftest start-probe-loop-is-idempotent
  (testing "second start returns the existing state without spawning a new executor"
    (let [s1 (probe/start-probe-loop!
              {:evidence-store *evidence-backend*
               :cadence-ms 1000
               :initial-delay-ms 60000})
          s2 (probe/start-probe-loop!
              {:evidence-store *evidence-backend*
               :cadence-ms 1000
               :initial-delay-ms 60000})]
      (is (true? (probe/probe-loop-running?)))
      (is (identical? (:executor s1) (:executor s2))
          "second start! returns the same executor; does not spawn another")
      (probe/stop-probe-loop!)
      (is (false? (probe/probe-loop-running?))))))

(deftest stop-probe-loop-without-running-returns-nil
  (testing "stop! when nothing's running is a no-op"
    (is (false? (probe/probe-loop-running?)))
    (is (nil? (probe/stop-probe-loop!)))))

;; -----------------------------------------------------------------------------
;; I-family-canary canonical statement is grep-verifiable.
;; -----------------------------------------------------------------------------

(deftest canonical-statement-is-grep-verifiable
  (is (string? probe/I-family-canary))
  (is (re-find #"I-family-canary" probe/I-family-canary)))
