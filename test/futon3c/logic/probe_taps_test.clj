(ns futon3c.logic.probe-taps-test
  "Tests for the tap factories that bridge core.logic invariant layers
   into the live-state probe.

   Tests use minimal in-memory state for each layer — empty-state should
   yield :outcome :ok; state with violations should yield :outcome :violation.

   Mission: M-invariant-queue-unstuck (futon3c/holes/missions/),
   INSTANTIATE-4."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [xtdb.api :as xtdb]
            [futon3c.evidence.xtdb-backend :as xb]
            [futon3c.logic.probe :as probe]
            [futon3c.logic.probe-taps :as taps]))

(def ^:dynamic *xtdb-backend* nil)

(use-fixtures
  :each
  (fn [f]
    (let [node (xtdb/start-node {})]
      (try
        (binding [*xtdb-backend* (xb/make-xtdb-backend node)]
          (reset! probe/family-check-fns {})
          (f))
        (finally
          (reset! probe/family-check-fns {})
          (.close node))))))

;; -----------------------------------------------------------------------------
;; agency tap
;; -----------------------------------------------------------------------------

(deftest agency-tap-empty-registry-is-ok
  (testing "tap against an empty registry → :outcome :ok"
    (let [registry-atom (atom {})
          check-fn (taps/make-agency-tap registry-atom)
          result (check-fn *xtdb-backend*)]
      (is (= :ok (:outcome result))
          (str "expected :ok, got " (pr-str result)))
      (is (zero? (get-in result [:detail :total-violations]))))))

(deftest agency-tap-with-registry-having-violations
  (testing "tap against a registry with an untyped agent → :outcome :violation"
    ;; Untyped agent (string key, no :agent-id with typed value) should
    ;; trigger the agency layer's :untyped-agents check.
    (let [registry-atom (atom
                         {"untyped-agent"
                          {:type :claude :status :idle}})
          check-fn (taps/make-agency-tap registry-atom)
          result (check-fn *xtdb-backend*)]
      (is (= :violation (:outcome result)) (str "got " (pr-str result)))
      (is (pos? (get-in result [:detail :total-violations]))))))

(deftest agency-tap-focuses-on-category-when-supplied
  (testing "category-scoped tap reports only that category"
    (let [registry-atom (atom {})
          check-fn (taps/make-agency-tap registry-atom :route-inconsistencies)
          result (check-fn *xtdb-backend*)]
      (is (= :ok (:outcome result)))
      (is (= #{:route-inconsistencies}
             (get-in result [:detail :checked-categories]))))))

(deftest agency-tap-converts-throw-to-violation
  (testing "throwing state source → :outcome :violation with exception detail"
    (let [bad-source (fn [] (throw (ex-info "bad state" {})))
          check-fn (taps/make-agency-tap bad-source)
          result (check-fn *xtdb-backend*)]
      (is (= :violation (:outcome result)))
      (is (re-find #"bad state" (get-in result [:detail :exception]))))))

;; -----------------------------------------------------------------------------
;; tickle tap
;; -----------------------------------------------------------------------------

(deftest tickle-tap-empty-state-is-ok
  (testing "tap against empty tickle state → :outcome :ok"
    ;; tickle-logic/build-db expects a map with at least keys for empty
    ;; collections of pages, escalations, etc.
    (let [state-source (atom {:pages [] :escalations []
                              :assignments [] :scans []
                              :evidence []})
          check-fn (taps/make-tickle-tap state-source)
          result (check-fn *xtdb-backend*)]
      (is (= :ok (:outcome result)) (str "got " (pr-str result)))
      (is (zero? (get-in result [:detail :total-violations]))))))

(deftest tickle-tap-converts-throw-to-violation
  (testing "tickle tap with throwing state → :outcome :violation"
    (let [bad-source (fn [] (throw (ex-info "tickle boom" {})))
          check-fn (taps/make-tickle-tap bad-source)
          result (check-fn *xtdb-backend*)]
      (is (= :violation (:outcome result)))
      (is (re-find #"tickle boom" (get-in result [:detail :exception]))))))

;; -----------------------------------------------------------------------------
;; portfolio tap
;; -----------------------------------------------------------------------------

(deftest portfolio-tap-converts-throw-to-violation
  (testing "portfolio tap with throwing state → :outcome :violation"
    (let [bad-source (fn [] (throw (ex-info "portfolio boom" {})))
          check-fn (taps/make-portfolio-tap bad-source)
          result (check-fn *xtdb-backend*)]
      (is (= :violation (:outcome result)))
      (is (re-find #"portfolio boom" (get-in result [:detail :exception]))))))

;; -----------------------------------------------------------------------------
;; integration with the probe
;; -----------------------------------------------------------------------------

(deftest taps-integrate-with-probe-sweep
  (testing "registered taps fire as part of probe sweep"
    (let [agency-state (atom {})
          tickle-state (atom {:pages [] :escalations []
                              :assignments [] :scans []
                              :evidence []})]
      (probe/register-family-check! :agency-invariants
                                    (taps/make-agency-tap agency-state))
      (probe/register-family-check! :tickle-invariants
                                    (taps/make-tickle-tap tickle-state))
      (let [summary (probe/run-probe-sweep!
                     *xtdb-backend*
                     [:agency-invariants :tickle-invariants])
            results-by-id (->> (:results summary)
                               (map (juxt :family-id identity))
                               (into {}))]
        (is (= 2 (:family-count summary)))
        (is (= :ok (get-in results-by-id [:agency-invariants :outcome])))
        (is (= :ok (get-in results-by-id [:tickle-invariants :outcome])))))))

;; -----------------------------------------------------------------------------
;; register-default-taps!
;; -----------------------------------------------------------------------------

(deftest register-default-taps-registers-only-supplied-sources
  (testing "register-default-taps! ignores absent sources"
    (let [agency-state (atom {})
          result (taps/register-default-taps! {:agency agency-state})]
      (is (= #{:agency-invariants} (:registered result)))
      (is (contains? @probe/family-check-fns :agency-invariants))
      (is (not (contains? @probe/family-check-fns :tickle-invariants))))))

;; -----------------------------------------------------------------------------
;; deferred-stub tap
;; -----------------------------------------------------------------------------

(deftest deferred-tap-returns-inactive-with-detail
  (testing "make-deferred-tap returns :inactive carrying its meta"
    (let [check-fn (taps/make-deferred-tap
                    {:source "test-source" :follow-on "M-test"})
          result (check-fn *xtdb-backend*)]
      (is (= :inactive (:outcome result)))
      (is (true? (get-in result [:detail :deferred?])))
      (is (= "test-source" (get-in result [:detail :source])))
      (is (= "M-test" (get-in result [:detail :follow-on]))))))

(deftest register-deferred-taps-registers-substrate-2-and-war-machine
  (testing "register-deferred-taps! covers substrate-2 + war-machine families"
    (let [result (taps/register-deferred-taps!)
          registered @probe/family-check-fns]
      (is (= 6 (:substrate-2-count result)))
      (is (= 6 (:war-machine-count result)))
      (is (= 12 (count (:registered result))))
      ;; Every substrate-2 invariant has a tap
      (doseq [fid taps/substrate-2-phase-1-invariants]
        (is (contains? registered fid) (str "missing tap for " fid)))
      ;; Every war-machine invariant has a tap
      (doseq [fid taps/war-machine-aif-invariants]
        (is (contains? registered fid) (str "missing tap for " fid))))))

(deftest deferred-taps-fire-as-inactive-in-probe-sweep
  (testing "deferred families show as :inactive (not silently absent) in sweep"
    (taps/register-deferred-taps!)
    (let [summary (probe/run-probe-sweep!
                   *xtdb-backend*
                   (concat taps/substrate-2-phase-1-invariants
                           taps/war-machine-aif-invariants))
          all-inactive? (every? #(= :inactive (:outcome %)) (:results summary))
          all-deferred? (every? #(true? (get-in % [:detail :deferred?]))
                                (:results summary))]
      (is (= 12 (:family-count summary)))
      (is all-inactive? "all 12 deferred families surface as :inactive")
      (is all-deferred? "all 12 carry :deferred? true in detail"))))
