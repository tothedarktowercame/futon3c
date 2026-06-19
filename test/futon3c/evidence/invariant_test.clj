(ns futon3c.evidence.invariant-test
  "Tests for futon3c.evidence.invariant/I-evidence-per-turn.

   The invariant has two checks:
     - check-store-backing: store must be XtdbBackend.
     - verify-persisted: after append, the entry id is readable back.

   These tests exercise both paths against every realistic store shape:
   an XTDB-backed backend (should pass), an AtomBackend (should fail
   check-store-backing but allow verify-persisted round-trip against
   itself), a raw Clojure atom (should fail check-store-backing), and a
   nil/unknown store (should fail check-store-backing)."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [xtdb.api :as xtdb]
            [futon3c.evidence.backend :as backend]
            [futon3c.evidence.invariant :as inv]
            [futon3c.evidence.store :as store]
            [futon3c.evidence.xtdb-backend :as xb]
            [futon3c.social.test-fixtures :as fix]))

(def ^:dynamic *node* nil)
(def ^:dynamic *xtdb-backend* nil)

(use-fixtures
  :each
  (fn [f]
    (let [node (xtdb/start-node {})]
      (try
        (binding [*node* node
                  *xtdb-backend* (xb/make-xtdb-backend node)]
          (f))
        (finally
          (.close node))))))

;; -----------------------------------------------------------------------------
;; check-store-backing
;; -----------------------------------------------------------------------------

(deftest check-store-backing-passes-for-xtdb
  (testing "XtdbBackend satisfies I-evidence-per-turn store-backing"
    (let [r (inv/check-store-backing *xtdb-backend*)]
      (is (true? (:ok r)))
      (is (= :xtdb (:kind r)))
      (is (string? (:invariant r))))))

(deftest check-store-backing-fails-for-atom-backend
  (testing "AtomBackend is flagged as non-durable"
    (let [ab (backend/->AtomBackend (atom {:entries {} :order []}))
          r (inv/check-store-backing ab)]
      (is (false? (:ok r)))
      (is (= :atom-backend (:kind r)))
      (is (re-find #"AtomBackend" (:reason r))))))

(deftest check-store-backing-fails-for-raw-atom
  (testing "Raw Clojure atom is flagged as non-durable"
    (let [r (inv/check-store-backing (atom {:entries {} :order []}))]
      (is (false? (:ok r)))
      (is (= :raw-atom (:kind r)))
      (is (re-find #"raw atom" (:reason r))))))

(deftest check-store-backing-fails-for-unknown
  (testing "Non-store values are flagged"
    (let [r (inv/check-store-backing "not-a-store")]
      (is (false? (:ok r)))
      (is (= :unknown (:kind r))))))

;; -----------------------------------------------------------------------------
;; verify-persisted
;; -----------------------------------------------------------------------------

(deftest verify-persisted-round-trips-through-xtdb
  (testing "append via store.clj/append* then verify-persisted reports :ok true"
    (let [entry (fix/make-evidence-entry {:evidence/id "e-inv-xtdb"})
          r (store/append* *xtdb-backend* entry)]
      (is (true? (:ok r)))
      (let [eid (get-in r [:entry :evidence/id])
            v (inv/verify-persisted *xtdb-backend* eid)]
        (is (true? (:ok v)))
        (is (= :xtdb (:kind v)))
        (is (= "e-inv-xtdb" (:evidence/id v)))))))

(deftest verify-persisted-detects-missing-entry
  (testing "missing id in XTDB reports :ok false with reason"
    (let [v (inv/verify-persisted *xtdb-backend* "e-does-not-exist")]
      (is (false? (:ok v)))
      (is (= :xtdb (:kind v)))
      (is (re-find #"not readable" (:reason v))))))

(deftest verify-persisted-round-trips-through-atom-backend
  (testing "AtomBackend round-trip still works (invariant violation is store-backing, not persistence)"
    (let [ab (backend/->AtomBackend (atom {:entries {} :order []}))
          entry (fix/make-evidence-entry {:evidence/id "e-inv-atom"})
          r (store/append* ab entry)]
      (is (true? (:ok r)))
      (let [v (inv/verify-persisted ab (get-in r [:entry :evidence/id]))]
        (is (true? (:ok v)))
        (is (= :atom-backend (:kind v)))))))

;; -----------------------------------------------------------------------------
;; End-to-end: the invariant as it fires per turn
;; -----------------------------------------------------------------------------

(deftest invariant-holds-end-to-end-on-xtdb
  (testing "against an XTDB-backed store, check-store-backing AND verify-persisted pass"
    (let [back (inv/check-store-backing *xtdb-backend*)]
      (is (:ok back))
      (let [entry (fix/make-evidence-entry {:evidence/id "e-inv-e2e"})
            r (store/append* *xtdb-backend* entry)]
        (is (:ok r))
        (let [v (inv/verify-persisted *xtdb-backend*
                                      (get-in r [:entry :evidence/id]))]
          (is (:ok v))
          (is (= :xtdb (:kind v))))))))
