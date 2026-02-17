(ns futon3c.evidence.backend-test
  "Protocol compliance tests for AtomBackend."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.evidence.backend :as backend]
            [futon3c.social.shapes :as shapes]
            [futon3c.social.test-fixtures :as fix])
  (:import [java.time Instant]))

(def ^:dynamic *backend* nil)

(use-fixtures
  :each
  (fn [f]
    (binding [*backend* (backend/->AtomBackend (atom {:entries {} :order []}))]
      (f))))

(defn- append! [entry]
  (backend/-append *backend* entry))

(deftest append-and-get
  (testing "append stores entry, -get retrieves it"
    (let [e (fix/make-evidence-entry {:evidence/id "e-1"})
          r (append! e)]
      (is (= true (:ok r)))
      (is (= e (:entry r)))
      (let [got (backend/-get *backend* "e-1")]
        (is (= "e-1" (:evidence/id got)))
        (is (= (:evidence/body e) (:evidence/body got)))))))

(deftest get-missing-returns-nil
  (testing "-get returns nil for nonexistent id"
    (is (nil? (backend/-get *backend* "nope")))))

(deftest exists-check
  (testing "-exists? returns true/false correctly"
    (is (false? (backend/-exists? *backend* "e-1")))
    (append! (fix/make-evidence-entry {:evidence/id "e-1"}))
    (is (true? (backend/-exists? *backend* "e-1")))))

(deftest duplicate-id-rejected
  (testing "append rejects duplicate evidence id"
    (append! (fix/make-evidence-entry {:evidence/id "e-dup"}))
    (let [r (append! (fix/make-evidence-entry {:evidence/id "e-dup"}))]
      (fix/assert-valid! shapes/SocialError r)
      (is (= :duplicate-id (:error/code r))))))

(deftest reply-not-found-rejected
  (testing "append rejects in-reply-to referencing missing entry"
    (let [e (fix/make-evidence-entry {:evidence/id "e-child"
                                      :evidence/in-reply-to "no-such"})
          r (append! e)]
      (fix/assert-valid! shapes/SocialError r)
      (is (= :reply-not-found (:error/code r))))))

(deftest fork-not-found-rejected
  (testing "append rejects fork-of referencing missing entry"
    (let [e (fix/make-evidence-entry {:evidence/id "e-fork"
                                      :evidence/fork-of "no-such"})
          r (append! e)]
      (fix/assert-valid! shapes/SocialError r)
      (is (= :fork-not-found (:error/code r))))))

(deftest reply-chain-works
  (testing "append allows valid in-reply-to"
    (append! (fix/make-evidence-entry {:evidence/id "e1"}))
    (let [r (append! (fix/make-evidence-entry {:evidence/id "e2"
                                               :evidence/in-reply-to "e1"}))]
      (is (= true (:ok r))))))

(deftest fork-works
  (testing "append allows valid fork-of"
    (append! (fix/make-evidence-entry {:evidence/id "base"}))
    (let [r (append! (fix/make-evidence-entry {:evidence/id "fork-1"
                                               :evidence/fork-of "base"}))]
      (is (= true (:ok r))))))

(deftest query-filters-by-subject
  (testing "-query filters by subject"
    (let [s1 (fix/make-artifact-ref :mission "M1")
          s2 (fix/make-artifact-ref :mission "M2")]
      (append! (fix/make-evidence-entry {:evidence/id "e1" :evidence/subject s1}))
      (append! (fix/make-evidence-entry {:evidence/id "e2" :evidence/subject s2}))
      (let [xs (backend/-query *backend* {:query/subject s1})]
        (is (= 1 (count xs)))
        (is (= s1 (:evidence/subject (first xs))))))))

(deftest query-filters-by-type
  (testing "-query filters by type"
    (append! (fix/make-evidence-entry {:evidence/id "e1" :evidence/type :reflection}))
    (append! (fix/make-evidence-entry {:evidence/id "e2" :evidence/type :presence-event}))
    (let [xs (backend/-query *backend* {:query/type :presence-event})]
      (is (= 1 (count xs)))
      (is (= :presence-event (:evidence/type (first xs)))))))

(deftest query-excludes-ephemeral-by-default
  (testing "-query excludes ephemeral entries by default"
    (append! (fix/make-evidence-entry {:evidence/id "e1"}))
    (append! (fix/make-evidence-entry {:evidence/id "e2" :evidence/ephemeral? true}))
    (let [xs (backend/-query *backend* {})]
      (is (= 1 (count xs)))
      (is (= "e1" (:evidence/id (first xs)))))))

(deftest query-includes-ephemeral-when-opted-in
  (testing "-query includes ephemeral when opted in"
    (append! (fix/make-evidence-entry {:evidence/id "e1"}))
    (append! (fix/make-evidence-entry {:evidence/id "e2" :evidence/ephemeral? true}))
    (let [xs (backend/-query *backend* {:query/include-ephemeral? true})]
      (is (= 2 (count xs))))))

(deftest query-newest-first
  (testing "-query returns results newest first"
    (let [older (str (Instant/ofEpochMilli 1000))
          newer (str (Instant/ofEpochMilli 2000))]
      (append! (fix/make-evidence-entry {:evidence/id "old" :evidence/at older}))
      (append! (fix/make-evidence-entry {:evidence/id "new" :evidence/at newer}))
      (let [xs (backend/-query *backend* {:query/include-ephemeral? true})]
        (is (= ["new" "old"] (mapv :evidence/id xs)))))))

(deftest query-with-limit
  (testing "-query respects limit"
    (append! (fix/make-evidence-entry {:evidence/id "e1" :evidence/at (str (Instant/ofEpochMilli 1000))}))
    (append! (fix/make-evidence-entry {:evidence/id "e2" :evidence/at (str (Instant/ofEpochMilli 2000))}))
    (append! (fix/make-evidence-entry {:evidence/id "e3" :evidence/at (str (Instant/ofEpochMilli 3000))}))
    (let [xs (backend/-query *backend* {:query/limit 2})]
      (is (= 2 (count xs))))))

(deftest forks-of
  (testing "-forks-of returns forked entries sorted by time"
    (append! (fix/make-evidence-entry {:evidence/id "base"
                                       :evidence/at (str (Instant/ofEpochMilli 1000))}))
    (append! (fix/make-evidence-entry {:evidence/id "fork-1"
                                       :evidence/fork-of "base"
                                       :evidence/at (str (Instant/ofEpochMilli 2000))}))
    (append! (fix/make-evidence-entry {:evidence/id "fork-2"
                                       :evidence/fork-of "base"
                                       :evidence/at (str (Instant/ofEpochMilli 3000))}))
    (append! (fix/make-evidence-entry {:evidence/id "unrelated"
                                       :evidence/at (str (Instant/ofEpochMilli 2500))}))
    (let [forks (backend/-forks-of *backend* "base")]
      (is (= ["fork-1" "fork-2"] (mapv :evidence/id forks))))))

(deftest delete-removes-entries
  (testing "-delete! removes entries and returns count"
    (append! (fix/make-evidence-entry {:evidence/id "e1"}))
    (append! (fix/make-evidence-entry {:evidence/id "e2"}))
    (append! (fix/make-evidence-entry {:evidence/id "e3"}))
    (let [r (backend/-delete! *backend* #{"e1" "e3"})]
      (is (= {:compacted 2} r)))
    (is (nil? (backend/-get *backend* "e1")))
    (is (some? (backend/-get *backend* "e2")))
    (is (nil? (backend/-get *backend* "e3")))))

(deftest all-returns-everything
  (testing "-all returns all entries"
    (append! (fix/make-evidence-entry {:evidence/id "e1"}))
    (append! (fix/make-evidence-entry {:evidence/id "e2" :evidence/ephemeral? true}))
    (let [all (backend/-all *backend*)]
      (is (= 2 (count all)))
      (is (= #{"e1" "e2"} (set (map :evidence/id all)))))))
