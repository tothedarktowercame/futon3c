(ns futon3c.evidence.store-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.evidence.store :as store]
            [futon3c.social.shapes :as shapes]
            [futon3c.social.test-fixtures :as fix])
  (:import [java.time Instant]))

(use-fixtures
  :each
  (fn [f]
    (store/reset-store!)
    (f)))

(deftest append-args-happy-path
  (testing "append! returns {:ok true :entry EvidenceEntry} for valid args"
    (let [subject (fix/make-artifact-ref :mission "M1")
          result (store/append! {:subject subject
                                 :type :reflection
                                 :claim-type :observation
                                 :author "claude-1"
                                 :body {:text "hi"}
                                 :tags [:t1]})]
      (is (= true (:ok result)))
      (fix/assert-valid! shapes/EvidenceEntry (:entry result))
      (is (= subject (:evidence/subject (:entry result)))))))

(deftest append-invalid-input
  (testing "append! returns SocialError on invalid claim-type"
    (let [result (store/append! {:subject (fix/make-artifact-ref :mission "M1")
                                 :type :reflection
                                 :claim-type :nope
                                 :author "claude-1"
                                 :body {}
                                 :tags [:t]})]
      (fix/assert-valid! shapes/SocialError result)
      (is (= :E-store (:error/component result)))
      (is (= :invalid-entry (:error/code result))))))

(deftest append-rejects-missing-in-reply-to
  (testing "append! rejects in-reply-to referencing missing entry"
    (let [result (store/append! {:evidence-id "e-child"
                                 :subject (fix/make-artifact-ref :mission "M1")
                                 :type :reflection
                                 :claim-type :step
                                 :author "claude-1"
                                 :body {:text "child"}
                                 :in-reply-to "no-such"
                                 :tags [:t]})]
      (fix/assert-valid! shapes/SocialError result)
      (is (= :reply-not-found (:error/code result))))))

(deftest append-rejects-missing-fork-of
  (testing "append! rejects fork-of referencing missing entry"
    (let [result (store/append! {:evidence-id "e-fork"
                                 :subject (fix/make-artifact-ref :mission "M1")
                                 :type :reflection
                                 :claim-type :step
                                 :author "claude-1"
                                 :body {:text "fork"}
                                 :fork-of "no-such"
                                 :tags [:t]})]
      (fix/assert-valid! shapes/SocialError result)
      (is (= :fork-not-found (:error/code result))))))

(deftest query-filters-by-subject
  (testing "query filters by subject"
    (let [s1 (fix/make-artifact-ref :mission "M1")
          s2 (fix/make-artifact-ref :mission "M2")]
      (store/append! {:subject s1 :type :reflection :claim-type :observation :author "a" :body {} :tags [:t]})
      (store/append! {:subject s2 :type :reflection :claim-type :observation :author "b" :body {} :tags [:t]})
      (let [xs (store/query {:query/subject s1})]
        (is (= 1 (count xs)))
        (is (= s1 (:evidence/subject (first xs))))))))

(deftest query-filters-by-type
  (testing "query filters by type"
    (let [s (fix/make-artifact-ref :mission "M1")]
      (store/append! {:subject s :type :reflection :claim-type :observation :author "a" :body {} :tags [:t]})
      (store/append! {:subject s :type :presence-event :claim-type :observation :author "a" :body {} :tags [:t]})
      (let [xs (store/query {:query/type :presence-event})]
        (is (= 1 (count xs)))
        (is (= :presence-event (:evidence/type (first xs))))))))

(deftest query-excludes-ephemeral-by-default
  (testing "query excludes ephemeral entries by default"
    (let [s (fix/make-artifact-ref :mission "M1")]
      (store/append! {:subject s :type :reflection :claim-type :observation :author "a" :body {} :tags [:t]})
      (store/append! {:subject s :type :reflection :claim-type :observation :author "a" :body {} :tags [:t]
                     :ephemeral? true})
      (let [xs (store/query {})]
        (is (= 1 (count xs)))
        (is (false? (true? (:evidence/ephemeral? (first xs)))))))))

(deftest query-includes-ephemeral-when-opted-in
  (testing "query includes ephemeral when include-ephemeral? is true"
    (let [s (fix/make-artifact-ref :mission "M1")]
      (store/append! {:subject s :type :reflection :claim-type :observation :author "a" :body {} :tags [:t]})
      (store/append! {:subject s :type :reflection :claim-type :observation :author "a" :body {} :tags [:t]
                     :ephemeral? true})
      (let [xs (store/query {:query/include-ephemeral? true})]
        (is (= 2 (count xs)))))))

(deftest get-entry-returns-entry-or-nil
  (testing "get-entry returns entry by id, nil for missing"
    (let [r (store/append! {:evidence-id "e-1"
                            :subject (fix/make-artifact-ref :mission "M1")
                            :type :reflection
                            :claim-type :observation
                            :author "a"
                            :body {}
                            :tags [:t]})]
      (is (= true (:ok r)))
      (is (= "e-1" (:evidence/id (store/get-entry "e-1"))))
      (is (nil? (store/get-entry "nope"))))))

(deftest get-reply-chain-is-ordered
  (testing "get-reply-chain returns ordered ancestor chain (root..leaf)"
    (let [s (fix/make-artifact-ref :mission "M1")]
      (store/append! {:evidence-id "e1" :subject s :type :coordination :claim-type :goal :author "a" :body {} :tags [:t]})
      (store/append! {:evidence-id "e2" :subject s :type :coordination :claim-type :step :author "a" :body {} :tags [:t]
                     :in-reply-to "e1"})
      (store/append! {:evidence-id "e3" :subject s :type :coordination :claim-type :evidence :author "a" :body {} :tags [:t]
                     :in-reply-to "e2"})
      (let [chain (store/get-reply-chain "e3")]
        (is (= ["e1" "e2" "e3"] (mapv :evidence/id chain)))))))

(deftest get-forks-returns-forked-entries
  (testing "get-forks returns all entries forked from a given entry"
    (let [s (fix/make-artifact-ref :mission "M1")]
      (store/append! {:evidence-id "base" :subject s :type :reflection :claim-type :goal :author "a" :body {} :tags [:t]})
      (store/append! {:evidence-id "fork-1" :subject s :type :reflection :claim-type :step :author "b" :body {} :tags [:t]
                     :fork-of "base"})
      (let [forks (store/get-forks "base")]
        (is (= 1 (count forks)))
        (is (= "fork-1" (:evidence/id (first forks))))))))

(deftest recent-activity-is-newest-first
  (testing "recent-activity returns recent non-ephemeral entries, newest first"
    (let [s (fix/make-artifact-ref :mission "M1")
          older (str (Instant/ofEpochMilli 1000))
          newer (str (Instant/ofEpochMilli 2000))]
      (store/append* store/!store (fix/make-evidence-entry {:evidence/id "old"
                                                            :evidence/subject s
                                                            :evidence/at older}))
      (store/append* store/!store (fix/make-evidence-entry {:evidence/id "new"
                                                            :evidence/subject s
                                                            :evidence/at newer}))
      (let [xs (store/recent-activity {:limit 2})]
        (is (= ["new" "old"] (mapv :evidence/id xs)))))))

(deftest compact-ephemeral-removes-only-old-ephemeral
  (testing "compact-ephemeral! removes ephemeral entries older than threshold"
    (let [s (fix/make-artifact-ref :mission "M1")
          t-old (str (Instant/ofEpochMilli 1000))
          t-new (str (Instant/ofEpochMilli 3000))
          cutoff (str (Instant/ofEpochMilli 2000))]
      (store/append* store/!store (fix/make-evidence-entry {:evidence/id "e-old-eph"
                                                            :evidence/subject s
                                                            :evidence/at t-old
                                                            :evidence/ephemeral? true}))
      (store/append* store/!store (fix/make-evidence-entry {:evidence/id "e-new-eph"
                                                            :evidence/subject s
                                                            :evidence/at t-new
                                                            :evidence/ephemeral? true}))
      (store/append* store/!store (fix/make-evidence-entry {:evidence/id "e-old-durable"
                                                            :evidence/subject s
                                                            :evidence/at t-old
                                                            :evidence/ephemeral? false}))
      (let [r (store/compact-ephemeral! {:older-than cutoff})]
        (is (= 1 (:compacted r))))
      (let [xs (store/query {:query/include-ephemeral? true})]
        (is (= #{"e-new-eph" "e-old-durable"} (set (map :evidence/id xs))))))))

