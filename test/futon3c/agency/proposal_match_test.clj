(ns futon3c.agency.proposal-match-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.agency.proposal-match :as proposal]))

(def common
  {:if "work is ready"
   :then "proceed with the work"
   :because "the gate has been met"
   :scope "this packet"})

(deftest same-name-opposite-however-is-adjacent
  (let [a (assoc common :id "operator/proceed" :however "approval is required")
        b (assoc common :id "operator/proceed" :however "approval is never required")
        result (proposal/match a b)]
    (is (= :adjacent (:verdict result)))
    (is (= :different (get-in result [:fields :however])))))

(deftest different-names-identical-structure-merge
  (let [a (assoc common :id "operator/proceed" :however "A risk remains.")
        b {:id "orchestration/go-ahead"
           :if "  WORK   IS READY. "
           :however "a risk remains"
           :then "Proceed with the work!"
           :because "THE GATE HAS BEEN MET。"
           :scope "This packet."}
        result (proposal/match a b)]
    (is (= :merge (:verdict result)))
    (is (every? #{:same} (vals (:fields result))))))

(deftest missing-fields-and-unscoped-patterns
  (testing "a one-sided missing BECAUSE is uncomparable"
    (is (= :uncomparable
           (:verdict (proposal/match (dissoc common :because)
                                     common)))))
  (testing "scope absent on both sides is equal and can merge"
    (let [a (assoc (dissoc common :scope) :however "risk")
          b (assoc (dissoc common :scope) :however "risk")
          result (proposal/match a b)]
      (is (= :merge (:verdict result)))
      (is (= :same (get-in result [:fields :scope]))))))

(deftest normalisation-preserves-negation
  (is (not= (proposal/normalise "approval is not required.")
            (proposal/normalise "Approval is required!"))))

(deftest parses-real-draft-and-library-flexiargs
  (let [draft (slurp (io/file "/home/joe/code/storage/operator-turns/candidates/operator/name-the-acceptance-test.flexiarg"))
        library (slurp (io/file "../futon3/library/orchestration/recorded-handoff.flexiarg"))
        d (proposal/flexiarg->clauses draft)
        l (proposal/flexiarg->clauses library)]
    (is (= "operator/name-the-acceptance-test" (:id d)))
    (is (= "orchestration/recorded-handoff" (:id l)))
    (doseq [parsed [d l] field [:if :however :then :because]]
      (is (not-empty (get parsed field))))))

(deftest corpus-report-keeps-only-relevant-pairs
  (let [a (assoc common :id "same" :however "one")
        b (assoc common :id "same" :however "two")
        c (assoc common :id "other" :however "one")
        report (proposal/corpus-report [a b c])]
    (is (= 3 (:pairs-compared report)))
    (is (= 1 (:same-id-adjacent-count report)))
    (is (= 1 (:merge-count report)))
    (is (= "other" (get-in report [:cross-id-merges 0 :right-id])))))
