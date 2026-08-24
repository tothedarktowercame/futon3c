(ns futon3c.apm.library-loop-checkpoint-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.library-loop-checkpoint :as checkpoint]))

(defn- claim [id]
  (let [statement "def Producer : Prop :=   True"]
    {:id id
     :declaration 'OrientedSurfacePreimageDuality.Producer
     :statement statement
     :statement-digest (checkpoint/statement-digest statement)
     :dependencies #{'Intersection.Pairing :surface/fundamental-class}
     :strength :equivalent
     :reduction-witness "Discharged the landed range-sum premise."
     :next-plan "Construct the remaining oriented-surface duality producer."}))

(defn- review [claim ruling]
  {:checkpoint-digest (checkpoint/checkpoint-digest claim)
   :obligation-id (:id claim)
   :ruling ruling
   :rationale "Independent comparison of the declarations and dependencies."
   :approved? true})

(deftest statement-digest-is-normalized-and-agent-strength-is-not-authority
  (let [a (claim :t00J02/producer)
        b (assoc a :statement "  def Producer : Prop := True  ")]
    (is (= (checkpoint/statement-digest (:statement a))
           (checkpoint/statement-digest (:statement b))))
    (is (= a (checkpoint/validate-checkpoint a)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"statement-digest-mismatch"
                          (checkpoint/validate-checkpoint
                           (assoc a :statement-digest "agent-says-reduced"))))))

(deftest second-equivalent-review-closes-the-valve-and-reduction-resets-it
  (let [claim (claim :t00J02/producer)
        first (checkpoint/review-decision
               {:obligation/id :t00J02/producer
                :consecutive-nonreductions 0}
               claim (review claim :equivalent))
        second (checkpoint/review-decision
                {:obligation/id :t00J02/producer
                 :consecutive-nonreductions 1}
                claim (review claim :equivalent))
        reduced (checkpoint/review-decision
                 {:obligation/id :t00J02/producer
                  :consecutive-nonreductions 1}
                 claim (review claim :reduced))]
    (is (:bank-authorized? first))
    (is (= 1 (:consecutive-nonreductions first)))
    (is (false? (:bank-authorized? second)))
    (is (= :checkpoint-nonreduction-limit (:finding second)))
    (is (:bank-authorized? reduced))
    (is (zero? (:consecutive-nonreductions reduced)))))

(deftest changed-id-requires-explicit-supersession-and-carries-history
  (let [new-claim (claim :t00J02/producer-v2)
        base-review (review new-claim :equivalent)
        state {:obligation/id :t00J02/producer
               :consecutive-nonreductions 1}]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"obligation-id-change-without-supersession"
         (checkpoint/review-decision state new-claim base-review)))
    (let [decision (checkpoint/review-decision
                    state new-claim
                    (assoc base-review :supersedes
                           {:id :t00J02/producer
                            :rationale "The new id names the same residual seam."}))]
      (is (= 2 (:consecutive-nonreductions decision)))
      (is (false? (:bank-authorized? decision))))))
