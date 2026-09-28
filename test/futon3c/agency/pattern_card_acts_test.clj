(ns futon3c.agency.pattern-card-acts-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.agency.pattern-card-acts :as acts]))

(def before "2026-09-28T10:59:59Z")
(def selected-at "2026-09-28T11:00:00Z")
(def withdrawn-at "2026-09-28T11:30:00Z")
(def after "2026-09-28T12:00:00Z")

(def card-a
  {:id "act:card-a" :kind :pattern-card/selection :author "agent-a"
   :agent "agent-a" :session "session-1" :at selected-at :pattern-id "pattern/a"})

(defn withdrawal
  ([id author status basis at]
   (withdrawal id author status basis at "act:card-a" nil))
  ([id author status basis at target reverses]
   {:id id :kind :act/withdrawal :author author :at at :target target
    :status status :basis {:kind basis} :reverses reverses}))

(defn project [records at]
  (acts/card-as-of records "agent-a" "session-1" at))

(deftest interpretation-never-withdraws-a-card
  (let [interpretation {:id "interpretation:1" :kind :interpretation :version 1
                        :intent :withdraw :target "act:card-a"
                        :at withdrawn-at}]
    (is (= card-a (:active (project [card-a interpretation] selected-at))))
    (is (= card-a (:active (project [card-a interpretation] after))))))

(deftest self-withdrawal-is-half-open-and-does-not-mutate-selection
  (let [effect (withdrawal "act:withdraw-a" "agent-a" :effective :self withdrawn-at)
        records [card-a effect]
        original records]
    (is (= card-a (:active (project records selected-at))))
    (is (nil? (:active (project records withdrawn-at))))
    (is (nil? (:active (project records after))))
    (is (= original records))
    (is (= card-a (first records)))))

(deftest non-author-cannot-self-withdraw
  (let [effect (withdrawal "act:foreign" "agent-b" :effective :self withdrawn-at)
        result (project [card-a effect] after)]
    (is (= card-a (:active result)))
    (is (= [{:record-id "act:foreign" :reason :not-author}]
           (:ignored result)))))

(deftest newer-selection-supersedes-older-selection
  (let [card-b (assoc card-a :id "act:card-b" :at withdrawn-at :pattern-id "pattern/b")]
    (is (= card-b (:active (project [card-a card-b] after))))))

(deftest another-session-is-independent
  (let [other (assoc card-a :id "act:other" :session "session-2"
                     :pattern-id "pattern/other")
        effect (withdrawal "act:other-withdrawal" "agent-a" :effective :self
                           withdrawn-at "act:other" nil)]
    (is (= card-a (:active (project [card-a other effect] after))))
    (is (empty? (:ignored (project [card-a other effect] after))))))

(deftest provisional-withdrawal-is-visible-until-reversed
  (let [provisional (withdrawal "act:provisional" "xiang" :provisional
                                :provisional-interpretation withdrawn-at)
        reversal (withdrawal "act:reverse" "xiang" :effective :self after
                             "act:card-a" "act:provisional")]
    (testing "a provisional interpretation effect does not terminate its target"
      (let [result (project [card-a provisional] after)]
        (is (= card-a (:active result)))
        (is (= [provisional] (:provisional result)))))
    (testing "a later reversal by the same author removes the provisional display"
      (let [result (project [card-a provisional reversal] after)]
        (is (= card-a (:active result)))
        (is (empty? (:provisional result)))))))

(deftest malformed-target-relations-are-ignored
  (let [unknown (withdrawal "act:unknown" "agent-a" :effective :self
                            withdrawn-at "act:missing" nil)
        early (withdrawal "act:early" "agent-a" :effective :self before)
        result (project [card-a unknown early] after)]
    (is (= card-a (:active result)))
    (is (= [{:record-id "act:unknown" :reason :unknown-target}
            {:record-id "act:early" :reason :effect-before-target}]
           (:ignored result)))))

(deftest grant-withdrawals-wait-for-the-grant-packet
  (let [effect (withdrawal "act:grant" "agent-b" :effective :grant withdrawn-at)
        result (project [card-a effect] after)]
    (is (= card-a (:active result)))
    (is (= [{:record-id "act:grant" :reason :grant-check-not-implemented}]
           (:ignored result)))))
