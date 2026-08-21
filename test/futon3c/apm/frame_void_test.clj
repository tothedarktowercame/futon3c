(ns futon3c.apm.frame-void-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.frame-void :as sut]))

(def projection
  {:projection/status :valid :campaign/id "campaign" :campaign/version 11
   :ledger/digest "digest"
   :active/frame {:frame-id "f18" :problem-id "a97J07"}})

(deftest void-is-bound-to-exact-baseline-and-content-addressed
  (let [result (sut/prepare
                {:projection projection :events (vec (repeat 11 {}))}
                {:frame-id "f18" :problem-id "a97J07"
                 :expected-version 11 :expected-ledger-digest "digest"
                 :failures [:required-legs :memory-path :apparatus-frozen]
                 :now "2026-08-21T10:00:00Z"})]
    (is (:ok result))
    (is (= (:certificate/id (:certificate result))
           (machine/ledger-digest
            [(dissoc (:certificate result) :certificate/id)])))
    (is (= (:event/id (:event result))
           (machine/ledger-digest [(dissoc (:event result) :event/id)])))
    (is (= :obligation/claimed (get-in result [:claim-event :event/type])))
    (is (= (:obligation/id (:obligation result))
           (get-in result [:event :event/body :obligation/id])))))

(deftest void-refuses-stale-or-ambiguous-authority
  (is (= :frame-void-digest-mismatch
         (:error/code
          (sut/prepare {:projection projection :events []}
                       {:frame-id "f18" :problem-id "a97J07"
                        :expected-version 11 :expected-ledger-digest "wrong"
                        :failures [:known]}))))
  (is (= :frame-void-failures-required
         (:error/code
          (sut/prepare {:projection projection :events []}
                       {:frame-id "f18" :problem-id "a97J07"
                        :expected-version 11 :expected-ledger-digest "digest"
                        :failures []})))))
