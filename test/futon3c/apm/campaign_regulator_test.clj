(ns futon3c.apm.campaign-regulator-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.campaign-regulator :as regulator]))

(def phases [:probe :solve :close])
(def block-plan [{:block-id "b1" :ordinal 1
                  :units [{:frame-id "f1" :problem-id "p1"}
                          {:frame-id "f2" :problem-id "p2"}]}])
(def obligation-plan
  {:probe {:kind :probe-soundness :role :proctor}
   :solve {:kind :solve :role :solver}
   :close {:kind :validate-close :role :proctor}})

(defn event [seq type body]
  {:event/id (str "e" seq) :event/seq seq :event/type type
   :event/campaign-id "apm-200" :event/actor "regulator"
   :event/at (str "2026-08-20T12:00:0" seq "Z")
   :event/expected-version seq :event/body body})

(defn certificate [events & [status]]
  (let [projection (machine/projection events)
        body {:certificate/type :campaign-projection :certificate/version 1
              :generated-at "2026-08-20T12:10:00Z"
              :snapshot/status (or status :valid)
              :campaign/id (:campaign/id projection)
              :campaign/series (:campaign/series projection)
              :campaign/status (:campaign/status projection)
              :campaign/version (:campaign/version projection)
              :campaign/phase-order (:campaign/phase-order projection)
              :campaign/block-plan (:campaign/block-plan projection)
              :campaign/obligation-plan (:campaign/obligation-plan projection)
              :campaign/blocks (:campaign/blocks projection)
              :campaign/frames (:campaign/frames projection)
              :ledger/digest (:ledger/digest projection)
              :ledger/event-count (:ledger/event-count projection)
              :facts/digest "facts" :active/block (:active/block projection)
              :active/frame (:active/frame projection)
              :counts (:counts projection)
              :reconciliation {:findings []}}]
    (assoc body :certificate/id (machine/ledger-digest [body]))))

(def registered
  [(event 0 :campaign/registered
          {:series :apm :phase-order phases :block-plan block-plan
           :obligation-plan obligation-plan})])

(deftest regulator-derives-the-preregistered-sequence
  (testing "open first block"
    (let [decision (regulator/decide (certificate registered))]
      (is (= :dispatch (:decision decision)))
      (is (= :open-block (get-in decision [:obligation :obligation/action :kind])))
      (is (= "b1" (get-in decision [:obligation :obligation/action :block-id])))))
  (testing "open first unit"
    (let [events (conj registered (event 1 :block/opened (first block-plan)))
          decision (regulator/decide (certificate events))]
      (is (= :open-frame (get-in decision [:obligation :obligation/action :kind])))
      (is (= "f1" (get-in decision [:obligation :obligation/action :frame-id])))))
  (testing "dispatch phase role"
    (let [events (into registered
                       [(event 1 :block/opened (first block-plan))
                        (event 2 :frame/opened
                               {:frame-id "f1" :problem-id "p1" :block-id "b1"})])
          decision (regulator/decide (certificate events))]
      (is (= :probe-soundness
             (get-in decision [:obligation :obligation/action :kind])))
      (is (= :proctor (get-in decision [:obligation :obligation/action :role])))
      (is (= :frame/advanced
             (get-in decision [:obligation :obligation/action
                               :completion :event/type]))))))

(deftest final-phase-completes-frame-rather-than-advancing-past-it
  (let [events (into registered
                     [(event 1 :block/opened (first block-plan))
                      (event 2 :frame/opened
                             {:frame-id "f1" :problem-id "p1" :block-id "b1"})
                      (event 3 :frame/advanced
                             {:frame-id "f1" :from :probe :to :solve})
                      (event 4 :frame/advanced
                             {:frame-id "f1" :from :solve :to :close})])
        decision (regulator/decide (certificate events))]
    (is (= :validate-close
           (get-in decision [:obligation :obligation/action :kind])))
    (is (= :frame/closed
           (get-in decision [:obligation :obligation/action
                             :completion :event/type])))))

(deftest exhausted-units-close-block-then-campaign
  (let [events (into registered
                     [(event 1 :block/opened (first block-plan))
                      (event 2 :frame/opened
                             {:frame-id "f1" :problem-id "p1" :block-id "b1"})
                      (event 3 :frame/advanced {:frame-id "f1" :from :probe :to :solve})
                      (event 4 :frame/advanced {:frame-id "f1" :from :solve :to :close})
                      (event 5 :frame/closed {:frame-id "f1" :certificate {:ok true}})
                      (event 6 :frame/opened
                             {:frame-id "f2" :problem-id "p2" :block-id "b1"})
                      (event 7 :frame/advanced {:frame-id "f2" :from :probe :to :solve})
                      (event 8 :frame/advanced {:frame-id "f2" :from :solve :to :close})
                      (event 9 :frame/closed {:frame-id "f2" :certificate {:ok true}})])
        close-block (regulator/decide (certificate events))
        closed-events (conj events
                            (event 10 :block/closed
                                   {:block-id "b1" :certificate {:ok true}}))
        close-campaign (regulator/decide (certificate closed-events))]
    (is (= :close-block
           (get-in close-block [:obligation :obligation/action :kind])))
    (is (= :close-campaign
           (get-in close-campaign [:obligation :obligation/action :kind])))))

(deftest stale-and-conflict-certificates-never-dispatch
  (doseq [status [:stale :conflict]]
    (let [decision (regulator/decide (certificate registered status))]
      (is (= :stop (:decision decision)))
      (is (= :campaign-snapshot-not-valid (:reason decision))))))

(deftest obligation-is-content-addressed-and-pinned
  (let [cert (certificate registered)
        obligation (:obligation (regulator/decide cert))]
    (is (= (:facts/digest cert)
           (get-in obligation [:obligation/preconditions :facts/digest])))
    (is (= (:ledger/digest cert)
           (get-in obligation [:obligation/preconditions :ledger/digest])))
    (is (= (:obligation/id obligation)
           (machine/ledger-digest [(dissoc obligation :obligation/id)])))
    (is (:authorized? (regulator/authorize obligation cert)))))

(deftest authorization-refuses-a-raced-certificate
  (let [old-cert (certificate registered)
        obligation (:obligation (regulator/decide old-cert))
        new-cert (certificate
                  (conj registered (event 1 :block/opened (first block-plan))))]
    (is (= :campaign-obligation-preconditions-stale
           (:error/code (regulator/authorize obligation new-cert))))))

(deftest plans-are-validated-by-the-ledger
  (let [bad (event 0 :campaign/registered
                   {:phase-order phases :block-plan block-plan
                    :obligation-plan {:probe {:kind :probe}}})]
    (is (= :campaign-obligation-plan-invalid
           (:error/code (machine/projection [bad]))))))
