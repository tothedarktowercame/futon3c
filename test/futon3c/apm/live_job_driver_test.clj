(ns futon3c.apm.live-job-driver-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.live-job-driver :as sut]))

(def request
  {:dispatch/id "dispatch-1" :agent-id "f19-proctor" :frame-id "f19"
   :problem-id "a01J05" :phase :preflight})

(defn effects [calls job]
  {:request request
   :announce-fn (fn [_] (swap! calls conj :announce)
                  {:ok true :job-id "job-1"})
   :activate-fn (fn [_ _] (swap! calls conj :activate) {:ok true})
   :job-fn (fn [_] (swap! calls conj :poll) @job)
   :persist-fn (fn [state] (swap! calls conj [:persist (:state/type state)])
                 {:ok true})
   :terminal-validator (fn [_ _ _] (swap! calls conj :validate) {:ok true})
   :receipt-provider (fn [_ _ _ _]
                       (swap! calls conj :receipt)
                       {:ok true :certificate {:receipt/id "receipt-1"}})})

(deftest announce-is-persisted-before-activation-and-never-repeated
  (let [calls (atom []) job (atom {:state :running})
        first-pass (sut/drive! (effects calls job))
        state (:state first-pass)
        waiting (sut/drive! (assoc (effects calls job) :state state))]
    (is (= :awaiting-terminal (:status first-pass)))
    (is (= [:announce [:persist :live-job-dispatched] :activate
            [:persist :live-job-dispatched]]
           (take 4 @calls)))
    (is (= :awaiting-terminal (:status waiting)))
    (is (= 1 (count (filter #{:announce} @calls))))
    (is (= (:ticket/id (:ticket state))
           (machine/ledger-digest [(dissoc (:ticket state) :ticket/id)])))))

(deftest persisted-ticket-retries-idempotent-activation-until-accepted
  (let [calls (atom []) job (atom {:state :running})
        failed (sut/drive!
                (assoc (effects calls job)
                       :activate-fn (fn [_ _]
                                      (swap! calls conj :activate-failed)
                                      {:ok false})))
        retried (sut/drive! (assoc (effects calls job) :state (:state failed)))]
    (is (= :live-job-activation-failed (:error/code failed)))
    (is (= :awaiting-terminal (:status retried)))
    (is (true? (get-in retried [:state :activation/accepted?])))
    (is (= 1 (count (filter #{:announce} @calls))))
    (is (= 1 (count (filter #{:activate-failed} @calls))))
    (is (= 1 (count (filter #{:activate} @calls))))))

(deftest matching-terminal-job-is-validated-receipted-and-persisted
  (let [calls (atom []) job (atom {:job-id "job-1" :agent-id "f19-proctor"
                                   :state :done})
        dispatched (:state (sut/drive! (effects calls (atom {:state :running}))))
        result (sut/drive! (assoc (effects calls job) :state dispatched))]
    (is (= :certified (:status result)))
    (is (= [:poll :validate :receipt [:persist :live-job-certified]]
           (take-last 4 @calls)))))

(deftest failure-and-mismatch-stop-closed
  (testing "terminal failure never reaches receipt provider"
    (let [calls (atom [])
          dispatched (:state (sut/drive! (effects calls (atom {:state :running}))))
          result (sut/drive! (assoc (effects calls (atom {:state :failed}))
                                    :state dispatched))]
      (is (= :live-job-terminal-failure (:error/code result)))
      (is (not-any? #{:validate :receipt} @calls))))
  (testing "a different immutable request cannot reuse the ticket"
    (let [calls (atom [])
          dispatched (:state (sut/drive! (effects calls (atom {:state :running}))))
          result (sut/drive! (assoc (effects calls (atom {:state :done}))
                                    :request (assoc request :dispatch/id "other")
                                    :state dispatched))]
      (is (= :live-job-request-state-mismatch (:error/code result))))))
