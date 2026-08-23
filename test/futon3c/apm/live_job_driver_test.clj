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

(deftest persisted-ticket-reconciles-observed-running-job-without-reactivation
  (let [calls (atom []) job (atom {:state :queued})
        failed (sut/drive!
                (assoc (effects calls job)
                       :activate-fn (fn [_ _]
                                      (swap! calls conj :activate-failed)
                                      {:ok false})))
        _ (reset! job {:state :running})
        retried (sut/drive! (assoc (effects calls job) :state (:state failed)))]
    (is (= :live-job-activation-failed (:error/code failed)))
    (is (= :awaiting-terminal (:status retried)))
    (is (true? (get-in retried [:state :activation/accepted?])))
    (is (= 1 (count (filter #{:announce} @calls))))
    (is (= 1 (count (filter #{:activate-failed} @calls))))
    (is (zero? (count (filter #{:activate} @calls))))
    (is (= :running (get-in retried [:state :activation/reconciled-from])))))

(deftest persisted-ticket-reconciles-terminal-job-before-validation
  (let [calls (atom []) job (atom {:state :queued})
        failed (sut/drive!
                (assoc (effects calls job)
                       :activate-fn (constantly {:ok false})))
        _ (reset! job {:job-id "job-1" :agent-id "f19-proctor" :state :done})
        reconciled (sut/drive! (assoc (effects calls job) :state (:state failed)))
        certified (sut/drive! (assoc (effects calls job) :state (:state reconciled)))]
    (is (= :awaiting-terminal (:status reconciled)))
    (is (= :done (get-in reconciled [:state :activation/reconciled-from])))
    (is (= :certified (:status certified)))
    (is (= 1 (count (filter #{:validate} @calls))))))

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

(deftest invalid-typed-terminal-gets-one-durable-repair-job
  (let [calls (atom [])
        jobs (atom {"job-1" {:job-id "job-1" :agent-id "f19-proctor"
                             :state :done}
                    "job-2" {:job-id "job-2" :agent-id "f19-proctor"
                             :state :running}})
        base (-> (effects calls (atom nil))
                 (assoc :job-fn (fn [id] (swap! calls conj [:poll id])
                                  (get @jobs id))
                        :announce-fn (fn [r]
                                       (swap! calls conj [:announce (:dispatch/id r)])
                                       {:ok true :job-id (if (:repair/attempt r)
                                                           "job-2" "job-1")})
                        :terminal-validator
                        (fn [_ _ job]
                          (if (= "job-1" (:job-id job))
                            {:ok false :error/code :typed-terminal-invalid
                             :findings [:frame-mismatch]}
                            {:ok true}))
                        :terminal-repair-request-fn
                        (fn [r ticket job failure]
                          {:ok true
                           :request (assoc r :dispatch/id "repair-dispatch"
                                           :repair/attempt 1
                                           :repair/of-job-id (:job-id job)
                                           :repair/of-ticket-id (:ticket/id ticket)
                                           :repair/findings (:findings failure))})))
        dispatched (:state (sut/drive! base))
        repairing (sut/drive! (assoc base :state dispatched))]
    (is (= :awaiting-terminal (:status repairing)))
    (is (true? (:repair? repairing)))
    (is (= "job-2" (get-in repairing [:state :ticket :job-id])))
    (is (= [:frame-mismatch]
           (get-in repairing [:state :terminal-repair/findings])))
    (is (= 1 (get-in repairing [:state :terminal-repair-attempts])))
    (is (< (.indexOf @calls [:persist :live-job-dispatched])
           (.lastIndexOf @calls :activate)))))

(deftest second-invalid-terminal-exhausts-repair-bound
  (let [calls (atom []) job (atom {:job-id "job-1" :state :done})
        state (assoc (:state (sut/drive! (effects calls job)))
                     :terminal-repair-attempts 1)
        result (sut/drive!
                (assoc (effects calls job) :state state
                       :terminal-validator
                       (constantly {:ok false :findings [:bad-shape]})
                       :terminal-repair-request-fn (constantly {:ok true})))]
    (is (= :live-job-terminal-repair-exhausted (:error/code result)))
    (is (= 1 (:repair/attempts result)))))

(deftest typed-submission-replaces-conversational-report
  (let [calls (atom [])
        seen (atom nil)
        job (atom {:job-id "job-1" :agent-id "f19-proctor" :state :done
                   :report {:frame-id "forged"}})
        dispatched (:state (sut/drive! (effects calls (atom {:state :running}))))
        result (sut/drive!
                (assoc (effects calls job) :state dispatched
                       :terminal-submission-provider
                       (fn [_ _ _]
                         {:authority {:frame-id "f19" :problem-id "a01J05"}
                          :payload {:command-own-exit 0 :outcome "complete"
                                    :failure-account []
                                    :evidence {:verified true}}})
                       :terminal-validator
                       (fn [_ _ terminal]
                         (reset! seen (:report terminal)) {:ok true})))]
    (is (= :certified (:status result)))
    (is (= "f19" (:frame-id @seen)))
    (is (= true (:verified @seen)))
    (is (not= "forged" (:frame-id @seen)))))

(deftest missing-typed-submission-never-validates-conversation
  (let [calls (atom [])
        job (atom {:job-id "job-1" :agent-id "f19-proctor" :state :done
                   :report {:looks "valid"}})
        dispatched (:state (sut/drive! (effects calls (atom {:state :running}))))
        result (sut/drive!
                (assoc (effects calls job) :state dispatched
                       :terminal-submission-provider (constantly nil)))]
    (is (= :live-job-submission-missing (:error/code result)))
    (is (not-any? #{:validate :receipt} @calls))))
