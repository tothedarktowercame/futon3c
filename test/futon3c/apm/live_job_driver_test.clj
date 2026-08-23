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
                     :terminal-repair-attempts 1
                     :typed-submission-migration-attempts 1)
        result (sut/drive!
                (assoc (effects calls job) :state state
                       :terminal-validator
                       (constantly {:ok false :findings [:bad-shape]})
                       :terminal-repair-request-fn (constantly {:ok true})))]
    (is (= :live-job-terminal-repair-exhausted (:error/code result)))
    (is (= 1 (:repair/attempts result)))))

(deftest exhausted-missing-submission-can-produce-controller-observation
  (let [calls (atom []) job (atom {:job-id "job-1" :state :done})
        state (assoc (:state (sut/drive! (effects calls job)))
                     :terminal-repair-attempts 1
                     :typed-submission-migration-attempts 1)
        receipt {:receipt/type :student-observation-missing
                 :receipt/author :controller}
        base (assoc (effects calls job) :state state
                    :terminal-submission-provider (constantly nil)
                    :missing-observation-provider
                    (fn [_ _ _ attempts _collection]
                      {:ok true :certificate (assoc receipt
                                                    :repair-attempts attempts)}))
        collected (sut/drive! base)
        result (sut/drive! (assoc base :state (:state collected)))]
    (is (= :terminal-collected (:status collected)))
    (is (= :certified (:status result)))
    (is (= :controller (get-in result [:certificate :receipt/author])))
    (is (= 1 (get-in result [:certificate :repair-attempts])))
    (is (= :unobserved (get-in result [:state :learning/outcome])))))

(deftest pre-contract-terminal-gets-one-fresh-typed-migration
  (let [calls (atom [])
        seen-failure (atom nil)
        job (atom {:job-id "legacy-repair-job" :state :done
                   :report {:looks "valid"}})
        base (assoc (effects calls job)
                    :announce-fn
                    (fn [repair-request]
                      (swap! calls conj [:announce (:dispatch/id repair-request)])
                      {:ok true :job-id "typed-migration-job"})
                    :terminal-submission-provider (constantly nil)
                    :terminal-repair-request-fn
                    (fn [r _ticket _job failure]
                      (reset! seen-failure failure)
                      {:ok true
                       :request (assoc r :dispatch/id "typed-migration-dispatch"
                                         :fresh-session? true)}))
        prior-state {:state/type :live-job-dispatched
                     :request request :active-request (assoc request :repair/attempt 1)
                     :ticket {:job-id "legacy-repair-job" :ticket/id "old-ticket"}
                     :activation/accepted? true :terminal-repair-attempts 1}
        legacy-collected (sut/drive! (assoc base :state prior-state))
        migrated (sut/drive! (assoc base :state (:state legacy-collected)))
        typed-base (assoc base :state (assoc (:state migrated)
                                             :activation/accepted? true)
                          :job-fn (constantly {:job-id "typed-migration-job"
                                              :state :done}))
        typed-collected (sut/drive! typed-base)
        exhausted (sut/drive! (assoc typed-base :state (:state typed-collected)))]
    (is (= :terminal-collected (:status legacy-collected)))
    (is (= :awaiting-terminal (:status migrated)))
    (is (true? (:repair? migrated)))
    (is (= :typed-submission-contract-migration
           (:repair/kind @seen-failure)))
    (is (= 1 (get-in migrated [:state
                               :typed-submission-migration-attempts])))
    (is (= 1 (get-in migrated [:state :terminal-repair-attempts])))
    (is (= :live-job-terminal-repair-exhausted (:error/code exhausted)))
    (is (= 1 (count (filter #(and (vector? %)
                                  (= :announce (first %)))
                            @calls))))))

(deftest unaccepted-queued-job-is-cancelled-before-one-distinct-supersession
  (let [calls (atom [])
        base (assoc (effects calls (atom nil))
                    :job-fn (constantly {:job-id "old-job" :state :queued})
                    :cancel-fn (fn [job-id]
                                 (swap! calls conj [:cancel job-id])
                                 {:ok true :cancelled-job-id job-id})
                    :announce-fn (fn [_]
                                   (swap! calls conj :announce-replacement)
                                   {:ok true :job-id "replacement-job"})
                    :terminal-submission-provider (constantly nil)
                    :ticket-register-fn
                    (fn [_ ticket]
                      (swap! calls conj [:register (:job-id ticket)])
                      {:ok true}))
        state {:state/type :live-job-dispatched :request request
               :active-request request
               :ticket {:job-id "old-job" :ticket/id "old-ticket"}
               :activation/accepted? false
               :typed-submission-migration-attempts 1}
        result (sut/drive! (assoc base :state state))]
    (is (= :awaiting-terminal (:status result)))
    (is (true? (:supersession? result)))
    (is (= "replacement-job" (get-in result [:state :ticket :job-id])))
    (is (= 1 (get-in result [:state :activation-supersession-attempts])))
    (is (= "old-job" (get-in result [:state :superseded-tickets 0 :job-id])))
    (is (< (.indexOf @calls [:cancel "old-job"])
           (.indexOf @calls :announce-replacement)
           (.indexOf @calls [:register "replacement-job"])
           (.indexOf @calls :activate)))))

(deftest persisted-cancellation-reconciles-without-a-second-cancel
  (let [calls (atom [])
        base (assoc (effects calls (atom nil))
                    :job-fn (constantly {:job-id "old-job" :state :cancelled})
                    :cancel-fn (fn [_] (swap! calls conj :unexpected-cancel)
                                 {:ok false})
                    :announce-fn (fn [_] (swap! calls conj :announce-replacement)
                                   {:ok true :job-id "replacement-job"})
                    :terminal-submission-provider (constantly nil)
                    :ticket-register-fn
                    (fn [_ _] (swap! calls conj :register) {:ok true}))
        state {:state/type :live-job-dispatched :request request
               :active-request request
               :ticket {:job-id "old-job" :ticket/id "old-ticket"}
               :activation/accepted? false
               :activation/failure {:status 409}
               :typed-submission-migration-attempts 1}
        result (sut/drive! (assoc base :state state))]
    (is (= :awaiting-terminal (:status result)))
    (is (true? (:supersession? result)))
    (is (= [:announce-replacement :register :activate]
           (remove #(and (vector? %) (= :persist (first %))) @calls)))
    (is (true? (get-in result [:state :superseded-tickets 0
                               :cancellation :reconciled?])))))

(deftest typed-submission-replaces-conversational-report
  (let [calls (atom [])
        seen (atom nil)
        collections (atom 0)
        job (atom {:job-id "job-1" :agent-id "f19-proctor" :state :done
                   :report {:frame-id "forged"}})
        dispatched (:state (sut/drive! (effects calls (atom {:state :running}))))
        base (assoc (effects calls job) :state dispatched
                       :terminal-submission-provider
                       (fn [_ _ _]
                         (swap! collections inc)
                         {:authority {:frame-id "f19" :problem-id "a01J05"}
                          :submission/id "persisted-submission"
                          :payload {:command-own-exit 0 :outcome "complete"
                                    :failure-account []
                                    :evidence {:verified true}}})
                       :terminal-validator
                       (fn [_ _ terminal]
                         (reset! seen (:report terminal)) {:ok true}))
        collected (sut/drive! base)
        result (sut/drive! (assoc base :state (:state collected)))]
    (is (= :terminal-collected (:status collected)))
    (is (= :certified (:status result)))
    (is (= "f19" (:frame-id @seen)))
    (is (= 1 @collections))
    (is (= true (:verified @seen)))
    (is (not= "forged" (:frame-id @seen)))))

(deftest missing-typed-submission-never-validates-conversation
  (let [calls (atom [])
        job (atom {:job-id "job-1" :agent-id "f19-proctor" :state :done
                   :report {:looks "valid"}})
        dispatched (:state (sut/drive! (effects calls (atom {:state :running}))))
        base (assoc (effects calls job) :state dispatched
                    :terminal-submission-provider (constantly nil))
        collected (sut/drive! base)
        result (sut/drive! (assoc base :state (:state collected)))]
    (is (= :terminal-collected (:status collected)))
    (is (= :live-job-submission-missing (:error/code result)))
    (is (not-any? #{:validate :receipt} @calls))))

(deftest all-live-role-schemas-collect-before-validation
  (doseq [role [:solver :student :guide :scribe :proctor
                :promotion-proctor :analyst]]
    (let [calls (atom [])
          job (atom {:job-id "job-1" :agent-id "f19-proctor" :state :done})
          dispatched (:state (sut/drive! (effects calls (atom {:state :running}))))
          base (assoc (effects calls job)
                      :request (assoc request :role role)
                      :state (assoc-in dispatched [:request :role] role)
                      :terminal-submission-provider
                      (fn [_ _ _]
                        {:submission/id (str "submission-" (name role))
                         :authority {:frame-id "f19" :problem-id "a01J05"}
                         :payload {:command-own-exit 0 :evidence {}}}))
          collected (sut/drive! base)
          certified (sut/drive! (assoc base :state (:state collected)))]
      (is (= :terminal-collected (:status collected)) (name role))
      (is (= :certified (:status certified)) (name role))
      (is (= (:collection/id (:collection collected))
             (machine/ledger-digest
              [(dissoc (:collection collected) :collection/id)]))
          (name role)))))

(deftest missing-observation-cannot-fire-before-persisted-collection
  (let [calls (atom []) missing-calls (atom 0)
        job (atom {:job-id "job-1" :state :done})
        dispatched (:state (sut/drive! (effects calls (atom {:state :running}))))
        result (sut/drive!
                (assoc (effects calls job) :state dispatched
                       :terminal-submission-provider (constantly nil)
                       :missing-observation-provider
                       (fn [& _] (swap! missing-calls inc)
                         {:ok true :certificate {}})))]
    (is (= :terminal-collected (:status result)))
    (is (zero? @missing-calls))
    (is (= false (get-in result [:collection :submission/available?])))))

(deftest non-student-exhaustion-fails-closed-without-substitution
  (let [calls (atom []) job (atom {:job-id "job-1" :state :done})
        state (assoc (:state (sut/drive! (effects calls job)))
                     :terminal-repair-attempts 1
                     :typed-submission-migration-attempts 1)
        base (assoc (effects calls job) :state state
                    :terminal-submission-provider (constantly nil))
        collected (sut/drive! base)
        result (sut/drive! (assoc base :state (:state collected)))]
    (is (= :terminal-collected (:status collected)))
    (is (= :live-job-terminal-repair-exhausted (:error/code result)))
    (is (nil? (:certificate result)))))

(deftest invalid-configured-terminal-budget-is-refused
  (let [calls (atom []) result
        (sut/drive! (assoc (effects calls (atom {:state :running}))
                           :terminal-budget-config {:collection-attempts 0
                                                    :repair-attempts 1}))]
    (is (= :live-job-driver-input-invalid (:error/code result)))
    (is (empty? @calls))))

(deftest receipt-provider-may-defer-certification-behind-a-further-job
  (let [calls (atom [])
        job (atom {:job-id "job-1" :agent-id "f19-proctor" :state :done})
        deferred (assoc (effects calls job)
                        :receipt-provider
                        (fn [_ _ _ _]
                          (swap! calls conj :receipt)
                          {:ok true :status :awaiting-terminal :job-id "review-1"}))
        dispatched (sut/drive! (assoc deferred :job-fn (fn [_] {:state :running})))
        waiting (sut/drive! (assoc deferred :state (:state dispatched)))]
    (is (= :awaiting-terminal (:status waiting)))
    (is (= "review-1" (:job-id waiting)))
    (is (not= :live-job-certified (get-in waiting [:state :state/type])))
    (is (nil? (get-in waiting [:state :receipt])))
    (is (= 1 (count (filter #{:receipt} @calls))))
    (testing "the validated terminal is re-observed on the next tick"
      (let [again (sut/drive! (assoc deferred :state (:state waiting)))]
        (is (= :awaiting-terminal (:status again)))
        (is (= 2 (count (filter #{:receipt} @calls))))))))
