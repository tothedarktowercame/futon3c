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

(deftest provider-usage-signatures-are-declared-and-extendable
  (is (= :usage-limit
         (:signature/id (sut/provider-usage-limit
                         {:error/message "Provider usage limit reached"}))))
  (is (= :glm-capacity
         (:signature/id
          (sut/provider-usage-limit
           {:error/message "GLM seat capacity window is closed"}
           [{:signature/id :glm-capacity
             :provider :glm
             :pattern #"GLM seat capacity window"}]))))
  (is (nil? (sut/provider-usage-limit
             {:error/message "ordinary invalid EDN"}))))

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

(deftest unaccepted-supersession-archives-cancellation-before-redispatch
  (let [calls (atom [])
        job (atom {:job-id "job-1" :state :queued})
        saved (atom nil)
        base (assoc (effects calls job)
                    :state {:state/type :live-job-dispatched :request request
                            :ticket {:job-id "job-1"}
                            :activation/accepted? false
                            :activation/failure {:ok false :error :timeout}}
                    :terminal-submission-provider (constantly nil)
                    :announce-fn (fn [_] {:ok true :job-id "job-2"})
                    :cancel-fn (fn [id]
                                 {:ok true :job-id id :state :cancelled})
                    :activate-fn (fn [& _] {:ok true})
                    :persist-fn (fn [state] (reset! saved state) {:ok true}))
        superseded (sut/drive! base)]
    (is (= :awaiting-terminal (:status superseded)))
    (is (= "job-1" (get-in @saved [:superseded-tickets 0 :job-id])))
    (is (= :cancelled
           (get-in @saved [:superseded-tickets 0 :cancellation :state]))))
  (let [announced (atom 0)
        state {:state/type :live-job-dispatched :request request
               :ticket {:job-id "job-1"} :activation/accepted? false
               :activation/failure {:ok false}}
        result (sut/drive!
                (assoc (effects (atom []) (atom {:job-id "job-1" :state :queued}))
                       :state state :terminal-submission-provider (constantly nil)
                       :cancel-fn (fn [_] {:ok true :state :cancelled})
                       :persist-fn (fn [s]
                                     (if (:superseded-tickets s)
                                       {:ok false} {:ok true}))
                       :announce-fn (fn [_]
                                      (swap! announced inc)
                                      {:ok true :job-id "job-2"})))]
    (is (= :live-job-supersession-archive-persistence-failed
           (:error/code result)))
    (is (zero? @announced))))

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

(deftest receipt-provider-hold-never-certifies-a-nil-receipt
  (let [calls (atom [])
        job (atom {:job-id "job-1" :agent-id "f19-proctor" :state :done})
        dispatched (:state (sut/drive! (effects calls (atom {:state :running}))))
        held (sut/drive!
              (assoc (effects calls job) :state dispatched
                     :receipt-provider
                     (fn [& _] {:ok true :status :awaiting-apparatus-repair
                                :findings [:candidate-not-materialized]})))
        missing (sut/drive!
                 (assoc (effects calls job) :state dispatched
                        :receipt-provider
                        (fn [& _] {:ok true :status :certified})))]
    (is (= :awaiting-apparatus-repair (:status held)))
    (is (= :live-job-dispatched (get-in held [:state :state/type])))
    (is (= :live-job-certificate-missing (:error/code missing)))
    (is (not-any? #{[:persist :live-job-certified]} @calls))))

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

(deftest wall-clock-budget-exhaustion-enters-durable-terminal-repair
  (let [calls (atom [])
        jobs (atom {"job-1" {:job-id "job-1" :agent-id "student-attempt-2"
                              :state :failed :terminal-code :invoke-error
                              :terminal-message "wall-clock-budget"}
                    "job-2" {:job-id "job-2" :agent-id "student-attempt-2"
                              :state :running}})
        persisted (atom [])
        base (assoc (effects calls (atom nil))
                    :job-fn (fn [id] (get @jobs id))
                    :persist-fn (fn [state]
                                  (swap! persisted conj state)
                                  {:ok true})
                    :terminal-submission-provider (constantly nil)
                    :announce-fn (fn [_] {:ok true :job-id "job-2"})
                    :terminal-validator
                    (fn [& _]
                      (throw (ex-info "missing submission must be classified first" {})))
                    :terminal-repair-request-fn
                    (fn [r _ticket job failure]
                      {:ok true
                       :request (assoc r :dispatch/id "repair-dispatch"
                                         :repair/of-job-id (:job-id job)
                                         :repair/findings (:findings failure))}))
        dispatched {:state/type :live-job-dispatched
                    :request request
                    :ticket {:job-id "job-1" :ticket/id "ticket-1"}
                    :activation/accepted? true}
        collected (sut/drive! (assoc base :state dispatched))
        repairing (sut/drive! (assoc base :state (:state collected)))
        archived (first (get-in repairing [:state :superseded-terminals]))]
    (is (= :terminal-collected (:status collected)))
    (is (= :awaiting-terminal (:status repairing)))
    (is (true? (:repair? repairing)))
    (is (= 1 (get-in repairing [:state :terminal-repair-attempts])))
    (is (= "job-1" (get-in archived [:job :job-id])))
    (is (= "wall-clock-budget" (get-in archived [:job :terminal-message])))
    (is (= [:typed-submission-missing] (:findings archived)))
    (is (= "job-2" (get-in repairing [:state :ticket :job-id])))
    (is (some #(= :live-job-dispatched (:state/type %)) @persisted))))

(deftest unrecognized-terminal-failure-remains-fatal-with-submission-provider
  (let [calls (atom [])
        job (atom {:job-id "job-1" :agent-id "student-attempt-2"
                   :state :failed :terminal-code :invoke-error
                   :terminal-message "worker-crashed"})
        result (sut/drive!
                (assoc (effects calls job)
                       :state {:state/type :live-job-dispatched
                               :request request
                               :ticket {:job-id "job-1"}
                               :activation/accepted? true}
                       :terminal-submission-provider (constantly nil)))]
    (is (= :live-job-terminal-failure (:error/code result)))
    (is (not-any? #{:validate :receipt} @calls))))

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

(deftest repair-retains-the-discarded-terminal
  (let [calls (atom [])
        persisted (atom nil)
        terminal {:job-id "job-1" :agent-id "f19-proctor" :state :done
                  :report {:memory-use {:used-ids ["memory-7"]}
                           :outcome :partial}}
        collection {:evidence {:collection/id "collection-1"}
                    :submission {:payload {:evidence (:report terminal)}}}
        base (assoc (effects calls (atom terminal))
                    :persist-fn #(do (reset! persisted %) {:ok true})
                    :announce-fn (constantly {:ok true :job-id "job-2"})
                    :terminal-validator
                    (constantly {:ok false
                                 :findings [:student-memory-used-despite-holdout]})
                    :terminal-repair-request-fn
                    (fn [r _ticket _job _failure]
                      {:ok true :request (assoc r :dispatch/id "repair-1")}))
        dispatched (assoc (:state (sut/drive! base))
                          :terminal-collection collection)
        repaired (sut/drive! (assoc base :state dispatched))
        discarded (first (:superseded-terminals @persisted))]
    (is (= :awaiting-terminal (:status repaired)))
    (is (= ["memory-7"]
           (get-in discarded [:job :report :memory-use :used-ids])))
    (is (= collection (:terminal-collection discarded)))
    (is (= [:student-memory-used-despite-holdout]
           (:findings discarded)))
    (is (= {:predecessor-id "job-1"
            :terminal-evidence-id "job-1"
            :collection-evidence-id "collection-1"
            :disposition "[:student-memory-used-despite-holdout]"
            :predecessor-persisted? true
            :successor-announced-id "job-2"
            :successor-activated-id "job-2"}
           (:trace/successor-observation discarded)))))

(deftest posthoc-rejection-enters-the-bounded-terminal-repair-transition
  (let [calls (atom [])
        job (atom {:job-id "job-1" :agent-id "f19-proctor" :state :done})
        base (assoc (effects calls job)
                    :job-fn (fn [job-id]
                              (if (= "job-1" job-id)
                                @job
                                {:job-id job-id :agent-id "f19-proctor"
                                 :state :running}))
                    :receipt-provider
                    (fn [_ _ _ _]
                      {:ok false :error/code :posthoc-invalid
                       :findings [:frame-mismatch]})
                    :terminal-repair-request-fn
                    (fn [request _ticket predecessor failure]
                      {:ok true
                       :request (assoc request :dispatch/id "repair-dispatch"
                                      :repair/of-job-id (:job-id predecessor)
                                      :repair/findings (:findings failure))})
                    :announce-fn (fn [request]
                                   {:ok true :job-id
                                    (if (= "repair-dispatch"
                                           (:dispatch/id request))
                                      "job-2" "job-1")}))
        results (loop [results [] state nil remaining 6]
                  (if (zero? remaining)
                    results
                    (let [result (sut/drive! (cond-> base state
                                               (assoc :state state)))]
                      (recur (conj results result) (:state result)
                             (dec remaining)))))
        rejected (first (filter :posthoc-rejection results))
        repaired (first (filter :repair? results))]
    (is (= :awaiting-terminal (:status rejected)))
    (is (= :posthoc-invalid
           (get-in rejected [:state :posthoc-rejection :error/code])))
    (is (= :awaiting-terminal (:status repaired)))
    (is (= 1 (get-in repaired [:state :terminal-repair-attempts])))
    (is (= "job-2" (get-in repaired [:state :ticket :job-id])))
    (is (nil? (get-in repaired [:state :posthoc-rejection])))))

(deftest repeated-posthoc-rejection-exhausts-one-repair-budget
  (let [repairs (atom 0)
        base (assoc (effects (atom [])
                             (atom {:job-id "ignored" :agent-id "f19-proctor"
                                    :state :done}))
                    :job-fn (fn [job-id]
                              {:job-id job-id :agent-id "f19-proctor"
                               :state :done})
                    :receipt-provider
                    (constantly {:ok false :error/code :posthoc-invalid
                                 :findings [:frame-mismatch]})
                    :terminal-repair-request-fn
                    (fn [request _ticket job failure]
                      (swap! repairs inc)
                      {:ok true
                       :request (assoc request :dispatch/id "repair-dispatch"
                                      :repair/of-job-id (:job-id job)
                                      :repair/findings (:findings failure))})
                    :announce-fn (fn [request]
                                   {:ok true :job-id
                                    (if (= "repair-dispatch"
                                           (:dispatch/id request))
                                      "job-2" "job-1")}))
        results (loop [results [] state nil remaining 12]
                  (if (zero? remaining)
                    results
                    (let [result (sut/drive! (cond-> base state
                                               (assoc :state state)))]
                      (if (= :live-job-terminal-repair-exhausted
                             (:error/code result))
                        (conj results result)
                        (recur (conj results result) (:state result)
                               (dec remaining))))))
        exhausted (last results)]
    (is (= 1 @repairs))
    (is (= :live-job-terminal-repair-exhausted (:error/code exhausted)))
    (is (= [:frame-mismatch] (:findings exhausted)))))

(deftest apparatus-repair-does-not-consume-the-agent-repair-turn
  (let [announcements (atom 0)
        repairs (atom [])
        base (assoc (effects (atom []) (atom nil))
                    :job-fn (fn [job-id]
                              {:job-id job-id :agent-id "f19-proctor"
                               :state (if (= "job-3" job-id) :running :done)})
                    :receipt-provider
                    (fn [_ ticket _ _]
                      {:ok false :error/code :posthoc-invalid
                       :repair/fault-origin
                       (if (= "job-1" (:job-id ticket)) :apparatus :agent)
                       :findings [:frame-mismatch]})
                    :terminal-repair-request-fn
                    (fn [request _ticket job failure]
                      (let [origin (if (= "job-1" (:job-id job))
                                     :apparatus :agent)]
                        (swap! repairs conj origin)
                        {:ok true
                         :request (assoc request
                                        :dispatch/id (str "repair-" (count @repairs))
                                        :repair/fault-origin origin
                                        :repair/findings (:findings failure))}))
                    :announce-fn (fn [_]
                                   {:ok true :job-id
                                    (str "job-" (swap! announcements inc))}))
        results (loop [results [] state nil remaining 14]
                  (if (zero? remaining)
                    results
                    (let [result (sut/drive! (cond-> base state
                                               (assoc :state state)))]
                      (recur (conj results result) (:state result)
                             (dec remaining)))))
        final-state (:state (last results))]
    (is (= [:apparatus :agent] @repairs))
    (is (= 1 (:apparatus-repair-attempts final-state)))
    (is (= 1 (:terminal-repair-attempts final-state)))
    (is (= [:apparatus :agent]
           (mapv :fault-origin (:repair-attempt-history final-state))))))

(deftest apparatus-origin-repairs-have-an-independent-bound
  (let [announcements (atom 0)
        repairs (atom 0)
        base (assoc (effects (atom []) (atom nil))
                    :job-fn (fn [job-id]
                              {:job-id job-id :agent-id "f19-proctor"
                               :state :done})
                    :receipt-provider
                    (constantly {:ok false :error/code :posthoc-invalid
                                 :repair/fault-origin :apparatus
                                 :findings [:frame-mismatch]})
                    :terminal-repair-request-fn
                    (fn [request _ticket _job failure]
                      (swap! repairs inc)
                      {:ok true
                       :request (assoc request :dispatch/id "apparatus-repair"
                                      :repair/fault-origin :apparatus
                                      :repair/findings (:findings failure))})
                    :announce-fn (fn [_]
                                   {:ok true :job-id
                                    (str "job-" (swap! announcements inc))}))
        exhausted
        (loop [state nil remaining 12]
          (let [result (sut/drive! (cond-> base state (assoc :state state)))]
            (if (or (= :live-job-apparatus-repair-exhausted
                       (:error/code result))
                    (zero? remaining))
              result
              (recur (:state result) (dec remaining)))))]
    (is (= 1 @repairs))
    (is (= :live-job-apparatus-repair-exhausted (:error/code exhausted)))
    (is (= :apparatus (:repair/fault-origin exhausted)))
    (is (= 1 (:repair/attempts exhausted)))
    (is (= 1 (count (:repair/history exhausted))))))

(deftest repair-archive-failure-blocks-successor-announcement
  (let [calls (atom [])
        job (atom {:job-id "job-1" :agent-id "f19-proctor" :state :done})
        base (assoc (effects calls job)
                    :persist-fn (fn [state]
                                  (swap! calls conj [:persist state])
                                  (if (:superseded-terminals state)
                                    {:ok false :error :disk-full}
                                    {:ok true}))
                    :terminal-validator
                    (constantly {:ok false :findings [:invalid-terminal]})
                    :terminal-repair-request-fn
                    (fn [r _ _ _]
                      {:ok true :request (assoc r :dispatch/id "repair-1")})
                    :announce-fn (fn [request]
                                   (when (= "repair-1" (:dispatch/id request))
                                     (swap! calls conj :repair-announced))
                                   {:ok true :job-id
                                    (if (= "repair-1" (:dispatch/id request))
                                      "job-2" "job-1")}))
        dispatched (sut/drive! base)
        result (sut/drive! (assoc base :state (:state dispatched)))]
    (is (= :live-job-terminal-repair-archive-persistence-failed
           (:error/code result)))
    (is (not-any? #{:repair-announced} @calls))))

(deftest durable-reference-scan-is-clean-and-reports-first-missing-reference
  (let [state {:ticket {:job-id "job-current"}
               :superseded-terminals
               [{:job {:job-id "job-old"}
                 :terminal-collection
                 {:submission {:submission/id "submission-old"}}}]}
        intact #{"job-current" "job-old" "submission-old"}
        resolve-intact (fn [{:keys [id]}]
                         (if (contains? intact id)
                           {:ok true :value {:id id}}
                           {:ok false :error/code :fixture-missing}))]
    (is (= {:ok true}
           (sut/scan-durable-references state resolve-intact)))
    (is (= {:ok false
            :error/code :fixture-missing
            :reference {:path [:superseded-terminals 0 :job :job-id]
                        :key :job-id :id "job-old"}
            :finding {:ok false :error/code :fixture-missing}}
           (sut/scan-durable-references
            state
            (fn [{:keys [id]}]
              (if (= id "job-old")
                {:ok false :error/code :fixture-missing}
                {:ok true :value {:id id}})))))))

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

(deftest recovered-controller-observation-is-observed
  (let [calls (atom []) job (atom {:job-id "job-1" :state :done})
        state (assoc (:state (sut/drive! (effects calls job)))
                     :terminal-repair-attempts 1
                     :typed-submission-migration-attempts 1)
        base (assoc (effects calls job) :state state
                    :terminal-submission-provider (constantly nil)
                    :missing-observation-provider
                    (fn [& _]
                      {:ok true
                       :certificate
                       {:receipt/type :student-observation-recovered
                        :receipt/author :controller}}))
        collected (sut/drive! base)
        result (sut/drive! (assoc base :state (:state collected)))]
    (is (= :certified (:status result)))
    (is (= :observed (get-in result [:state :learning/outcome])))))

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
                                    :queries ["dyadic shell summability"]
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
    (is (= ["dyadic shell summability"] (:queries @seen)))
    (is (not= "forged" (:frame-id @seen)))))

(deftest guide-report-lifts-mode-from-authoritative-channel-audit
  (let [calls (atom [])
        seen (atom nil)
        job (atom {:job-id "guide-job" :agent-id "f57-guide" :state :done})
        guide-request (assoc request :dispatch/type :guide-intervention
                             :agent-id "f57-guide" :mode :store-mode)
        dispatched (:state
                    (sut/drive! (assoc (effects calls (atom {:state :running}))
                                       :request guide-request)))
        base (assoc (effects calls job)
                    :request guide-request
                    :state dispatched
                    :terminal-submission-provider
                    (fn [_ _ _]
                      {:authority {:frame-id "f57" :problem-id "a99J08"}
                       :submission/id "guide-submission"
                       :payload {:command-own-exit 0
                                 :outcome "complete"
                                 :mode "harness-mode"
                                 :failure-account []
                                 :evidence
                                 {:channel-audit
                                  {:mode "store-mode"
                                   :direct-student-contact? false}}}})
                    :terminal-validator
                    (fn [_ _ terminal]
                      (reset! seen (:report terminal))
                      {:ok true}))
        collected (sut/drive! base)
        result (sut/drive! (assoc base :state (:state collected)))]
    (is (= :certified (:status result)))
    (is (= "store-mode" (:mode @seen)))
    (is (= false (get-in @seen [:channel-audit
                                :direct-student-contact?])))))

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
