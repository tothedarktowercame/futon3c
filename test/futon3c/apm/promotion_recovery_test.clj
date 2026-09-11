(ns futon3c.apm.promotion-recovery-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.job-port :as job-port]
            [futon3c.apm.live-job-driver :as driver]
            [futon3c.apm.live-promotion :as sut]
            [futon3c.apm.promotion-pipeline :as pipeline]
            [futon3c.apm.typed-role-submission :as submission]))

(defn checkpoint []
  {:state/type :promotion :stage :awaiting-apparatus-repair
   :repair/kind :promotion-pass :repair/attempts 1 :repair/max-attempts 1
   :findings [{:finding :review-evidence-not-materialized}]
   :persisted-review-result {:reviews [{:verdict :cannot-judge}]}
   :last-valid-state
   {:state/type :promotion :stage :independent-review :job "old"
    :request {:dispatch/id "dispatch" :agent-id "reviewer"
              :phase :promotion-review :role :promotion-proctor}
    :ticket {:job-id "old"} :review-successor-attempt 1
    :candidates [{:memory-id "memory"}]
    :terminal-collection
    {:evidence (driver/terminal-collection-record
                {:dispatch/id "dispatch" :role :promotion-proctor}
                {:job-id "old"} {:state :done :terminal-code 0}
                {:submission/id "submitted"} 1)}}})

(defn authorization [state]
  {:recovery/id "recovery-1" :operator "operator"
   :reason "Repaired source trace routing" :repair/evidence-ref "test-report"
   :expected-state-digest (machine/ledger-digest [state])
   :predecessor-job-id "old"
   :implementation-id (:source-id (sut/transport-implementation-identity))})

(deftest recovery-refuses-stale-incomplete-and-uncollected-authority
  (let [state (checkpoint) auth (authorization state)]
    (doseq [[changed expected]
            [[(assoc auth :expected-state-digest "stale") :promotion-recovery-stale-checkpoint]
             [(assoc auth :predecessor-job-id "other") :promotion-recovery-predecessor-mismatch]
             [(assoc auth :implementation-id "old-code") :promotion-recovery-implementation-mismatch]
             [(dissoc auth :operator) :promotion-recovery-authorization-incomplete]]]
      (is (= expected (:error/code (sut/prepare-review-recovery state changed))))))
  (let [state (update (checkpoint) :last-valid-state dissoc :terminal-collection)]
    (is (= :terminal-collection-authority-missing
           (:error/code (sut/prepare-review-recovery state (authorization state)))))))

(deftest failed-authorization-write-does-not-dispatch
  (let [state (checkpoint) calls (atom [])
        result (sut/authorize-review-recovery!
                {:state state :authorization (authorization state)
                 :persist-fn #(do (swap! calls conj %) {:ok false})})]
    (is (= :promotion-recovery-authorization-persistence-failed (:error/code result)))
    (is (= 1 (count @calls)))
    (is (= state (:recovery/origin (first @calls))))))

(deftest recovery-replays-one-identity-after-final-write-failure
  (let [state (checkpoint) auth (authorization state)
        pending (:state (sut/prepare-review-recovery state auth))
        requests (atom []) saved (atom nil)
        review (fn [candidates predecessor attempt]
                 (swap! requests conj [candidates predecessor attempt])
                 {:ok true :job (:successor/job-id pending)})
        failed (sut/drive! {:state pending :review-fn review
                            :persist-fn (constantly {:ok false})})
        done (sut/drive! {:state pending :review-fn review
                          :persist-fn #(do (reset! saved %) {:ok true})})]
    (is (= :promotion-recovery-successor-persistence-failed (:error/code failed)))
    (is (= pending (:state failed)))
    (is (= 2 (count @requests)))
    (is (apply = @requests))
    (is (= :awaiting-terminal (:status done)))
    (is (= (:successor/job-id pending) (:job @saved)))
    (is (= 1 (:projection-repair-attempt @saved)))
    (is (= [auth] (:recovery/history @saved)))
    (is (nil? (:terminal-collection @saved)))
    (is (= "old" (get-in @saved [:superseded-terminals 0 :job :job-id])))
    (is (= (:terminal-collection (:last-valid-state state))
           (get-in @saved [:superseded-terminals 0 :terminal-collection])))
    (is (false? (:ok (sut/prepare-review-recovery @saved auth))))))

(deftest pending-tampering-and-runtime-drift-never-dispatch
  (let [state (checkpoint)
        pending (:state (sut/prepare-review-recovery state (authorization state)))
        forbidden (fn [& _] (throw (ex-info "must not dispatch" {})))]
    (is (= :promotion-recovery-pending-invalid
           (:error/code (sut/drive! {:state (assoc pending :successor/attempt 90)
                                    :review-fn forbidden :persist-fn forbidden}))))
    (with-redefs [sut/transport-implementation-identity
                  (constantly {:source-id "new" :loaded-runtime-id "old"})]
      (is (= :promotion-recovery-pending-invalid
             (:error/code (sut/drive! {:state pending :review-fn forbidden
                                      :persist-fn forbidden})))))))

(deftest terminal-review-collection-is-created-with-real-submission-identity
  (let [typed {:authority {:job-id "job" :dispatch/id "dispatch"
                           :role :promotion-proctor}
               :submission/id "submission" :payload {:evidence {}}}]
    (with-redefs [job-port/observe (fn [& _] {:ok true :job-id "job" :state :done :terminal-code 0})
                  submission/submitted (constantly typed)]
      (let [result ((#'sut/agency-stage "http://agency" {} "prompt") "job")]
        (is (:ok result))
        (is (:ok (driver/terminal-collection-authority "job" (:terminal-collection result))))
        (is (= "submission" (get-in result [:terminal-collection :evidence :submission/id])))))
    (with-redefs [job-port/observe (fn [& _] {:ok true :job-id "job" :state :done})
                  submission/submitted (constantly (assoc-in typed [:authority :job-id] "other"))]
      (is (false? (:ok ((#'sut/agency-stage "http://agency" {} "prompt") "job")))))))

(deftest collection-write-failure-stops-before-review-publication
  (let [prior (:last-valid-state (checkpoint))
        result (sut/drive!
                {:state prior
                 :review-fn (fn [& _] {:ok true :terminal-collection (:terminal-collection prior)})
                 :persist-fn (constantly {:ok false})
                 :persist-reviews-fn (fn [& _] (throw (ex-info "must not publish" {})))})]
    (is (= :promotion-review-collection-persistence-failed (:error/code result)))))

(deftest collected-review-is-preserved-in-a-failed-pass
  (let [prior (:last-valid-state (checkpoint)) saved (atom nil)
        result (sut/drive!
                {:state prior :promotion-policy {:completed-pass-required true}
                 :review-fn (fn [& _] {:ok true :reviewer "reviewer" :reviews []
                                       :terminal-collection (:terminal-collection prior)})
                 :persist-fn #(do (reset! saved %) {:ok true})})]
    (is (= :awaiting-apparatus-repair (:status result)))
    (is (= (:terminal-collection prior)
           (get-in @saved [:last-valid-state :terminal-collection])))))

(deftest a-consumed-recovery-id-cannot-authorize-another-review
  (let [state (assoc-in (checkpoint) [:last-valid-state :recovery/history]
                        [{:recovery/id "recovery-1"}])]
    (is (= :promotion-recovery-already-consumed
           (:error/code (sut/prepare-review-recovery state (authorization state)))))))

(deftest failed-disposition-keeps-the-reviewers-explanation
  (let [materialization {:artifact-id "a" :content-digest "d"
                         :persisted-content-digest "d" :read-back-content-digest "d"
                         :persistence-receipt-id "p"}
        result (pipeline/validate-complete-dispositions
                [{:memory-id "m" :materialization materialization}]
                [{:memory-id "m" :verdict :cannot-judge
                  :reason "Trace endpoint returned HTTP 404" :residual "Witness unavailable"}])]
    (is (false? (:ok result)))
    (is (= :review-evidence-not-materialized (get-in result [:findings 0 :finding])))
    (is (= "Trace endpoint returned HTTP 404" (get-in result [:findings 0 :review/reason])))))


(deftest recovery-identity-matches-dispatch-for-producer-phase-requests
  ;; Persisted requests retain producer phases; actual review dispatch normalizes
  ;; those phases. Compare with that real authority constructor, not pending itself.
  (doseq [phase [:promote-solver :guide-intervention-1 :guide-intervention-2]]
    (let [state (assoc-in (checkpoint) [:last-valid-state :request :phase] phase)
          prior (:last-valid-state state)
          pending (:state (sut/prepare-review-recovery state (authorization state)))
          dispatched (#'sut/reviewer-authority (:request prior) (:candidates prior) [])
          actual-id (submission/canonical-job-id
                     (assoc dispatched :submission/attempt (:successor/attempt pending)))
          saved (atom nil)
          result (sut/drive! {:state pending
                              :review-fn (fn [_ _ _] {:ok true :job actual-id})
                              :persist-fn #(do (reset! saved %) {:ok true})})]
      (is (= actual-id (:successor/job-id pending)))
      (is (= :awaiting-terminal (:status result)))
      (is (= actual-id (:job @saved))))))
