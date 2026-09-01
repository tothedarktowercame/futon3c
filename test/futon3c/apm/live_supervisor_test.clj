(ns futon3c.apm.live-supervisor-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.apm.phase-status :as phase-status]
            [futon3c.apm.live-supervisor :as sut]))

(def inspection
  {:ok true :stepper/status :ready
   :obligation {:obligation/action
                {:kind :preflight :phase :preflight :frame-id "f19"}}})

(defn base [calls driven]
  {:launch-audit-fn (fn [] (swap! calls conj :audit) {:ok true})
   :inspect-fn (fn [] (swap! calls conj :inspect) inspection)
   :drive-phase-fn (fn [_] (swap! calls conj :drive) driven)
   :advance-fn (fn [_ _] (swap! calls conj :advance) {:ok true})
   :project-fn (fn [] (swap! calls conj :project) {:ok true})
   :park-fn (fn [request] (swap! calls conj [:park request]) {:ok true})
   :continuation-payload "continue-f19"})

(deftest awaiting-phase-parks-on-exact-job-without-advance
  (let [calls (atom [])
        result (sut/tick! (base calls {:ok true :status :awaiting-terminal
                                      :state {:ticket {:job-id "job-19"}}}))]
    (is (= :parked (:status result)))
    (is (= "job-19" (:job-id result)))
    (is (= [:audit :inspect :drive :project] (take 4 @calls)))
    (is (not-any? #{:advance} @calls))
    (is (= ["job-19"] (get-in (last @calls) [1 :awaiting])))))

(deftest nested-solver-round-parks-on-its-active-ticket
  (let [calls (atom [])
        result (sut/tick!
                (base calls {:ok true :status :awaiting-terminal
                             :state {:state/type :solver-rounds
                                     :active {:ticket {:job-id "solve-round-2"}}}}))]
    (is (= :parked (:status result)))
    (is (= "solve-round-2" (:job-id result)))
    (is (= ["solve-round-2"] (get-in (last @calls) [1 :awaiting])))))

(deftest awaiting-phase-without-job-id-fails-before-park
  (let [calls (atom [])
        result (sut/tick! (base calls {:ok true :status :awaiting-terminal
                                      :state {:state/type :solver-rounds}}))]
    (is (= :live-supervisor-job-id-missing (:error/code result)))
    (is (not-any? #(and (vector? %) (= :park (first %))) @calls))))

(deftest awaiting-apparatus-repair-projects-and-parks-for-immediate-retry
  (let [calls (atom [])
        finding {:findings [:review-patterns-invalid]}
        result (sut/tick!
                (base calls {:ok true :status :awaiting-apparatus-repair
                             :finding finding}))]
    (is (= :parked (:status result)))
    (is (= finding (:apparatus-repair result)))
    (is (= [:audit :inspect :drive :project] (take 4 @calls)))
    (is (not-any? #{:advance} @calls))
    (is (= [] (get-in (last @calls) [1 :awaiting])))
    (is (nil? (get-in (last @calls) [1 :retry/not-before-ms])))))

(deftest certified-phase-defers-successor-projection-until-it-is-driven
  (let [calls (atom [])
        result (sut/tick! (base calls {:ok true :status :certified
                                      :certificate {:receipt/id "r1"}}))]
    (is (= :phase-advanced (:status result)))
    (is (= [:audit :inspect :drive :advance]
           (take 4 @calls)))
    (is (:projection/deferred? result))
    (is (not-any? #{:project} @calls))
    (is (= [] (get-in (last @calls) [1 :awaiting])))))

(deftest terminal-collection-is-persisted-progress-not-an-invalid-phase-status
  (let [calls (atom [])
        result (sut/tick! (base calls {:ok true :status :terminal-collected
                                      :collection {:collection/id "collection-1"}}))]
    (is (= :terminal-collected (:status result)))
    (is (= "collection-1" (get-in result [:collection :collection/id])))
    (is (= [:audit :inspect :drive :project] (take 4 @calls)))
    (is (not-any? #{:advance} @calls))
    (is (= [] (get-in (last @calls) [1 :awaiting])))))

(deftest scheduled-transport-retry-remains-nonterminal-and-pins-deadline
  (let [calls (atom [])
        result (sut/tick!
                (base calls
                      {:ok true :status :transport-retry-scheduled
                       :state {:transport-retry/not-before-ms 601000}}))]
    (is (= :transport-retry-scheduled (:status result)))
    (is (= [:audit :inspect :drive :project] (take 4 @calls)))
    (is (not-any? #{:advance} @calls))
    (is (= [] (get-in (last @calls) [1 :awaiting])))
    (is (= 601000 (get-in (last @calls) [1 :retry/not-before-ms])))))

(deftest scheduled-provider-transport-retry-preserves-provider-boundary
  (let [calls (atom [])
        result (sut/tick!
                (base calls
                      {:ok true :status :transport-retry-scheduled
                       :state {:state/type :live-job-dispatched}
                       :provider/state
                       {:transport-retry/not-before-ms 601000
                        :transport-retry/attempt 0
                        :transport-retry/max-attempts 3
                        :transport-retry/history [{:attempt 0}]}}))]
    (is (= :transport-retry-scheduled (:status result)))
    (is (= 601000 (:retry/not-before-ms result)))
    (is (= {:attempt 0 :max-attempts 3} (:transport-retry result)))
    (is (= [{:attempt 0}] (:transport-retry/history result)))
    (is (= 601000 (get-in (last @calls) [1 :retry/not-before-ms])))))

(deftest provider-usage-limit-parks-until-recorded-resumption-time
  (let [calls (atom [])
        result (sut/tick!
                (base calls
                      {:ok true :status :awaiting-substrate
                       :substrate/condition
                       {:condition/type :provider-usage-limit
                        :resume-at-ms 18001000}}))]
    (is (= :awaiting-substrate (:status result)))
    (is (= 18001000 (:substrate/resume-at-ms result)))
    (is (= [:audit :inspect :drive :project] (take 4 @calls)))
    (is (not-any? #{:advance} @calls))
    (is (= [] (get-in (last @calls) [1 :awaiting])))
    (is (= 18001000
           (get-in (last @calls) [1 :retry/not-before-ms])))))

(deftest phase-status-vocabulary-is-closed-and-unknowns-name-the-gap
  (is (= #{:awaiting-terminal :awaiting-apparatus-repair :awaiting-substrate
           :transport-retry-scheduled :terminal-collected :certified}
         (phase-status/known-statuses :phase-driver)))
  (is (= :waiting-substrate
         (phase-status/classify :phase-driver :awaiting-substrate)))
  (is (= :unknown (phase-status/classify :phase-driver
                                         :new-producer-status)))
  (let [calls (atom [])
        result (sut/tick!
                (base calls {:ok true :status :new-producer-status}))]
    (is (= :live-supervisor-phase-status-vocabulary-incomplete
           (:error/code result)))
    (is (= :new-producer-status (get-in result [:finding :status])))
    (is (= :unknown (get-in result [:finding :classification])))
    (is (= (vec (sort (phase-status/known-statuses :phase-driver)))
           (get-in result [:finding :known-statuses])))
    (is (not-any? #(and (vector? %) (= :park (first %))) @calls))))

(deftest status-propagation-closure-names-the-rejecting-consumer
  (is (empty? (phase-status/closure-findings)))
  (let [driver-only
        (assoc-in phase-status/boundary-status-classes
                  [:phase-driver :awaiting-new-substrate] :waiting-substrate)
        missing-mapping
        (phase-status/closure-findings driver-only phase-status/propagation)]
    (is (= [{:error/code :phase-status-propagation-mapping-missing
             :producer :phase-driver
             :consumer :live-supervisor-frame
             :status :awaiting-new-substrate}]
           (filterv #(= :awaiting-new-substrate (:status %))
                    missing-mapping))))
  (let [vocabulary (-> phase-status/boundary-status-classes
                       (assoc-in [:phase-driver :awaiting-new-substrate]
                                 :waiting-substrate)
                       (assoc-in [:live-supervisor-frame
                                  :awaiting-new-substrate]
                                 :waiting-substrate))
        routes (mapv #(if (#{:phase-driver :live-supervisor-frame}
                            (:producer %))
                        (update % :mapping assoc
                                :awaiting-new-substrate
                                :awaiting-new-substrate)
                        %)
                     phase-status/propagation)
        downstream (phase-status/closure-findings vocabulary routes)]
    (is (= #{:problem-queue-frame :live-batch-frame}
           (set (map :consumer
                     (filter #(= :awaiting-new-substrate (:output-status %))
                             downstream)))))))

(deftest audit-and-projection-failures-stop-without-routing-around
  (testing "audit"
    (let [calls (atom [])
          result (sut/tick! (assoc (base calls {})
                                   :launch-audit-fn #(do (swap! calls conj :audit)
                                                         {:ok false})))]
      (is (= :live-supervisor-launch-audit-failed (:error/code result)))
      (is (= [:audit] @calls))))
  (testing "projection"
    (let [calls (atom [])
          result (sut/tick! (assoc (base calls {:ok true
                                                :status :awaiting-terminal
                                                :job-id "job-19"})
                                   :project-fn #(do (swap! calls conj :project)
                                                    {:ok false})))]
      (is (= :live-supervisor-projection-failed (:error/code result)))
      (is (not-any? #(and (vector? %) (= :park (first %))) @calls)))))

(deftest exact-active-claim-can-use-typed-recovery
  (let [calls (atom [])
        recovering {:ok true :stepper/status :stop
                    :decision {:reason :campaign-obligation-claim-recovery-required}}
        result (sut/tick!
                (assoc (base calls {})
                       :inspect-fn (constantly recovering)
                       :recover-claim-fn
                       (fn [_] (swap! calls conj :recover) {:ok true})))]
    (is (= :claim-recovered (:status result)))
    (is (= [:audit :recover :project] (take 3 @calls)))
    (is (= [] (get-in (last @calls) [1 :awaiting])))))
