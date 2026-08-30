(ns futon3c.apm.post-f58-lifecycle-qualification-test
  "Hermetic qualification for the F59 apparatus-invalidated lifecycle.

  The production F59 files are evidence only.  Every mutable path in this
  namespace is a fresh temporary directory, and no coordinator is started."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.durable-coordinator :as coordinator]
            [futon3c.apm.frame-void :as frame-void]
            [futon3c.apm.live-regulator :as regulator]
            [futon3c.apm.problem-queue-supervisor :as queue])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-paths []
  (let [root (Files/createTempDirectory
              "post-f58-lifecycle-" (make-array FileAttribute 0))]
    {:root root
     :registry (str (.resolve root "registry.edn"))
     :state (str (.resolve root "coordinator-state.edn"))}))

(def problems
  [{:problem/id "b00J01" :repository "/tmp/hermetic-repo"
    :revision "f59-frozen" :path "b00J01.lean" :blob "blob-f59"
    :classification :non-excluded}
   {:problem/id "b00J02" :repository "/tmp/hermetic-repo"
    :revision "successor-revision" :path "b00J02.lean" :blob "blob-next"
    :classification :non-excluded}])

(defn- queue-harness []
  (let [plan (queue/queue-plan problems)
        state (atom nil)
        calls (atom [])
        providers
        {:plan plan
         :state-provider #(deref state)
         :persist-state-fn #(do (reset! state %)
                                (swap! calls conj [:persist])
                                {:ok true})
         :mint-frame-fn
         (fn [{:keys [problem ordinal]}]
           (swap! calls conj [:mint (:problem/id problem)])
           {:ok true
            :frame {:frame/id (str "f" (+ 59 ordinal))
                    :problem/id (:problem/id problem)
                    :problem problem}})
         :qualify-frame-fn (constantly {:ok true})
         :prepare-frame-fn
         (fn [frame] {:ok true :preparation/id
                      (str "prep-" (:frame/id frame))})
         :frame-tick-fn
         (constantly
          {:ok true :status :frame-complete :frame/result :void
           :terminal-receipt
           {:receipt/id "f59-apparatus-void"
            :frame/result :void :problem/outcome :unsolved
            :learning/outcome :skipped
            :void/classification :apparatus-invalidated
            :void/failed-invariants [:post-f58-stop-boundary-violated]}})
         :retire-frame-fn (constantly {:ok true})}]
    {:plan plan :state state :calls calls :providers providers}))

(deftest typed-apparatus-void-retains-diagnostic-certificate
  (let [projection {:projection/status :valid
                    :campaign/id "qualification"
                    :campaign/version 15
                    :ledger/digest "frozen-f59-ledger"
                    :active/frame {:frame-id "f59" :problem-id "b00J01"}}
        result (frame-void/prepare
                {:projection projection :events (vec (repeat 15 {}))}
                {:frame-id "f59" :problem-id "b00J01"
                 :expected-version 15
                 :expected-ledger-digest "frozen-f59-ledger"
                 :classification :apparatus-invalidated
                 :failures [:post-f58-stop-boundary-violated]
                 :actor "qualification" :now "2026-08-30T11:00:08Z"})]
    (is (:ok result) (pr-str result))
    (is (= :apparatus-invalidated
           (get-in result [:certificate :classification])))
    (is (= [:post-f58-stop-boundary-violated]
           (get-in result [:certificate :failed-invariants])))
    (is (= "frozen-f59-ledger"
           (get-in result [:certificate :source/ledger-digest])))
    (is (= :apparatus-invalidated
           (get-in result [:event :event/body :reason])))))

(deftest voided-active-frame-stays-paused-until-explicit-resume
  (let [{:keys [providers state calls]} (queue-harness)]
    (is (= :frame-prepared (:status (queue/tick! providers))))
    (is (= "b00J01" (get-in @state [:active :frame :problem/id])))
    ((:persist-state-fn providers)
     (:state (queue/pause-after-active @state)))
    (let [voided (queue/tick! providers)
          mint-count (count (filter #(= :mint (first %)) @calls))]
      (is (= :batch-paused (:status voided)))
      (is (= :paused (:status @state)))
      (is (nil? (:active @state)))
      (is (empty? (:completed @state))
          "apparatus-invalidated frames are diagnostic, not experiments")
      (is (= 1 (:next-index @state)))
      (is (= :batch-paused (:status (queue/tick! providers))))
      (is (= mint-count (count (filter #(= :mint (first %)) @calls)))
          "a paused queue cannot silently reuse or advance a slot")
      (let [resumed (queue/resume-paused @state)]
        (is (:ok resumed))
        (is (= 1 (get-in resumed [:state :next-index])))
        (is (nil? (get-in resumed [:state :active])))
        ((:persist-state-fn providers) (:state resumed)))
      (is (= mint-count (count (filter #(= :mint (first %)) @calls)))
          "resume changes durable authority but does not itself dispatch")
      (is (= :frame-prepared (:status (queue/tick! providers))))
      (is (= "b00J02" (get-in @state [:active :frame :problem/id])))
      (is (= [[:mint "b00J01"] [:mint "b00J02"]]
             (filterv #(= :mint (first %)) @calls))))))

(deftest durable-stop-preserves-frame-and-recovery-refuses-dispatch
  (let [{:keys [registry state]} (temp-paths)
        coordinator-id "qualification:f59"
        dispatches (atom 0)
        active-frame {:frame/id "f59" :problem/id "b00J01"
                      :phase :guide-intervention-1}]
    (coordinator/register-adapter!
     :qualification/no-dispatch
     (fn [_]
       {:decide-fn (fn [_]
                     (swap! dispatches inc)
                     {:ok true :status :must-not-run})
        :reconcile-fn (fn [_ _]
                        (swap! dispatches inc)
                        {:ok true :status :must-not-run})}))
    (is (:ok (coordinator/register!
              {:registry-path registry :coordinator-id coordinator-id
               :adapter :qualification/no-dispatch :config {}
               :state-path state :period-ms 10})))
    (spit state
          (str (pr-str (assoc (regulator/initial-state coordinator-id)
                              :active/frame active-frame)) "\n"))
    (binding [coordinator/*watchdog-stop-fn*
              (fn [_] {:ok true :status :stopped})
              coordinator/*quiescence-now-fn*
              (constantly "2026-08-30T11:05:00Z")]
      (with-redefs [regulator/cancel-scheduler!
                    (fn [_] {:ok true :status :stopped})]
        (let [stopped (coordinator/stop! registry coordinator-id)
              durable (edn/read-string (slurp state))
              recovered (coordinator/recover-all! registry)]
          (is (= :stopped (:status stopped)))
          (is (true? (:durably-disabled? stopped)))
          (is (= active-frame (:active/frame durable)))
          (is (= :stopped (:regulator/status durable)))
          (is (= :durable-quiescence-witness
                 (get-in durable
                         [:regulator/quiescence-witness :state/type])))
          (is (nil? (:regulator/tick-claim durable)))
          (is (= :disabled
                 (get-in recovered [:results coordinator-id :status])))
          (is (zero? @dispatches)
              "startup recovery may not dispatch while durably disabled"))))))
