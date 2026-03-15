(ns futon3c.dev.fm-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.agents.tickle-queue :as tq]
            [futon3c.dev.fm :as fm]))

(defn- structural-aggregate
  [& {:keys [dispatchable needs-review informational]
      :or {dispatchable [] needs-review [] informational []}}]
  {:reports []
   :summary {:active 1
             :dormant 0
             :violating 1
             :clean-active 0
             :obligations-total (+ (count dispatchable)
                                   (count needs-review)
                                   (count informational))
             :auto-fixable (count dispatchable)
             :needs-review (count needs-review)
             :informational (count informational)
             :dispatchable-tasks (count dispatchable)}
   :obligations []
   :obligations-by-actionability {:auto-fixable dispatchable
                                  :needs-review needs-review
                                  :informational informational}
   :dispatchable-tasks dispatchable})

(deftest sync-structural-law-tasks-upserts-and-cleans-pending
  (testing "dispatchable structural-law tasks are added, refreshed, and stale pending ones removed"
    (tq/clear!)
    (tq/add-task! {:id "INV-old"
                   :label "Old structural task"
                   :source "structural-law/mission"
                   :priority :high})
    (tq/add-task! {:id "MANUAL-1"
                   :label "Manual task"
                   :source "manual"
                   :priority :normal})
    (let [aggregate (structural-aggregate
                     :dispatchable [{:id "INV-new"
                                     :label "Repair mission blocker"
                                     :priority :high
                                     :source "structural-law/mission"
                                     :depends-on #{}
                                     :domain-id :mission
                                     :violation-key :missing-blockers
                                     :actionability :auto-fixable
                                     :family :existence
                                     :payload ["C1" "O-x" :blocker]}])
          result (fm/sync-structural-law-tasks! aggregate)]
      (is (= {:desired-count 1
              :task-ids ["INV-new"]}
             result))
      (is (nil? (tq/task "INV-old")))
      (is (= "Repair mission blocker" (:label (tq/task "INV-new"))))
      (is (= :pending (:status (tq/task "INV-new"))))
      (is (= "Manual task" (:label (tq/task "MANUAL-1")))))))

(deftest sync-structural-law-tasks-preserves-assigned-work
  (testing "in-flight structural-law tasks stay assigned during refresh"
    (tq/clear!)
    (tq/add-task! {:id "INV-live"
                   :label "Old label"
                   :source "structural-law/mission"
                   :priority :high})
    (tq/pick-task! "codex-1")
    (let [aggregate (structural-aggregate
                     :dispatchable [{:id "INV-live"
                                     :label "New label"
                                     :priority :high
                                     :source "structural-law/mission"
                                     :depends-on #{}
                                     :domain-id :mission
                                     :violation-key :missing-phase-outputs
                                     :actionability :auto-fixable
                                     :family :required-outputs
                                     :payload {:cycle "C1"}}])]
      (fm/sync-structural-law-tasks! aggregate)
      (is (= :assigned (:status (tq/task "INV-live"))))
      (is (= "codex-1" (:assignee (tq/task "INV-live"))))
      (is (= "New label" (:label (tq/task "INV-live")))))))

(deftest refresh-structural-law-tasks-updates-conductor-state
  (testing "the conductor keeps the last invariant sync summary for inspection"
    (tq/clear!)
    (let [state (atom {})
          result (fm/refresh-structural-law-tasks!
                  {:conductor-state state
                   :invariant-aggregate-fn
                   (fn []
                     (structural-aggregate
                      :dispatchable [{:id "INV-1"
                                      :label "Repair orphan announcement"
                                      :priority :high
                                      :source "structural-law/codex"
                                      :depends-on #{}
                                      :domain-id :codex
                                      :violation-key :orphan-announcements
                                      :actionability :auto-fixable
                                      :family :cross-store-agreement
                                      :payload ["a-1" "missing-job"]}]
                      :needs-review [{:id "INV-2"}]
                      :informational [{:id "INV-3"}]))})]
      (is (= 1 (:dispatchable-count result)))
      (is (= 1 (:needs-review-count result)))
      (is (= 1 (:informational-count result)))
      (is (= result (:last-invariant-sync @state)))
      (is (number? (:last-invariant-sync-ms @state)))
      (is (= "Repair orphan announcement" (:label (tq/task "INV-1")))))))

(deftest make-fm-conductor-config-builds-default-invariant-hook
  (testing "domain specs can be injected and compiled into an aggregate hook"
    (let [cfg (fm/make-fm-conductor-config
               {:invariant-load-profile {:mission true}
                :invariant-domains [{:domain-id :mission
                                     :input nil
                                     :check (fn [_]
                                              {:missing-blockers [["C1" "O-x" :blocker]]})}]}
               {})
          aggregate ((:invariant-aggregate-fn cfg))]
      (is (= 1 (get-in aggregate [:summary :dispatchable-tasks])))
      (is (= "Repair mission cycle blocker reference: [\"C1\" \"O-x\" :blocker]"
             (get-in aggregate [:dispatchable-tasks 0 :label]))))))
