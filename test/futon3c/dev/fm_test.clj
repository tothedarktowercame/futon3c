(ns futon3c.dev.fm-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.agents.mfuton-prompt-override :as mfuton-prompt-override]
            [futon3c.agents.tickle-queue :as tq]
            [futon3c.dev.config :as config]
            [futon3c.dev.irc :as dev-irc]
            [futon3c.dev.fm :as fm]
            [futon3c.mfuton-mode :as mfuton-mode]))

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

(deftest task->prompt-mfuton-mode-rewrites-git-language
  (testing "mfuton mode keeps Git as truth while rewriting publication toward the gh commit algorithm"
    (with-redefs [mfuton-mode/mfuton-mode (constantly "mfuton")]
      (let [prompt (fm/task->prompt {:id "F1-opposite"
                                     :label "Falsification attempt"
                                     :source "proof-ledger"
                                     :depends-on #{"F0-root"}})]
        (is (re-find #"Git is truth" prompt))
        (is (re-find #"run the commit algorithm for gh when done" prompt))
        (is (not (re-find #"Push results to git when done" prompt)))))))

(deftest fm-dispatch-message-override-keeps-git-as-truth
  (testing "the page-style assignment message keeps Git as truth while redirecting publication wording"
    (let [message (mfuton-prompt-override/fm-dispatch-message-override
                   "@codex F1-opposite: Falsification attempt. Push results to git when done.")]
      (is (re-find #"Git is truth" message))
      (is (re-find #"run the commit algorithm for gh when done" message))
      (is (not (re-find #"Push results to git when done" message))))))

(deftest task->prompt-default-mode-preserves-original-git-language
  (testing "default mfuton mode leaves the original futon task prompt unchanged"
    (with-redefs [mfuton-mode/mfuton-mode (constantly "futon")]
      (let [prompt (fm/task->prompt {:id "F1-opposite"
                                     :label "Falsification attempt"
                                     :source "proof-ledger"
                                     :depends-on #{"F0-root"}})]
        (is (re-find #"Push results to git when done" prompt))
        (is (not (re-find #"run the commit algorithm for gh when done" prompt)))))))

(deftest handle-claim-prompt-accepts-math-lane-claims
  (testing "tickle honors the existing #math claim text only on the math-irc lane"
    (let [conductor-state (atom {})
          original @fm/!fm-conductor]
      (reset! fm/!fm-conductor {:conductor-state conductor-state})
      (try
        (with-redefs [config/env-bool (fn [k default]
                                        (if (= k "MATH_IRC") true default))
                      fm/fm-assignable-obligations (fn [_]
                                                     [{:item/id "T3-general"
                                                       :item/label "Tier 3"}])]
          (let [response (fm/handle-claim-prompt!
                          "[Surface: IRC | Channel: #math | Speaker: codex | Mode: brief]\n\ncodex: I'll take T3-general"
                          "fm-s-1")]
            (is (= {:agent-id "codex-1"
                    :nick "codex"
                    :label "Tier 3"
                    :claimed-at (get-in @conductor-state [:claimed-obligations "T3-general" :claimed-at])}
                   (get-in @conductor-state [:claimed-obligations "T3-general"])))
            (is (= :claim (get-in @conductor-state [:last-cycle :action])))
            (is (= "codex-1" (get-in @conductor-state [:last-cycle :target])))
            (is (= "fm-s-1" (:session-id response)))
            (is (= "@codex T3-general: Tier 3. Push results to git when done."
                   (:result response)))))
        (finally
          (reset! fm/!fm-conductor original))))))

(deftest handle-claim-prompt-accepts-current-runtime-surface-claims
  (testing "the current IRC runtime surface still routes #math claims through the dedicated seam"
    (let [conductor-state (atom {})
          original @fm/!fm-conductor]
      (reset! fm/!fm-conductor {:conductor-state conductor-state})
      (try
        (with-redefs [config/env-bool (fn [k default]
                                        (if (= k "MATH_IRC") true default))
                      fm/fm-assignable-obligations (fn [_]
                                                     [{:item/id "T3-general"
                                                       :item/label "Tier 3"}])]
          (let [response (fm/handle-claim-prompt!
                          (str "Runtime surface contract:\n"
                               "- Current surface: IRC.\n"
                               "- Channel: #math\n"
                               "- Sender: codex\n\n"
                               "codex: I'll take T3-general")
                          "fm-s-1b")]
            (is (= "fm-s-1b" (:session-id response)))
            (is (= "codex-1" (get-in @conductor-state [:last-cycle :target])))
            (is (= "@codex T3-general: Tier 3. Push results to git when done."
                   (:result response)))))
        (finally
          (reset! fm/!fm-conductor original))))))

(deftest handle-claim-prompt-ignores-non-math-rooms
  (testing "claim-like text outside #math falls through to generic tickle behavior"
    (with-redefs [config/env-bool (fn [k default]
                                    (if (= k "MATH_IRC") true default))]
      (is (nil? (fm/handle-claim-prompt!
                 "[Surface: IRC | Channel: #futon | Speaker: codex | Mode: brief]\n\ncodex: I'll take T3-general"
                 "fm-s-2"))))))

(deftest handle-bell-prompt-acknowledges-math-lane-bells
  (testing "tickle honors the documented BELL phrase only on the math-irc lane"
    (let [conductor-state (atom {})
          original @fm/!fm-conductor]
      (reset! fm/!fm-conductor {:conductor-state conductor-state})
      (try
        (with-redefs [config/env-bool (fn [k default]
                                        (if (= k "MATH_IRC") true default))
                      config/configured-codex-agent-id (fn [] "codex-1")
                      config/configured-codex-relay-nick (fn [] "codex")
                      fm/fm-dispatch-mechanical!
                      (fn [agent-id _config]
                        {:action :page
                         :target agent-id
                         :text "@codex T4-next: Tier 4. Push results to git when done."})]
          (let [response (fm/handle-bell-prompt!
                          "[Surface: IRC | Channel: #math | Speaker: codex_probe | Mode: brief]\n\n@tickle BELL SPEC_VERIFIED"
                          "fm-s-3")]
            (is (= "fm-s-3" (:session-id response)))
            (is (= "BELL SPEC_VERIFIED acknowledged for codex_probe. Next work was posted to #math."
                   (:result response)))
            (is (= {:action :bell
                    :target "codex-1"
                    :event "SPEC_VERIFIED"
                    :dispatch-action :page
                    :text "BELL SPEC_VERIFIED acknowledged for codex_probe. Next work was posted to #math."}
                   (get-in @conductor-state [:last-cycle])))))
        (finally
          (reset! fm/!fm-conductor original))))))

(deftest handle-bell-prompt-accepts-current-runtime-surface-bells
  (testing "the current IRC runtime surface still routes #math Bell prompts through the dedicated seam"
    (let [conductor-state (atom {})
          original @fm/!fm-conductor]
      (reset! fm/!fm-conductor {:conductor-state conductor-state})
      (try
        (with-redefs [config/env-bool (fn [k default]
                                        (if (= k "MATH_IRC") true default))
                      config/configured-codex-agent-id (fn [] "codex-1")
                      config/configured-codex-relay-nick (fn [] "codex")
                      fm/fm-dispatch-mechanical!
                      (fn [agent-id _config]
                        {:action :page
                         :target agent-id
                         :text "@codex T4-next: Tier 4. Push results to git when done."})]
          (let [response (fm/handle-bell-prompt!
                          (str "Runtime surface contract:\n"
                               "- Current surface: IRC.\n"
                               "- Channel: #math\n"
                               "- Sender: codex\n\n"
                               "@tickle BELL SPEC_VERIFIED")
                          "fm-s-3b")]
            (is (= "fm-s-3b" (:session-id response)))
            (is (= "BELL SPEC_VERIFIED acknowledged for codex. Next work was posted to #math."
                   (:result response)))
            (is (= :bell (get-in @conductor-state [:last-cycle :action])))))
        (finally
          (reset! fm/!fm-conductor original))))))

(deftest handle-bell-prompt-ignores-non-math-rooms
  (testing "bell-like text outside #math falls through to generic tickle behavior"
    (with-redefs [config/env-bool (fn [k default]
                                    (if (= k "MATH_IRC") true default))]
      (is (nil? (fm/handle-bell-prompt!
                 "[Surface: IRC | Channel: #futon | Speaker: codex | Mode: brief]\n\n@tickle BELL SPEC_VERIFIED"
                 "fm-s-4"))))))

(deftest handle-bell-prompt-accepts-bare-bell-phrase
  (testing "bridge-shaped bare BELL text still routes through the dedicated math seam"
    (let [conductor-state (atom {})
          original @fm/!fm-conductor]
      (reset! fm/!fm-conductor {:conductor-state conductor-state})
      (try
        (with-redefs [config/env-bool (fn [k default]
                                        (if (= k "MATH_IRC") true default))
                      config/configured-codex-agent-id (fn [] "codex-1")
                      config/configured-codex-relay-nick (fn [] "codex")
                      fm/fm-dispatch-mechanical!
                      (fn [agent-id _config]
                        {:action :pass
                         :target agent-id})]
          (let [response (fm/handle-bell-prompt!
                          "[Surface: IRC | Channel: #math | Speaker: codex_probe | Mode: brief]\n\nBELL SPEC_VERIFIED"
                          "fm-s-5")]
            (is (= "fm-s-5" (:session-id response)))
            (is (= "BELL SPEC_VERIFIED acknowledged for codex_probe. No new assignable obligations right now."
                   (:result response)))))
        (finally
          (reset! fm/!fm-conductor original))))))

(deftest fm-dispatch-skips-claimed-obligations
  (testing "manual FM dispatch does not re-offer an obligation that was already claimed"
    (let [sent (atom [])
          conductor-state (atom {:claimed-obligations
                                 {"T3-general" {:agent-id "codex-1"
                                                :nick "codex"
                                                :label "Tier 3"
                                                :claimed-at "2026-03-22T00:00:00Z"}}})
          original @fm/!fm-conductor]
      (reset! fm/!fm-conductor {:conductor-state conductor-state})
      (try
        (with-redefs [fm/fm-assignable-obligations (fn [_]
                                                     [{:item/id "T3-general" :item/label "Tier 3"}
                                                      {:item/id "T4-next" :item/label "Tier 4"}])
                      dev-irc/make-bridge-irc-send-fn
                      (fn []
                        (fn [channel from text]
                          (swap! sent conj {:channel channel :from from :text text})))]
          (is (= {:assigned "T4-next" :label "Tier 4"}
                 (fm/fm-dispatch! "FM-001")))
          (is (= [{:channel "#math"
                   :from "tickle"
                   :text "TASK ASSIGNMENT [FM-001 / T4-next]: Tier 4. Current mode: FALSIFY. Who wants to take this? Claim with @tickle I'll take T4-next"}]
                 @sent)))
        (finally
          (reset! fm/!fm-conductor original))))))

(deftest dispatch-task-defaults-to-futon-room
  (testing "legacy queue dispatch stays on #futon outside FrontierMath mode"
    (tq/clear!)
    (let [sent (atom [])]
      (try
        (tq/add-task! {:id "T3-general"
                       :label "Tier 3"
                       :source "proof-ledger"
                       :priority :high})
        (with-redefs [config/env-bool (fn [_ default] default)
                      fm/refresh-structural-law-tasks! (fn [_] nil)
                      futon3c.agency.registry/invoke-agent! (fn [_ _ _] {:ok true :result "ok"})]
          (is (= {:action :dispatch
                  :task-id "T3-general"
                  :agent-id "codex-1"
                  :label "Tier 3"}
                 (fm/dispatch-task! "codex-1"
                                    {:bridge-send-fn (fn [channel from text]
                                                       (swap! sent conj {:channel channel
                                                                         :from from
                                                                         :text text}))})))
          (is (= "#futon" (:channel (first @sent)))))
        (finally
          (tq/clear!))))))

(deftest dispatch-task-honors-math-room-when-enabled
  (testing "legacy queue dispatch is rebound to #math only on the FrontierMath lane"
    (tq/clear!)
    (let [sent (atom [])]
      (try
        (tq/add-task! {:id "T3-general"
                       :label "Tier 3"
                       :source "proof-ledger"
                       :priority :high})
        (with-redefs [config/env-bool (fn [k default]
                                        (if (= k "MATH_IRC") true default))
                      fm/refresh-structural-law-tasks! (fn [_] nil)
                      futon3c.agency.registry/invoke-agent! (fn [_ _ _] {:ok true :result "ok"})]
          (is (= {:action :dispatch
                  :task-id "T3-general"
                  :agent-id "codex-1"
                  :label "Tier 3"}
                 (fm/dispatch-task! "codex-1"
                                    {:bridge-send-fn (fn [channel from text]
                                                       (swap! sent conj {:channel channel
                                                                         :from from
                                                                         :text text}))})))
          (is (= "#math" (:channel (first @sent)))))
        (finally
          (tq/clear!))))))
