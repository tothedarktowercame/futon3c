(ns futon3c.transport.work-order-nudges-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.agency.parked-on :as parked-on]
            [futon3c.agency.registry :as reg]
            [futon3c.agency.work-orders :as wo]
            [futon3c.social.mesh-test-fixtures :as mesh-fixtures]
            [futon3c.transport.http :as http]))

(defn- fresh [f]
  (wo/reset-ledger!)
  (f))
(use-fixtures :each mesh-fixtures/with-store fresh)

(defn- terminal-job! [job-id agent]
  (#'http/create-invoke-job! {:requested-job-id job-id :agent-id agent
                              :prompt "turn" :caller "joe" :surface "test"
                              :mode "brief"})
  (#'http/mark-invoke-job-running! job-id)
  (#'http/finalize-invoke-job! job-id "done" nil nil {:ok true} nil))

(defn- root [nudges]
  {:id "wo-root" :requester "joe" :debtor "codex-19" :parent nil
   :job-id "root-job" :text "finish the work" :opened-at 1000 :moved-at 1000
   :state :open :closed-by nil :nudges nudges})

(defn- child [state bellback]
  {:id "wo-child" :requester "codex-19" :debtor "codex-20" :parent "wo-root"
   :job-id "child-job" :bellback-job-id bellback :text "child"
   :opened-at 2000 :moved-at 2000 :state state :closed-by nil :nudges []})

(defn- run-case! [orders enabled?]
  (let [sent (atom [])]
    (reset! wo/!orders orders)
    (with-redefs-fn {#'http/!invoke-jobs-ledger (atom (#'http/default-invoke-jobs-ledger))
                     #'http/!active-invoke-job-index (atom nil)
                     #'http/persist-invoke-jobs-ledger! (constantly true)
                     #'http/read-commission-archive (constantly nil)
                     #'http/*schedule-work-order-check!* (fn [agent]
                                                          (#'http/run-work-order-check! agent))
                     #'http/*enqueue-work-order-bell!* #(swap! sent conj %)
                     #'http/work-order-nudges-enabled? (constantly enabled?)
                     #'http/parked-on-notify! (constantly {})
                     #'http/auto-bellback-enabled? (constantly false)
                     #'http/inbox-agent? (constantly false)
                     #'parked-on/snapshot (constantly {:records {}})
                     #'reg/get-agent (constantly nil)}
      #(terminal-job! "ending" "codex-19"))
    @sent))

(deftest finalize-settles-e2-before-one-shot-nudge-and-escalation
  (is (= {:agent-id "codex-19" :prompt "continue" :caller "work-orders"
          :surface "work-orders"}
         (#'http/work-order-bell-request {:to "codex-19" :text "continue"})))
  (let [sent (run-case! {"wo-root" (root [])
                         "wo-child" (child :delivered "ending")} true)]
    (is (= 1 (count sent)))
    (is (= "wo-root" (:order (first sent)))
        "the just-closed child is not nudged; E2 ran before the check")
    (is (= :closed (get-in @wo/!orders ["wo-child" :state])))
    (is (= :nudge (get-in @wo/!orders ["wo-root" :nudges 0 :kind]))))
  (let [acts (atom [])]
    (binding [wo/*append-act!* #(swap! acts conj %)]
      (is (empty? (run-case! {"wo-root" (root [{:at 4000 :to "codex-19" :kind :nudge}])}
                             true)))
      (is (= :escalate (get-in @wo/!orders ["wo-root" :nudges 1 :kind])))
      (is (= :report-problem (get-in (last @acts) [:evidence/body :kind]))))))

(deftest dispatch-movement-resets-escalation-and-off-switch-suppresses
  ;; codex-19 moved after its nudge (dispatched new-child), so it is not
  ;; escalated. It is nudged again: an open child does not excuse an idle,
  ;; unparked holder (Joe, 2026-10-04: "you will not wait for them").
  (let [sent (run-case! {"wo-root" (root [{:at 1500 :to "codex-19" :kind :nudge}])
                         "new-child" (assoc (child :open nil) :id "new-child"
                                            :job-id "new" :moved-at 5000)} true)]
    (is (= [:nudge] (map :action sent)))
    (is (= "wo-root" (:order (first sent)))))
  (is (empty? (run-case! {"wo-root" (root [])} false)))
  (is (empty? (:nudges (get @wo/!orders "wo-root")))))

(deftest swapped-e2-check-order-is-the-constructed-bad-case
  (let [sent (atom [])]
    (reset! wo/!orders {"wo-root" (root [])
                        "wo-child" (child :delivered "ending")})
    (with-redefs-fn {#'http/*enqueue-work-order-bell!* #(swap! sent conj %)
                     #'http/work-order-nudges-enabled? (constantly true)
                     #'http/work-order-agent-state (constantly {:running-jobs 0
                                                                 :queued-jobs 0
                                                                 :parked? false})}
      #(do
         ;; This deliberately models the rejected ordering: E3 before E2.
         (#'http/run-work-order-check! "codex-19")
         (is (= "wo-child" (:order (first @sent)))
             "the bad ordering nudges about the child whose bellback is ending")))
    (wo/reset-ledger!)
    (let [sent-after (run-case! {"wo-root" (root [])
                                 "wo-child" (child :delivered "ending")} true)]
      (is (= "wo-root" (:order (first sent-after)))
          "production finalize runs E2 first, so the closed child cannot be selected"))))
