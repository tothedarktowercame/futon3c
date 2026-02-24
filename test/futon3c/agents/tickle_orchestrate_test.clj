(ns futon3c.agents.tickle-orchestrate-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.agency.registry :as reg]
            [futon3c.agents.tickle-orchestrate :as orch]
            [futon3c.blackboard :as bb]
            [futon3c.evidence.store :as estore]))

(use-fixtures
  :each
  (fn [f]
    (reg/reset-registry!)
    (estore/reset-store!)
    (f)))

(defn- register-mock-agent!
  "Register an agent with a controlled invoke-fn."
  [agent-id invoke-fn]
  (reg/register-agent!
   {:agent-id {:id/value agent-id :id/type :continuity}
    :type :mock
    :invoke-fn invoke-fn
    :capabilities [:coordination/execute]}))

(defn- make-evidence-store []
  (atom {:entries {} :order []}))

(def sample-issue
  {:number 99
   :title "Test issue: add a button"
   :body "## Scope\n\nAdd a button to index.html.\n\n## Criteria\n\n- [ ] Button exists"
   :labels ["codex"]})

(defn- append-orch!
  "Append a synthetic orchestration entry at `at` with event keyword."
  [store at event body]
  (estore/append*
   store
   {:evidence/id (str (gensym "e-"))
    :evidence/subject {:ref/type :task :ref/id "test#1"}
    :evidence/type :coordination
    :evidence/claim-type :observation
    :evidence/author "tickle-1"
    :evidence/at at
    :evidence/body body
    :evidence/tags [:tickle :orchestrate event]})
  store)

;; =============================================================================
;; tickle-status
;; =============================================================================

(deftest tickle-status-empty-store
  (testing "tickle-status returns zeroed summary when no evidence exists"
    (let [store (make-evidence-store)
          summary (orch/tickle-status store {:now "2026-02-24T12:00:00Z"})]
      (is (nil? (:last-kick-at summary)))
      (is (nil? (:last-complete-at summary)))
      (is (nil? (:last-agent summary)))
      (is (= 0 (:issues-completed summary)))
      (is (= 0 (:issues-failed summary)))
      (is (= 0 (:total-kicks summary)))
      (is (= 0 (:uptime-seconds summary))))))

(deftest tickle-status-counts-successful-kicks
  (testing "tickle-status summarises successful kick queue"
    (let [store (-> (make-evidence-store)
                    (append-orch! "2026-02-24T10:00:00Z" :kick-start {:agent "codex-1"})
                    (append-orch! "2026-02-24T10:05:00Z" :kick-complete {:status :done})
                    (append-orch! "2026-02-24T11:00:00Z" :kick-start {:agent "codex-2"})
                    (append-orch! "2026-02-24T11:06:00Z" :kick-complete {:status :done}))
          summary (orch/tickle-status store {:now "2026-02-24T12:00:00Z"})]
      (is (= "2026-02-24T11:00:00Z" (:last-kick-at summary)))
      (is (= "2026-02-24T11:06:00Z" (:last-complete-at summary)))
      (is (= "codex-2" (:last-agent summary)))
      (is (= 2 (:issues-completed summary)))
      (is (= 0 (:issues-failed summary)))
      (is (= 2 (:total-kicks summary)))
      (is (= 7200 (:uptime-seconds summary))))))

(deftest tickle-status-counts-mixed-outcomes
  (testing "tickle-status differentiates successful vs failed completions"
    (let [store (-> (make-evidence-store)
                    (append-orch! "2026-02-24T10:00:00Z" :workflow-start {:agent "codex-3"})
                    (append-orch! "2026-02-24T10:15:00Z" :workflow-complete {:status :complete})
                    (append-orch! "2026-02-24T11:00:00Z" :kick-start {:agent "codex-4"})
                    (append-orch! "2026-02-24T11:05:00Z" :kick-complete {:status :failed}))
          summary (orch/tickle-status store {:now "2026-02-24T12:00:00Z"})]
      (is (= "2026-02-24T11:00:00Z" (:last-kick-at summary)))
      (is (= "2026-02-24T11:05:00Z" (:last-complete-at summary)))
      (is (= "codex-4" (:last-agent summary)))
      (is (= 1 (:issues-completed summary)))
      (is (= 1 (:issues-failed summary)))
      (is (= 2 (:total-kicks summary)))
      (is (= 7200 (:uptime-seconds summary))))))

(deftest report-status-broadcasts-message-and-emits-evidence
  (testing "report-status! posts to IRC and records evidence"
    (let [store (-> (make-evidence-store)
                    (append-orch! "2026-02-24T10:00:00Z" :kick-start {:agent "codex-1"})
                    (append-orch! "2026-02-24T10:01:00Z" :kick-complete {:status :done})
                    (append-orch! "2026-02-24T11:00:00Z" :kick-start {:agent "codex-2"})
                    (append-orch! "2026-02-24T11:02:00Z" :kick-complete {:status :failed}))
          sent (atom [])]
      (let [result (orch/report-status!
                    {:evidence-store store
                     :send-to-channel! (fn [ch from text]
                                         (swap! sent conj {:ch ch :from from :text text}))
                     :room "#status"
                     :repo-dir "/tmp/repo"
                     :session-id "session-report"
                     :now "2026-02-24T12:00:00Z"})]
        (is (= "Tickle: 2 kicks (1 ok, 1 failed), last: codex-2 @ 2026-02-24T11:02:00Z"
               (:message result)))
        (is (= 1 (count @sent)))
        (is (= {:ch "#status" :from "tickle-1"
                :text "Tickle: 2 kicks (1 ok, 1 failed), last: codex-2 @ 2026-02-24T11:02:00Z"}
               (first @sent)))
        (let [entries (estore/query* store {})
              report-entry (some #(when (= :status-report (last (:evidence/tags %))) %) entries)]
          (is (some? report-entry))
          (is (= :status-report (last (:evidence/tags report-entry))))
          (is (= "tickle-1" (:evidence/author report-entry)))
          (is (= "session-report" (:evidence/session-id report-entry)))
          (is (= "/tmp/repo" (get-in report-entry [:evidence/body :repo])))
          (is (= (:message result) (get-in report-entry [:evidence/body :message])))
          (is (= 2 (get-in report-entry [:evidence/body :summary :total-kicks]))))))))

;; =============================================================================
;; Prompt construction (via assign/review)
;; =============================================================================

(deftest assign-issue-invokes-and-emits-evidence
  (testing "assign-issue! invokes agent and emits evidence entries"
    (let [invoked (atom nil)
          store (make-evidence-store)]
      (register-mock-agent!
       "codex-1"
       (fn [prompt _session]
         (reset! invoked prompt)
         {:result "I added the button to index.html."
          :session-id "sess-codex-1"}))
      (let [result (orch/assign-issue!
                    sample-issue
                    {:evidence-store store
                     :repo-dir "/tmp/test-repo"
                     :timeout-ms 5000
                     :session-id "tko-test-1"})]
        ;; Invoke succeeded
        (is (true? (:ok result)))
        (is (string? (:result result)))
        (is (pos? (:elapsed-ms result)))
        ;; Codex was invoked with a prompt containing the issue
        (is (some? @invoked))
        (is (re-find #"Issue #99" @invoked))
        (is (re-find #"/tmp/test-repo" @invoked))
        ;; Evidence emitted (assigned + complete)
        (let [entries (estore/query* store {})]
          (is (= 2 (count entries)))
          (is (some #(= :agent-assigned (last (:evidence/tags %))) entries))
          (is (some #(= :agent-complete (last (:evidence/tags %))) entries)))))))

(deftest assign-issue-handles-failure
  (testing "assign-issue! handles invoke failure gracefully"
    (let [store (make-evidence-store)]
      (register-mock-agent!
       "codex-1"
       (fn [_prompt _session]
         {:error "sandbox timeout" :exit-code 1}))
      (let [result (orch/assign-issue!
                    sample-issue
                    {:evidence-store store
                     :repo-dir "/tmp/test-repo"
                     :timeout-ms 5000
                     :session-id "tko-test-2"})]
        (is (false? (:ok result)))
        ;; Still emits both evidence entries
        (let [entries (estore/query* store {})]
          (is (= 2 (count entries))))))))

(deftest request-review-invokes-and-parses-verdict
  (testing "request-review! invokes claude-1 and parses APPROVE verdict"
    (let [store (make-evidence-store)]
      (register-mock-agent!
       "codex-1"
       (fn [_ _] {:result "done" :session-id "s1"}))
      (register-mock-agent!
       "claude-1"
       (fn [prompt _session]
         {:result "APPROVE — the button was added correctly to index.html."
          :session-id "sess-claude-1"}))
      (let [codex-result {:ok true :result "I added the button."}
            result (orch/request-review!
                    sample-issue codex-result
                    {:evidence-store store
                     :repo-dir "/tmp/test-repo"
                     :timeout-ms 5000
                     :session-id "tko-test-3"})]
        (is (true? (:ok result)))
        (is (= :approve (:verdict result)))
        ;; Evidence: review-assigned + review-complete
        (let [entries (estore/query* store {})]
          (is (= 2 (count entries)))
          (is (some #(= :review-assigned (last (:evidence/tags %))) entries))
          (is (some #(= :review-complete (last (:evidence/tags %))) entries)))))))

(deftest request-review-parses-request-changes
  (testing "request-review! parses REQUEST_CHANGES verdict"
    (let [store (make-evidence-store)]
      (register-mock-agent!
       "claude-1"
       (fn [_ _]
         {:result "REQUEST_CHANGES — the button has no click handler."
          :session-id "s1"}))
      (let [result (orch/request-review!
                    sample-issue {:ok true :result "done"}
                    {:evidence-store store
                     :repo-dir "/tmp/test-repo"
                     :timeout-ms 5000
                     :session-id "tko-test-4"})]
        (is (= :request-changes (:verdict result)))))))

(deftest run-issue-workflow-happy-path
  (testing "run-issue-workflow! runs full pipeline and emits 6 evidence entries"
    (let [store (make-evidence-store)
          irc-messages (atom [])]
      (register-mock-agent!
       "codex-1"
       (fn [_ _] {:result "Implemented the feature." :session-id "s1"}))
      (register-mock-agent!
       "claude-1"
       (fn [_ _] {:result "APPROVE — looks good." :session-id "s2"}))
      (let [result (orch/run-issue-workflow!
                    sample-issue
                    {:evidence-store store
                     :repo-dir "/tmp/test-repo"
                     :send-to-channel! (fn [ch from text]
                                         (swap! irc-messages conj {:ch ch :from from :text text}))
                     :room "#test"})]
        ;; Workflow completed
        (is (= :complete (:status result)))
        (is (= :approve (:verdict result)))
        (is (= 99 (:issue-number result)))
        (is (pos? (:total-elapsed-ms result)))
        ;; 6 evidence entries: start, agent-assigned, agent-complete,
        ;;                     review-assigned, review-complete, workflow-complete
        (let [entries (estore/query* store {})]
          (is (= 6 (count entries)))
          (is (some #(= :workflow-start (last (:evidence/tags %))) entries))
          (is (some #(= :workflow-complete (last (:evidence/tags %))) entries)))
        ;; IRC report sent
        (is (= 1 (count @irc-messages)))
        (is (re-find #"#99" (:text (first @irc-messages))))))))

(deftest run-issue-workflow-codex-failure-stops
  (testing "run-issue-workflow! stops if Codex fails (no review invoked)"
    (let [store (make-evidence-store)
          claude-invoked (atom false)]
      (register-mock-agent!
       "codex-1"
       (fn [_ _] {:error "crash" :exit-code 1}))
      (register-mock-agent!
       "claude-1"
       (fn [_ _]
         (reset! claude-invoked true)
         {:result "APPROVE" :session-id "s2"}))
      (let [result (orch/run-issue-workflow!
                    sample-issue
                    {:evidence-store store
                     :repo-dir "/tmp/test-repo"})]
        (is (= :codex-failed (:status result)))
        ;; Claude was never invoked
        (is (false? @claude-invoked))
        ;; 4 evidence entries: start, agent-assigned, agent-complete, workflow-complete
        (let [entries (estore/query* store {})]
          (is (= 4 (count entries))))))))

(deftest assign-issue-uses-configured-agent
  (testing "assign-issue! invokes the agent specified in :agent-id, not hardcoded codex-1"
    (let [store (make-evidence-store)]
      (register-mock-agent!
       "claude-1"
       (fn [prompt _session]
         {:result "Review complete." :session-id "s1"}))
      (let [result (orch/assign-issue!
                    sample-issue
                    {:agent-id "claude-1"
                     :evidence-store store
                     :repo-dir "/tmp/test-repo"
                     :timeout-ms 5000
                     :session-id "tko-test-agent"})]
        (is (true? (:ok result)))
        (is (= "Review complete." (:result result)))
        ;; Evidence records the actual agent
        (let [entries (estore/query* store {})
              assigned (first (filter #(= :agent-assigned (last (:evidence/tags %))) entries))]
          (is (= "claude-1" (get-in assigned [:evidence/body :agent]))))))))

(deftest kick-invokes-without-review
  (testing "kick! invokes Codex and reports without Claude review"
    (let [store (make-evidence-store)
          claude-invoked (atom false)]
      (register-mock-agent!
       "codex-1"
       (fn [_ _] {:result "I made the CSS changes." :session-id "s1"}))
      (register-mock-agent!
       "claude-1"
       (fn [_ _]
         (reset! claude-invoked true)
         {:result "APPROVE" :session-id "s2"}))
      (let [result (orch/kick! sample-issue
                               {:evidence-store store
                                :repo-dir "/tmp/test-repo"})]
        (is (= :done (:status result)))
        (is (= 99 (:issue-number result)))
        (is (pos? (:total-elapsed-ms result)))
        ;; Claude was NOT invoked
        (is (false? @claude-invoked))
        ;; 4 evidence entries: kick-start, agent-assigned, agent-complete, kick-complete
        (let [entries (estore/query* store {})]
          (is (= 4 (count entries)))
          (is (some #(= :kick-start (last (:evidence/tags %))) entries))
          (is (some #(= :kick-complete (last (:evidence/tags %))) entries)))))))

(deftest kick-projects-status-blackboard
  (testing "kick! updates the *Tickle Status* blackboard buffer"
    (let [store (make-evidence-store)
          buffers (atom [])]
      (register-mock-agent!
       "codex-1"
       (fn [_ _] {:result "done" :session-id "sess"}))
      (let [fake-blackboard
            (fn
              ([buffer content]
               (swap! buffers conj {:buffer buffer :content content :opts {}})
               {:ok true})
              ([buffer content opts]
               (swap! buffers conj {:buffer buffer :content content :opts opts})
               {:ok true}))]
        (with-redefs [bb/blackboard! fake-blackboard]
          (orch/kick! sample-issue
                      {:evidence-store store
                       :repo-dir "/tmp/test-repo"})))
      (let [status-call (some #(when (= "*Tickle Status*" (:buffer %)) %) @buffers)]
        (is (some? status-call))
        (is (true? (:no-display (:opts status-call))))
        (is (re-find #"Last kick" (:content status-call)))))))

(deftest kick-queue-processes-sequentially
  (testing "kick-queue! processes multiple issues in order without review"
    (let [store (make-evidence-store)
          invoke-order (atom [])]
      (register-mock-agent!
       "codex-1"
       (fn [prompt _]
         (let [num (re-find #"#(\d+)" prompt)]
           (swap! invoke-order conj (second num)))
         {:result "done" :session-id "s1"}))
      (let [issues [(assoc sample-issue :number 1 :title "First")
                    (assoc sample-issue :number 2 :title "Second")]
            results (orch/kick-queue! issues
                                      {:evidence-store store
                                       :repo-dir "/tmp/test-repo"})]
        (is (= 2 (count results)))
        (is (every? #(= :done (:status %)) results))
        (is (= ["1" "2"] @invoke-order))))))

(deftest run-batch-processes-sequentially
  (testing "run-batch! processes multiple issues in order"
    (let [store (make-evidence-store)
          invoke-order (atom [])]
      (register-mock-agent!
       "codex-1"
       (fn [prompt _]
         (let [num (re-find #"#(\d+)" prompt)]
           (swap! invoke-order conj (second num)))
         {:result "done" :session-id "s1"}))
      (register-mock-agent!
       "claude-1"
       (fn [_ _] {:result "APPROVE" :session-id "s2"}))
      (let [issues [(assoc sample-issue :number 1 :title "First")
                    (assoc sample-issue :number 2 :title "Second")
                    (assoc sample-issue :number 3 :title "Third")]
            results (orch/run-batch! issues
                                     {:evidence-store store
                                      :repo-dir "/tmp/test-repo"})]
        (is (= 3 (count results)))
        (is (every? #(= :complete (:status %)) results))
        ;; Codex invoked in order
        (is (= ["1" "2" "3"] @invoke-order))))))
