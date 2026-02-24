(ns futon3c.agents.tickle-orchestrate-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.agency.registry :as reg]
            [futon3c.agents.tickle-orchestrate :as orch]
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
