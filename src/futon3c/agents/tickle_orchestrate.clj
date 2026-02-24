(ns futon3c.agents.tickle-orchestrate
  "Tickle issue orchestration — assign work to agents and track completion.

   This is Tickle's conductor mode: active work assignment (vs the watchdog
   in tickle.clj which does passive liveness detection). The two modes are
   complementary and can run concurrently.

   Current topology: linear pipeline (fetch → assign → review → report).
   This is one of many possible topologies — the module is structured so
   the individual steps can be recomposed into different workflows later.

   REPL-driveable: each function is standalone. Compose via
   run-issue-workflow! or call steps individually."
  (:require [clojure.java.shell :as shell]
            [clojure.string :as str]
            [cheshire.core :as json]
            [futon3c.agency.registry :as reg]
            [futon3c.evidence.store :as estore]
            [futon3c.blackboard :as bb])
  (:import [java.time Instant Duration]
           [java.util UUID]))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- now-str [] (str (Instant/now)))

(defn- parse-instant
  [x]
  (cond
    (instance? Instant x) x
    (string? x)
    (try
      (Instant/parse x)
      (catch Exception _ nil))
    :else nil))

(defn- newer?
  [candidate baseline]
  (let [c (:tickle/at candidate)
        b (:tickle/at baseline)]
    (cond
      (nil? candidate) false
      (nil? c) false
      (nil? baseline) true
      (nil? b) true
      :else (.isAfter ^Instant c ^Instant b))))

(defn- latest-entry
  [entries pred]
  (reduce
   (fn [best entry]
     (if (pred entry)
       (if (newer? entry best) entry best)
       best))
   nil
   entries))

(def ^:private start-tags #{:kick-start :workflow-start})
(def ^:private complete-tags #{:kick-complete :workflow-complete})
(def ^:private success-statuses #{:done :complete})
(def ^:private failure-statuses #{:failed :codex-failed :review-failed})

(defn tickle-status
  "Summarise orchestration evidence from the given store.

   Args:
     evidence-store — evidence backend or atom
     opts — {:now Instant|String} override for current time (testing)

   Returns a map with timing and outcome counts."
  ([evidence-store]
   (tickle-status evidence-store {}))
  ([evidence-store {:keys [now]}]
   (let [entries (if evidence-store (estore/query* evidence-store {}) [])
         orch-entries (->> entries
                           (filter #(some #{:orchestrate} (:evidence/tags %)))
                           (map (fn [entry]
                                  (assoc entry
                                         :tickle/at (parse-instant (:evidence/at entry))
                                         :tickle/event (-> entry :evidence/tags last)))))
         now-inst (or (parse-instant now) (Instant/now))
         earliest (->> orch-entries (keep :tickle/at) (sort) first)
         uptime (if (and earliest now-inst)
                  (max 0 (.getSeconds (Duration/between earliest now-inst)))
                  0)
         last-kick (latest-entry orch-entries #(contains? start-tags (:tickle/event %)))
         last-complete (latest-entry orch-entries #(contains? complete-tags (:tickle/event %)))
         last-agent (latest-entry orch-entries #(get-in % [:evidence/body :agent]))
         total-kicks (count (filter #(contains? start-tags (:tickle/event %)) orch-entries))
         completion-entries (filter #(contains? complete-tags (:tickle/event %)) orch-entries)
         issues-completed (count (filter #(success-statuses (get-in % [:evidence/body :status]))
                                         completion-entries))
         issues-failed (count (filter #(failure-statuses (get-in % [:evidence/body :status]))
                                      completion-entries))]
     {:last-kick-at (:evidence/at last-kick)
      :last-complete-at (:evidence/at last-complete)
      :issues-completed issues-completed
      :issues-failed issues-failed
      :total-kicks total-kicks
      :last-agent (get-in last-agent [:evidence/body :agent])
      :uptime-seconds uptime})))

(defn- format-status-board
  [{:keys [last-kick-at last-complete-at issues-completed issues-failed
           total-kicks last-agent uptime-seconds]}]
  (str "Tickle Status\n"
       "================\n"
       "Last kick: " (or last-kick-at "never") "\n"
       "Last complete: " (or last-complete-at "never") "\n"
       "Completed: " (or issues-completed 0)
       "  Failed: " (or issues-failed 0) "\n"
       "Total kicks: " (or total-kicks 0) "\n"
       "Last agent: " (or last-agent "-") "\n"
       "Uptime: " (or uptime-seconds 0) "s\n"))

(defn- project-status!
  [evidence-store]
  (when evidence-store
    (let [summary (tickle-status evidence-store)
          content (format-status-board summary)]
      (bb/blackboard! "*Tickle Status*" content {:no-display true})
      summary)))

(defn- format-status-board
  [{:keys [last-kick-at last-complete-at issues-completed issues-failed
           total-kicks last-agent uptime-seconds]}]
  (str "Tickle Status\n"
       "================\n"
       "Last kick: " (or last-kick-at "never") "\n"
       "Last complete: " (or last-complete-at "never") "\n"
       "Completed: " (or issues-completed 0)
       "  Failed: " (or issues-failed 0) "\n"
       "Total kicks: " (or total-kicks 0) "\n"
       "Last agent: " (or last-agent "-") "\n"
       "Uptime: " (or uptime-seconds 0) "s\n"))

(defn- project-status!
  [evidence-store]
  (when evidence-store
    (let [summary (tickle-status evidence-store)
          content (format-status-board summary)]
      (bb/blackboard! "*Tickle Status*" content {:no-display true})
      summary)))
(defn- workflow-id [] (str "tko-" (UUID/randomUUID)))

(defn- emit!
  "Emit a workflow evidence entry. All orchestration evidence shares a
   consistent shape: subject keyed by repo+issue, author tickle-1,
   type :coordination, session-id from the workflow run."
  [evidence-store {:keys [session-id issue-number repo
                          claim-type event-tag body]}]
  (when evidence-store
    (estore/append* evidence-store
                    {:subject {:ref/type :task
                               :ref/id (str (or repo "unknown") "#" issue-number)}
                     :type :coordination
                     :claim-type claim-type
                     :author "tickle-1"
                     :tags [:tickle :orchestrate event-tag]
                     :session-id session-id
                     :body body})))

(defn- project!
  "Update the *Tickle Orchestrate* blackboard buffer."
  [state]
  (bb/blackboard!
   "*Tickle Orchestrate*"
   (str "Tickle Issue Orchestration\n"
        "==========================\n\n"
        (when-let [issue (:issue state)]
          (str "Issue: #" (:number issue) " " (:title issue) "\n"))
        "Status: " (name (or (:status state) :idle)) "\n"
        "Phase: " (or (:phase state) "-") "\n"
        (when-let [elapsed (:elapsed-ms state)]
          (str "Elapsed: " (long (/ elapsed 1000)) "s\n"))
        (when-let [verdict (:verdict state)]
          (str "Verdict: " (name verdict) "\n"))
        "\nLast updated: " (now-str))
   {:no-display true}))

;; =============================================================================
;; GitHub issue fetching
;; =============================================================================

(defn fetch-issue!
  "Fetch a GitHub issue by number from a repo directory.
   Returns {:ok true :issue {:number int :title str :body str :labels [str]}}
   or {:ok false :error str}.

   Uses `gh issue view` — requires gh CLI authenticated."
  [repo-dir issue-number]
  (try
    (let [{:keys [exit out err]}
          (shell/sh "gh" "issue" "view" (str issue-number)
                    "--json" "number,title,body,labels"
                    :dir repo-dir)]
      (if (zero? exit)
        (let [parsed (json/parse-string (str/trim out) true)
              labels (mapv :name (:labels parsed))]
          {:ok true
           :issue {:number (:number parsed)
                   :title (:title parsed)
                   :body (:body parsed)
                   :labels labels}})
        {:ok false :error (str "gh issue view failed: " (str/trim err))}))
    (catch Exception e
      {:ok false :error (str "Exception: " (.getMessage e))})))

(defn fetch-open-issues!
  "Fetch open issues with a given label from a repo directory.
   Returns {:ok true :issues [{:number :title :body :labels} ...]}.

   Uses `gh issue list`."
  [repo-dir & {:keys [label limit] :or {label "codex" limit 20}}]
  (try
    (let [{:keys [exit out err]}
          (shell/sh "gh" "issue" "list"
                    "--label" label
                    "--state" "open"
                    "--limit" (str limit)
                    "--json" "number,title,body,labels"
                    :dir repo-dir)]
      (if (zero? exit)
        (let [parsed (json/parse-string (str/trim out) true)
              issues (mapv (fn [i]
                             {:number (:number i)
                              :title (:title i)
                              :body (:body i)
                              :labels (mapv :name (:labels i))})
                           parsed)]
          {:ok true :issues issues})
        {:ok false :error (str "gh issue list failed: " (str/trim err))}))
    (catch Exception e
      {:ok false :error (str "Exception: " (.getMessage e))})))

;; =============================================================================
;; Prompt construction
;; =============================================================================

(defn- make-assign-prompt
  "Build an assignment prompt from an issue map. Includes surface contract."
  [issue repo-dir agent-id]
  (str "Runtime surface contract:\n"
       "- Agent: " agent-id " (Tickle orchestration)\n"
       "- Working directory: " repo-dir "\n"
       "- Task: Implement GitHub issue #" (:number issue) "\n"
       "- Your changes will be reviewed by another agent after completion.\n"
       "- Do not claim changes you did not make.\n\n"
       "--- GitHub Issue #" (:number issue) ": " (:title issue) " ---\n\n"
       (:body issue) "\n\n"
       "--- End of Issue ---\n\n"
       "Implement the changes described above. Work in " repo-dir ".\n"))

(defn- make-review-prompt
  "Build a Claude review prompt from issue + Codex result."
  [issue codex-result repo-dir]
  (str "Runtime surface contract:\n"
       "- Agent: claude-1 (Tickle orchestration — review mode)\n"
       "- Working directory: " repo-dir "\n"
       "- Task: Review implementation of GitHub issue #" (:number issue) "\n"
       "- Your verdict will be reported to Joe.\n\n"
       "--- GitHub Issue #" (:number issue) ": " (:title issue) " ---\n\n"
       (:body issue) "\n\n"
       "--- Codex Result ---\n\n"
       (:result codex-result) "\n\n"
       "--- End ---\n\n"
       "Review the changes Codex made for this issue in " repo-dir ". Check:\n"
       "1. Do the files contain the expected changes?\n"
       "2. Does the implementation match the criteria checklist?\n"
       "3. Are there obvious bugs or missing pieces?\n\n"
       "Respond with:\n"
       "- APPROVE if the implementation looks correct\n"
       "- REQUEST_CHANGES if there are issues (explain what)\n"
       "- UNCLEAR if you cannot determine (explain why)\n"))

(defn- parse-verdict
  "Extract a verdict keyword from Claude's review response."
  [result-text]
  (let [upper (str/upper-case (or result-text ""))]
    (cond
      (str/includes? upper "APPROVE") :approve
      (str/includes? upper "REQUEST_CHANGES") :request-changes
      (str/includes? upper "REQUEST CHANGES") :request-changes
      :else :unclear)))

;; =============================================================================
;; Workflow steps
;; =============================================================================

(defn assign-issue!
  "Invoke an agent with an issue prompt. Blocks until agent returns.
   Returns {:ok bool :result str :elapsed-ms long}.

   config:
     :agent-id — agent to invoke (default \"codex-1\")
     :evidence-store — for workflow tracking
     :repo-dir — repository root
     :timeout-ms — invoke timeout (default 180000 = 3 min)
     :session-id — workflow session id"
  [issue config]
  (let [{:keys [evidence-store repo-dir timeout-ms session-id agent-id]} config
        agent-id (or agent-id "codex-1")
        timeout-ms (or timeout-ms 180000)
        prompt (make-assign-prompt issue repo-dir agent-id)
        issue-number (:number issue)
        start (System/currentTimeMillis)]
    (emit! evidence-store
           {:session-id session-id
            :issue-number issue-number
            :repo repo-dir
            :claim-type :step
            :event-tag :agent-assigned
            :body {:agent agent-id
                   :prompt-preview (subs prompt 0 (min 200 (count prompt)))}})
    (project! {:issue issue :status :running :phase (str "Invoking " agent-id "...")})
    (let [result (reg/invoke-agent! agent-id prompt timeout-ms)
          elapsed (- (System/currentTimeMillis) start)
          ok? (:ok result)]
      (emit! evidence-store
             {:session-id session-id
              :issue-number issue-number
              :repo repo-dir
              :claim-type :observation
              :event-tag :agent-complete
              :body {:agent agent-id
                     :ok ok?
                     :result-preview (when (:result result)
                                       (subs (:result result)
                                             0 (min 300 (count (:result result)))))
                     :error (when-not ok? (str (:error result)))
                     :elapsed-ms elapsed}})
      {:ok ok?
       :result (:result result)
       :session-id (:session-id result)
       :error (:error result)
       :elapsed-ms elapsed})))

(defn request-review!
  "Invoke claude-1 to review Codex's work on an issue.
   Returns {:ok bool :result str :verdict keyword :elapsed-ms long}.

   config:
     :evidence-store — for workflow tracking
     :repo-dir — repository root
     :timeout-ms — invoke timeout (default 300000 = 5 min)
     :session-id — workflow session id"
  [issue codex-result config]
  (let [{:keys [evidence-store repo-dir timeout-ms session-id]} config
        timeout-ms (or timeout-ms 300000)
        prompt (make-review-prompt issue codex-result repo-dir)
        issue-number (:number issue)]
    (emit! evidence-store
           {:session-id session-id
            :issue-number issue-number
            :repo repo-dir
            :claim-type :step
            :event-tag :review-assigned
            :body {:reviewer "claude-1"}})
    (project! {:issue issue :status :running :phase "Claude reviewing..."})
    (let [start (System/currentTimeMillis)
          result (reg/invoke-agent! "claude-1" prompt timeout-ms)
          elapsed (- (System/currentTimeMillis) start)
          ok? (:ok result)
          verdict (when ok? (parse-verdict (:result result)))]
      (emit! evidence-store
             {:session-id session-id
              :issue-number issue-number
              :repo repo-dir
              :claim-type :observation
              :event-tag :review-complete
              :body {:ok ok?
                     :verdict verdict
                     :result-preview (when (:result result)
                                       (subs (:result result)
                                             0 (min 300 (count (:result result)))))
                     :error (when-not ok? (str (:error result)))
                     :elapsed-ms elapsed}})
      {:ok ok?
       :result (:result result)
       :verdict verdict
       :error (:error result)
       :elapsed-ms elapsed})))

(defn comment-on-issue!
  "Post a comment on a GitHub issue via gh CLI.
   Returns {:ok true} or {:ok false :error str}."
  [repo-dir issue-number comment-text]
  (try
    (let [{:keys [exit err]}
          (shell/sh "gh" "issue" "comment" (str issue-number)
                    "--body" comment-text
                    :dir repo-dir)]
      (if (zero? exit)
        {:ok true}
        {:ok false :error (str/trim err)}))
    (catch Exception e
      {:ok false :error (str "Exception: " (.getMessage e))})))

;; =============================================================================
;; Composed workflows
;; =============================================================================

(defn run-issue-workflow!
  "Run the full workflow for a single issue: assign → review → report.
   Returns a workflow result map.

   config:
     :evidence-store — evidence store
     :repo-dir — repository root (e.g. \"/home/joe/code/futon4\")
     :codex-timeout-ms — (default 180000)
     :review-timeout-ms — (default 300000)
     :send-to-channel! — IRC send fn (optional)
     :room — IRC room (default \"#futon\")
     :comment-on-issue? — post result as GH comment (default false)"
  [issue config]
  (let [{:keys [evidence-store repo-dir send-to-channel! room
                codex-timeout-ms review-timeout-ms comment-on-issue?]} config
        session-id (workflow-id)
        issue-number (:number issue)
        start (System/currentTimeMillis)
        step-config (assoc config :session-id session-id)]

    ;; 1. Start
    (emit! evidence-store
           {:session-id session-id
            :issue-number issue-number
            :repo repo-dir
            :claim-type :goal
            :event-tag :workflow-start
            :body {:issue-number issue-number
                   :title (:title issue)
                   :repo repo-dir}})
    (project! {:issue issue :status :running :phase "Starting workflow"})

    ;; 2. Assign to Codex
        (let [codex-result (assign-issue! issue
                                          (assoc step-config
                                                 :timeout-ms codex-timeout-ms))]
      (if-not (:ok codex-result)
        ;; Codex failed — report and stop
        (let [elapsed (- (System/currentTimeMillis) start)
              summary {:issue-number issue-number
                       :status :codex-failed
                       :error (:error codex-result)
                       :total-elapsed-ms elapsed}]
          (emit! evidence-store
                 {:session-id session-id
                  :issue-number issue-number
                  :repo repo-dir
                  :claim-type :observation
                  :event-tag :workflow-complete
                  :body summary})
          (project! {:issue issue :status :failed :phase "Codex failed"
                     :elapsed-ms elapsed})
          (project-status! evidence-store)
          summary)

        ;; 3. Request review from Claude
        (let [review-result (request-review! issue codex-result
                                             (assoc step-config
                                                    :timeout-ms review-timeout-ms))
              elapsed (- (System/currentTimeMillis) start)
              verdict (or (:verdict review-result) :unclear)
              summary {:issue-number issue-number
                       :status (if (:ok review-result) :complete :review-failed)
                       :verdict verdict
                       :total-elapsed-ms elapsed}]

          ;; 4. Report
          (emit! evidence-store
                 {:session-id session-id
                  :issue-number issue-number
                  :repo repo-dir
                  :claim-type :observation
                  :event-tag :workflow-complete
                  :body summary})
          (project! {:issue issue :status (:status summary)
                     :phase "Complete" :verdict verdict :elapsed-ms elapsed})

          ;; IRC report
          (when (fn? send-to-channel!)
            (send-to-channel! (or room "#futon") "tickle-1"
                              (str "Issue #" issue-number " (" (:title issue) "): "
                                   (name (:status summary))
                                   (when (= :complete (:status summary))
                                     (str " — verdict: " (name verdict))))))

          ;; GH comment
          (when comment-on-issue?
            (comment-on-issue! repo-dir issue-number
                               (str "**Tickle orchestration result**\n\n"
                                    "- Status: " (name (:status summary)) "\n"
                                    "- Verdict: " (name verdict) "\n"
                                    "- Elapsed: " (long (/ elapsed 1000)) "s")))

          (project-status! evidence-store)
          summary)))))

(defn kick!
  "Simplest conductor: invoke one agent with one issue, report result.
   No review loop. Just fetch → assign → done.

   config:
     :evidence-store — evidence store
     :repo-dir — repository root
     :agent-id — agent to invoke (default \"codex-1\")
     :timeout-ms — invoke timeout (default 180000)
     :send-to-channel! — IRC send fn (optional)
     :room — IRC room (default \"#futon\")"
  [issue config]
  (let [{:keys [evidence-store repo-dir agent-id timeout-ms
                send-to-channel! room]} config
        agent-id (or agent-id "codex-1")
        session-id (workflow-id)
        issue-number (:number issue)
        start (System/currentTimeMillis)]
    (emit! evidence-store
           {:session-id session-id
            :issue-number issue-number
            :repo repo-dir
            :claim-type :goal
            :event-tag :kick-start
            :body {:issue-number issue-number
                   :title (:title issue)
                   :agent agent-id
                   :repo repo-dir}})
    (project! {:issue issue :status :running
               :phase (str "Kicking " agent-id "...")})
    (let [result (assign-issue! issue
                                (assoc config
                                       :agent-id agent-id
                                       :session-id session-id
                                       :timeout-ms (or timeout-ms 180000)))
          elapsed (- (System/currentTimeMillis) start)
          summary {:issue-number issue-number
                   :status (if (:ok result) :done :failed)
                   :agent agent-id
                   :result-preview (when (:result result)
                                     (subs (:result result)
                                           0 (min 200 (count (:result result)))))
                   :error (when-not (:ok result) (str (:error result)))
                   :total-elapsed-ms elapsed}]
      (emit! evidence-store
             {:session-id session-id
              :issue-number issue-number
              :repo repo-dir
              :claim-type :observation
              :event-tag :kick-complete
              :body summary})
      (project! {:issue issue :status (:status summary)
                 :phase "Done" :elapsed-ms elapsed})
      (when (fn? send-to-channel!)
        (send-to-channel! (or room "#futon") "tickle-1"
                          (str "Kicked #" issue-number " → " agent-id
                               ": " (name (:status summary)))))
      (project-status! evidence-store)
      summary)))

(defn kick-queue!
  "Feed a queue of issues to an agent one at a time.
   Returns [{:issue-number :status :total-elapsed-ms} ...]."
  [issues config]
  (mapv (fn [issue]
          (project! {:issue issue :status :pending
                     :phase (str (count issues) " in queue")})
          (kick! issue config))
        issues))

(defn run-batch!
  "Run full workflows (assign + review) for a batch of issues sequentially.
   Returns [{:issue-number :status :verdict :total-elapsed-ms} ...]."
  [issues config]
  (mapv (fn [issue]
          (project! {:issue issue :status :pending
                     :phase (str "Queue: " (count issues) " issues")})
          (run-issue-workflow! issue config))
        issues))
