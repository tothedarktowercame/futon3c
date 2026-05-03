(ns futon3c.agents.tickle-orchestrate
  "Tickle issue orchestration — assign work to agents and track completion.

   This is Tickle's conductor mode: active work assignment (vs the watchdog
   in tickle.clj which does passive liveness detection). The two modes are
   complementary and can run concurrently.

   Two workflow types:
   1. Issue or work-item workflows (fetch → assign → review → report)
   2. FM proof conductor (round-robin tickle of agents working proof obligations)

   REPL-driveable: each function is standalone. Compose via
  run-issue-workflow! or call steps individually."
  (:require [clojure.java.shell :as shell]
            [clojure.string :as str]
            [cheshire.core :as json]
            [futon3c.agents.mfuton-prompt-override :as mfuton-prompt-override]
            [futon3c.agency.registry :as reg]
            [futon3c.evidence.store :as estore]
            [futon3c.blackboard :as bb]
            [futon3c.dev.config :as config])
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

(defn report-status!
  "Broadcast a concise orchestration status line and emit supporting evidence.

   opts:
     :evidence-store   — evidence backend/atom for summarising + emission
     :send-to-channel! — IRC send fn (optional)
     :room             — IRC room (default \"#futon\")
     :repo-dir         — repo identifier for evidence context (optional)
     :now              — override Instant/string for deterministic tests
     :session-id       — optional fixed evidence session id
     :status-subject   — optional evidence subject map"
  [{:keys [evidence-store send-to-channel! room repo-dir now session-id status-subject] :as _opts}]
  (let [summary (tickle-status evidence-store {:now now})
        total (or (:total-kicks summary) 0)
        completed (or (:issues-completed summary) 0)
        failed (or (:issues-failed summary) 0)
        last-agent (or (:last-agent summary) "-")
        last-time (or (:last-complete-at summary)
                     (:last-kick-at summary)
                     "never")
        message (format "Tickle: %d kicks (%d ok, %d failed), last: %s @ %s"
                        total completed failed last-agent last-time)
        subject (or status-subject {:ref/type :component :ref/id "tickle-status"})]
    (when (fn? send-to-channel!)
      (send-to-channel! (or room "#futon") "tickle-1" message))
    (when evidence-store
      (let [append-result
            (estore/append* evidence-store
                            {:subject subject
                             :type :coordination
                             :claim-type :observation
                             :author "tickle-1"
                             :session-id (or session-id (workflow-id))
                             :tags [:tickle :orchestrate :status-report]
                             :body {:repo repo-dir
                                    :summary summary
                                    :message message}})]
        (when (:error/component append-result)
          (throw (ex-info "Failed to append tickle status report" {:error append-result})))))
    {:summary summary
     :message message}))

;; =============================================================================
;; GitHub issue fetching
;; =============================================================================

(defn fetch-issue!
  "Fetch a GitHub issue by number from a repo directory.
   Returns {:ok true :issue {:number int :title str :body str :labels [str]}}
   or {:ok false :error str}.

   Prompt rewrites may generalize outward issue language, but the current
   recovery path keeps the underlying fetch on `gh issue view`."
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

   Prompt rewrites may generalize outward issue language, but the current
   recovery path keeps queue reads on `gh issue list`."
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
  (let [original-prompt
        (str "Runtime surface contract:\n"
             "- Agent: " agent-id " (Tickle orchestration)\n"
             "- Working directory: " repo-dir "\n"
             "- Task: Implement GitHub issue #" (:number issue) "\n"
             "- Your changes will be reviewed by another agent after completion.\n"
             "- Do not claim changes you did not make.\n\n"
             "--- GitHub Issue #" (:number issue) ": " (:title issue) " ---\n\n"
             (:body issue) "\n\n"
             "--- End of Issue ---\n\n"
             "Implement the changes described above. Work in " repo-dir ".\n")]
    (mfuton-prompt-override/maybe-assign-prompt original-prompt)))

(defn- make-review-prompt
  "Build a Claude review prompt from issue + Codex result."
  [issue codex-result repo-dir]
  (let [original-prompt
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
             "- UNCLEAR if you cannot determine (explain why)\n")]
    (mfuton-prompt-override/maybe-review-prompt original-prompt)))

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
  (let [results (mapv (fn [issue]
                        (project! {:issue issue :status :pending
                                   :phase (str (count issues) " in queue")})
                        (kick! issue config))
                      issues)]
    (report-status! config)
    results))

(defn run-batch!
  "Run full workflows (assign + review) for a batch of issues sequentially.
   Returns [{:issue-number :status :verdict :total-elapsed-ms} ...]."
  [issues config]
  (let [results (mapv (fn [issue]
                        (project! {:issue issue :status :pending
                                   :phase (str "Queue: " (count issues) " issues")})
                        (run-issue-workflow! issue config))
                      issues)]
    (report-status! config)
    results))

;; =============================================================================
;; FM Proof Conductor — round-robin agent tickling for proof obligations
;; =============================================================================
;;
;; The conductor cycles through agents (claude-1 → codex-1 → claude-2), invoking
;; tickle-1 (Haiku) each cycle to decide whether to nudge/assign/pass for the
;; target agent. State is tracked in the conductor atom so tickle doesn't re-page
;; agents that are actively working (war-bulletin-5 finding 2: stateless noise).

(def ^:private default-fm-rotation
  "Default round-robin agent rotation."
  ["claude-1" "codex-1" "claude-2"])

(def ^:private agent-roles
  "Role descriptions for round-robin context."
  {"claude-1" "claude-1 (proof ledger owner, Clojure developer)"
   "codex-1"  "codex (computational worker, runs SAT solvers and writes code)"
   "claude-2" "claude-2 (Mentor, epistemic risk monitor, pattern reviewer)"})

(def ^:private default-cooldown-ms
  "Minimum interval between pages to the same agent (15 minutes).
   War-bulletin-5 finding: stateless orchestrator caused ~40% noise."
  (* 15 60 1000))

(defn- cooldown-elapsed?
  "True if enough time has passed since last page to this agent."
  [conductor-state target-agent cooldown-ms]
  (if-let [last-paged (get-in @conductor-state [:last-paged target-agent])]
    (> (- (System/currentTimeMillis) last-paged) cooldown-ms)
    true))

(defn- record-page!
  "Record that we paged an agent at the current time."
  [conductor-state target-agent]
  (swap! conductor-state assoc-in [:last-paged target-agent] (System/currentTimeMillis)))

(defn fm-assignable-obligations
  "Find proof obligations that are assignable (open, all deps met).
   Reads from the proof bridge. Returns sorted by DAG impact."
  [problem-id]
  (try
    (require 'futon3c.proof.bridge)
    (let [pb (find-ns 'futon3c.proof.bridge)
          ledger-fn (ns-resolve pb 'ledger)
          dag-fn (ns-resolve pb 'dag-impact)
          ledger-items (:result (ledger-fn problem-id))
          dag-scores (into {} (map (fn [{:keys [id score]}] [id score])
                                   (:result (dag-fn problem-id))))
          open-items (if (map? ledger-items)
                       (vals ledger-items)
                       ledger-items)
          resolved-ids (->> open-items
                            (remove #(= :open (:item/status %)))
                            (map :item/id)
                            set)
          assignable (->> open-items
                          (filter #(= :open (:item/status %)))
                          (filter (fn [item]
                                    (every? #(or (contains? resolved-ids %)
                                                 (= % (:item/id item)))
                                            (:item/depends-on item #{}))))
                          (sort-by #(- (get dag-scores (:item/id %) 0))))]
      assignable)
    (catch Exception e
      (println (str "[fm-conductor] Error reading proof ledger: " (.getMessage e)))
      [])))

(defn- build-fm-context-original
  "Build the Tickle decision prompt for an FM conductor cycle."
  [problem-id target-agent recent-msgs assignable-tasks]
  (let [room (config/frontiermath-room)
        agent-role (get agent-roles target-agent target-agent)
        irc-nick (config/irc-nick-for-agent-id target-agent)
        recent-text (str/join "\n"
                     (map (fn [{:keys [nick text at]}]
                            (str (when at (subs (str at) 11 19)) " <" nick "> " text))
                          recent-msgs))
        task-text (str/join "\n"
                   (map (fn [item]
                          (str "  " (:item/id item) ": " (:item/label item)))
                        assignable-tasks))]
    (str "You are Tickle, task coordinator for " problem-id " (Ramsey numbers for book graphs) on " room ".\n\n"
         "ROLE: You assign proof obligations and follow up on progress. You are mechanical/queue-based, not epistemic.\n"
         "Current mode: FALSIFY (must try to disprove before constructing).\n"
         "Problem state doc: futon6/data/first-proof/frontiermath-pilot/FM-001-ramsey-book-graphs-state.md\n"
         "Strategy doc: futon6/data/frontiermath-pilot/FM-001-strategy.md\n\n"
         "TARGET THIS CYCLE: " agent-role "\n"
         "IRC NICK: " irc-nick " (use @" irc-nick " when mentioning this agent)\n\n"
         "ASSIGNABLE OBLIGATIONS:\n" (if (seq task-text) task-text "  (none — all blocked by dependencies)") "\n\n"
         "RECENT " room " (" (count recent-msgs) " messages):\n"
         (if (seq recent-text) recent-text "(no messages captured)") "\n\n"
         "RULES:\n"
         "1. If the target agent claimed work and is actively working — PASS (don't interrupt)\n"
         "2. If the target agent seems stuck or silent — brief nudge\n"
         "3. If work is completed — acknowledge and dispatch next obligation\n"
         "4. Max 1-2 lines, <400 chars. Always @mention the target using their IRC NICK (not agent ID). Post to " room ".\n"
         "5. Do NOT repeat the full problem statement — agents know it.\n"
         "6. Remind agents to push artifacts to git — never reference local paths or /tmp.\n"
         "7. If YOU (tickle) already said something similar in RECENT " room " — PASS. Do not re-page.\n\n"
         "RESPOND WITH ONLY:\n"
         "- A short IRC message (start with @agent-name), OR\n"
         "- PASS")))

(defn build-fm-context
  "Build the Tickle decision prompt for an FM conductor cycle."
  [problem-id target-agent recent-msgs assignable-tasks]
  (let [original-prompt (build-fm-context-original
                         problem-id target-agent recent-msgs assignable-tasks)]
    (mfuton-prompt-override/maybe-build-fm-context original-prompt)))

(defn fm-conduct-cycle!
  "Run one FM proof conductor cycle targeting a specific agent.
   Invokes tickle-1 to decide whether to nudge/assign, then posts via bridge-send-fn.

   Two-layer anti-noise guard (war-bulletin-5):
   1. Mechanical cooldown: skip if agent was paged within :cooldown-ms
   2. Prompt rule 7: tells Haiku to PASS if it already said something similar

   config:
     :problem-id       — proof problem ID (default \"FM-001\")
     :irc-read-fn      — fn [] → [{:nick :text :at :channel}] recent IRC messages
     :bridge-send-fn   — fn [channel from-nick message] → posts to IRC via bridge
     :evidence-store   — for tracking conductor cycles
     :conductor-state  — atom tracking per-agent cooldown state
     :cooldown-ms      — min interval between pages to same agent (default 15 min)
     :whistle-fn       — fn [map] → escalates via Agency when PASS + empty ledger
                          map has {:from :to :type :problem-id :reason :action}

   Returns {:action :pass|:cooldown|:message|:error, :target str, :text str}."
  [target-agent config]
  (let [{:keys [problem-id irc-read-fn bridge-send-fn evidence-store
                conductor-state cooldown-ms whistle-fn invoke-fn]
         :or {problem-id "FM-001"
              cooldown-ms default-cooldown-ms}} config
        conductor-state (or conductor-state (atom {}))
        ;; Prefer explicit invoke-fn from config; fall back to tickle-1 registry
        invoke-fn (or invoke-fn
                      (let [agent (reg/get-agent "tickle-1")]
                        (or (:agent/invoke-fn agent) (:invoke-fn agent))))]
    ;; Layer 1: mechanical cooldown guard
    (if-not (cooldown-elapsed? conductor-state target-agent cooldown-ms)
      (do (println (str "[fm-conductor] " target-agent " → COOLDOWN (paged recently)"))
          {:action :cooldown :target target-agent})
      (let [recent-msgs (when (fn? irc-read-fn)
                          (->> (irc-read-fn)
                               (filter #(= (config/frontiermath-room) (:channel %)))
                               (take-last 20)))
            assignable (fm-assignable-obligations problem-id)
            context (build-fm-context problem-id target-agent recent-msgs assignable)]
        (if-not invoke-fn
          (do (println "[fm-conductor] no invoke-fn configured and tickle-1 not registered")
              {:action :error :target target-agent})
          (let [{:keys [result]} (invoke-fn context nil)
                response (str/trim (or result "PASS"))
                first-line (first (str/split-lines response))
                pass? (or (str/blank? response)
                          (= "PASS" (str/upper-case (str first-line))))]
            ;; Emit evidence for this cycle
            (when evidence-store
              (estore/append* evidence-store
                              {:subject {:ref/type :session
                                         :ref/id (str "fm-conductor/" problem-id)}
                               :type :coordination
                               :claim-type :observation
                               :author "tickle-1"
                               :tags [:tickle :fm-conductor :cycle]
                               :session-id "fm-conductor"
                               :body {:problem-id problem-id
                                      :target target-agent
                                      :action (if pass? :pass :message)
                                      :message (when-not pass? first-line)
                                      :assignable-count (count assignable)
                                      :recent-msg-count (count recent-msgs)
                                      :cycle-at (str (Instant/now))}}))
            (if pass?
              (do (println (str "[fm-conductor] " target-agent " → PASS"))
                  ;; Whistle: if PASS and no assignable work, escalate via Agency
                  (let [mentor "claude-2"
                        whistle? (and (empty? assignable)
                                      (not= target-agent mentor)
                                      (cooldown-elapsed? conductor-state
                                                         (str "whistle:" mentor)
                                                         cooldown-ms))]
                    (when (and whistle? (fn? whistle-fn))
                      (println (str "[fm-conductor] whistling " mentor " (no obligations for " target-agent ")"))
                      (record-page! conductor-state (str "whistle:" mentor))
                      (whistle-fn {:from "tickle-1"
                                   :to mentor
                                   :type :whistle
                                   :problem-id problem-id
                                   :reason (str "No assignable obligations for " target-agent)
                                   :action :ledger-review})))
                  {:action :pass :target target-agent})
              (do (println (str "[fm-conductor] " target-agent " → " first-line))
                  (record-page! conductor-state target-agent)
                  (when (fn? bridge-send-fn)
                    (bridge-send-fn (config/frontiermath-room) "tickle" first-line))
                  {:action :message :target target-agent :text first-line}))))))))

(defn start-fm-conductor!
  "Start the FM proof conductor loop with round-robin agent tickling.
   Cycles through agents one per step. Evidence is emitted each cycle.

   config:
     :step-ms          — interval between agent checks (default 300000 = 5 min)
     :rotation         — agent rotation vector (default [\"claude-1\" \"codex-1\" \"claude-2\"])
     :problem-id       — proof problem ID (default \"FM-001\")
     :irc-read-fn      — fn [] → IRC messages
     :bridge-send-fn   — fn [channel from-nick message]
     :evidence-store   — for tracking

   Returns {:stop-fn fn, :started-at Instant, :conductor-state atom}."
  [config]
  (let [{:keys [step-ms rotation on-cycle-fn]
         :or {step-ms 300000
              rotation default-fm-rotation}} config
        conductor-state (or (:conductor-state config) (atom {}))
        config (assoc config :conductor-state conductor-state)
        running (atom true)
        idx (atom 0)]
    ;; Seed conductor-state with rotation metadata
    (swap! conductor-state assoc
           :rotation rotation
           :idx 0
           :cycles-completed 0
           :last-cycle nil
           :started-at-ms (System/currentTimeMillis))
    (future
      (while @running
        (try
          (Thread/sleep (long step-ms))
          (when @running
            (let [i @idx
                  target (nth rotation (mod i (count rotation)))
                  _ (swap! idx inc)
                  result (fm-conduct-cycle! target config)]
              ;; Update conductor state for inspectability
              (swap! conductor-state assoc
                     :idx (inc i)
                     :cycles-completed (inc (or (:cycles-completed @conductor-state) 0))
                     :last-cycle {:target target
                                  :action (:action result)
                                  :text (:text result)
                                  :at (str (Instant/now))}
                     :last-cycle-ms (System/currentTimeMillis))
              ;; Notify caller (e.g. for blackboard refresh)
              (when (fn? on-cycle-fn)
                (try (on-cycle-fn result) (catch Exception _ nil)))))
          (catch Exception e
            (println (str "[fm-conductor] Error: " (.getMessage e)))))))
    (let [handle {:stop-fn #(do (reset! running false)
                                (println "[fm-conductor] Stopped."))
                  :started-at (Instant/now)
                  :conductor-state conductor-state}]
      (println (str "[fm-conductor] Round-robin started (step=" (/ step-ms 60000)
                    "min, agents=" (str/join "→" rotation) ")"))
      handle)))

(defn stop-fm-conductor!
  "Stop a running FM conductor. Takes the handle returned by start-fm-conductor!."
  [handle]
  (when handle
    ((:stop-fn handle))))
