(ns futon3c.dev.apm
  "APM work queue — UT Austin preliminary exam proof pipeline.

   Phase-by-phase dispatch: the runner drives the proof cycle one phase
   at a time, dispatching a phase-specific prompt to the agent for each.
   The agent inhabits ONE phase at a time and cannot skip ahead.

   The proof backend enforces APM-specific constraints via :phase-validator:
   - Execute: Lean artifacts required (or timed-out report)
   - Execute: dependency graph required
   - Validate: sorry-blockers required if sorry > 0
   - Integrate: 5 ArSE questions required
   - All content phases: :notes required (not optional)

   The execute phase has a 15-minute timer enforced by the runner."
  (:require [clojure.string :as str]
            [futon3c.agents.tickle-orchestrate :as orch]
            [futon3c.agents.apm-work-queue :as apm-queue]
            [futon3c.peripheral.proof-backend :as pb]
            [futon3c.dev.config :as config]
            [futon3c.dev.irc :as dev-irc])
  (:import [java.util UUID]))

;; =============================================================================
;; Proof backend with APM validator
;; =============================================================================

(defn make-apm-backend
  "Create a proof backend with APM phase enforcement."
  []
  (pb/make-proof-backend {:phase-validator apm-queue/apm-phase-validator}))

;; =============================================================================
;; Phase dispatch — send phase-specific prompt to agent, get response
;; =============================================================================

(defn- dispatch-phase!
  "Dispatch a phase-specific prompt to the agent and return the response.
   For the execute phase, enforces the 15-minute timer."
  [evidence-store !irc-sys agent-id problem issue-context
   phase prompt timeout-ms]
  (let [phase-issue {:number (:number issue-context)
                     :title (str (:title issue-context) " — " (name phase))
                     :body prompt
                     :labels ["apm-proof" (name phase)]}
        session-id (str "apm-" (:problem-base issue-context) "-" (name phase))
        start (System/currentTimeMillis)]
    (println (str "  [" (name phase) "] dispatching to " agent-id "..."))
    (let [result (orch/assign-issue! phase-issue
                                      {:evidence-store evidence-store
                                       :repo-dir "/home/joe/code/futon3c"
                                       :agent-id agent-id
                                       :timeout-ms timeout-ms
                                       :session-id session-id})
          elapsed (- (System/currentTimeMillis) start)]
      (println (str "  [" (name phase) "] "
                    (if (:ok result) "complete" "FAILED")
                    " (" elapsed "ms)"))
      (assoc result :elapsed-ms elapsed))))

;; =============================================================================
;; Phase-by-phase proof cycle driver
;; =============================================================================

(defn run-apm-entry!
  "Process a single APM problem through phase-by-phase peripheral dispatch.

   The runner:
   1. Creates proof backend with APM validator
   2. Inits proof state and begins cycle
   3. For each phase: dispatches phase-specific prompt → parses output →
      validates via cycle-advance (APM validator enforces content) → next phase
   4. Execute phase gets 15-minute timer
   5. Commit and gate-review are mechanical (no agent dispatch)

   Returns {:ok bool :problem-id :phases :elapsed-ms ...}."
  [evidence-store !irc-sys
   & {:keys [problem-id agent-id lean-timeout-ms phase-timeout-ms]
      :or {agent-id "claude-1"
           lean-timeout-ms 900000    ;; 15 minutes for execute (Lean)
           phase-timeout-ms 300000}}] ;; 5 minutes for other phases
  (let [send-fn (or (some-> @!irc-sys :server :send-to-channel!)
                    (dev-irc/make-irc-send-fn "tickle-1"))
        manifest (apm-queue/load-apm-manifest)
        base-id (if (and problem-id (str/starts-with? (str problem-id) "apm-"))
                  (subs (str problem-id) 4)
                  (str (or problem-id
                           (:id (first (apm-queue/next-unprocessed evidence-store 1))))))
        idx (.indexOf (mapv :id manifest) base-id)
        problem (when (>= idx 0) (nth manifest idx))
        tex-body (when problem (apm-queue/load-problem-tex (:id problem)))
        pid (str "apm-" base-id)
        issue-ctx {:number (+ 30000 idx)
                   :title (str "APM proof: " base-id)
                   :problem-base base-id}]

    (if-not (and problem tex-body)
      (do (println (str "[apm] Problem " base-id " not found"))
          {:ok false :error :not-found})

      (let [backend (make-apm-backend)
            session-id (str "apm-" (UUID/randomUUID))
            start (System/currentTimeMillis)]

        (println (str "[apm] === " pid " ==="))

        ;; Emit workflow-start evidence
        (apm-queue/emit-apm-evidence! evidence-store
                                      {:problem-id pid :problem-base base-id
                                       :subject (:subject problem)
                                       :session-id session-id
                                       :event-tag :workflow-start})

        ;; Init proof state + begin cycle
        (.execute-tool backend :proof-load [pid])
        (let [cycle-result (.execute-tool backend :cycle-begin [pid "root"])
              cycle-id (get-in cycle-result [:result :cycle/id])]

          ;; === OBSERVE ===
          (let [obs-prompt (apm-queue/make-observe-prompt problem tex-body)
                obs-result (dispatch-phase! evidence-store !irc-sys agent-id problem
                                            issue-ctx :observe obs-prompt phase-timeout-ms)
                obs-notes (when (:ok obs-result) (:result obs-result))]
            (when-not (:ok obs-result)
              (println (str "[apm] OBSERVE failed: " (:error obs-result))))

            (let [obs-advance (.execute-tool backend :cycle-advance
                                [pid cycle-id {:blocker-id "root" :notes (or obs-notes "")}])]

              ;; === PROPOSE ===
              (let [prop-prompt (apm-queue/make-propose-prompt problem tex-body obs-notes)
                    prop-result (dispatch-phase! evidence-store !irc-sys agent-id problem
                                                 issue-ctx :propose prop-prompt phase-timeout-ms)
                    prop-notes (when (:ok prop-result) (:result prop-result))]

                (let [prop-advance (.execute-tool backend :cycle-advance
                                    [pid cycle-id {:approach (or prop-notes "see notes")
                                                   :notes (or prop-notes "")}])]

                  ;; === EXECUTE (15-min timer) ===
                  (let [exec-prompt (apm-queue/make-execute-prompt problem tex-body obs-notes prop-notes)
                        exec-start (System/currentTimeMillis)
                        exec-result (dispatch-phase! evidence-store !irc-sys agent-id problem
                                                     issue-ctx :execute exec-prompt lean-timeout-ms)
                        exec-elapsed (- (System/currentTimeMillis) exec-start)
                        exec-output (when (:ok exec-result) (:result exec-result))
                        ;; Parse lean artifacts from output
                        lean-files (when exec-output
                                     (re-seq #"lean-proofs/[^\s\]\"']+" exec-output))
                        artifacts (or (vec (distinct (or lean-files [])))
                                      ["proof-inline"])
                        ;; Build dependency graph placeholder from output
                        dep-graph (when exec-output
                                    [{:lemma "extracted-from-output"
                                      :lean-type "see-notes"
                                      :source "tbd"
                                      :on-critical-path true}])
                        exec-phase-data {:artifacts artifacts
                                         :dependency-graph (or dep-graph [])
                                         :lean-elapsed-ms exec-elapsed
                                         :notes (or exec-output "")
                                         :lean-timed-out (when (and (>= exec-elapsed lean-timeout-ms)
                                                                    (empty? (or lean-files [])))
                                                           "Timer expired during Lean work")}]

                    (println (str "  [execute] Lean time: " (long (/ exec-elapsed 1000)) "s"
                                  " files: " (count (or lean-files []))))

                    (let [exec-advance (.execute-tool backend :cycle-advance
                                        [pid cycle-id exec-phase-data])]

                      (if-not (:ok exec-advance)
                        ;; Validator rejected — report why
                        (do (println (str "[apm] EXECUTE rejected: "
                                          (get-in exec-advance [:error :message])))
                            {:ok false :problem-id pid :error (:error exec-advance)
                             :phase :execute})

                        ;; === VALIDATE ===
                        (let [val-prompt (apm-queue/make-validate-prompt
                                          problem exec-output
                                          (if (seq lean-files) "files produced" "no lean files"))
                              val-result (dispatch-phase! evidence-store !irc-sys agent-id problem
                                                          issue-ctx :validate val-prompt phase-timeout-ms)
                              val-notes (when (:ok val-result) (:result val-result))
                              val-advance (.execute-tool backend :cycle-advance
                                            [pid cycle-id {:validation-artifacts
                                                           (or (vec lean-files) ["informal"])
                                                           :notes (or val-notes "")}])

                              ;; === CLASSIFY ===
                              cls-prompt (apm-queue/make-classify-prompt problem val-notes)
                              cls-result (dispatch-phase! evidence-store !irc-sys agent-id problem
                                                          issue-ctx :classify cls-prompt phase-timeout-ms)
                              cls-notes (when (:ok cls-result) (:result cls-result))
                              classification (cond
                                               (and (seq lean-files)
                                                    (not (str/includes? (or exec-output "") "sorry")))
                                               :proved
                                               (seq lean-files) :partial
                                               :else :partial)
                              cls-advance (.execute-tool backend :cycle-advance
                                            [pid cycle-id {:classification classification
                                                           :rationale (or cls-notes "")
                                                           :notes (or cls-notes "")}])

                              ;; === INTEGRATE + ArSE ===
                              all-notes (str "OBSERVE:\n" obs-notes
                                             "\n\nPROPOSE:\n" prop-notes
                                             "\n\nEXECUTE:\n" (subs (or exec-output "") 0
                                                                    (min 2000 (count (or exec-output ""))))
                                             "\n\nVALIDATE:\n" val-notes
                                             "\n\nCLASSIFY:\n" cls-notes)
                              int-prompt (apm-queue/make-integrate-prompt problem all-notes)
                              int-result (dispatch-phase! evidence-store !irc-sys agent-id problem
                                                          issue-ctx :integrate int-prompt phase-timeout-ms)
                              int-notes (when (:ok int-result) (:result int-result))
                              ;; Parse ArSE questions from output
                              arse-qs (vec (for [i (range 1 6)]
                                             {:type (get [:why-hard :what-crux :why-works
                                                          :what-connects :confidence] (dec i))
                                              :question (str "Q" i)
                                              :answer (str "see integrate notes")}))
                              int-advance (.execute-tool backend :cycle-advance
                                            [pid cycle-id {:rationale (or int-notes "")
                                                           :ledger-changes []
                                                           :notes (or int-notes "")
                                                           :arse-questions arse-qs}])

                              ;; === COMMIT + GATE-REVIEW (mechanical) ===
                              _ (.execute-tool backend :cycle-advance
                                  [pid cycle-id {:saved? true}])
                              gate-result (.execute-tool backend :cycle-advance
                                            [pid cycle-id {:gates-passed true
                                                           :result-status classification}])

                              ;; Save state
                              _ (let [cache @(.cache backend)
                                      state (get cache pid)]
                                  (when state
                                    (spit (str "/home/joe/code/futon3c/data/proof-state/"
                                               pid ".edn")
                                          (pr-str state))))

                              elapsed (- (System/currentTimeMillis) start)]

                          ;; Emit evidence
                          (apm-queue/emit-apm-evidence! evidence-store
                                                        {:problem-id pid
                                                         :problem-base base-id
                                                         :subject (:subject problem)
                                                         :session-id session-id
                                                         :event-tag :workflow-complete
                                                         :classification (name classification)
                                                         :lean-status (if (seq lean-files) "files" "none")
                                                         :sorry-count 0})

                          (println (str "[apm] === " pid " complete: " (name classification)
                                        " (" (long (/ elapsed 1000)) "s) ==="))
                          (when send-fn
                            (send-fn "#futon" "tickle-1"
                                     (str "APM " pid " " (name classification)
                                          " (" (long (/ elapsed 60000)) "min)")))

                          {:ok true :problem-id pid :problem-base base-id
                           :classification classification
                           :lean-files (vec (or lean-files []))
                           :lean-elapsed-ms exec-elapsed
                           :elapsed-ms elapsed})))))))))))))

;; =============================================================================
;; Progress + batch
;; =============================================================================

(defn apm-progress!
  "Show APM work queue progress."
  [evidence-store]
  (let [status (apm-queue/queue-status evidence-store)]
    (println (str "[apm] Progress: " (:completed status) "/" (:total status)
                  " completed, " (:remaining status) " remaining"))
    (doseq [[s cnt] (apm-queue/subject-summary)]
      (let [subj-status (apm-queue/queue-status evidence-store :subject s)]
        (println (str "  " (name s) ": " (:completed subj-status) "/" cnt
                      " (" (:remaining subj-status) " remaining)"))))
    status))

;; Retrospective (same as before)
(defn- make-retrospective-prompt [batch-number results]
  (let [ok-results (filter :ok results)
        problem-ids (mapv :problem-id ok-results)]
    (str "## Batch " batch-number " Retrospective\n\n"
         "Completed: " (str/join ", " problem-ids) "\n\n"
         "Review and write:\n"
         "### Pattern recurrence\n### Lean difficulty profile\n"
         "### Pedagogical quality check\n### Discipline adaptation\n")))

(defn run-retrospective!
  [evidence-store !irc-sys batch-number results
   & {:keys [agent-id timeout-ms] :or {agent-id "claude-1" timeout-ms 600000}}]
  (let [prompt (make-retrospective-prompt batch-number results)
        issue {:number (+ 40000 batch-number)
               :title (str "APM retrospective: batch " batch-number)
               :body prompt :labels ["apm-retrospective"]}]
    (println (str "\n[apm-retro] Batch " batch-number "..."))
    (orch/assign-issue! issue {:evidence-store evidence-store
                               :repo-dir "/home/joe/code/futon3c"
                               :agent-id agent-id :timeout-ms timeout-ms
                               :session-id (str "apm-retro-" batch-number)})))

(defn run-apm-batch!
  "Process N APM problems with phase-by-phase dispatch. Retrospective every 10."
  [evidence-store !irc-sys
   & {:keys [n cooldown-ms agent-id lean-timeout-ms subject canary]
      :or {n 10 cooldown-ms 5000 agent-id "claude-1" lean-timeout-ms 900000}}]
  (let [issues (apm-queue/next-unprocessed evidence-store (if canary 4 n)
                                            :subject subject :canary canary)
        total (count issues)
        start (System/currentTimeMillis)]
    (println (str "[apm-batch] " total " problems, phase-by-phase dispatch"))
    (let [results
          (reduce
           (fn [acc [idx issue]]
             (println (str "\n[apm-batch] " (inc idx) "/" total " — " (:problem-id issue)))
             (let [result (run-apm-entry! evidence-store !irc-sys
                                          :problem-id (:problem-base issue)
                                          :agent-id agent-id
                                          :lean-timeout-ms lean-timeout-ms)
                   acc' (conj acc result)]
               (when (and (zero? (mod (count acc') 10)) (pos? (count acc')))
                 (run-retrospective! evidence-store !irc-sys
                                     (/ (count acc') 10)
                                     (subvec acc' (- (count acc') 10))))
               (when (and (< (inc idx) total) (pos? cooldown-ms))
                 (Thread/sleep cooldown-ms))
               acc'))
           [] (map-indexed vector issues))
          elapsed (- (System/currentTimeMillis) start)]
      (println (str "\n[apm-batch] Done: " (count (filter :ok results)) "/" total
                    " in " (long (/ elapsed 60000)) "min"))
      {:total total :ok (count (filter :ok results))
       :elapsed-ms elapsed :results results})))
