(ns futon3c.dev-irc-summary-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [futon3c.dev :as dev]))

(deftest summarize-irc-result-keeps-artifact-refs
  (testing "long replies are shortened and keep concrete refs"
    (let [input (str "Implemented the fix for relay routing and added tests across the stack. "
                     "Updated /home/joe/code/futon3c/src/futon3c/transport/irc.clj and "
                     "opened https://github.com/tothedarktowercame/futon3c/issues/42 with "
                     "follow-up notes. Commit 29d18a9 contains the exact change set.")
          out (#'futon3c.dev/summarize-irc-result input)]
      (is (<= (count out) 320))
      (is (.contains out "refs:"))
      (is (.contains out "29d18a9")))))

(deftest summarize-irc-result-avoids-json-dumps
  (testing "structured payloads are reduced to a short status line"
    (let [input "{\"thread_id\":\"019cc01c\",\"result\":\"very long JSON payload\"}"
          out (#'futon3c.dev/summarize-irc-result input)]
      (is (= "Structured output generated. (no artifact reference yet)" out))
      (is (not (.contains out "thread_id"))))))

(deftest summarize-irc-result-flags-missing-artifacts
  (testing "non-planning text without refs is explicitly marked"
    (let [out (#'futon3c.dev/summarize-irc-result
               "Ran checks locally and prepared a candidate fix for review.")]
      (is (.contains out "no artifact reference yet")))))

(deftest summarize-irc-result-normalizes-absolute-local-paths
  (testing "absolute local paths are rendered in a surface-safe form for IRC"
    (let [out (#'futon3c.dev/summarize-irc-result
               "Indexed /home/joe/code/futon6/data/arxiv-ct-metadata.jsonl and updated /home/joe/code/futon3c/dev/futon3c/dev.clj")]
      (is (not (.contains out "/home/joe/code/")))
      (is (.contains out "~/code/futon6/data/arxiv-ct-metadata.jsonl"))
      (is (.contains out "~/code/futon3c/dev/futon3c/dev.clj")))))

(deftest invoke-trace-response-block-persists-full-payload
  (testing "invoke trace stays compact and writes full payload to disk"
    (let [tmp-dir (.toFile (java.nio.file.Files/createTempDirectory
                            "f3c-invoke-trace-test"
                            (make-array java.nio.file.attribute.FileAttribute 0)))
          payload "{\"thread_id\":\"synth-p2-s3a-000\",\"title\":\"long structured payload\"}"
          trace-id "invoke-1234"
          block (with-redefs [futon3c.dev/env (fn [k & [default]]
                                                (if (= k "FUTON3C_INVOKE_ARTIFACT_DIR")
                                                  (.getAbsolutePath tmp-dir)
                                                  default))]
                  (#'futon3c.dev/invoke-trace-response-block "codex-1" "019cc01c-b049-7ce1" trace-id payload))
          artifact-path (some->> (re-find #"Artifact: (.+)" block) second)]
      (is (.contains block (str "Trace: " trace-id " | result=structured")))
      (is (.contains block " | chars="))
      (is (.contains block " | sha256="))
      (is artifact-path)
      (is (not (.contains block "thread_id")))
      (is (not (.contains block "Delivery: pending")))
      (is (not (.contains block "Delivery guarantee: caller must record where reply was sent.")))
      (is (.exists (java.io.File. artifact-path)))
      (is (= payload (slurp artifact-path))))))

(deftest format-delivery-receipt-line-includes-surface-and-destination
  (let [line (#'futon3c.dev/format-delivery-receipt-line
              "invoke-42"
              {:surface "irc"
               :destination "#futon as <codex>"
               :delivered? true
               :note "dispatch-relay"})]
    (is (.contains line "Delivery: delivered via irc -> #futon as <codex>"))
    (is (.contains line "(trace-id invoke-42)"))
    (is (.contains line "[dispatch-relay]"))))

(deftest invoke-meta-trace-id-supports-keyword-and-string-keys
  (testing "trace-id extraction accepts invoke-meta maps from mixed JSON/Clojure sources"
    (is (= "invoke-kw"
           (#'futon3c.dev/invoke-meta-trace-id {:invoke-trace-id "invoke-kw"})))
    (is (= "invoke-str"
           (#'futon3c.dev/invoke-meta-trace-id {"invoke-trace-id" "invoke-str"})))
    (is (= "invoke-camel"
           (#'futon3c.dev/invoke-meta-trace-id {"invokeTraceId" "invoke-camel"})))))

(deftest configured-codex-cwd-prefers-workspace-root
  (testing "Codex defaults to the nearest AGENTS.md ancestor instead of repo-local user.dir"
    (let [original-user-dir (System/getProperty "user.dir")]
      (with-redefs [futon3c.dev/env (fn [_k & [_default]] nil)]
        (is (= "/home/joe/code"
               (#'futon3c.dev/workspace-root-dir "/home/joe/code/futon3c")))
        (System/setProperty "user.dir" "/home/joe/code/futon3c")
        (try
          (is (= "/home/joe/code"
                 (dev/configured-codex-cwd)))
          (finally
            (System/setProperty "user.dir" original-user-dir)))))))

(deftest configured-codex-cwd-allows-env-override
  (testing "CODEX_CWD overrides workspace-root auto-detection"
    (with-redefs [futon3c.dev/env (fn [k & [_default]]
                                    (when (= k "CODEX_CWD")
                                      "/tmp/codex-override"))]
      (is (= "/tmp/codex-override"
             (dev/configured-codex-cwd))))))

(deftest record-invoke-delivery-uses-agent-emacs-socket
  (testing "delivery updates target the same emacs socket used by the agent invoke buffer"
    (let [calls (atom [])]
      (with-redefs [futon3c.agency.registry/get-agent
                    (fn [_agent-id] {:agent/metadata {:emacs-socket "workspace1"}})
                    futon3c.blackboard/blackboard-eval!
                    (fn [elisp opts]
                      (swap! calls conj {:elisp elisp :opts opts})
                      {:ok true :output "\"replaced\""})]
        (is (true? (futon3c.dev/record-invoke-delivery!
                    "claude-1"
                    "invoke-xyz"
                    {:surface "whistle"
                     :destination "caller joe"
                     :delivered? true
                     :note "whistle-response"})))
        (is (= 1 (count @calls)))
        (is (= "workspace1" (get-in (first @calls) [:opts :emacs-socket])))
        (is (.contains (get-in (first @calls) [:elisp]) "insert-file-contents"))
        (is (not (.contains (get-in (first @calls) [:elisp]) " via whistle -> caller joe")))))))

(deftest record-invoke-delivery-uses-irc-in-mfuton-mode
  (testing "mfuton mode projects invoke delivery receipts to the primary IRC room instead of Emacs"
    (let [irc-calls (atom [])
          emacs-calls (atom [])]
      (with-redefs [futon3c.mfuton-mode/mfuton-mode? (constantly true)
                    futon3c.dev.config/env (fn [k & [default]]
                                             (if (= k "IRC_CHANNEL")
                                               "#futon"
                                               default))
                    futon3c.agency.registry/get-agent
                    (fn [_agent-id] {:agent/metadata {:emacs-socket "workspace1"}})
                    futon3c.blackboard/blackboard-eval!
                    (fn [_elisp _opts]
                      (swap! emacs-calls conj :called)
                      {:ok true :output "\"replaced\""})
                    futon3c.dev.irc/send-irc!
                    (fn [channel from-nick message]
                      (swap! irc-calls conj {:channel channel
                                             :from-nick from-nick
                                             :message message})
                      true)]
        (is (true? (futon3c.dev/record-invoke-delivery!
                    "codex-1"
                    "invoke-xyz"
                    {:surface "whistle"
                     :destination "caller joe"
                     :delivered? true
                     :note "whistle-response"})))
        (is (empty? @emacs-calls))
        (is (= 1 (count @irc-calls)))
        (is (= "#futon" (get-in (first @irc-calls) [:channel])))
        (is (= "codex" (get-in (first @irc-calls) [:from-nick])))
        (is (.contains (get-in (first @irc-calls) [:message])
                       "[invoke-delivery] codex-1 Delivery: delivered via whistle -> caller joe"))
        (is (.contains (get-in (first @irc-calls) [:message]) "(trace-id invoke-xyz)"))))))

(deftest tickle-system-prompt-prefers-corpus-names-over-futon6-paths
  (testing "Tickle prompt no longer seeds stale corpus context or absolute futon6 data paths into IRC-visible coordination"
    (let [prompt @#'futon3c.dev/tickle-system-prompt]
      (is (not (.contains prompt "/home/joe/code/futon6/data/")))
      (is (not (.contains prompt "~/code/futon6/data/")))
      (is (.contains prompt "Do NOT reference Category Theory, PlanetMath, or arXiv"))
      (is (not (.contains prompt "arxiv-math-ct-eprints")))
      (is (not (.contains prompt "pm-full-dictionary.json"))))))

(deftest format-codex-status-board-renders-last-invoke-snapshot
  (testing "Codex status board preserves last invoke details after the agent returns idle"
    (let [board (#'futon3c.dev/format-codex-status-board
                 {"codex-1" {:lifecycle-status :resting
                             :phase :completed
                             :updated-at "2026-03-09T21:00:00Z"
                             :started-at "2026-03-09T20:59:00Z"
                             :session-id "sess-codex"
                             :prompt-preview "Investigate delivery pending in invoke window"
                             :last-terminal {:status :done
                                             :finished-at "2026-03-09T21:00:00Z"
                                             :result-preview "Recorded delivery and projected refs"
                                             :invoke-trace-id "invoke-abc"
                                             :execution {:executed? true
                                                         :tool-events 2
                                                         :command-events 1}}
                             :trace ["1s using Read" "4s using Edit"]}})]
      (is (.contains board "Codex Code"))
      (is (.contains board "codex-1"))
      (is (.contains board "Status: resting"))
      (is (.contains board "Phase: completed"))
      (is (.contains board "Last terminal: done at 2026-03-09T21:00:00Z"))
      (is (.contains board "Session: sess-codex"))
      (is (.contains board "Last trace: invoke-abc"))
      (is (.contains board "Last evidence: executed=true, tool-events=2, command-events=1"))
      (is (.contains board "Last outcome: Recorded delivery and projected refs"))
      (is (.contains board "Recent transitions:")))))

(deftest format-codex-status-board-accepts-executed-without-question-mark
  (testing "Codex status board accepts remote invoke-job execution maps and infers terminal status from old snapshots"
    (let [board (#'futon3c.dev/format-codex-status-board
                 {"codex-1" {:lifecycle-status :resting
                             :finished-at "2026-03-09T21:03:00Z"
                             :execution {:executed true
                                         :tool-events 3
                                         :command-events 4}}})]
      (is (.contains board "Last terminal: done at 2026-03-09T21:03:00Z"))
      (is (.contains board "Last evidence: executed=true, tool-events=3, command-events=4")))))

(deftest format-codex-status-board-separates-active-state-from-last-terminal
  (testing "Active Codex invokes render previous completion as last terminal, not as the current run"
    (let [board (#'futon3c.dev/format-codex-status-board
                 {"codex-1" {:lifecycle-status :invoking
                             :phase :executing
                             :updated-at "2026-03-09T21:25:05Z"
                             :started-at "2026-03-09T21:24:50Z"
                             :session-id "sess-live"
                             :prompt-preview "Current prompt"
                             :activity "preparing response"
                             :trace ["7s preparing response"]
                             :finished-at "2026-03-09T20:54:58Z"
                             :result-preview "Old outcome"
                             :invoke-trace-id "invoke-old"
                             :execution {:executed true
                                         :tool-events 10
                                         :command-events 10}
                             :last-terminal-status :done}})]
      (is (.contains board "Status: invoking"))
      (is (.contains board "Phase: executing"))
      (is (.contains board "Detail: preparing response"))
      (is (.contains board "Last terminal: done at 2026-03-09T20:54:58Z"))
      (is (.contains board "Last trace: invoke-old"))
      (is (.contains board "Last outcome: Old outcome"))
      (is (not (.contains board "\n  Finished: ")))
      (is (not (.contains board "\n  Outcome: ")))
      (is (not (.contains board "\n  Trace: "))))))

(deftest format-codex-status-board-renders-verified-runtime-state
  (testing "Active Codex invokes expose verified PID/output/process-tree facts"
    (let [board (#'futon3c.dev/format-codex-status-board
                 {"codex-1" {:lifecycle-status :invoking
                             :phase :executing
                             :session-id "sess-runtime"
                             :prompt-preview "Run the solver for n=8"
                             :runtime {:process-state :running
                                       :root-pid 2478123
                                       :child-pids [2478124 2478125]
                                       :live-pids [2478123 2478124 2478125]
                                       :processes [{:pid 2478123
                                                    :command "codex exec --json"}
                                                   {:pid 2478124
                                                    :command "python3 scripts/fm001/ramsey_book_sat.py --n 8"}
                                                   {:pid 2478125
                                                    :command "z3 -smt2 /tmp/fm001.smt2"}]
                                       :last-command "python3 scripts/fm001/ramsey_book_sat.py --n 8"
                                       :last-output-at "2026-03-10T10:13:45Z"
                                       :last-output-stream :stderr
                                       :last-output-bytes 128
                                       :total-output-bytes 4096}
                             :trace ["5s Using Bash"]}})]
      (is (.contains board "Runtime: running (root pid 2478123, live 3, children 2)"))
      (is (.contains board "Command: python3 scripts/fm001/ramsey_book_sat.py --n 8"))
      (is (.contains board "Last output: stderr"))
      (is (.contains board "Live processes:"))
      (is (.contains board "2478124 python3 scripts/fm001/ramsey_book_sat.py --n 8"))
      (is (.contains board "2478125 z3 -smt2 /tmp/fm001.smt2")))))

(deftest format-codex-status-board-persists-detached-launch-evidence
  (testing "terminal snapshots keep detached/background launch commands without claiming liveness"
    (let [board (#'futon3c.dev/format-codex-status-board
                 {"codex-1" {:lifecycle-status :resting
                             :phase :completed
                             :last-terminal {:status :done
                                             :finished-at "2026-03-10T11:35:00Z"
                                             :result-preview "solver launched in tmux"
                                             :execution {:executed? true
                                                         :tool-events 1
                                                         :command-events 1}
                                             :runtime {:process-state :exited
                                                       :root-pid 2501000
                                                       :background-command "tmux new-session -d -s fm8 'python3 scripts/fm001/ramsey_book_sat.py --n 8'"
                                                       :last-command "tmux new-session -d -s fm8 'python3 scripts/fm001/ramsey_book_sat.py --n 8'"}}}})]
      (is (.contains board "Runtime: exited (root pid 2501000"))
      (is (.contains board "Detached launch observed: tmux new-session -d -s fm8"))
      (is (.contains board "not verified after invoke exit")))))

(deftest format-codex-status-board-handles-empty-state
  (testing "Codex status board is explicit when nothing has been recorded yet"
    (let [board (#'futon3c.dev/format-codex-status-board {})]
      (is (.contains board "No Codex invokes recorded yet.")))))

(deftest invoke-response->irc-reply-covers-success-and-failure
  (testing "successful invoke with text preserves refs in summary"
    (let [out (#'futon3c.dev/invoke-response->irc-reply
               {:ok true
                :result "Updated docs and committed 29d18a9 in /home/joe/code/futon3c/README.md"})]
      (is (.contains out "refs:"))
      (is (.contains out "29d18a9"))))
  (testing "successful invoke with nil result still returns a non-blank fallback"
    (let [out (#'futon3c.dev/invoke-response->irc-reply {:ok true :result nil})]
      (is (string? out))
      (is (not (str/blank? out)))
      (is (.contains out "invoke completed"))))
  (testing "failed invoke always returns a visible failure marker"
    (let [out (#'futon3c.dev/invoke-response->irc-reply
               {:ok false :error {:error/message "timeout waiting for response"}})]
      (is (.contains out "[invoke failed]"))
      (is (.contains out "timeout")))))

(deftest codex-invoke-enforces-execution-retry-for-work-claims
  (testing "work claim with zero execution evidence triggers one enforced retry"
    (let [calls (atom [])
          responses (atom
                     [{:result "I'll take F1-opposite."
                       :session-id "sess-1"
                       :execution {:executed? false :tool-events 0 :command-events 0}}
                      {:result "Ran first search and logged notes in /tmp/f1.md"
                       :session-id "sess-1"
                       :execution {:executed? true :tool-events 1 :command-events 0}}])]
      (with-redefs [futon3c.agents.codex-cli/make-invoke-fn
                    (fn [_opts]
                      (fn [prompt sid]
                        (swap! calls conj {:prompt prompt :sid sid})
                        (let [resp (first @responses)]
                          (swap! responses subvec 1)
                          resp)))
                    futon3c.dev/emit-invoke-evidence! (fn [& _] nil)
                    futon3c.dev/preferred-session-id (fn [& _] "sess-1")
                    futon3c.dev/persist-session-id! (fn [& _] nil)
                    futon3c.dev/start-invoke-ticker! (fn [& _] (fn [] nil))
                    futon3c.blackboard/blackboard! (fn [& _] {:ok true})]
        (let [invoke-fn (dev/make-codex-invoke-fn {:agent-id "codex-1"})
              result (invoke-fn "Take F1-opposite and start now" nil)]
          (is (= 2 (count @calls)))
          (is (re-find #"work/progress claim without execution evidence"
                       (-> @calls second :prompt)))
          (is (nil? (:error result)))
          (is (true? (get-in result [:execution :executed?])))
          (is (true? (get-in result [:execution :enforced-retry?]))))))))

(deftest codex-invoke-fails-when-work-claim-still-has-no-execution-after-retry
  (testing "second work-claim with zero execution evidence is surfaced as invoke error"
    (let [responses (atom
                     [{:result "I'll start now."
                       :session-id "sess-1"
                       :execution {:executed? false :tool-events 0 :command-events 0}}
                      {:result "Still starting now."
                       :session-id "sess-1"
                       :execution {:executed? false :tool-events 0 :command-events 0}}])]
      (with-redefs [futon3c.agents.codex-cli/make-invoke-fn
                    (fn [_opts]
                      (fn [_prompt _sid]
                        (let [resp (first @responses)]
                          (swap! responses subvec 1)
                          resp)))
                    futon3c.dev/emit-invoke-evidence! (fn [& _] nil)
                    futon3c.dev/preferred-session-id (fn [& _] "sess-1")
                    futon3c.dev/persist-session-id! (fn [& _] nil)
                    futon3c.dev/start-invoke-ticker! (fn [& _] (fn [] nil))
                    futon3c.blackboard/blackboard! (fn [& _] {:ok true})]
        (let [invoke-fn (dev/make-codex-invoke-fn {:agent-id "codex-1"})
              result (invoke-fn "Take F1-opposite and start now" nil)]
          (is (some? (:error result)))
          (is (.contains (str (:error result))
                         "work-claim without execution evidence after enforcement retry"))
          (is (= 0 (get-in result [:execution :tool-events])))
          (is (= 0 (get-in result [:execution :command-events]))))))))

(deftest agent-invoke-complete-hook-bells-tickle-when-returning-to-idle
  (testing "registry completion hook delegates to the availability bell"
    (let [bell-calls (atom [])]
      (with-redefs [futon3c.dev/bell-tickle-available!
                    (fn [agent-id payload]
                      (swap! bell-calls conj {:agent-id agent-id :payload payload})
                      nil)]
        (#'futon3c.dev/on-agent-invoke-complete!
         {:agent/id {:id/value "codex-1" :id/type :continuity}}
         {:result "Completed work."
          :session-id "sess-1"
          :invoke-trace-id "invoke-123"})
        (is (= 1 (count @bell-calls)))
        (is (= "codex-1" (:agent-id (first @bell-calls))))
        (is (= true (get-in (first @bell-calls) [:payload :ok?])))
        (is (= "sess-1" (get-in (first @bell-calls) [:payload :session-id])))
        (is (= "invoke-123" (get-in (first @bell-calls) [:payload :invoke-trace-id])))))))

(deftest codex-invoke-fails-task-mode-nonplanning-reply-without-execution
  (testing "task-mode no-evidence reply without planning-only marker is rejected after retry"
    (let [calls (atom [])
          task-prompt "[Surface: IRC | Channel: #math | Speaker: joe | Mode: task | Execute work asynchronously.]\\nPlease investigate."
          responses (atom
                     [{:result "Loaded data/proof-state/FM-001.edn; moving on."
                       :session-id "sess-1"
                       :execution {:executed? false :tool-events 0 :command-events 0}}
                      {:result "Loaded data/proof-state/FM-001.edn; still moving on."
                       :session-id "sess-1"
                       :execution {:executed? false :tool-events 0 :command-events 0}}])]
      (with-redefs [futon3c.agents.codex-cli/make-invoke-fn
                    (fn [_opts]
                      (fn [prompt sid]
                        (swap! calls conj {:prompt prompt :sid sid})
                        (let [resp (first @responses)]
                          (swap! responses subvec 1)
                          resp)))
                    futon3c.dev/emit-invoke-evidence! (fn [& _] nil)
                    futon3c.dev/preferred-session-id (fn [& _] "sess-1")
                    futon3c.dev/persist-session-id! (fn [& _] nil)
                    futon3c.dev/start-invoke-ticker! (fn [& _] (fn [] nil))
                    futon3c.blackboard/blackboard! (fn [& _] {:ok true})]
        (let [invoke-fn (dev/make-codex-invoke-fn {:agent-id "codex-1"})
              result (invoke-fn task-prompt nil)]
          (is (= 2 (count @calls)))
          (is (some? (:error result)))
          (is (.contains (str (:error result))
                         "work-claim without execution evidence after enforcement retry")))))))

(deftest codex-invoke-allows-proof-authoring-turn-without-execution-evidence
  (testing "text-authoring proof-peripheral phases do not trigger execution enforcement"
    (let [calls (atom [])
          prompt (str "[Surface: IRC | Channel: #math | Speaker: joe | Mode: task | Execute work asynchronously.]\n"
                      "Execution evidence required: no.\n"
                      "You are in the OBSERVE phase of the proof peripheral.\n"
                      "Your reply is the authoritative phase record.")
          response {:result "**WHAT IS REALLY BEING ASKED**\n- This asks how the residue controls derivative asymptotics.\n\n**WHY IT IS HARD**\n- The trap is forgetting to subtract the principal part before using Cauchy estimates."
                    :session-id "sess-1"
                    :execution {:executed? false :tool-events 0 :command-events 0}}]
      (with-redefs [futon3c.agents.codex-cli/make-invoke-fn
                    (fn [_opts]
                      (fn [seen-prompt sid]
                        (swap! calls conj {:prompt seen-prompt :sid sid})
                        response))
                    futon3c.dev/emit-invoke-evidence! (fn [& _] nil)
                    futon3c.dev/preferred-session-id (fn [& _] "sess-1")
                    futon3c.dev/persist-session-id! (fn [& _] nil)
                    futon3c.dev/start-invoke-ticker! (fn [& _] (fn [] nil))
                    futon3c.blackboard/blackboard! (fn [& _] {:ok true})]
        (let [invoke-fn (dev/make-codex-invoke-fn {:agent-id "codex-1"})
              result (invoke-fn prompt nil)]
          (is (= 1 (count @calls)))
          (is (nil? (:error result)))
          (is (.contains (:result result) "WHAT IS REALLY BEING ASKED")))))))

(deftest codex-invoke-retries-micro-increment-task-update
  (testing "task-mode reconnaissance-only update with execution evidence is retried for closure"
    (let [calls (atom [])
          task-prompt "[Surface: IRC | Channel: #futon | Speaker: joe | Mode: task | Prefer completing one bounded unit of work in this turn.]\\n@codex I think tickle-llm is stale"
          responses (atom
                     [{:result "@joe logged an initial #56 step by grepping for `tickle-llm` to map every registration/startup call before we disable the stale agent entry."
                       :session-id "sess-1"
                       :execution {:executed? true :tool-events 1 :command-events 1}}
                      {:result "@joe removed stale tickle-llm registration in ~/code/futon3c/dev/futon3c/dev.clj refs: ~/code/futon3c/dev/futon3c/dev.clj"
                       :session-id "sess-1"
                       :execution {:executed? true :tool-events 2 :command-events 1}}])]
      (with-redefs [futon3c.agents.codex-cli/make-invoke-fn
                    (fn [_opts]
                      (fn [prompt sid]
                        (swap! calls conj {:prompt prompt :sid sid})
                        (let [resp (first @responses)]
                          (swap! responses subvec 1)
                          resp)))
                    futon3c.dev/emit-invoke-evidence! (fn [& _] nil)
                    futon3c.dev/preferred-session-id (fn [& _] "sess-1")
                    futon3c.dev/persist-session-id! (fn [& _] nil)
                    futon3c.dev/start-invoke-ticker! (fn [& _] (fn [] nil))
                    futon3c.blackboard/blackboard! (fn [& _] {:ok true})]
        (let [invoke-fn (dev/make-codex-invoke-fn {:agent-id "codex-1"})
              result (invoke-fn task-prompt nil)]
          (is (= 2 (count @calls)))
          (is (re-find #"too small for a task/work turn"
                       (-> @calls second :prompt)))
          (is (nil? (:error result)))
          (is (true? (get-in result [:execution :enforced-retry?])))
          (is (.contains (:result result) "removed stale tickle-llm registration")))))))

(deftest codex-invoke-fails-brief-mission-reply-without-execution
  (testing "mission/work prompt still requires execution even when not explicitly mode: task"
    (let [responses (atom
                     [{:result "@joe FM-001 state of play captured in data/proof-state/FM-001.edn"
                       :session-id "sess-1"
                       :execution {:executed? false :tool-events 0 :command-events 0}}
                      {:result "@joe FM-001 still captured."
                       :session-id "sess-1"
                       :execution {:executed? false :tool-events 0 :command-events 0}}])]
      (with-redefs [futon3c.agents.codex-cli/make-invoke-fn
                    (fn [_opts]
                      (fn [_prompt _sid]
                        (let [resp (first @responses)]
                          (swap! responses subvec 1)
                          resp)))
                    futon3c.dev/emit-invoke-evidence! (fn [& _] nil)
                    futon3c.dev/preferred-session-id (fn [& _] "sess-1")
                    futon3c.dev/persist-session-id! (fn [& _] nil)
                    futon3c.dev/start-invoke-ticker! (fn [& _] (fn [] nil))
                    futon3c.blackboard/blackboard! (fn [& _] {:ok true})]
        (let [invoke-fn (dev/make-codex-invoke-fn {:agent-id "codex-1"})
              result (invoke-fn "@codex can you give me a summary of the state of play on FM-001" nil)]
          (is (some? (:error result)))
          (is (.contains (str (:error result))
                         "work-claim without execution evidence after enforcement retry")))))))
