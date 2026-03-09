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
  (testing "invoke trace shows only metadata and writes full payload to disk"
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
      (is (.contains block "--- response trace (metadata only) ---"))
      (is (.contains block "Result: kind=structured"))
      (is (.contains block ", chars="))
      (is (.contains block "sha256="))
      (is (.contains block (str "Delivery: pending (trace-id " trace-id ")")))
      (is (.contains block "Delivery guarantee: caller must record where reply was sent."))
      (is artifact-path)
      (is (not (.contains block "thread_id")))
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
        (is (= "workspace1" (get-in (first @calls) [:opts :emacs-socket])))))))

(deftest tickle-system-prompt-prefers-corpus-names-over-futon6-paths
  (testing "Tickle prompt no longer seeds absolute futon6 data paths into IRC-visible coordination"
    (let [prompt @#'futon3c.dev/tickle-system-prompt]
      (is (not (.contains prompt "/home/joe/code/futon6/data/")))
      (is (not (.contains prompt "~/code/futon6/data/")))
      (is (.contains prompt "arxiv-math-ct-eprints"))
      (is (.contains prompt "pm-full-dictionary.json")))))

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
