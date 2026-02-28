(ns futon3c.agents.codex-cli-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.agents.codex-cli :as codex-cli]))

(deftest parse-output-prefers-agent-message-and-thread-id
  (testing "thread.started + item.completed agent_message"
    (let [raw (str "{\"type\":\"thread.started\",\"thread_id\":\"tid-123\"}\n"
                   "{\"type\":\"item.completed\",\"item\":{\"type\":\"agent_message\",\"content\":[{\"type\":\"text\",\"text\":\"hello\"}]}}\n")
          parsed (codex-cli/parse-output raw nil)]
      (is (= "tid-123" (:session-id parsed)))
      (is (= "hello" (:text parsed))))))

(deftest parse-output-falls-back-to-prior-session-and-error-message
  (testing "no thread.start event keeps prior session id"
    (let [raw "{\"type\":\"error\",\"message\":\"boom\"}\n"
          parsed (codex-cli/parse-output raw "sid-prior")]
      (is (= "sid-prior" (:session-id parsed)))
      (is (= "boom" (:text parsed))))))

(deftest build-exec-args-new-and-resume
  (testing "new session appends '-'"
    (let [args (codex-cli/build-exec-args {:codex-bin "codex"
                                           :model "gpt-5-codex"
                                           :sandbox "workspace-write"
                                           :approval-policy "never"})]
      (is (= "codex" (first args)))
      (is (some #{"exec"} args))
      (is (some #{"--json"} args))
      (is (some #{"--model"} args))
      (is (= "-" (last args)))))
  (testing "resume session includes resume <sid> -"
    (let [args (codex-cli/build-exec-args {:codex-bin "codex"
                                           :model nil
                                           :sandbox "workspace-write"
                                           :approval-policy "never"
                                           :session-id "sid-1"})
          idx (.indexOf args "resume")]
      (is (>= idx 0))
      (is (= "sid-1" (nth args (inc idx))))
      (is (= "-" (last args))))))

(deftest build-exec-args-defaults-to-full-access
  (let [args (codex-cli/build-exec-args {:codex-bin "codex"})]
    (is (some #{"--sandbox"} args))
    (is (some #{"danger-full-access"} args))
    (is (some #{"approval_policy=\"never\""} args))))

(deftest make-invoke-fn-success-and-error-paths
  (testing "successful invoke returns parsed text and session-id"
    (let [calls (atom [])
          invoke (codex-cli/make-invoke-fn {:codex-bin "codex"
                                            :model nil
                                            :sandbox "workspace-write"
                                            :approval-policy "never"
                                            :cwd "/tmp"})
          fake-run (fn [cmd prompt opts]
                     (swap! calls conj {:cmd cmd :prompt prompt :opts opts})
                     {:exit 0
                      :timed-out? false
                      :session-id "sid-new"
                      :text "answer"
                      :error-text nil
                      :stderr ""
                      :raw-output (str "{\"type\":\"thread.started\",\"thread_id\":\"sid-new\"}\n"
                                       "{\"type\":\"item.completed\",\"item\":{\"type\":\"agent_message\",\"text\":\"answer\"}}\n")})]
      (with-redefs [codex-cli/run-codex-stream! fake-run]
        (let [resp (invoke "hello codex" nil)]
          (is (= "answer" (:result resp)))
          (is (= "sid-new" (:session-id resp)))
          (is (nil? (:error resp)))
          (is (map? (:execution resp)))
          (let [{:keys [cmd prompt opts]} (first @calls)]
            (is (= "codex" (first cmd)))
            (is (= "exec" (second cmd)))
            (is (some #{"--json"} cmd))
            (is (some #{"-"} cmd))
            (is (= "hello codex" prompt))
            (is (= 600000 (:timeout-ms opts)))
            (is (= "/tmp" (:cwd opts))))))))
  (testing "non-zero exit returns error and preserves prior session-id fallback"
    (let [invoke (codex-cli/make-invoke-fn {:codex-bin "codex"
                                            :model nil
                                            :sandbox "workspace-write"
                                            :approval-policy "never"})]
      (with-redefs [codex-cli/run-codex-stream! (fn [& _]
                                                  {:exit 2
                                                   :timed-out? false
                                                   :session-id nil
                                                   :text nil
                                                   :error-text "nope"
                                                   :stderr "nope"
                                                   :raw-output "{\"type\":\"error\",\"message\":\"nope\"}\n"})]
        (let [resp (invoke "x" "sid-old")]
          (is (nil? (:result resp)))
          (is (= "sid-old" (:session-id resp)))
          (is (string? (:error resp)))
          (is (map? (:execution resp)))
          (is (str/includes? (:error resp) "Exit 2")))))))

(deftest make-invoke-fn-recovers-from-stale-resume-action-type-error
  (let [calls (atom [])
        invoke (codex-cli/make-invoke-fn {:codex-bin "codex"
                                          :model nil
                                          :sandbox "workspace-write"
                                          :approval-policy "never"})
        stale-msg "Invalid value: 'other'. Supported values are: 'search', 'open_page', and 'find_in_page'. param: input[83].action.type"
        fake-run (fn [cmd _prompt _opts]
                   (swap! calls conj cmd)
                   (if (some #{"resume"} cmd)
                     {:exit 1
                      :timed-out? false
                      :session-id nil
                      :text nil
                      :error-text stale-msg
                      :stderr stale-msg
                      :raw-output (str "{\"type\":\"error\",\"message\":\"" stale-msg "\"}\n")}
                     {:exit 0
                      :timed-out? false
                      :session-id "sid-new"
                      :text "recovered"
                      :error-text nil
                      :stderr ""
                      :raw-output (str "{\"type\":\"thread.started\",\"thread_id\":\"sid-new\"}\n"
                                       "{\"type\":\"item.completed\",\"item\":{\"type\":\"agent_message\",\"text\":\"recovered\"}}\n")}))]
    (with-redefs [codex-cli/run-codex-stream! fake-run]
      (let [resp (invoke "hello" "sid-old")]
        (is (= "recovered" (:result resp)))
        (is (= "sid-new" (:session-id resp)))
        (is (nil? (:error resp)))
        (is (= 2 (count @calls)))
        (is (some #{"resume"} (first @calls)))
        (is (not (some #{"resume"} (second @calls))))))))

(deftest event->activity-maps-tool-and-reasoning-events
  (is (= "using bash"
         (codex-cli/event->activity
          {:type "item.started"
           :item {:type "tool_call" :name "command_execution"}})))
  (is (= "preparing response"
         (codex-cli/event->activity {:type "reasoning"}))))

(deftest enforce-execution-guard-rewrites-promise-without-runtime-evidence
  (let [text "I'll start now and push in a minute."
        execution {:tool-events 0 :command-events 0 :executed? false}
        guarded (codex-cli/enforce-execution-guard text execution)]
    (is (str/includes? guarded "Planning-only"))
    (is (str/includes? guarded "No work has started"))))

(deftest enforce-execution-guard-keeps-promise-when-runtime-evidence-exists
  (let [text "I'll start now and push in a minute."
        execution {:tool-events 2 :command-events 1 :executed? true}]
    (is (= text (codex-cli/enforce-execution-guard text execution)))))
