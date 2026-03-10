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

(deftest parse-output-ignores-placeholder-agent-messages
  (testing "tool narration placeholders do not count as final assistant text"
    (let [raw (str "{\"type\":\"thread.started\",\"thread_id\":\"tid-456\"}\n"
                   "{\"type\":\"item.completed\",\"item\":{\"type\":\"agent_message\",\"text\":\"Using Bash\"}}\n")
          parsed (codex-cli/parse-output raw nil)]
      (is (= "tid-456" (:session-id parsed)))
      (is (= "[No assistant message returned]" (:text parsed))))))

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
            (is (= 1800000 (:timeout-ms opts)))
            (is (= "/tmp" (:cwd opts))))))))
  (testing "placeholder tool narration does not leak as final response text"
    (let [invoke (codex-cli/make-invoke-fn {:codex-bin "codex"
                                            :model nil
                                            :sandbox "workspace-write"
                                            :approval-policy "never"})]
      (with-redefs [codex-cli/run-codex-stream! (fn [& _]
                                                  {:exit 0
                                                   :timed-out? false
                                                   :session-id "sid-placeholder"
                                                   :text "Using Bash"
                                                   :error-text nil
                                                   :stderr ""
                                                   :raw-output (str "{\"type\":\"thread.started\",\"thread_id\":\"sid-placeholder\"}\n"
                                                                    "{\"type\":\"item.completed\",\"item\":{\"type\":\"agent_message\",\"text\":\"Using Bash\"}}\n")})]
        (let [resp (invoke "status?" nil)]
          (is (= "[Codex produced no text response]" (:result resp)))
          (is (= "sid-placeholder" (:session-id resp)))
          (is (nil? (:error resp)))))))
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
  (is (= "using bash"
         (codex-cli/event->activity
          {:type "item.started"
           :item {:type "command_execution"
                  :command "/bin/bash -lc 'ls'"}})))
  (is (= "using bash (done)"
         (codex-cli/event->activity
          {:type "item.completed"
           :item {:type "command_execution"
                  :command "/bin/bash -lc 'ls'"
                  :status "completed"}})))
  (is (= "preparing response"
         (codex-cli/event->activity {:type "reasoning"}))))

(deftest command-execution-item-events-count-as-execution-evidence
  (let [evt-start {:type "item.started"
                   :item {:type "command_execution"
                          :command "/bin/bash -lc 'ls'"}}
        evt-done {:type "item.completed"
                  :item {:type "command_execution"
                         :command "/bin/bash -lc 'ls'"
                         :status "completed"
                         :exit_code 0}}]
    (is (true? (#'futon3c.agents.codex-cli/tool-event? evt-start)))
    (is (true? (#'futon3c.agents.codex-cli/tool-event? evt-done)))
    (is (true? (#'futon3c.agents.codex-cli/command-event? evt-start)))
    (is (true? (#'futon3c.agents.codex-cli/command-event? evt-done)))))

(deftest run-codex-stream-emits-runtime-events
  (testing "real subprocess launch emits verified process/output lifecycle callbacks"
    (let [events (atom [])
          result (codex-cli/run-codex-stream!
                  ["bash" "-lc"
                   "printf '{\"type\":\"thread.started\",\"thread_id\":\"sid-runtime\"}\\n'; printf 'stderr-line\\n' 1>&2"]
                  "ignored"
                  {:timeout-ms 5000
                   :on-runtime-event #(swap! events conj %)})
          kinds (map :kind @events)
          start-event (some #(when (= :process-started (:kind %)) %) @events)
          stdout-event (some #(when (and (= :output (:kind %))
                                         (= :stdout (:stream %))) %)
                             @events)
          stderr-event (some #(when (and (= :output (:kind %))
                                         (= :stderr (:stream %))) %)
                             @events)
          exit-event (some #(when (= :process-exit (:kind %)) %) @events)]
      (is (= 0 (:exit result)))
      (is (= "sid-runtime" (:session-id result)))
      (is (some #{:process-started} kinds))
      (is (some #{:output} kinds))
      (is (some #{:process-exit} kinds))
      (is (number? (:pid start-event)))
      (is (= (:pid start-event) (:pid exit-event)))
      (is (= :stdout (:stream stdout-event)))
      (is (= :stderr (:stream stderr-event)))
      (is (pos? (or (:bytes stdout-event) 0)))
      (is (pos? (or (:bytes stderr-event) 0))))))
