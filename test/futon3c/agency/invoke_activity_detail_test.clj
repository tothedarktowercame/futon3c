(ns futon3c.agency.invoke-activity-detail-test
  "An activity string must name the work, not the tool.

  2026-09-09: `claude-5` read `using Bash` for eight minutes while it ran the
  queue-supervisor tests. Fresh stamp, healthy lane, and still the only way to
  learn what it was doing was to open its transcript."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.agency.invoke-activity :as act]))

(defn- bash [input] {:name "Bash" :input input})

(deftest bash-prefers-the-description-the-model-wrote
  (is (= "bash: Run queue supervisor tests"
         (act/tool-details->activity
          [(bash {:description "Run queue supervisor tests"
                  :command "timeout 600 clojure -M:test"})]))))

(deftest bash-without-a-description-falls-back-to-the-command
  (testing "the first line only — a heredoc body is not a status line"
    (is (= "bash: git status --porcelain"
           (act/tool-details->activity
            [(bash {:command "git status --porcelain\nfoo\nbar"})])))))

(deftest a-bare-tool-name-is-still-better-than-nothing
  (is (= "bash" (act/tool-details->activity [(bash {})])))
  (is (= "bash" (act/tool-details->activity [{:name "Bash"}]))))

(deftest file-tools-name-their-file-not-its-path
  (is (= "read: problem_queue_supervisor.clj"
         (act/tool-details->activity
          [{:name "Read" :input {:file_path "/home/joe/code/futon3c/src/futon3c/apm/problem_queue_supervisor.clj"}}])))
  (is (= "edit: registry.clj"
         (act/tool-details->activity
          [{:name "Edit" :input {:file_path "src/futon3c/agency/registry.clj"}}]))))

(deftest search-tools-name-the-pattern
  (is (= "grep: frame-ordinal in apm"
         (act/tool-details->activity
          [{:name "Grep" :input {:pattern "frame-ordinal" :path "src/futon3c/apm"}}]))))

(deftest subagents-name-their-errand
  (is (= "task: Audit ordinal accounting"
         (act/tool-details->activity
          [{:name "Task" :input {:description "Audit ordinal accounting"
                                 :subagent_type "Explore"}}]))))

(deftest mcp-tools-read-as-server-slash-verb
  (is (= "gmail/send_message: joe@example.com"
         (act/tool-details->activity
          [{:name "mcp__gmail__send_message" :input {:to "joe@example.com"}}]))))

(deftest parallel-calls-are-both-visible
  (is (= "bash: Lint changed files + read: deps.edn"
         (act/tool-details->activity
          [(bash {:description "Lint changed files"})
           {:name "Read" :input {:file_path "/home/joe/code/futon3c/deps.edn"}}]))))

(deftest a-long-detail-is-clipped-not-wrapped
  (let [s (act/tool-details->activity
           [(bash {:command (apply str "clojure -M:test " (repeat 400 "x"))})])]
    (is (<= (count s) 72) "the roster renders this on one indented line")
    (is (str/starts-with? s "bash: clojure -M:test"))
    (is (str/ends-with? s "…") "clipping must be visible, or a reader trusts a truncated command")))

(deftest no-tool-calls-is-no-activity
  (is (nil? (act/tool-details->activity [])))
  (is (nil? (act/tool-details->activity nil))))
