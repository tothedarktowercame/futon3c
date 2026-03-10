(ns futon3c.agents.codex-code-logic-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.agents.codex-code-logic :as codex-logic]))

(deftest query-violations-clean-when-stores-agree
  (testing "registry, canonical jobs, and public announcements tell the same story"
    (let [db (codex-logic/build-db
              {:registry-status
               {:agents {"codex-1" {:status :invoking
                                    :session-id "sid-1"
                                    :queued-jobs 0
                                    :running-jobs 1
                                    :nonterminal-jobs 1}}}
               :jobs [{:job-id "job-1"
                       :agent-id "codex-1"
                       :state "running"
                       :session-id "sid-1"}]
               :announcements [{:announce-id "a-1"
                                :job-id "job-1"
                                :agent-id "codex-1"
                                :surface "irc"}]})]
      (is (= {:jobs-for-unregistered-agents []
              :running-jobs-on-idle-agents []
              :running-count-mismatches []
              :orphan-announcements []
              :announcement-agent-mismatches []
              :running-session-mismatches []}
             (codex-logic/query-violations db))))))

(deftest query-violations-catches-cross-store-contradictions
  (testing "orphan announcements, idle/running contradictions, and session drift are reported"
    (let [db (codex-logic/build-db
              {:registry-status
               {:agents {"codex-1" {:status :idle
                                    :session-id "sid-reg"
                                    :queued-jobs 0
                                    :running-jobs 1
                                    :nonterminal-jobs 1}}}
               :jobs [{:job-id "job-1"
                       :agent-id "codex-1"
                       :state "running"
                       :session-id "sid-job"}
                      {:job-id "job-ghost"
                       :agent-id "ghost-agent"
                       :state "queued"}]
               :announcements [{:announce-id "a-1"
                                :job-id "missing-job"
                                :agent-id "codex-1"
                                :surface "irc"}
                               {:announce-id "a-2"
                                :job-id "job-1"
                                :agent-id "codex-9"
                                :surface "irc"}]})]
      (is (= #{["job-ghost" "ghost-agent"]}
             (set (codex-logic/query-jobs-for-unregistered-agents db))))
      (is (= #{["job-1" "codex-1" :idle]}
             (set (codex-logic/query-running-jobs-on-idle-agents db))))
      (is (= [["codex-1" 1 :idle]]
             (codex-logic/query-running-count-mismatches db)))
      (is (= #{["a-1" "missing-job"]}
             (set (codex-logic/query-orphan-announcements db))))
      (is (= #{["a-2" "job-1" "codex-9" "codex-1"]}
             (set (codex-logic/query-announcement-agent-mismatches db))))
      (is (= #{["job-1" "codex-1" "sid-job" "sid-reg"]}
             (set (codex-logic/query-running-session-mismatches db)))))))
