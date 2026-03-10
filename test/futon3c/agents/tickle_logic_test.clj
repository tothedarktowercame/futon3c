(ns futon3c.agents.tickle-logic-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.agents.tickle-logic :as tickle-logic])
  (:import [java.time Instant]))

(defn- ts
  [seconds]
  (str (.plusSeconds (Instant/parse "2026-03-10T12:00:00Z") seconds)))

(deftest query-violations-clean-when-facts-align
  (testing "structural and watchdog evidence-backed facts produce no violations"
    (let [db (tickle-logic/build-db
              {:registry-status
               {:agents {"codex-1" {:type :codex
                                    :status :idle
                                    :invoke-ready? true
                                    :invoke-route :local
                                    :capabilities [:coordination/execute :edit]}}}
               :authorities {"codex-1" :registry}
               :assignments [{:agent-id "codex-1" :obligation-id "ob-1"}]
               :pages [{:event-id "p-1"
                        :agent-id "codex-1"
                        :obligation-id "ob-1"
                        :cause :tick}]
               :escalations [{:event-id "e-1"
                              :agent-id "codex-1"
                              :obligation-id "ob-1"
                              :cause :timeout}]
               :precedes [["p-1" "e-1"]]
               :evidence-entries
               [{:evidence/author "codex-1"
                 :evidence/at (ts 0)}
                {:evidence/author "tickle-1"
                 :evidence/at (ts 300)
                 :evidence/tags [:tickle :scan]
                 :evidence/body {:event :scan
                                 :activity {"codex-1" {:last-active (ts 0)
                                                       :stale? false
                                                       :stale-seconds 300}}
                                 :stalled []
                                 :threshold-seconds 600
                                 :cycle-at (ts 300)}}
                {:evidence/author "tickle-1"
                 :evidence/at (ts 360)
                 :evidence/tags [:tickle :page]
                 :evidence/body {:event :page
                                 :agent-id "codex-1"
                                 :method :bell
                                 :at (ts 360)}}
                {:evidence/author "tickle-1"
                 :evidence/at (ts 420)
                 :evidence/tags [:tickle :escalation]
                 :evidence/body {:event :escalation
                                 :agent-id "codex-1"
                                 :cause :page-failed
                                 :at (ts 420)}}]})]
      (is (= {:unregistered-pages []
              :unroutable-pages []
              :pages-without-assignments []
              :pages-without-authority []
              :orphan-escalations []
              :watchdog-pages-without-scans []
              :watchdog-escalations-without-pages []
              :stall-evidence-mismatches []}
             (tickle-logic/query-violations db))))))

(deftest query-violations-catches-structural-mismatches
  (testing "missing authority, assignment, readiness, and causal backing are reported"
    (let [db (tickle-logic/build-db
              {:registry-status
               {:agents {"codex-2" {:type :codex
                                    :status :idle
                                    :invoke-ready? false
                                    :invoke-route :ws
                                    :capabilities [:edit]}}}
               :pages [{:event-id "p-bad"
                        :agent-id "codex-2"
                        :obligation-id "ob-missing"
                        :cause :tick}
                       {:event-id "p-ghost"
                        :agent-id "ghost-agent"
                        :obligation-id "ob-2"
                        :cause :manual}]
               :escalations [{:event-id "e-bad"
                              :agent-id "codex-2"
                              :obligation-id "ob-missing"
                              :cause :timeout}]})]
      (is (= ["p-ghost"]
             (tickle-logic/query-unregistered-pages db)))
      (is (= #{["p-bad" "codex-2"] ["p-ghost" "ghost-agent"]}
             (set (tickle-logic/query-unroutable-pages db))))
      (is (= #{["p-bad" "codex-2" "ob-missing"]
               ["p-ghost" "ghost-agent" "ob-2"]}
             (set (tickle-logic/query-pages-without-assignments db))))
      (is (= #{["p-bad" "codex-2"] ["p-ghost" "ghost-agent"]}
             (set (tickle-logic/query-pages-without-authority db))))
      (is (= ["e-bad"]
             (tickle-logic/query-orphan-escalations db))))))

(deftest query-violations-catches-watchdog-chain-and-stall-mismatch
  (testing "watchdog evidence must preserve scan -> page -> escalation ordering and stale alignment"
    (let [db (tickle-logic/build-db
              {:registry-status
               {:agents {"codex-1" {:type :codex
                                    :status :idle
                                    :invoke-ready? true
                                    :invoke-route :local
                                    :capabilities [:coordination/execute :edit]}
                         "claude-1" {:type :claude
                                     :status :idle
                                     :invoke-ready? true
                                     :invoke-route :local
                                     :capabilities [:coordination/execute]}}}
               :evidence-entries
               [{:evidence/author "codex-1"
                 :evidence/at (ts 250)}
                {:evidence/author "tickle-1"
                 :evidence/at (ts 300)
                 :evidence/tags [:tickle :scan]
                 :evidence/body {:event :scan
                                 :activity {"codex-1" {:last-active (ts 250)
                                                       :stale? false
                                                       :stale-seconds 50}
                                            "claude-1" {:last-active nil
                                                        :stale? true
                                                        :stale-seconds 999999}}
                                 :stalled ["codex-1"]
                                 :threshold-seconds 300
                                 :cycle-at (ts 300)}}
                {:evidence/author "tickle-1"
                 :evidence/at (ts 330)
                 :evidence/tags [:tickle :page]
                 :evidence/body {:event :page
                                 :agent-id "ghost-agent"
                                 :method :irc
                                 :at (ts 330)}}
                {:evidence/author "tickle-1"
                 :evidence/at (ts 360)
                 :evidence/tags [:tickle :escalation]
                 :evidence/body {:event :escalation
                                 :agent-id "claude-1"
                                 :cause :page-failed
                                 :at (ts 360)}}]})]
      (is (= ["ghost-agent"]
             (tickle-logic/query-watchdog-pages-without-scans db)))
      (is (= ["claude-1"]
             (tickle-logic/query-watchdog-escalations-without-pages db)))
      (is (= #{{:agent-id "codex-1"
                :scan-at (ts 300)
                :threshold-seconds 300
                :expected-stale? false
                :actual-stale? true
                :last-evidence-at (ts 250)}
               {:agent-id "claude-1"
                :scan-at (ts 300)
                :threshold-seconds 300
                :expected-stale? true
                :actual-stale? false
                :last-evidence-at nil}}
             (set (tickle-logic/query-stall-evidence-mismatches db)))))))

(deftest query-watchdog-probe-candidates-prefers-silence-context
  (testing "watchdog probe candidates are long-silent non-idle agents, not recently-finished ones"
    (let [db (tickle-logic/build-db
              {:registry-status
               {:agents {"codex-1" {:type :codex
                                    :status :invoking
                                    :invoke-ready? true
                                    :invoke-route :local
                                    :capabilities [:coordination/execute :edit]}
                         "claude-1" {:type :claude
                                     :status :idle
                                     :invoke-ready? true
                                     :invoke-route :local
                                     :capabilities [:coordination/execute]}}}
               :evidence-entries
               [{:evidence/author "codex-1"
                 :evidence/at (ts 0)}
                {:evidence/author "tickle-1"
                 :evidence/at (ts 500)
                 :evidence/tags [:tickle :availability-bell :coordination]
                 :evidence/body {:event :agent-availability-bell
                                 :agent-id "claude-1"
                                 :availability :available
                                 :invoke-status :done
                                 :message "I'm available"
                                 :at (ts 500)}}]})]
      (is (= [{:agent-id "codex-1"
               :status :invoking
               :silent-seconds 1000
               :last-evidence-at (ts 0)
               :latest-availability-bell nil}]
             (tickle-logic/query-watchdog-probe-candidates
              db
              {:now (ts 1000)
               :threshold-seconds 600}))))))
