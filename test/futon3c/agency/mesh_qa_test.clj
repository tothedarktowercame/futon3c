(ns futon3c.agency.mesh-qa-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is]]
            [futon3c.agency.mesh-qa :as mesh-qa]
            [futon3c.transport.http :as http]))

(def terminal-job
  {:job-id "j1" :agent-id "codex-1" :caller "claude-6" :surface "bell"
   :state "succeeded" :created-at "2026-06-10T10:00:00Z"
   :finished-at "2026-06-10T10:01:00Z" :session-id "s-codex"
   :delivery {:status "delivered" :surface "job-status"}})

(deftest job-reader-projects-unified-schema
  (let [edge (mesh-qa/job->edge terminal-job)]
    (is (= {:edge-id "j1" :source :invoke-job :from "claude-6" :to "codex-1"
            :surface "bell" :kind :invoke :accepted-at "2026-06-10T10:00:00Z"
            :terminal-at "2026-06-10T10:01:00Z" :terminal-state "succeeded"
            :delivered? true :delivery-surface "job-status" :session-id "s-codex"
            :ok? true :bellback-of nil}
           (dissoc edge :raw)))))

(deftest coordination-events-coalesce-invoke-and-result
  (let [events [{:id "e1" :edge-id "edge-a" :from "irc-user" :to "codex-1"
                 :surface "irc" :kind :invoke :at "2026-06-10T10:00:00Z"}
                {:id "e2" :edge-id "edge-a" :from "irc-user" :to "codex-1"
                 :surface "irc" :kind :invoke-result :ok? true
                 :at "2026-06-10T10:00:03Z"}]
        [edge] (mesh-qa/coordination-events->edges events)]
    (is (= "edge-a" (:edge-id edge)))
    (is (= :mesh-evidence (:source edge)))
    (is (= "succeeded" (:terminal-state edge)))
    (is (= "irc-user" (:from edge)))
    (is (= "codex-1" (:to edge)))))

(deftest invariant-checks-trip-individually
  (let [cases {:MQ-1 {:edges [(assoc (mesh-qa/job->edge terminal-job)
                                     :edge-id "mq1" :delivered? false)]}
               :MQ-2 {:edges [(assoc (mesh-qa/job->edge terminal-job)
                                     :edge-id "mq2" :terminal-state "running"
                                     :terminal-at nil :delivered? false)]}
               :MQ-3 {:edges [{:edge-id "orig" :source :invoke-job :from "a" :to "b"
                                :surface "bell" :terminal-state "succeeded"
                                :delivered? true :delivery-surface "job-status"}
                               {:edge-id "back" :source :invoke-job :from "b" :to "c"
                                :surface "bell" :terminal-state "succeeded"
                                :delivered? true :delivery-surface "job-status"
                                :bellback-of "orig"}]}
               :MQ-4 {:edges [(assoc (mesh-qa/job->edge terminal-job)
                                     :edge-id "mq4" :session-id "wrong")]
                      :sessions {"codex-1" "s-codex"}}
               :MQ-5 {:edges [(assoc (mesh-qa/job->edge terminal-job)
                                     :edge-id "mq5" :surface "irc"
                                     :delivery-surface "emacs")]} }]
    (doseq [[invariant {:keys [edges sessions]}] cases]
      (is (= [invariant]
             (mapv :invariant (:violations (mesh-qa/check-mesh edges (or sessions {})))))
          (str invariant " should trip exactly itself")))))

(deftest conforming-mesh-has-no-violations
  (let [edges [(mesh-qa/job->edge terminal-job)]
        report (mesh-qa/check-mesh edges {"codex-1" "s-codex"})]
    (is (:ok report))
    (is (zero? (:violation-count report)))
    (is (contains? (:counts report) :MQ-7))))

(deftest mq7-flags-unaddressable-codex-caller-only
  (let [codex-job (mesh-qa/job->edge terminal-job)
        registry {:sessions {"codex-1" "s-codex"}
                  :types {"codex-1" :codex "claude-1" :claude}
                  :registered #{"claude-6" "codex-1" "claude-1"}}]
    (is (empty? (mesh-qa/check-mq-7-unaddressable-caller [codex-job] registry)))
    (is (= [:MQ-7]
           (mapv :invariant
                 (mesh-qa/check-mq-7-unaddressable-caller
                  [(assoc codex-job :from "claude-missing")] registry))))
    (is (empty? (mesh-qa/check-mq-7-unaddressable-caller
                 [(assoc codex-job :from "http-caller")] registry)))
    (is (empty? (mesh-qa/check-mq-7-unaddressable-caller
                 [(assoc codex-job :from "joe")] registry)))
    (is (empty? (mesh-qa/check-mq-7-unaddressable-caller
                 [(assoc codex-job :from "http-caller" :to "claude-1")] registry)))
    (is (= 1 (get-in (mesh-qa/check-mesh [(assoc codex-job :from "claude-missing")] registry)
                     [:counts :MQ-7])))))

(deftest qa-http-route-returns-report
  (with-redefs [mesh-qa/current-report (fn [limit]
                                         {:ok true :edge-count limit
                                          :violation-count 0
                                          :counts {:MQ-1 0}
                                          :violations []
                                          :checkability mesh-qa/checkability})]
    (let [handler (http/make-handler {})
          response (handler {:request-method :get
                             :uri "/api/alpha/coordination/qa"
                             :query-string "limit=7"})
          body (json/parse-string (:body response) true)]
      (is (= 200 (:status response)))
      (is (= true (:ok body)))
      (is (= 7 (:edge-count body))))))
