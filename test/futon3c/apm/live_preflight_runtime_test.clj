(ns futon3c.apm.live-preflight-runtime-test
  (:require [clojure.java.shell :as shell]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.live-preflight-runtime :as sut]))

(deftest terminal-job-and-fenced-edn-are-normalized
  (let [report {:command-own-exit 0 :mutations []}
        delivery {:terminal-job-id "j1" :delivery-status "delivery-failed"
                  :inbox-file-created? false
                  :registered-push-performed? false
                  :polling-available? true}
        terminal (sut/job->terminal
                  {:job {:job-id "j1" :agent-id "f19-proctor" :state "failed"
                         :terminal-code "invoke-error"
                         :terminal-message "wall-clock-budget"
                         :invocation/model "gpt-5.6-sol"
                         :trace/delivery-observation delivery
                         :result (str "```edn\n" (pr-str report) "\n```")}})]
    (is (= :failed (:state terminal)))
    (is (= :invoke-error (:terminal-code terminal)))
    (is (= "wall-clock-budget" (:terminal-message terminal)))
    (is (= report (:report terminal)))
    (is (= "gpt-5.6-sol" (:invocation/model terminal)))
    (is (= delivery (:trace/delivery-observation terminal)))))

(deftest non-edn-result-is-not-evidence
  (is (nil? (sut/parse-report "I think it passed"))))

(deftest invalid-edn-retains-reader-diagnostic
  (let [result (sut/parse-report-diagnostic
                "{:lane \"challenge\" :ran-empty :memory-ids []}")]
    (is (false? (:ok result)))
    (is (= :report-edn-lint-failed (:error/code result)))
    (is (= 3 (:linter/exit result)))
    (is (re-find #"missing value for key" (:error/message result)))
    (is (re-find #"1:" (:error/message result)))))

(deftest linter-unavailability-fails-closed
  (with-redefs [shell/sh
                (fn [& _] (throw (java.io.IOException. "missing clj-kondo")))]
    (let [result (sut/parse-report-diagnostic "{:ok true}")]
      (is (false? (:ok result)))
      (is (= :report-edn-linter-unavailable (:error/code result))))))

(deftest generic-dispatch-state-rehydrates-without-rewriting-authority
  (let [request {:dispatch/id "dispatch" :problem-id "m94A03"}
        ticket {:job-id "job" :dispatch/id "dispatch"}
        state {:state/type :live-job-dispatched
               :request request :ticket ticket
               :activation/accepted? true
               :terminal-collection {:submission/id "observed"}}
        normalized (sut/normalize-preflight-state state)]
    (is (= :preflight-dispatched (:state/type normalized)))
    (is (= request (:request normalized)))
    (is (= ticket (:ticket normalized)))
    (is (= normalized (sut/normalize-preflight-state normalized)))))
