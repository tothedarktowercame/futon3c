(ns futon3c.apm.live-preflight-runtime-test
  (:require [clojure.java.shell :as shell]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.live-preflight-runtime :as sut]))

(deftest terminal-job-and-fenced-edn-are-normalized
  (let [report {:command-own-exit 0 :mutations []}
        terminal (sut/job->terminal
                  {:job {:job-id "j1" :agent-id "f19-proctor" :state "done"
                         :result (str "```edn\n" (pr-str report) "\n```")}})]
    (is (= :done (:state terminal)))
    (is (= report (:report terminal)))))

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
