(ns futon3c.apm.live-preflight-runtime-test
  (:require [clojure.test :refer [deftest is]]
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
