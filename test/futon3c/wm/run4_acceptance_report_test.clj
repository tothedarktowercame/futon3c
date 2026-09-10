(ns futon3c.wm.run4-acceptance-report-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.wm.run4-acceptance-report :as sut]))

(def sha (apply str (repeat 64 "a")))
(def visibility {:schema "wm/run-visibility-v1" :run_id "RUN4-x"
                 :stage "complete" :result "passed"
                 :trials [{:trial_id "t1" :stage "complete" :result "passed"}]})

(deftest current-durable-results-do-not-invent-preregistration-acceptance
  (let [input {:visibility visibility :expected-series-sha256 sha
               :expected-series-id "RUN4-x"
               :observed-series-sha256 sha}
        a (sut/report input) b (sut/report input)]
    (is (= a b))
    (is (= :missing-route-to-preregistration-bridge (:decision a)))
    (is (false? (:accepted? a)))
    (is (= :operator-reserved (:acceptance-authority a)))))

(deftest drift-and-unknown-states-refuse
  (is (= :refused-source-drift
         (:decision (sut/report {:visibility visibility
                                 :expected-series-id "RUN4-x"
                                 :expected-series-sha256 sha
                                 :observed-series-sha256 (apply str (repeat 64 "b"))}))))
  (is (= :unsupported-or-incomplete-terminal-state
         (:decision (sut/report {:visibility (assoc visibility :stage "working")
                                 :expected-series-id "RUN4-x"
                                 :expected-series-sha256 sha :observed-series-sha256 sha}))))
  (is (false? (:accepted? (sut/report {:visibility nil}))))
  (let [forged (sut/report {:visibility visibility
                            :expected-series-id "RUN4-x"
                            :expected-series-sha256 sha :observed-series-sha256 sha
                            :route-evidence {:schema :wm/run4-route-conformance-v1
                                             :routes ["invented"]}
                            :recording-evidence
                            {:schema :wm/run4-recording-completeness-v1
                             :complete? true :battery-ref "/missing"}})]
    (is (= false (get-in forged [:checks :route-conformance])))
    (is (= false (get-in forged [:checks :recording-completeness])))
    (is (= :missing-route-to-preregistration-bridge (:decision forged)))
    (is (false? (:accepted? forged)))))

(deftest incomplete-or-foreign-visibility-cannot-green-terminal-check
  (doseq [v [(dissoc visibility :run_id)
             (assoc visibility :run_id "")
             (assoc visibility :run_id "FOREIGN")
             (assoc visibility :trials [{:trial_id "t1" :stage "working"
                                         :result "pending"}])]]
    (is (false? (get-in (sut/report {:visibility v
                                     :expected-series-id "RUN4-x"
                                     :expected-series-sha256 sha
                                     :observed-series-sha256 sha})
                        [:checks :terminal-task-results])))))
