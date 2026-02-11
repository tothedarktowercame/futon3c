(ns futon3c.peripheral.test-runner-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.test-runner :as test-runner]
            [futon3c.peripheral.tools :as tools]
            [futon3c.social.shapes :as shapes]
            [futon3c.social.test-fixtures :as fix]))

(deftest test-runner-start-produces-goal-evidence
  (let [p (test-runner/make-test-runner (tools/make-mock-backend))
        start (runner/start p {:session-id "sess-test-1" :agent-id "codex-1"})]
    (is (:ok start))
    (is (= :goal (get-in start [:evidence :evidence/claim-type])))
    (fix/assert-valid! shapes/EvidenceEntry (:evidence start))))

(deftest test-runner-rejects-non-test-tools
  (let [p (test-runner/make-test-runner (tools/make-mock-backend))
        start (runner/start p {:session-id "sess-test-2"})
        err (runner/step p (:state start) {:tool :edit :args ["src/a.clj"]})]
    (fix/assert-valid! shapes/SocialError err)
    (is (= :tool-not-allowed (:error/code err)))))

(deftest test-runner-stop-summarizes-pass
  (let [backend (tools/make-mock-backend {:bash-test {:status :pass :test-count 12}})
        p (test-runner/make-test-runner backend)
        start (runner/start p {:session-id "sess-test-3"})
        step (runner/step p (:state start) {:tool :bash-test :args ["clojure -X:test"]})
        stop (runner/stop p (:state step) "tests complete")]
    (is (:ok stop))
    (is (= :pass (get-in stop [:fruit :result])))
    (is (= 12 (get-in stop [:fruit :test-count])))
    (is (= [] (get-in stop [:fruit :failures])))))

(deftest test-runner-stop-summarizes-failures
  (let [backend (tools/make-mock-backend {:bash-test {:status :fail
                                                       :test-count 4
                                                       :failures ["foo_test.clj:42"]}})
        p (test-runner/make-test-runner backend)
        start (runner/start p {:session-id "sess-test-4"})
        step (runner/step p (:state start) {:tool :bash-test :args ["clojure -X:test"]})
        stop (runner/stop p (:state step) "tests complete")]
    (is (:ok stop))
    (is (= :fail (get-in stop [:fruit :result])))
    (is (= 4 (get-in stop [:fruit :test-count])))
    (is (= ["foo_test.clj:42"] (get-in stop [:fruit :failures])))))

(deftest test-runner-stop-prioritizes-flaky
  (let [backend (tools/make-mock-backend {:bash-test {:status :flaky
                                                       :test-count 8
                                                       :failures ["intermittent failure"]}})
        p (test-runner/make-test-runner backend)
        start (runner/start p {:session-id "sess-test-5"})
        step (runner/step p (:state start) {:tool :bash-test :args ["clojure -X:test"]})
        stop (runner/stop p (:state step) "tests complete")]
    (is (:ok stop))
    (is (= :flaky (get-in stop [:fruit :result])))
    (fix/assert-valid! shapes/EvidenceEntry (:evidence stop))))
