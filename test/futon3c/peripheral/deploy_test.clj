(ns futon3c.peripheral.deploy-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.peripheral.deploy :as deploy]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]
            [futon3c.social.shapes :as shapes]
            [futon3c.social.test-fixtures :as fix]))

(deftest deploy-start-produces-goal-evidence
  (let [p (deploy/make-deploy (tools/make-mock-backend))
        start (runner/start p {:session-id "sess-deploy-1" :agent-id "codex-1"})]
    (is (:ok start))
    (is (= :goal (get-in start [:evidence :evidence/claim-type])))
    (fix/assert-valid! shapes/EvidenceEntry (:evidence start))))

(deftest deploy-rejects-non-deploy-tools
  (let [p (deploy/make-deploy (tools/make-mock-backend))
        start (runner/start p {:session-id "sess-deploy-2"})
        err (runner/step p (:state start) {:tool :write :args ["src/a.clj"]})]
    (fix/assert-valid! shapes/SocialError err)
    (is (= :tool-not-allowed (:error/code err)))))

(deftest deploy-captures-commit-and-push-state
  (let [backend (tools/make-mock-backend {:bash-git {:status :committed :sha "abc123"}
                                          :bash-deploy {:status :pushed}})
        p (deploy/make-deploy backend)
        start (runner/start p {:session-id "sess-deploy-3"})
        step1 (runner/step p (:state start) {:tool :bash-git :args ["commit -m \"msg\""]})
        step2 (runner/step p (:state step1) {:tool :bash-deploy :args ["git push"]})
        stop (runner/stop p (:state step2) "deployed")]
    (is (:ok stop))
    (is (true? (get-in stop [:fruit :committed?])))
    (is (true? (get-in stop [:fruit :pushed?])))
    (is (= "abc123" (get-in stop [:fruit :sha])))))

(deftest deploy-stop-emits-conclusion-evidence
  (let [backend (tools/make-mock-backend {:bash-git {:status :committed}})
        p (deploy/make-deploy backend)
        start (runner/start p {:session-id "sess-deploy-4"})
        step (runner/step p (:state start) {:tool :bash-git :args ["commit -m \"msg\""]})
        stop (runner/stop p (:state step) "ship it")]
    (is (:ok stop))
    (is (= :conclusion (get-in stop [:evidence :evidence/claim-type])))
    (is (= :coordination (get-in stop [:evidence :evidence/type])))
    (fix/assert-valid! shapes/EvidenceEntry (:evidence stop))))
