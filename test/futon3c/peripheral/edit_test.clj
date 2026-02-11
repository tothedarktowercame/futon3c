(ns futon3c.peripheral.edit-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.peripheral.edit :as edit]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]
            [futon3c.social.shapes :as shapes]
            [futon3c.social.test-fixtures :as fix]))

(deftest edit-start-produces-goal-evidence
  (let [p (edit/make-edit (tools/make-mock-backend))
        start (runner/start p {:session-id "sess-edit-1" :agent-id "codex-1"})]
    (is (:ok start))
    (is (= :goal (get-in start [:evidence :evidence/claim-type])))
    (is (= :coordination (get-in start [:evidence :evidence/type])))
    (fix/assert-valid! shapes/EvidenceEntry (:evidence start))))

(deftest edit-step-tracks-changed-files
  (let [backend (tools/make-mock-backend {:edit {:changed-files ["src/futon3c/peripheral/edit.clj"]}})
        p (edit/make-edit backend)
        start (runner/start p {:session-id "sess-edit-2"})
        step (runner/step p (:state start) {:tool :edit :args ["src/futon3c/peripheral/edit.clj"]})]
    (is (:ok step))
    (is (= ["src/futon3c/peripheral/edit.clj"]
           (get-in step [:state :changed-files])))
    (is (= :step (get-in step [:evidence :evidence/claim-type])))))

(deftest edit-rejects-out-of-scope-paths
  (let [p (edit/make-edit (tools/make-mock-backend {:edit {:ok true}}))
        start (runner/start p {:session-id "sess-edit-3"})
        err (runner/step p (:state start) {:tool :edit :args ["test/futon3c/peripheral/edit_test.clj"]})]
    (fix/assert-valid! shapes/SocialError err)
    (is (= :out-of-scope (:error/code err)))))

(deftest edit-rejects-out-of-scope-write
  (let [p (edit/make-edit (tools/make-mock-backend {:write {:ok true}}))
        start (runner/start p {:session-id "sess-edit-3b"})
        err (runner/step p (:state start) {:tool :write :args ["resources/peripherals.edn"]})]
    (fix/assert-valid! shapes/SocialError err)
    (is (= :out-of-scope (:error/code err)))))

(deftest edit-rejects-disallowed-tools
  (let [p (edit/make-edit (tools/make-mock-backend))
        start (runner/start p {:session-id "sess-edit-4"})
        err (runner/step p (:state start) {:tool :bash-git :args ["commit -m test"]})]
    (fix/assert-valid! shapes/SocialError err)
    (is (= :tool-not-allowed (:error/code err)))))

(deftest edit-stop-returns-changed-files-context
  (let [backend (tools/make-mock-backend {:write {:file "src/futon3c/peripheral/explore.clj"}})
        p (edit/make-edit backend)
        start (runner/start p {:session-id "sess-edit-5"})
        step (runner/step p (:state start) {:tool :write :args ["src/futon3c/peripheral/explore.clj"]})
        stop (runner/stop p (:state step) "ready to test")]
    (is (:ok stop))
    (is (= 1 (get-in stop [:fruit :changes])))
    (is (true? (get-in stop [:fruit :ready-to-test?])))
    (is (= ["src/futon3c/peripheral/explore.clj"]
           (get-in stop [:context :changed-files])))
    (fix/assert-valid! shapes/EvidenceEntry (:evidence stop))))
