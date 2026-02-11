(ns futon3c.peripheral.explore-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.evidence.store :as store]
            [futon3c.peripheral.explore :as explore]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]
            [futon3c.social.shapes :as shapes]
            [futon3c.social.test-fixtures :as fix]))

(deftest explore-start-produces-goal-evidence
  (let [p (explore/make-explore (tools/make-mock-backend))
        start (runner/start p {:session-id "sess-explore-1" :agent-id "codex-1"})]
    (is (:ok start))
    (fix/assert-valid! shapes/EvidenceEntry (:evidence start))
    (is (= :goal (get-in start [:evidence :evidence/claim-type])))
    (is (= :coordination (get-in start [:evidence :evidence/type])))))

(deftest explore-step-collects-found-targets
  (let [backend (tools/make-mock-backend {:glob {:found ["src/futon3c/peripheral/explore.clj"]}})
        p (explore/make-explore backend)
        start (runner/start p {:session-id "sess-explore-2" :agent-id "codex-1"})
        step (runner/step p (:state start) {:tool :glob :args ["**/*.clj"]})]
    (is (:ok step))
    (is (= :step (get-in step [:evidence :evidence/claim-type])))
    (is (= :glob (get-in step [:evidence :evidence/body :tool])))
    (is (= ["src/futon3c/peripheral/explore.clj"]
           (get-in step [:state :found])))))

(deftest explore-rejects-write-tool
  (let [p (explore/make-explore (tools/make-mock-backend))
        start (runner/start p {:session-id "sess-explore-3"})
        err (runner/step p (:state start) {:tool :edit :args ["src/a.clj"]})]
    (fix/assert-valid! shapes/SocialError err)
    (is (= :tool-not-allowed (:error/code err)))))

(deftest explore-stop-returns-target-files-context
  (let [backend (tools/make-mock-backend {:read {:file "src/futon3c/core.clj"}})
        p (explore/make-explore backend)
        start (runner/start p {:session-id "sess-explore-4"})
        step (runner/step p (:state start) {:tool :read :args ["src/futon3c/core.clj"]})
        stop (runner/stop p (:state step) "target found")]
    (is (:ok stop))
    (is (= ["src/futon3c/core.clj"] (get-in stop [:context :target-files])))
    (is (= :conclusion (get-in stop [:evidence :evidence/claim-type])))
    (fix/assert-valid! shapes/EvidenceEntry (:evidence stop))))

(deftest explore-optionally-appends-evidence-to-store
  (let [evidence-store (atom {:entries {} :order []})
        backend (tools/make-mock-backend {:glob {:found ["src/futon3c/peripheral/edit.clj"]}})
        p (explore/make-explore backend)
        start (runner/start p {:session-id "sess-explore-5" :evidence-store evidence-store})
        step (runner/step p (:state start) {:tool :glob :args ["src/futon3c/peripheral/*.clj"]})
        stop (runner/stop p (:state step) "done")
        entries (store/query* evidence-store {})]
    (is (:ok start))
    (is (:ok step))
    (is (:ok stop))
    (is (= 3 (count entries)))
    (is (= [:goal :step :conclusion]
           (mapv :evidence/claim-type (sort-by :evidence/at entries))))))
