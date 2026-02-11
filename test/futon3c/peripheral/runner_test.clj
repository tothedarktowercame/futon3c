(ns futon3c.peripheral.runner-test
  "M-peripheral-behavior Part I tests.

   Covers: PeripheralRunner protocol, tool dispatch (allowed? + in-scope?),
   scope enforcement, evidence emission helpers, and mock backend recording."
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.peripheral.evidence :as evidence]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]
            [futon3c.social.shapes :as shapes]
            [futon3c.social.test-fixtures :as fix]))

;; =============================================================================
;; Test peripheral spec (explore — read-only, full-codebase)
;; =============================================================================

(def explore-spec
  {:peripheral/id :explore
   :peripheral/tools #{:read :glob :grep :bash-readonly :web-fetch}
   :peripheral/scope :full-codebase
   :peripheral/entry #{:default :from-reflect}
   :peripheral/exit #{:found-target :ready-to-edit :user-request :hop-reflect}
   :peripheral/context {:session-id :inherit}})

(def edit-spec
  {:peripheral/id :edit
   :peripheral/tools #{:read :edit :write :bash}
   :peripheral/scope {:paths ["src/" "docs/" "scripts/"]}
   :peripheral/entry #{:from-explore :user-request}
   :peripheral/exit #{:tests-pass :ready-to-commit :blocked :hop-test :hop-reflect}
   :peripheral/context {:session-id :inherit :target-files :from-explore}})

(def test-spec
  {:peripheral/id :test
   :peripheral/tools #{:read :bash-test}
   :peripheral/scope :test-commands-only
   :peripheral/entry #{:from-edit :user-request}
   :peripheral/exit #{:pass :fail :flaky :hop-edit :hop-deploy :hop-reflect}
   :peripheral/context {:session-id :inherit :changed-files :from-edit}})

;; =============================================================================
;; Tool dispatch — allowed?
;; =============================================================================

(deftest allowed-tool-in-spec
  (testing "tool in the peripheral's tool set is allowed"
    (is (true? (tools/allowed? :read explore-spec)))
    (is (true? (tools/allowed? :glob explore-spec)))
    (is (true? (tools/allowed? :grep explore-spec)))
    (is (true? (tools/allowed? :edit edit-spec)))
    (is (true? (tools/allowed? :write edit-spec)))))

(deftest disallowed-tool-not-in-spec
  (testing "tool NOT in the peripheral's tool set is rejected"
    (is (false? (tools/allowed? :edit explore-spec)))
    (is (false? (tools/allowed? :write explore-spec)))
    (is (false? (tools/allowed? :bash explore-spec)))
    (is (false? (tools/allowed? :glob test-spec)))
    (is (false? (tools/allowed? :bash-git edit-spec)))))

;; =============================================================================
;; Tool dispatch — in-scope?
;; =============================================================================

(deftest in-scope-keyword-scope-unrestricted
  (testing "keyword scopes (:full-codebase, :test-commands-only) are unrestricted"
    (is (true? (tools/in-scope? :read ["any/path/file.clj"] explore-spec)))
    (is (true? (tools/in-scope? :bash-test ["clojure -X:test"] test-spec)))))

(deftest in-scope-path-scope-enforced
  (testing "path-scoped peripheral allows args within scope"
    (is (true? (tools/in-scope? :edit ["src/futon3c/core.clj"] edit-spec)))
    (is (true? (tools/in-scope? :write ["docs/README.md"] edit-spec)))
    (is (true? (tools/in-scope? :edit ["scripts/build.sh"] edit-spec)))))

(deftest out-of-scope-path-rejected
  (testing "path-scoped peripheral rejects args outside scope"
    (is (false? (tools/in-scope? :edit ["test/futon3c/core_test.clj"] edit-spec)))
    (is (false? (tools/in-scope? :write ["resources/config.edn"] edit-spec)))
    (is (false? (tools/in-scope? :edit ["/etc/passwd"] edit-spec)))))

;; =============================================================================
;; Tool dispatch — dispatch-tool (full enforcement)
;; =============================================================================

(deftest dispatch-tool-allowed-and-in-scope
  (testing "dispatch-tool succeeds for allowed + in-scope tool"
    (let [backend (tools/make-mock-backend {:read {:lines 42}})
          result (tools/dispatch-tool :read ["src/futon3c/core.clj"] explore-spec backend)]
      (is (= true (:ok result)))
      (is (= {:lines 42} (:result result)))
      ;; Call was recorded
      (is (= [{:tool :read :args ["src/futon3c/core.clj"]}]
             (tools/recorded-calls backend))))))

(deftest dispatch-tool-rejects-disallowed-tool
  (testing "dispatch-tool returns SocialError for disallowed tool"
    (let [backend (tools/make-mock-backend)
          result (tools/dispatch-tool :edit ["src/a.clj"] explore-spec backend)]
      (fix/assert-valid! shapes/SocialError result)
      (is (= :tool-not-allowed (:error/code result)))
      ;; Backend was NOT called
      (is (empty? (tools/recorded-calls backend))))))

(deftest dispatch-tool-rejects-out-of-scope
  (testing "dispatch-tool returns SocialError for out-of-scope args"
    (let [backend (tools/make-mock-backend {:edit "ok"})
          result (tools/dispatch-tool :edit ["test/core_test.clj"] edit-spec backend)]
      (fix/assert-valid! shapes/SocialError result)
      (is (= :out-of-scope (:error/code result)))
      ;; Backend was NOT called
      (is (empty? (tools/recorded-calls backend))))))

;; =============================================================================
;; Mock backend — recording
;; =============================================================================

(deftest mock-backend-records-all-calls
  (testing "mock backend records every tool invocation in order"
    (let [backend (tools/make-mock-backend {:read {:content "hello"}
                                            :glob {:matches 5}})]
      (tools/dispatch-tool :read ["file.clj"] explore-spec backend)
      (tools/dispatch-tool :glob ["**/*.clj"] explore-spec backend)
      (tools/dispatch-tool :grep ["pattern"] explore-spec backend)
      (let [calls (tools/recorded-calls backend)]
        (is (= 3 (count calls)))
        (is (= :read (:tool (nth calls 0))))
        (is (= :glob (:tool (nth calls 1))))
        (is (= :grep (:tool (nth calls 2))))))))

;; =============================================================================
;; Evidence emission — shape-valid entries
;; =============================================================================

(deftest start-evidence-is-shape-valid
  (testing "make-start-evidence produces a valid EvidenceEntry with :goal claim"
    (let [ev (evidence/make-start-evidence :explore "sess-1" "claude-1")]
      (fix/assert-valid! shapes/EvidenceEntry ev)
      (is (= :goal (:evidence/claim-type ev)))
      (is (= :coordination (:evidence/type ev)))
      (is (= "claude-1" (:evidence/author ev)))
      (is (= "sess-1" (:evidence/session-id ev)))
      (is (= {:ref/type :session :ref/id "sess-1"} (:evidence/subject ev))))))

(deftest step-evidence-is-shape-valid
  (testing "make-step-evidence produces a valid EvidenceEntry with :step claim"
    (let [start-ev (evidence/make-start-evidence :explore "sess-1" "claude-1")
          ev (evidence/make-step-evidence :explore "sess-1" "claude-1"
                                          :glob ["**/*.clj"] {:matches 42}
                                          (:evidence/id start-ev))]
      (fix/assert-valid! shapes/EvidenceEntry ev)
      (is (= :step (:evidence/claim-type ev)))
      (is (= :coordination (:evidence/type ev)))
      (is (= (:evidence/id start-ev) (:evidence/in-reply-to ev)))
      (is (= :glob (get-in ev [:evidence/body :tool]))))))

(deftest stop-evidence-is-shape-valid
  (testing "make-stop-evidence produces a valid EvidenceEntry with :conclusion claim"
    (let [ev (evidence/make-stop-evidence :explore "sess-1" "claude-1"
                                          {:found ["src/a.clj"] :summary "found target"}
                                          "found target file"
                                          "prev-id")]
      (fix/assert-valid! shapes/EvidenceEntry ev)
      (is (= :conclusion (:evidence/claim-type ev)))
      (is (= :coordination (:evidence/type ev)))
      (is (= "prev-id" (:evidence/in-reply-to ev)))
      (is (= {:found ["src/a.clj"] :summary "found target"}
             (get-in ev [:evidence/body :fruit]))))))

(deftest reflect-evidence-uses-reflection-type
  (testing "reflect peripheral emits :reflection type evidence, not :coordination"
    (let [start (evidence/make-start-evidence :reflect "sess-1" "claude-1")
          step (evidence/make-step-evidence :reflect "sess-1" "claude-1"
                                            :read ["session.log"] {:lines 100}
                                            (:evidence/id start))
          stop (evidence/make-stop-evidence :reflect "sess-1" "claude-1"
                                            {:par {:went-well "stuff"}}
                                            "session close"
                                            (:evidence/id step))]
      (is (= :reflection (:evidence/type start)))
      (is (= :reflection (:evidence/type step)))
      (is (= :reflection (:evidence/type stop))))))

;; =============================================================================
;; Runner helpers
;; =============================================================================

(deftest runner-error-produces-social-error
  (testing "runner-error produces a valid SocialError"
    (let [err (runner/runner-error :explore :tool-not-allowed "bad tool" :tool :edit)]
      (fix/assert-valid! shapes/SocialError err)
      (is (= :peripheral (:error/component err)))
      (is (= :tool-not-allowed (:error/code err))))))

(deftest validate-context-detects-missing-keys
  (testing "validate-context returns error for missing required keys"
    (let [err (runner/validate-context :edit {:session-id "s1"} #{:session-id :target-files})]
      (fix/assert-valid! shapes/SocialError err)
      (is (= :missing-context (:error/code err)))))
  (testing "validate-context returns nil when all keys present"
    (is (nil? (runner/validate-context :edit {:session-id "s1" :target-files ["a.clj"]}
                                       #{:session-id :target-files})))))

;; =============================================================================
;; Evidence chain — reply threading
;; =============================================================================

(deftest evidence-chain-forms-reply-thread
  (testing "start → step → step → stop forms a valid reply chain"
    (let [start (evidence/make-start-evidence :explore "sess-1" "claude-1")
          step1 (evidence/make-step-evidence :explore "sess-1" "claude-1"
                                             :glob ["**/*.clj"] {:matches 5}
                                             (:evidence/id start))
          step2 (evidence/make-step-evidence :explore "sess-1" "claude-1"
                                             :read ["src/a.clj"] {:content "..."}
                                             (:evidence/id step1))
          stop (evidence/make-stop-evidence :explore "sess-1" "claude-1"
                                            {:found ["src/a.clj"]}
                                            "found target"
                                            (:evidence/id step2))]
      ;; All are valid entries
      (doseq [ev [start step1 step2 stop]]
        (fix/assert-valid! shapes/EvidenceEntry ev))
      ;; Chain is threaded
      (is (nil? (:evidence/in-reply-to start)))
      (is (= (:evidence/id start) (:evidence/in-reply-to step1)))
      (is (= (:evidence/id step1) (:evidence/in-reply-to step2)))
      (is (= (:evidence/id step2) (:evidence/in-reply-to stop)))
      ;; All share same session
      (is (every? #(= "sess-1" (:evidence/session-id %)) [start step1 step2 stop]))
      ;; Claim types progress correctly
      (is (= :goal (:evidence/claim-type start)))
      (is (= :step (:evidence/claim-type step1)))
      (is (= :step (:evidence/claim-type step2)))
      (is (= :conclusion (:evidence/claim-type stop))))))
