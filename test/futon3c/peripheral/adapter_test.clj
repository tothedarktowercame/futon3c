(ns futon3c.peripheral.adapter-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [futon3c.peripheral.adapter :as adapter]
            [futon3c.peripheral.common :as common]
            [futon3c.social.shapes :as shapes]))

(def explore-spec (common/load-spec :explore))
(def edit-spec (common/load-spec :edit))
(def deploy-spec (common/load-spec :deploy))
(def test-spec (common/load-spec :test))
(def reflect-spec (common/load-spec :reflect))

;; =============================================================================
;; Tool mapping
;; =============================================================================

(deftest tool-mapping-explore
  (testing "explore maps read-only Claude tools"
    (let [m (adapter/tool-mapping explore-spec)]
      (is (= :read (get m "Read")))
      (is (= :glob (get m "Glob")))
      (is (= :grep (get m "Grep")))
      (is (= :bash-readonly (get m "Bash")))
      (is (= :web-fetch (get m "WebFetch")))
      (is (nil? (get m "Edit")))
      (is (nil? (get m "Write"))))))

(deftest tool-mapping-edit
  (testing "edit maps read/write Claude tools"
    (let [m (adapter/tool-mapping edit-spec)]
      (is (= :read (get m "Read")))
      (is (= :edit (get m "Edit")))
      (is (= :write (get m "Write")))
      (is (= :bash (get m "Bash"))))))

(deftest tool-mapping-deploy
  (testing "deploy maps bash variants"
    (let [m (adapter/tool-mapping deploy-spec)]
      (is (= :bash-git (get m "Bash")))
      (is (nil? (get m "Read")))
      (is (nil? (get m "Edit"))))))

(deftest claude-tools-explore
  (testing "claude-tools returns the right set for explore"
    (let [tools (adapter/claude-tools explore-spec)]
      (is (contains? tools "Read"))
      (is (contains? tools "Glob"))
      (is (contains? tools "Grep"))
      (is (contains? tools "Bash"))
      (is (contains? tools "WebFetch"))
      (is (not (contains? tools "Edit")))
      (is (not (contains? tools "Write"))))))

;; =============================================================================
;; Tool call translation
;; =============================================================================

(deftest tool-call->action-read
  (testing "translates Read tool call"
    (let [action (adapter/tool-call->action explore-spec
                   {:name "Read" :input {:file_path "src/a.clj"}})]
      (is (= {:tool :read :args ["src/a.clj"]} action)))))

(deftest tool-call->action-glob
  (testing "translates Glob tool call"
    (let [action (adapter/tool-call->action explore-spec
                   {:name "Glob" :input {:pattern "**/*.clj"}})]
      (is (= {:tool :glob :args ["**/*.clj"]} action)))))

(deftest tool-call->action-bash-variant
  (testing "translates Bash tool call to peripheral's bash variant"
    (let [action (adapter/tool-call->action explore-spec
                   {:name "Bash" :input {:command "ls -la"}})]
      (is (= {:tool :bash-readonly :args ["ls -la"]} action)))))

(deftest tool-call->action-unmapped-tool
  (testing "returns SocialError for tool not in peripheral"
    (let [result (adapter/tool-call->action explore-spec
                   {:name "Edit" :input {:file_path "src/a.clj"}})]
      (is (shapes/valid? shapes/SocialError result))
      (is (= :unmapped-tool (:error/code result))))))

(deftest tool-call->action-no-input
  (testing "handles missing input gracefully"
    (let [action (adapter/tool-call->action explore-spec
                   {:name "Read"})]
      (is (= :read (:tool action)))
      (is (= [] (:args action))))))

;; =============================================================================
;; Constraint description
;; =============================================================================

(deftest describe-constraints-explore
  (testing "describe-constraints for explore"
    (let [c (adapter/describe-constraints explore-spec)]
      (is (some #{"read" "glob" "grep"} (:allowed-tools c)))
      (is (some #{"edit" "write"} (:forbidden-tools c)))
      (is (= "Full codebase — no path restrictions" (:scope c))))))

(deftest describe-constraints-edit-shows-paths
  (testing "describe-constraints for edit shows path scope"
    (let [c (adapter/describe-constraints edit-spec)]
      (is (str/includes? (:scope c) "src/"))
      (is (str/includes? (:scope c) "docs/")))))

(deftest peripheral-prompt-section-generates-text
  (testing "peripheral-prompt-section produces formatted prompt"
    (let [prompt (adapter/peripheral-prompt-section explore-spec {:session-id "s1"})]
      (is (string? prompt))
      (is (str/includes? prompt "EXPLORE"))
      (is (str/includes? prompt "read"))
      (is (str/includes? prompt "glob"))
      (is (str/includes? prompt "s1"))
      ;; Contains forbidden tools
      (is (str/includes? prompt "edit")))))

;; =============================================================================
;; Exit detection
;; =============================================================================

(deftest detect-exit-found-target
  (testing "detect-exit finds 'found target' signal in explore"
    (let [result (adapter/detect-exit explore-spec "I've found the target file.")]
      (is (some? result))
      (is (= :found-target (:exit-condition result))))))

(deftest detect-exit-hop-reflect
  (testing "detect-exit finds hop-to-reflect signal"
    (let [result (adapter/detect-exit explore-spec "Time to hop to reflect on this session.")]
      (is (some? result))
      (is (= :hop-reflect (:exit-condition result))))))

(deftest detect-exit-returns-nil-for-no-match
  (testing "detect-exit returns nil when no exit signal found"
    (is (nil? (adapter/detect-exit explore-spec "Just reading some files...")))))

(deftest detect-exit-only-matches-peripheral-exits
  (testing "detect-exit ignores signals not in peripheral's exit set"
    ;; "tests pass" is not in explore's exit conditions
    (is (nil? (adapter/detect-exit explore-spec "All tests pass!")))))

(deftest detect-exit-nil-and-empty-text
  (testing "detect-exit returns nil for nil or empty text"
    (is (nil? (adapter/detect-exit explore-spec nil)))
    (is (nil? (adapter/detect-exit explore-spec "")))))
