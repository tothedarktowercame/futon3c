(ns futon3c.agents.mfuton-invoke-override-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.agents.mfuton-invoke-override :as mfuton-invoke-override]
            [futon3c.mfuton-mode :as mfuton-mode]))

(deftest claude-role-codex-opts-disabled-outside-mfuton-mode
  (testing "generic futon mode leaves Claude-backed paths unchanged"
    (with-redefs [mfuton-mode/mfuton-mode? (constantly false)]
      (is (nil? (mfuton-invoke-override/claude-role-codex-opts
                 {:agent-id "claude-1"}))))))

(deftest claude-role-codex-opts-uses-safe-codex-defaults-in-mfuton-mode
  (testing "mfuton mode redirects Claude-role invokes to constrained Codex defaults"
    (with-redefs [mfuton-mode/mfuton-mode? (constantly true)]
      (let [opts (mfuton-invoke-override/claude-role-codex-opts
                  {:agent-id "claude-1"
                   :session-file "/tmp/futon-session-id"
                   :session-id-atom :sid-atom
                   :timeout-ms 12345
                   :cwd "i:/gh/mfuton"})]
        (is (= "claude-1" (:agent-id opts)))
        (is (= "codex" (:codex-bin opts)))
        (is (= "gpt-5-codex" (:model opts)))
        (is (= "workspace-write" (:sandbox opts)))
        (is (= "untrusted" (:approval-policy opts)))
        (is (= 12345 (:timeout-ms opts)))
        (is (= "i:/gh/mfuton" (:cwd opts)))))))
