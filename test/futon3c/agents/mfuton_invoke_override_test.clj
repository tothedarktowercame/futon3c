(ns futon3c.agents.mfuton-invoke-override-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.agents.mfuton-invoke-override :as mfuton-invoke-override]
            [futon3c.dev.config :as config]
            [futon3c.dev.irc :as dev-irc]
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

(deftest maybe-record-delivery-disabled-outside-mfuton-mode
  (testing "generic futon mode leaves invoke delivery on the generic path"
    (with-redefs [mfuton-mode/mfuton-mode? (constantly false)]
      (is (nil? (mfuton-invoke-override/maybe-record-delivery!
                 {:agent-id "codex-1"
                  :invoke-trace-id "invoke-xyz"
                  :receipt-line "Delivery: delivered via whistle -> caller joe (trace-id invoke-xyz)"}))))))

(deftest maybe-record-delivery-projects-to-irc-in-mfuton-mode
  (testing "mfuton mode owns the IRC projection seam for invoke delivery receipts"
    (let [calls (atom [])]
      (with-redefs [mfuton-mode/mfuton-mode? (constantly true)
                    config/env (fn [k & [default]]
                                 (if (= k "IRC_CHANNEL")
                                   "#math"
                                   default))
                    dev-irc/send-irc!
                    (fn [channel from-nick message]
                      (swap! calls conj {:channel channel
                                         :from-nick from-nick
                                         :message message})
                      true)]
        (is (true? (mfuton-invoke-override/maybe-record-delivery!
                    {:agent-id "codex-1"
                     :invoke-trace-id "invoke-xyz"
                     :receipt-line "Delivery: delivered via whistle -> caller joe (trace-id invoke-xyz)"})))
        (is (= [{:channel "#math"
                 :from-nick "codex"
                 :message "[invoke-delivery] codex-1 Delivery: delivered via whistle -> caller joe (trace-id invoke-xyz)"}]
               @calls))))))

(deftest maybe-record-delivery-skips-irc-echo-for-irc-surface-in-mfuton-mode
  (testing "mfuton mode treats IRC-surface delivery receipts as already delivered"
    (let [calls (atom [])]
      (with-redefs [mfuton-mode/mfuton-mode? (constantly true)
                    config/env (fn [_k & [default]] default)
                    dev-irc/send-irc!
                    (fn [channel from-nick message]
                      (swap! calls conj {:channel channel
                                         :from-nick from-nick
                                         :message message})
                      true)]
        (is (true? (mfuton-invoke-override/maybe-record-delivery!
                    {:agent-id "codex-1"
                     :invoke-trace-id "invoke-xyz"
                     :receipt {:surface "irc"
                               :destination "#test as <codex>"
                               :delivered? true
                               :note "matrix-ircd"}
                     :receipt-line "Delivery: delivered via irc -> #test as <codex> (trace-id invoke-xyz)"})))
        (is (empty? @calls))))))
