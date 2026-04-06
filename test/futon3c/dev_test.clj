(ns futon3c.dev-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.test :refer [deftest is testing]]
            [futon3c.agency.registry]
            [futon3c.agents.mfuton-invoke-override]
            [futon3c.agents.tickle-work-queue]
            [futon3c.mfuton-mode :as mfuton-mode]
            [futon3c.evidence.store]
            [futon3c.dev.apm-conductor-v2 :as apm-v2]
            [futon3c.dev :as dev]))

(deftest compatible-codex-ws-bridge-agent-detection
  (testing "existing codex ws-bridge registrations are recognized as reusable"
    (let [body (json/generate-string
                {:ok true
                 :agent-id "codex-1"
                 :agent {:id {:id/value "codex-1" :id/type "continuity"}
                         :type "codex"
                         :metadata {:ws-bridge? true}}})]
      (is (true? (#'dev/compatible-codex-ws-bridge-agent? "codex-1" body)))))
  (testing "non-ws-bridge entries are not treated as reusable bridge registrations"
    (let [body (json/generate-string
                {:ok true
                 :agent-id "codex-1"
                 :agent {:id {:id/value "codex-1" :id/type "continuity"}
                         :type "codex"
                         :metadata {:proxy? true}}})]
      (is (false? (#'dev/compatible-codex-ws-bridge-agent? "codex-1" body))))))

(deftest codex-ws-bridge-registration-classification
  (testing "fresh registration succeeds directly"
    (is (= {:ok? true :action :registered}
           (#'dev/classify-codex-ws-bridge-registration
            "codex-1" 201 "{\"ok\":true}" nil nil))))
  (testing "duplicate registration keeps a compatible existing ws-bridge agent"
    (let [existing-body (json/generate-string
                         {:ok true
                          :agent-id "codex-1"
                          :agent {:id {:id/value "codex-1" :id/type "continuity"}
                                  :type "codex"
                                  :metadata {:ws-bridge? true}}})]
      (is (= {:ok? true :action :kept-existing}
             (#'dev/classify-codex-ws-bridge-registration
              "codex-1" 409 "{\"ok\":false}" 200 existing-body)))))
  (testing "duplicate registration fails loudly on incompatible existing state"
    (let [existing-body (json/generate-string
                         {:ok true
                          :agent-id "codex-1"
                          :agent {:id {:id/value "codex-1" :id/type "continuity"}
                                  :type "codex"
                                  :metadata {:proxy? true}}})
          result (#'dev/classify-codex-ws-bridge-registration
                  "codex-1" 409 "{\"ok\":false}" 200 existing-body)]
      (is (false? (:ok? result)))
      (is (= :conflict (:action result))))))

(deftest codex-ws-bridge-exception-summary
  (testing "blank top-level exception messages fall back to the root cause"
    (let [e (RuntimeException. nil (IllegalStateException. "connection refused"))]
      (is (= "IllegalStateException: connection refused"
             (#'dev/exception-summary e))))))

(deftest codex-ws-bridge-repeated-failure-tracking
  (testing "identical failures emit once immediately and then on the throttle interval"
    (let [state* (atom nil)]
      (is (= {:count 1 :first? true :emit? true}
             (#'dev/note-repeated-failure! state* "exception: boom" 3)))
      (is (= {:count 2 :first? false :emit? false}
             (#'dev/note-repeated-failure! state* "exception: boom" 3)))
      (is (= {:count 3 :first? false :emit? true}
             (#'dev/note-repeated-failure! state* "exception: boom" 3)))))
  (testing "clearing a repeated failure reports recovery only after multiple attempts"
    (let [state* (atom {:detail "exception: boom" :count 4})
          output (with-out-str
                   (#'dev/clear-repeated-failure! state* "[dev] codex ws bridge registration"))]
      (is (nil? @state*))
      (is (.contains output "recovered after 4 attempts")))))

(deftest status-reports-agent-routes-and-counts
  (testing "dev/status distinguishes registration from invocability"
    (with-redefs [futon3c.agency.registry/registered-agents
                  (fn [] [{:id/value "codex-1" :id/type :continuity}
                          {:id/value "slot-1" :id/type :continuity}])
                  futon3c.agency.registry/registry-status
                  (fn []
                    {:count 2
                     :ws-connected []
                     :agents {"codex-1" {:type :codex
                                         :status :idle
                                         :last-active "2026-03-29T13:33:33Z"
                                         :invoke-route :local
                                         :invoke-ready? true
                                         :invoke-diagnostic "local invoke-fn registered"
                                         :metadata {:ws-bridge? true}}
                              "slot-1" {:type :codex
                                        :status :idle
                                        :last-active "2026-03-29T13:33:33Z"
                                        :invoke-route :none
                                        :invoke-ready? false
                                        :invoke-diagnostic "no local invoke-fn and no ws bridge"
                                        :metadata {}}}})
                  futon3c.evidence.store/query* (fn [_ _] [])
                  futon3c.agents.tickle-work-queue/queue-status (fn [_] {:completed 0 :remaining 0})]
      (let [result (dev/status)]
        (is (= {:registered 2
                :invocable 1
                :local 1
                :ws 0
                :unreachable 1
                :inbound-ws-connected 0}
               (:agent-counts result)))
        (is (= :local (get-in result [:agents "codex-1" :invoke-route])))
        (false? (get-in result [:agents "slot-1" :invoke-ready?]))))))

(deftest codex-lane-runtime-state-reports-live-control
  (let [old-status @dev/!codex-status
        old-controls @dev/!invoke-controls]
    (try
      (reset! dev/!codex-status
              {"codex-1" {:agent-id "codex-1"
                          :turn-count 2
                          :phase :completed
                          :lifecycle-status :resting
                          :prompt-preview "Multiply by 2"
                          :result-preview "153578"}})
      (reset! dev/!invoke-controls
              {"codex-1" {:registered-at "2026-03-29T14:00:00Z"}})
      (let [result (dev/codex-lane-runtime-state "codex-1")]
        (is (= 2 (:turn-count result)))
        (is (= :completed (:phase result)))
        (is (true? (:interrupt-available? result)))
        (is (= "2026-03-29T14:00:00Z"
               (:invoke-control-registered-at result))))
      (finally
        (reset! dev/!codex-status old-status)
        (reset! dev/!invoke-controls old-controls)))))

(deftest clear-codex-lane-runtime-state-drops-cached-status
  (let [old-status @dev/!codex-status
        old-controls @dev/!invoke-controls]
    (try
      (reset! dev/!codex-status {"codex-1" {:turn-count 1}})
      (reset! dev/!invoke-controls {"codex-1" {:registered-at "now"}})
      (is (true? (dev/clear-codex-lane-runtime-state! "codex-1")))
      (is (nil? (get @dev/!codex-status "codex-1")))
      (is (nil? (get @dev/!invoke-controls "codex-1")))
      (finally
        (reset! dev/!codex-status old-status)
        (reset! dev/!invoke-controls old-controls)))))

(deftest codex-lane-runtime-state-uses-persisted-rollout-turn-count
  (let [old-status @dev/!codex-status
        old-controls @dev/!invoke-controls
        root (.toFile (java.nio.file.Files/createTempDirectory "codex-rollout-test"
                                                               (make-array java.nio.file.attribute.FileAttribute 0)))
        session-id "019d39df-087c-7310-87c2-c4e419e449ae"
        session-dir (io/file root "2026" "03" "29")
        rollout-file (io/file session-dir (str "rollout-2026-03-29T14-53-31-" session-id ".jsonl"))]
    (try
      (.mkdirs session-dir)
      (spit rollout-file
            (str
             (json/generate-string {:type "session_meta"
                                    :payload {:id session-id}})
             "\n"
             (json/generate-string {:type "response_item"
                                    :payload {:type "message"
                                              :role "user"
                                              :content [{:type "input_text"
                                                         :text "turn one"}]}})
             "\n"
             (json/generate-string {:type "response_item"
                                    :payload {:type "message"
                                              :role "assistant"
                                              :content [{:type "output_text"
                                                         :text "ok"}]}})
             "\n"
             (json/generate-string {:type "response_item"
                                    :payload {:type "message"
                                              :role "user"
                                              :content [{:type "input_text"
                                                         :text "turn two"}]}})
             "\n"))
      (reset! dev/!codex-status {"codex-1" {:agent-id "codex-1"
                                            :session-id session-id
                                            :phase :completed}})
      (reset! dev/!invoke-controls {})
      (reset! (var-get #'dev/!codex-rollout-summary-cache) {})
      (with-redefs-fn {#'futon3c.dev/codex-sessions-root (fn [] root)}
        (fn []
        (let [result (dev/codex-lane-runtime-state "codex-1")]
          (is (= 2 (:turn-count result)))
          (is (= (.getPath rollout-file) (:rollout-file result))))))
      (finally
        (reset! dev/!codex-status old-status)
        (reset! dev/!invoke-controls old-controls)
        (reset! (var-get #'dev/!codex-rollout-summary-cache) {})
        (doseq [f (reverse (file-seq root))]
          (io/delete-file f true))))))

(deftest apm-v2-mirror-state-combines-run-and-rollout-context
  (with-redefs [dev/codex-lane-runtime-state
                (fn [_]
                  {:agent-id "codex-1"
                   :session-id "sid-123"
                   :rollout-file "/tmp/codex-rollout.jsonl"
                   :turn-count 7})
                apm-v2/state-for-agent
                (fn [_]
                  {:current-phase :solve
                   :current-problem {:id "t97J01" :subject :topology}
                   :frame-workspace {:frame/id "frame-t97J01"
                                     :frame/workspace-root "/tmp/frame-t97J01"}})]
    (is (= {:agent-id "codex-1"
            :active? true
            :phase :solve
            :problem-id "t97J01"
            :subject :topology
            :frame-id "frame-t97J01"
            :frame-workspace-root "/tmp/frame-t97J01"
            :session-id "sid-123"
            :rollout-file "/tmp/codex-rollout.jsonl"
            :turn-count 7}
           (dev/apm-v2-mirror-state "codex-1")))))

(deftest mirror-apm-conductor-v2-to-codex-repl-opens-current-rollout
  (let [called (atom nil)]
    (with-redefs [dev/apm-v2-mirror-state
                  (fn [_]
                    {:agent-id "codex-1"
                     :active? true
                     :phase :solve
                     :problem-id "t97J01"
                     :rollout-file "/tmp/codex-rollout.jsonl"})
                  shell/sh
                  (fn [& args]
                    (reset! called args)
                    {:exit 0 :out "ok" :err ""})]
      (let [result (dev/mirror-apm-conductor-v2-to-codex-repl! :agent-id "codex-1"
                                                               :emacsclient-bin "emacsclient")]
        (is (true? (:ok? result)))
        (is (= "emacsclient" (first @called)))
        (is (some #{"-e"} @called))
        (is (some #(and (string? %)
                        (.contains ^String % "codex-repl-mirror-rollout")
                        (.contains ^String % "/tmp/codex-rollout.jsonl"))
                  @called))))))

(deftest make-claude-invoke-fn-delegates-to-codex-in-mfuton-mode
  (testing "mfuton mode redirects Claude-role invoke construction through Codex"
    (let [called (atom nil)
          sentinel (fn [_ _] {:result "ok"})]
      (with-redefs [futon3c.agents.mfuton-invoke-override/claude-role-codex-opts
                    (fn [_]
                      {:agent-id "claude-1"
                       :sandbox "workspace-write"
                       :approval-policy "untrusted"})
                    futon3c.dev/make-codex-invoke-fn
                    (fn [opts]
                      (reset! called opts)
                      sentinel)]
        (is (identical? sentinel
                        (dev/make-claude-invoke-fn {:agent-id "claude-1"})))
        (is (= "claude-1" (:agent-id @called)))
        (is (= "workspace-write" (:sandbox @called)))
        (is (= "untrusted" (:approval-policy @called)))))))

(deftest irc-invoke-prompt-mfuton-math-lane-pins-local-frontiermath-scope
  (testing "mfuton mode injects the n=3-only local contract on #math"
    (with-redefs [mfuton-mode/mfuton-mode (constantly "mfuton")]
      (let [prompt (#'dev/irc-invoke-prompt
                    {:nick "claude-2"
                     :sender "bobprobe-live"
                     :channel "#math"
                     :user-text "what is the next FM-001 step?"})]
        (is (re-find #"Local FrontierMath contract \(mfuton mode only\)" prompt))
        (is (re-find #"n=3 orchestration control" prompt))
        (is (re-find #"Do not propose or execute n=11, n=6 SAT/harness" prompt))
        (is (re-find #"mfuton/data/frontiermath-local/FM-001/artifacts/T3-search/2026-03-26-generated-witness/scripts/fm001/generate_witness.py" prompt))
        (is (re-find #"~/code/futon6" prompt))))))

(deftest irc-invoke-prompt-default-mode-leaves-math-lane-unpinned
  (testing "default futon mode leaves the generic IRC prompt unchanged"
    (with-redefs [mfuton-mode/mfuton-mode (constantly "futon")]
      (let [prompt (#'dev/irc-invoke-prompt
                    {:nick "claude-2"
                     :sender "bobprobe-live"
                     :channel "#math"
                     :user-text "what is the next FM-001 step?"})]
        (is (not (re-find #"Local FrontierMath contract \(mfuton mode only\)" prompt)))
        (is (not (re-find #"Do not propose or execute n=11, n=6 SAT/harness" prompt)))))))
