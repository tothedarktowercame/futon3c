(ns futon3c.dev-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is testing]]
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
