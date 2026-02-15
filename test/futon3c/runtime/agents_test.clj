(ns futon3c.runtime.agents-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.runtime.agents :as runtime]
            [futon3c.agency.registry :as reg]
            [futon3c.social.dispatch :as dispatch]
            [futon3c.social.mode :as mode]
            [futon3c.social.shapes :as shapes]
            [futon3c.social.test-fixtures :as fix]))

(use-fixtures
  :each
  (fn [f]
    (reg/reset-registry!)
    (f)))

(defn- ok-invoke
  [_prompt _session-id]
  {:result "ok" :session-id nil :exit-code 0})

(deftest register-codex-defaults
  (testing "register-codex! uses codex defaults and registers continuity ID"
    (let [result (runtime/register-codex! {:agent-id "codex-rt-1"
                                           :invoke-fn ok-invoke})]
      (is (map? result))
      (is (= :codex (:agent/type result)))
      (is (= "codex-rt-1" (get-in result [:agent/id :id/value])))
      (is (= :continuity (get-in result [:agent/id :id/type])))
      (is (some #{:edit} (:agent/capabilities result))))))

(deftest registry-snapshot-shape-valid
  (testing "registry-snapshot conforms to AgentRegistryShape and keeps agent type"
    (runtime/register-codex! {:agent-id "codex-rt-2" :invoke-fn ok-invoke})
    (runtime/register-claude! {:agent-id "claude-rt-2" :invoke-fn ok-invoke})
    (let [snapshot (runtime/registry-snapshot)]
      (is (shapes/valid? shapes/AgentRegistryShape snapshot))
      (is (= :codex (get-in snapshot [:agents "codex-rt-2" :type])))
      (is (= :claude (get-in snapshot [:agents "claude-rt-2" :type]))))))

(deftest runtime-config-codex-routes-to-edit
  (testing "runtime-config snapshot drives codex action messages to :edit peripheral"
    (runtime/register-codex! {:agent-id "codex-rt-3" :invoke-fn ok-invoke})
    (let [cfg (runtime/runtime-config
               {:patterns (fix/mock-patterns)
                :peripheral-config (fix/make-peripheral-config)})
          msg {:msg/id "msg-rt-codex-1"
               :msg/payload "fix auth bug"
               :msg/from (fix/make-agent-id "codex-rt-3" :continuity)
               :msg/to (fix/make-agent-id "codex-rt-3" :continuity)
               :msg/at (fix/now-str)}
          classified (mode/classify msg (:patterns cfg))
          result (dispatch/dispatch classified (:registry cfg))]
      (is (shapes/valid? shapes/DispatchReceipt result))
      (is (= "peripheral/run-chain" (:receipt/route result)))
      (is (= :edit (:receipt/peripheral-id result))))))

(deftest ws-codex-ready-and-action
  (testing "WS ready handshake + action message works for codex runtime config"
    (runtime/register-codex! {:agent-id "codex-rt-4" :invoke-fn ok-invoke})
    (let [sent (atom [])
          callbacks (runtime/make-ws-callbacks
                     {:patterns (fix/mock-patterns)
                      :peripheral-config (fix/make-peripheral-config)
                      :send-fn (fn [_ch data] (swap! sent conj data))
                      :close-fn (fn [_ch] nil)})
          ch :codex-rt-ch
          {:keys [on-open on-receive]} callbacks]
      (on-open ch {:request-method :get :uri "/ws" :request-uri "/ws"})
      (on-receive ch (json/generate-string {"type" "ready" "agent_id" "codex-rt-4"}))
      (on-receive ch (json/generate-string {"type" "message"
                                            "msg_id" "msg-rt-codex-2"
                                            "payload" "apply patch"
                                            "to" "codex-rt-4"}))
      (let [frames (map #(json/parse-string % true) @sent)
            ack (first frames)
            receipt (second frames)]
        (is (= "ready_ack" (:type ack)))
        (is (= "receipt" (:type receipt)))
        (is (= "edit" (:peripheral_id receipt)))))))
