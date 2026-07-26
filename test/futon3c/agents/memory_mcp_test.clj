(ns futon3c.agents.memory-mcp-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.agents.memory-mcp :as mcp]
            [futon3c.agents.memory-provisioning :as provisioning]
            [futon3c.peripheral.memory-write :as memory-write]
            [futon3c.peripheral.real-backend :as real-backend]
            [futon3c.peripheral.tools :as tools]))

(deftest configured-seats-have-mathematics-domain-and-scribe-is-exclusive
  (doseq [agent-id ["zai-4" "codex-2" "codex-5" "codex-6"]]
    (is (= :mathematics (provisioning/domain-for agent-id))))
  (is (provisioning/tool-enabled? "codex-2" :memory-record))
  (doseq [agent-id ["zai-4" "codex-5" "codex-6"]]
    (is (not (provisioning/tool-enabled? agent-id :memory-record)))))

(deftest mcp-tool-list-teaches-required-contract
  (let [response (mcp/handle-request
                  {}
                  {:jsonrpc "2.0" :id 1 :method "tools/list"})
        description (get-in response [:result :tools 0 :description])]
    (doseq [required ["name (non-blank string)"
                      "body (memory content)"
                      "subjects [{ref/type, ref/id}]"]]
      (is (str/includes? description required)))))

(deftest memory-call-stamps-controller-identity-and-domain
  (let [seen (atom nil)
        response
        (mcp/handle-request
         {:agent-id "codex-2"
          :session-file nil
          :domain :mathematics
          :evidence-store ::store
          :record-memory-fn
          (fn [ctx payload]
            (reset! seen [ctx payload])
            {:ok true :id "e-test"})}
         {:jsonrpc "2.0" :id 2 :method "tools/call"
          :params {:name "memory_record"
                   :arguments {:name "n" :body "b"
                               :subjects [{"ref/type" "mission"
                                           "ref/id" "M"}]}}})]
    (is (= "codex-2" (get-in @seen [0 :agent-id])))
    (is (= :mathematics (get-in @seen [0 :domain])))
    (is (= ::store (get-in @seen [0 :evidence-store])))
    (is (= "n" (get-in @seen [1 :name])))
    (is (false? (get-in response [:result :isError])))))

(deftest codex-adapter-shaped-call-reaches-writer-with-controller-authorship
  (let [seen (atom nil)
        backend (real-backend/make-real-backend
                 {:evidence-store ::store
                  :agent-id "codex-2"
                  :session-id-fn (constantly "codex-session")
                  :memory-domain :mathematics})
        payload {:name "n" :body "b"
                 :subjects [{:ref/type "mission" :ref/id "M"}]}]
    (with-redefs [memory-write/record-memory!
                  (fn [ctx actual-payload]
                    (reset! seen [ctx actual-payload])
                    {:ok true :id "e-test"})]
      (tools/execute-tool backend :memory-record [payload]))
    (is (= "codex-2" (get-in @seen [0 :agent-id])))
    (is (= "codex-session" (get-in @seen [0 :session-id])))
    (is (= :mathematics (get-in @seen [0 :domain])))
    (is (= ::store (get-in @seen [0 :evidence-store])))
    (is (= payload (second @seen)))))
