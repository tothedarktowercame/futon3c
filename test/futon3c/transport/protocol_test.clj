(ns futon3c.transport.protocol-test
  "Tests for transport protocol translation layer (Part I).

   Verifies parse/render/extract-params functions against mission criteria:
   - Parse produces valid pipeline shapes from well-formed JSON
   - Parse returns SocialError for malformed/missing fields
   - Render produces valid JSON preserving all fields
   - Error rendering maps :error/code to correct HTTP status codes
   - extract-params handles both HTTP and WS upgrade requests (L3)
   - Round-trip: parse → pipeline → render preserves information"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [cheshire.core :as json]
            [futon3c.transport.protocol :as proto]
            [futon3c.social.shapes :as shapes]
            [futon3c.social.test-fixtures :as tf]
            [futon3c.social.mode :as mode]))

;; =============================================================================
;; parse-dispatch-request tests
;; =============================================================================

(deftest parse-dispatch-request-valid-json
  (testing "well-formed JSON produces a pre-classified message map"
    (let [input (json/generate-string {"msg_id" "msg-1"
                                       "payload" {"type" "standup"}
                                       "from" {"value" "claude-1" "type" "continuity"}
                                       "to" {"value" "mock-1" "type" "continuity"}})
          result (proto/parse-dispatch-request input)]
      (is (= "msg-1" (:msg/id result)))
      (is (= {:type "standup"} (:msg/payload result)))
      (is (= {:id/value "claude-1" :id/type :continuity} (:msg/from result)))
      (is (= {:id/value "mock-1" :id/type :continuity} (:msg/to result)))
      (is (string? (:msg/at result))))))

(deftest parse-dispatch-request-string-agent-shorthand
  (testing "string agent-ids are coerced to TypedAgentId with :continuity type"
    (let [input (json/generate-string {"msg_id" "msg-2"
                                       "payload" "hello"
                                       "from" "claude-1"
                                       "to" "mock-1"})
          result (proto/parse-dispatch-request input)]
      (is (= {:id/value "claude-1" :id/type :continuity} (:msg/from result)))
      (is (= {:id/value "mock-1" :id/type :continuity} (:msg/to result))))))

(deftest parse-dispatch-request-classifiable
  (testing "parsed message can be classified by mode/classify"
    (let [input (json/generate-string {"msg_id" "msg-3"
                                       "payload" {"type" "standup"}
                                       "from" "claude-1"
                                       "to" "mock-1"})
          parsed (proto/parse-dispatch-request input)
          classified (mode/classify parsed (tf/mock-patterns))]
      (is (= :coordination (:msg/mode classified)))
      (is (shapes/valid? shapes/ClassifiedMessage classified))
      ;; :msg/to passes through (Malli open maps)
      (is (= {:id/value "mock-1" :id/type :continuity} (:msg/to classified))))))

(deftest parse-dispatch-request-malformed-json
  (testing "malformed JSON returns SocialError"
    (let [result (proto/parse-dispatch-request "{invalid json")]
      (is (= :invalid-json (:error/code result)))
      (is (= :transport (:error/component result))))))

(deftest parse-dispatch-request-missing-required-fields
  (testing "missing msg_id returns SocialError"
    (let [result (proto/parse-dispatch-request
                  (json/generate-string {"payload" "x" "from" "a" "to" "b"}))]
      (is (= :invalid-message (:error/code result)))))

  (testing "missing from returns SocialError"
    (let [result (proto/parse-dispatch-request
                  (json/generate-string {"msg_id" "m1" "payload" "x" "to" "b"}))]
      (is (= :invalid-message (:error/code result)))))

  (testing "missing to returns SocialError"
    (let [result (proto/parse-dispatch-request
                  (json/generate-string {"msg_id" "m1" "payload" "x" "from" "a"}))]
      (is (= :invalid-message (:error/code result))))))

(deftest parse-dispatch-request-non-string-input
  (testing "non-string input returns SocialError"
    (is (= :invalid-json (:error/code (proto/parse-dispatch-request nil))))
    (is (= :invalid-json (:error/code (proto/parse-dispatch-request 42))))))

;; =============================================================================
;; parse-presence-request tests
;; =============================================================================

(deftest parse-presence-request-valid
  (testing "well-formed JSON produces AgentConnection"
    (let [input (json/generate-string {"agent_id" {"value" "claude-1" "type" "continuity"}
                                       "transport" "websocket"
                                       "metadata" {"ready" true}})
          result (proto/parse-presence-request input)]
      (is (= {:id/value "claude-1" :id/type :continuity} (:conn/agent-id result)))
      (is (= :websocket (:conn/transport result)))
      (is (true? (:ready (:conn/metadata result))))
      (is (string? (:conn/id result)))
      (is (string? (:conn/at result)))
      (is (shapes/valid? shapes/AgentConnection result)))))

(deftest parse-presence-request-string-agent-shorthand
  (testing "string agent_id coerced to TypedAgentId"
    (let [input (json/generate-string {"agent_id" "claude-1" "transport" "http"})
          result (proto/parse-presence-request input)]
      (is (= {:id/value "claude-1" :id/type :continuity} (:conn/agent-id result)))
      (is (= :http (:conn/transport result)))
      (is (nil? (:conn/metadata result))))))

(deftest parse-presence-request-default-transport
  (testing "missing transport defaults to :http"
    (let [input (json/generate-string {"agent_id" "claude-1"})
          result (proto/parse-presence-request input)]
      (is (= :http (:conn/transport result))))))

(deftest parse-presence-request-invalid-transport
  (testing "invalid transport returns SocialError"
    (let [result (proto/parse-presence-request
                  (json/generate-string {"agent_id" "a" "transport" "carrier-pigeon"}))]
      (is (= :invalid-message (:error/code result))))))

;; =============================================================================
;; parse-ws-message tests
;; =============================================================================

(deftest parse-ws-message-ready-frame
  (testing "ready frame parsed correctly"
    (let [input (json/generate-string {"type" "ready"
                                       "agent_id" "claude-1"
                                       "session_id" "sess-123"})
          result (proto/parse-ws-message input)]
      (is (= :ready (:ws/type result)))
      (is (= "claude-1" (:agent-id result)))
      (is (= "sess-123" (:session-id result))))))

(deftest parse-ws-message-ready-missing-agent
  (testing "ready frame without agent_id returns error"
    (let [result (proto/parse-ws-message
                  (json/generate-string {"type" "ready"}))]
      (is (= :invalid-frame (:error/code result))))))

(deftest parse-ws-message-message-frame
  (testing "message frame parsed into pre-classified message"
    (let [input (json/generate-string {"type" "message"
                                       "msg_id" "msg-42"
                                       "payload" "implement X"
                                       "from" "claude-1"
                                       "to" "codex-1"})
          result (proto/parse-ws-message input)]
      (is (= :message (:ws/type result)))
      (is (= "msg-42" (:msg/id result)))
      (is (= "implement X" (:msg/payload result)))
      (is (= {:id/value "claude-1" :id/type :continuity} (:msg/from result)))
      (is (= {:id/value "codex-1" :id/type :continuity} (:msg/to result))))))

(deftest parse-ws-message-unknown-type
  (testing "unknown frame type returns error"
    (let [result (proto/parse-ws-message
                  (json/generate-string {"type" "ping"}))]
      (is (= :invalid-frame (:error/code result))))))

(deftest parse-ws-message-missing-type
  (testing "frame without type field returns error"
    (let [result (proto/parse-ws-message
                  (json/generate-string {"data" "hello"}))]
      (is (= :invalid-frame (:error/code result))))))

;; =============================================================================
;; render-receipt tests
;; =============================================================================

(deftest render-receipt-basic
  (testing "DispatchReceipt renders to valid JSON"
    (let [receipt (tf/make-dispatch-receipt {:receipt/msg-id "msg-1"
                                            :receipt/route "registry/invoke"})
          json-str (proto/render-receipt receipt)
          parsed (json/parse-string json-str true)]
      (is (string? json-str))
      (is (= "msg-1" (:msg_id parsed)))
      (is (true? (:delivered parsed)))
      (is (= "registry/invoke" (:route parsed))))))

(deftest render-receipt-with-peripheral-fields
  (testing "optional peripheral fields included when present"
    (let [receipt (tf/make-dispatch-receipt {:receipt/msg-id "msg-2"
                                            :receipt/session-id "sess-abc"
                                            :receipt/peripheral-id :explore
                                            :receipt/fruit {:summary "done"}})
          json-str (proto/render-receipt receipt)
          parsed (json/parse-string json-str true)]
      (is (= "sess-abc" (:session_id parsed)))
      (is (= "explore" (:peripheral_id parsed)))
      (is (= {:summary "done"} (:fruit parsed))))))

;; =============================================================================
;; render-error tests
;; =============================================================================

(deftest render-error-maps-status-codes
  (testing "error codes map to correct HTTP statuses"
    (let [cases [[:invalid-message 400]
                 [:agent-not-found 404]
                 [:not-ready 403]
                 [:invoke-failed 502]
                 [:peripheral-failed 502]
                 [:invalid-registry 500]]]
      (doseq [[code expected-status] cases]
        (let [error (tf/make-social-error {:error/code code})
              rendered (proto/render-error error)]
          (is (= expected-status (:status rendered))
              (str "Expected " expected-status " for " code))
          (is (string? (:body rendered))))))))

(deftest render-error-body-is-valid-json
  (testing "error body parses as JSON with expected fields"
    (let [error (tf/make-social-error {:error/code :agent-not-found
                                       :error/message "Agent not found"})
          rendered (proto/render-error error)
          parsed (json/parse-string (:body rendered) true)]
      (is (true? (:error parsed)))
      (is (= "agent-not-found" (:code parsed)))
      (is (= "Agent not found" (:message parsed))))))

(deftest render-error-unknown-code-defaults-to-500
  (testing "unknown error codes default to 500"
    (let [error (tf/make-social-error {:error/code :something-weird})
          rendered (proto/render-error error)]
      (is (= 500 (:status rendered))))))

;; =============================================================================
;; render-ws-frame tests
;; =============================================================================

(deftest render-ws-frame-receipt
  (testing "DispatchReceipt renders as type=receipt frame"
    (let [receipt (tf/make-dispatch-receipt {:receipt/msg-id "msg-1"})
          frame (proto/render-ws-frame receipt)
          parsed (json/parse-string frame true)]
      (is (= "receipt" (:type parsed)))
      (is (= "msg-1" (:msg_id parsed)))
      (is (true? (:delivered parsed))))))

(deftest render-ws-frame-error
  (testing "SocialError renders as type=error frame"
    (let [error (tf/make-social-error {:error/code :not-ready
                                       :error/message "Not ready"})
          frame (proto/render-ws-frame error)
          parsed (json/parse-string frame true)]
      (is (= "error" (:type parsed)))
      (is (= "not-ready" (:code parsed)))
      (is (= "Not ready" (:message parsed))))))

(deftest render-ws-frame-presence
  (testing "PresenceRecord renders as type=presence frame"
    (let [presence (tf/make-presence)
          frame (proto/render-ws-frame presence)
          parsed (json/parse-string frame true)]
      (is (= "presence" (:type parsed)))
      (is (true? (:ready parsed))))))

(deftest render-ready-ack
  (testing "ready-ack frame has correct structure"
    (let [frame (proto/render-ready-ack)
          parsed (json/parse-string frame true)]
      (is (= "ready_ack" (:type parsed))))))

;; =============================================================================
;; extract-params tests — per realtime/request-param-resilience (L1, L3)
;; =============================================================================

(deftest extract-params-from-query-string
  (testing "tier 1: extracts from :query-string"
    (let [request {:query-string "agent_id=claude-1&session_id=sess-42"
                   :request-uri "/ws?agent_id=claude-1&session_id=sess-42"}
          params (proto/extract-params request)]
      (is (= "claude-1" (:agent-id params)))
      (is (= "sess-42" (:session-id params))))))

(deftest extract-params-from-request-uri-fallback
  (testing "tier 2: falls back to :request-uri when :query-string is nil (L3)"
    (let [request {:query-string nil
                   :request-uri "/ws?agent_id=claude-1&session_id=sess-42"}
          params (proto/extract-params request)]
      (is (= "claude-1" (:agent-id params)))
      (is (= "sess-42" (:session-id params))))))

(deftest extract-params-malformed-uri
  (testing "handles malformed URI without leading slash (L1)"
    (let [request {:query-string nil
                   :request-uri "?agent_id=claude-1"}
          params (proto/extract-params request)]
      (is (= "claude-1" (:agent-id params))))))

(deftest extract-params-empty-when-no-params
  (testing "tier 3: returns nil values when no params present"
    (let [request {:query-string nil
                   :request-uri "/ws"}
          params (proto/extract-params request)]
      (is (nil? (:agent-id params)))
      (is (nil? (:session-id params))))))

(deftest extract-params-url-encoded
  (testing "handles URL-encoded parameter values"
    (let [request {:query-string "agent_id=claude%201&session_id=sess%2042"}
          params (proto/extract-params request)]
      (is (= "claude 1" (:agent-id params)))
      (is (= "sess 42" (:session-id params))))))

;; =============================================================================
;; Round-trip test
;; =============================================================================

(deftest round-trip-dispatch-preserves-information
  (testing "parse → classify → render preserves key fields"
    (let [original {"msg_id" "msg-rt"
                    "payload" {"type" "standup" "topic" "progress"}
                    "from" {"value" "claude-1" "type" "continuity"}
                    "to" {"value" "mock-1" "type" "continuity"}}
          json-str (json/generate-string original)
          parsed (proto/parse-dispatch-request json-str)
          classified (mode/classify parsed (tf/mock-patterns))
          ;; Simulate dispatch producing a receipt
          receipt {:receipt/msg-id (:msg/id classified)
                   :receipt/to (:msg/to classified)
                   :receipt/delivered? true
                   :receipt/at (:msg/at classified)
                   :receipt/route "registry/invoke"}
          rendered (proto/render-receipt receipt)
          back (json/parse-string rendered true)]
      ;; msg-id survives the round trip
      (is (= "msg-rt" (:msg_id back)))
      ;; to agent survives
      (is (= "mock-1" (get-in back [:to :value])))
      (is (= "continuity" (get-in back [:to :type])))
      ;; delivered flag present
      (is (true? (:delivered back))))))

;; =============================================================================
;; Shape validation — transport errors are valid SocialErrors
;; =============================================================================

(deftest transport-errors-are-valid-social-errors
  (testing "all transport parse errors conform to SocialError shape"
    (let [errors [(proto/parse-dispatch-request "not json")
                  (proto/parse-dispatch-request "")
                  (proto/parse-dispatch-request
                   (json/generate-string {"payload" "x"}))
                  (proto/parse-presence-request "not json")
                  (proto/parse-ws-message
                   (json/generate-string {"no-type" true}))]]
      (doseq [err errors]
        (is (shapes/valid? shapes/SocialError err)
            (str "Expected valid SocialError: " (pr-str err)))))))
