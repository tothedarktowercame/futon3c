(ns futon3c.social.shapes-test
  "Shape validation tests — the shapes themselves are tested first.

   Pattern from futon3b: shapes are the first thing tested. Every shape
   must have at least one valid example and at least one invalid example.
   This ensures the Malli schemas are well-formed and catch real errors."
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.social.shapes :as shapes]))

;; =============================================================================
;; Test data factories
;; =============================================================================

(def valid-agent-id
  {:id/value "claude-1" :id/type :continuity})

(def valid-transport-id
  {:id/value "ws-conn-42" :id/type :transport})

(def now "2026-02-10T12:00:00Z")

;; =============================================================================
;; TypedAgentId
;; =============================================================================

(deftest typed-agent-id-valid
  (testing "valid typed agent IDs"
    (is (shapes/valid? shapes/TypedAgentId {:id/value "claude-1" :id/type :continuity}))
    (is (shapes/valid? shapes/TypedAgentId {:id/value "ws-42" :id/type :transport}))
    (is (shapes/valid? shapes/TypedAgentId {:id/value "agency/codex" :id/type :protocol}))))

(deftest typed-agent-id-invalid
  (testing "invalid typed agent IDs"
    (is (some? (shapes/validate shapes/TypedAgentId {:id/value "x"})))
    (is (some? (shapes/validate shapes/TypedAgentId {:id/value "x" :id/type :bogus})))
    (is (some? (shapes/validate shapes/TypedAgentId "just-a-string")))))

;; =============================================================================
;; AgentConnection
;; =============================================================================

(deftest agent-connection-valid
  (testing "valid agent connection"
    (is (shapes/valid? shapes/AgentConnection
                       {:conn/id "conn-1"
                        :conn/transport :websocket
                        :conn/agent-id valid-transport-id
                        :conn/at now}))
    (is (shapes/valid? shapes/AgentConnection
                       {:conn/id "conn-2"
                        :conn/transport :irc
                        :conn/agent-id valid-agent-id
                        :conn/at now
                        :conn/metadata {:source "irc-bridge"}}))))

(deftest agent-connection-invalid
  (testing "invalid agent connection — missing required fields"
    (is (some? (shapes/validate shapes/AgentConnection
                                {:conn/id "c1" :conn/transport :websocket})))
    (is (some? (shapes/validate shapes/AgentConnection
                                {:conn/id "c1"
                                 :conn/transport :smoke-signal
                                 :conn/agent-id valid-agent-id
                                 :conn/at now})))))

;; =============================================================================
;; PresenceRecord
;; =============================================================================

(deftest presence-record-valid
  (testing "valid presence record"
    (is (shapes/valid? shapes/PresenceRecord
                       {:presence/agent-id valid-agent-id
                        :presence/conn-id "conn-1"
                        :presence/ready? true
                        :presence/transport :websocket
                        :presence/at now}))))

(deftest presence-record-invalid
  (testing "invalid presence record — missing ready?"
    (is (some? (shapes/validate shapes/PresenceRecord
                                {:presence/agent-id valid-agent-id
                                 :presence/conn-id "conn-1"
                                 :presence/transport :websocket
                                 :presence/at now})))))

;; =============================================================================
;; AgentIdentity
;; =============================================================================

(deftest agent-identity-valid
  (testing "valid agent identity"
    (is (shapes/valid? shapes/AgentIdentity
                       {:identity/agent-id valid-agent-id
                        :identity/type :claude
                        :identity/capabilities [:explore :edit :test]
                        :identity/at now}))))

(deftest agent-identity-invalid
  (testing "invalid agent identity — bad type"
    (is (some? (shapes/validate shapes/AgentIdentity
                                {:identity/agent-id valid-agent-id
                                 :identity/type :unknown-type
                                 :identity/capabilities []
                                 :identity/at now})))))

;; =============================================================================
;; ClassifiedMessage
;; =============================================================================

(deftest classified-message-valid
  (testing "valid classified message"
    (is (shapes/valid? shapes/ClassifiedMessage
                       {:msg/id "msg-1"
                        :msg/mode :coordination
                        :msg/payload {:type "standup"}
                        :msg/from valid-agent-id
                        :msg/at now}))
    (is (shapes/valid? shapes/ClassifiedMessage
                       {:msg/id "msg-2"
                        :msg/mode :action
                        :msg/payload "run tests"
                        :msg/from valid-agent-id
                        :msg/at now}))))

(deftest classified-message-invalid
  (testing "invalid classified message — bad mode"
    (is (some? (shapes/validate shapes/ClassifiedMessage
                                {:msg/id "msg-1"
                                 :msg/mode :unknown
                                 :msg/payload {}
                                 :msg/from valid-agent-id
                                 :msg/at now})))))

;; =============================================================================
;; DispatchReceipt
;; =============================================================================

(deftest dispatch-receipt-valid
  (testing "valid dispatch receipt"
    (is (shapes/valid? shapes/DispatchReceipt
                       {:receipt/msg-id "msg-1"
                        :receipt/to valid-agent-id
                        :receipt/delivered? true
                        :receipt/at now}))
    (is (shapes/valid? shapes/DispatchReceipt
                       {:receipt/msg-id "msg-2"
                        :receipt/to valid-agent-id
                        :receipt/delivered? false
                        :receipt/at now
                        :receipt/route "ws-direct"}))))

(deftest dispatch-receipt-invalid
  (testing "invalid dispatch receipt — missing delivered?"
    (is (some? (shapes/validate shapes/DispatchReceipt
                                {:receipt/msg-id "msg-1"
                                 :receipt/to valid-agent-id
                                 :receipt/at now})))))

;; =============================================================================
;; CoordinationOutcome
;; =============================================================================

(deftest coordination-outcome-valid
  (testing "valid coordination outcome"
    (is (shapes/valid? shapes/CoordinationOutcome
                       {:outcome/id "out-1"
                        :outcome/type :task-submission
                        :outcome/valid? true
                        :outcome/evidence {:gate :G5 :payload-size 42}
                        :outcome/at now}))))

(deftest coordination-outcome-invalid
  (testing "invalid coordination outcome — bad type"
    (is (some? (shapes/validate shapes/CoordinationOutcome
                                {:outcome/id "out-1"
                                 :outcome/type :magic
                                 :outcome/valid? true
                                 :outcome/evidence {}
                                 :outcome/at now})))))

;; =============================================================================
;; SessionRecord
;; =============================================================================

(deftest session-record-valid
  (testing "valid session record"
    (is (shapes/valid? shapes/SessionRecord
                       {:session/id "sess-1"
                        :session/agent-id valid-agent-id
                        :session/state {:thread-id "t-1" :connected true}
                        :session/at now}))))

(deftest session-record-invalid
  (testing "invalid session record — agent-id not typed"
    (is (some? (shapes/validate shapes/SessionRecord
                                {:session/id "sess-1"
                                 :session/agent-id "claude-1"
                                 :session/state {}
                                 :session/at now})))))

;; =============================================================================
;; PatternLibrary
;; =============================================================================

(deftest pattern-library-valid
  (testing "valid pattern library"
    (is (shapes/valid? shapes/PatternLibrary
                       {:patterns/ids [:rendezvous-handshake :delivery-receipt]}))))

;; =============================================================================
;; AgentRegistryShape
;; =============================================================================

(deftest agent-registry-shape-valid
  (testing "valid agent registry shape"
    (is (shapes/valid? shapes/AgentRegistryShape
                       {:agents {"claude-1" {:capabilities [:explore :edit :test]
                                             :type :claude}
                                 "codex-1"  {:capabilities [:edit]}}}))))

;; =============================================================================
;; SocialError
;; =============================================================================

(deftest social-error-valid
  (testing "valid social error"
    (is (shapes/valid? shapes/SocialError
                       {:error/component :S-presence
                        :error/code :agent-not-found
                        :error/message "Agent ws-42 not in registry"
                        :error/at now}))
    (is (shapes/valid? shapes/SocialError
                       {:error/component :registry
                        :error/code :duplicate-registration
                        :error/message "Agent claude-1 already registered"
                        :error/at now
                        :error/context {:existing-id "claude-1"}}))))

(deftest social-error-invalid
  (testing "invalid social error — bad component"
    (is (some? (shapes/validate shapes/SocialError
                                {:error/component :S-unknown
                                 :error/code :bad
                                 :error/message "nope"
                                 :error/at now})))))

;; =============================================================================
;; Shape registry completeness
;; =============================================================================

(deftest all-shapes-registered
  (testing "shapes map contains all expected shapes"
    (let [expected #{:AgentConnection :PresenceRecord :AgentIdentity
                     :ClassifiedMessage :DispatchReceipt :CoordinationOutcome
                     :SessionRecord :PatternLibrary :AgentRegistryShape
                     :SocialError :TypedAgentId}]
      (is (= expected (set (keys shapes/shapes)))))))

(deftest all-shapes-are-valid-malli-schemas
  (testing "every shape in the registry is a valid Malli schema"
    (doseq [[shape-name schema] shapes/shapes]
      (testing (str "shape: " shape-name)
        (is (some? (malli.core/schema schema))
            (str shape-name " is not a valid Malli schema"))))))
