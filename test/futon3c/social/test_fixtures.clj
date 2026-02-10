(ns futon3c.social.test-fixtures
  "Shared test utilities for social pipeline tests.

   Provides factories for all shape-conforming test data, hermetic temp
   directories, and mock registries. Pattern from futon3b: test data
   factories produce shape-validated instances."
  (:require [futon3c.social.shapes :as shapes])
  (:import [java.util UUID]
           [java.io File]
           [java.time Instant]))

;; =============================================================================
;; Hermetic temp directory (same pattern as futon3b)
;; =============================================================================

(defn temp-dir!
  "Create a fresh temporary directory under /tmp for test isolation."
  []
  (let [d (File. "/tmp" (str "futon3c-test-" (UUID/randomUUID)))]
    (.mkdirs d)
    (.getAbsolutePath d)))

(defmacro with-temp-dir
  "Execute body with a fresh temp directory bound to sym."
  [sym & body]
  `(let [~sym (temp-dir!)]
     (try
       ~@body
       (finally
         ;; Best-effort cleanup — tests should not depend on this
         (doseq [f# (reverse (file-seq (File. ~sym)))]
           (.delete ^File f#))))))

;; =============================================================================
;; Test data factories — produce shape-conforming instances
;; =============================================================================

(defn make-agent-id
  "Create a typed agent ID."
  ([]
   (make-agent-id "claude-1" :continuity))
  ([value]
   (make-agent-id value :continuity))
  ([value id-type]
   {:id/value value :id/type id-type}))

(defn make-transport-id
  "Create a transport-typed agent ID."
  ([]
   (make-transport-id (str "ws-" (UUID/randomUUID))))
  ([value]
   {:id/value value :id/type :transport}))

(defn now-str
  "Current instant as ISO string."
  []
  (str (Instant/now)))

(defn make-connection
  "Create a valid AgentConnection."
  ([]
   (make-connection {}))
  ([overrides]
   (merge {:conn/id (str "conn-" (UUID/randomUUID))
           :conn/transport :websocket
           :conn/agent-id (make-transport-id)
           :conn/at (now-str)}
          overrides)))

(defn make-presence
  "Create a valid PresenceRecord."
  ([]
   (make-presence {}))
  ([overrides]
   (merge {:presence/agent-id (make-agent-id)
           :presence/conn-id (str "conn-" (UUID/randomUUID))
           :presence/ready? true
           :presence/transport :websocket
           :presence/at (now-str)}
          overrides)))

(defn make-identity
  "Create a valid AgentIdentity."
  ([]
   (make-identity {}))
  ([overrides]
   (merge {:identity/agent-id (make-agent-id)
           :identity/type :claude
           :identity/capabilities [:explore :edit :test]
           :identity/at (now-str)}
          overrides)))

(defn make-classified-message
  "Create a valid ClassifiedMessage."
  ([]
   (make-classified-message {}))
  ([overrides]
   (merge {:msg/id (str "msg-" (UUID/randomUUID))
           :msg/mode :coordination
           :msg/payload {:type "standup"}
           :msg/from (make-agent-id)
           :msg/at (now-str)}
          overrides)))

(defn make-dispatch-receipt
  "Create a valid DispatchReceipt."
  ([]
   (make-dispatch-receipt {}))
  ([overrides]
   (merge {:receipt/msg-id (str "msg-" (UUID/randomUUID))
           :receipt/to (make-agent-id)
           :receipt/delivered? true
           :receipt/at (now-str)}
          overrides)))

(defn make-coordination-outcome
  "Create a valid CoordinationOutcome."
  ([]
   (make-coordination-outcome {}))
  ([overrides]
   (merge {:outcome/id (str "out-" (UUID/randomUUID))
           :outcome/type :task-submission
           :outcome/valid? true
           :outcome/evidence {:gate :G5}
           :outcome/at (now-str)}
          overrides)))

(defn make-session-record
  "Create a valid SessionRecord."
  ([]
   (make-session-record {}))
  ([overrides]
   (merge {:session/id (str "sess-" (UUID/randomUUID))
           :session/agent-id (make-agent-id)
           :session/state {:thread-id "t-1"}
           :session/at (now-str)}
          overrides)))

(defn make-social-error
  "Create a valid SocialError."
  ([]
   (make-social-error {}))
  ([overrides]
   (merge {:error/component :S-presence
           :error/code :agent-not-found
           :error/message "Agent not found in registry"
           :error/at (now-str)}
          overrides)))

;; =============================================================================
;; Mock registries and patterns
;; =============================================================================

(defn mock-registry
  "Agent registry with test agents for use in component tests."
  ([]
   (mock-registry {}))
  ([overrides]
   (merge {:agents {"claude-1" {:capabilities [:explore :edit :test]
                                :type :claude}
                    "codex-1"  {:capabilities [:edit]
                                :type :codex}
                    "mock-1"   {:capabilities [:explore]
                                :type :mock}}}
          overrides)))

(defn mock-patterns
  "Minimal pattern library for tests."
  ([]
   (mock-patterns {}))
  ([overrides]
   (merge {:patterns/ids [:rendezvous-handshake
                          :delivery-receipt
                          :single-routing-authority
                          :mode-gate]}
          overrides)))

;; =============================================================================
;; Validation assertions
;; =============================================================================

(defn assert-valid!
  "Assert that data conforms to shape, throwing with explain on failure."
  [shape data]
  (when-let [err (shapes/validate shape data)]
    (throw (ex-info (str "Shape validation failed: " (:error err))
                    {:shape (:shape err) :data data :error (:error err)}))))
