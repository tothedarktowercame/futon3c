(ns futon3c.social.shapes
  "Malli shapes for the social pipeline.

   Each shape defines the data contract at a pipeline boundary. These are
   the typed edges from social-exotype.edn made concrete as Malli schemas.

   Every component function's input/output is validated against these shapes
   in test (and optionally at runtime with instrumentation).

   Derived from: library/social/ARGUMENT.flexiarg (R1-R11)
                 futon5/data/missions/social-exotype.edn (ports + components)"
  (:require [malli.core :as m]
            [malli.error :as me]))

;; =============================================================================
;; Primitive types
;; =============================================================================

(def Timestamp
  "ISO-8601 instant or java.time.Instant."
  [:or :string inst?])

(def AgentIdType
  "Typed identifier namespace — transport, continuity, or protocol.
   R6 (identifier-separation): these are distinct namespaces."
  [:enum :transport :continuity :protocol])

(def TypedAgentId
  "Agent identifier with explicit type namespace (R6).
   Transport IDs (ws-conn-123) are not continuity IDs (claude-1)
   are not protocol IDs (agency/codex)."
  [:map
   [:id/value :string]
   [:id/type AgentIdType]])

(def AgentType
  "Agent type — what kind of agent this is."
  [:enum :claude :codex :mock :peripheral])

;; =============================================================================
;; Pipeline input — agent connection event
;; =============================================================================

(def AgentConnection
  "Raw agent connection event — the input that triggers the social pipeline.
   Maps to I-connections in social-exotype.edn (:http-request, :social timescale).

   A connection is a transport-level event: a WebSocket open, an IRC join,
   a peripheral hop. It carries transport identity but not yet verified presence."
  [:map
   [:conn/id :string]
   [:conn/transport [:enum :websocket :irc :http :peripheral]]
   [:conn/agent-id TypedAgentId]
   [:conn/at Timestamp]
   [:conn/metadata {:optional true} [:map-of :keyword :any]]])

;; =============================================================================
;; S-presence output — verified presence record (R7)
;; =============================================================================

(def PresenceRecord
  "Verified agent presence — output of S-presence.
   R7 (rendezvous-handshake): connection + explicit readiness = presence.
   Not just TCP-open, but confirmed ready to coordinate.

   Maps to S-presence :produces :xtdb-entity in social-exotype.edn."
  [:map
   [:presence/agent-id TypedAgentId]
   [:presence/conn-id :string]
   [:presence/ready? :boolean]
   [:presence/transport [:enum :websocket :irc :http :peripheral]]
   [:presence/at Timestamp]])

;; =============================================================================
;; S-authenticate output — resolved identity (R6)
;; =============================================================================

(def AgentIdentity
  "Resolved agent identity — output of S-authenticate.
   R6 (identifier-separation): transport ID resolved to typed identity
   with capabilities lookup from registry.

   Maps to S-authenticate :produces :xtdb-entity in social-exotype.edn."
  [:map
   [:identity/agent-id TypedAgentId]
   [:identity/type AgentType]
   [:identity/capabilities [:vector :keyword]]
   [:identity/at Timestamp]])

;; =============================================================================
;; S-mode output — classified message (R10)
;; =============================================================================

(def MessageMode
  "Message classification — coordination talk vs action talk.
   R10 (mode-gate): these must be distinguishable."
  [:enum :coordination :action])

(def ClassifiedMessage
  "Mode-classified message — output of S-mode.
   R10 (mode-gate): coordination and action are distinguishable modes.

   Maps to S-mode :produces :xtdb-entity in social-exotype.edn."
  [:map
   [:msg/id :string]
   [:msg/mode MessageMode]
   [:msg/payload :any]
   [:msg/from TypedAgentId]
   [:msg/at Timestamp]])

;; =============================================================================
;; S-dispatch output — routed message with receipt (R1, R2)
;; =============================================================================

(def DispatchReceipt
  "Dispatch receipt — output of S-dispatch.
   R1 (delivery-receipt): every message produces receipt or explicit failure.
   R2 (single-routing-authority): one authority routes each agent-id.

   Maps to S-dispatch :produces :xtdb-entity in social-exotype.edn."
  [:map
   [:receipt/msg-id :string]
   [:receipt/to TypedAgentId]
   [:receipt/delivered? :boolean]
   [:receipt/at Timestamp]
   [:receipt/route {:optional true} :string]])

;; =============================================================================
;; S-validate output — validated coordination outcome (R5)
;; =============================================================================

(def CoordinationOutcome
  "Validated coordination outcome — output of S-validate.
   R5 (bounded-lifecycle): transient resources have deterministic bounds.
   Validates that coordination produced a well-formed outcome.

   Maps to S-validate :produces :xtdb-entity in social-exotype.edn."
  [:map
   [:outcome/id :string]
   [:outcome/type [:enum :task-submission :coordination-complete :coordination-failed]]
   [:outcome/valid? :boolean]
   [:outcome/evidence [:map-of :keyword :any]]
   [:outcome/at Timestamp]])

;; =============================================================================
;; S-persist output — persisted session state (R8, R9)
;; =============================================================================

(def SessionRecord
  "Persisted session record — output of S-persist.
   R8 (authoritative-transcript): one transcript is authoritative.
   R9 (structured-events): events are structured and machine-parseable.

   Maps to S-persist :produces :xtdb-entity in social-exotype.edn."
  [:map
   [:session/id :string]
   [:session/agent-id TypedAgentId]
   [:session/state [:map-of :keyword :any]]
   [:session/at Timestamp]])

;; =============================================================================
;; Constraint inputs (shared with coordination-exotype)
;; =============================================================================

(def PatternLibrary
  "Pattern library constraints — same shape as futon3b.
   Maps to I-patterns in social-exotype.edn (:config, :glacial timescale).
   Shared with coordination-exotype."
  [:map
   [:patterns/ids [:vector :keyword]]])

(def AgentRegistryShape
  "Agent registry constraints — same shape as futon3b.
   Maps to I-registry in social-exotype.edn (:config, :slow timescale).
   Shared with coordination-exotype."
  [:map
   [:agents [:map-of :string
             [:map
              [:capabilities [:vector :keyword]]
              [:type {:optional true} AgentType]]]]])

;; =============================================================================
;; Error shape (R4: loud failure)
;; =============================================================================

(def SocialError
  "Structured error — R4 (loud failure): errors surface at the layer that
   caused them. No silent catch-and-swallow. Every component can produce this.

   Maps to :error-response outputs in social-exotype.edn."
  [:map
   [:error/component [:enum :S-presence :S-authenticate :S-mode
                       :S-dispatch :S-validate :S-persist :registry]]
   [:error/code :keyword]
   [:error/message :string]
   [:error/at Timestamp]
   [:error/context {:optional true} [:map-of :keyword :any]]])

;; =============================================================================
;; Validation helpers
;; =============================================================================

(defn validate
  "Validate data against a shape. Returns nil on success, error map on failure."
  [shape data]
  (when-not (m/validate shape data)
    {:error (me/humanize (m/explain shape data))
     :shape (m/form shape)}))

(defn valid?
  "Returns true if data conforms to shape."
  [shape data]
  (m/validate shape data))

;; =============================================================================
;; Shape registry — all shapes by name for programmatic access
;; =============================================================================

(def shapes
  "Map of shape name → Malli schema for all social pipeline shapes."
  {:AgentConnection     AgentConnection
   :PresenceRecord      PresenceRecord
   :AgentIdentity       AgentIdentity
   :ClassifiedMessage   ClassifiedMessage
   :DispatchReceipt     DispatchReceipt
   :CoordinationOutcome CoordinationOutcome
   :SessionRecord       SessionRecord
   :PatternLibrary      PatternLibrary
   :AgentRegistryShape  AgentRegistryShape
   :SocialError         SocialError
   :TypedAgentId        TypedAgentId})
