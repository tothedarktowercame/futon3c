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
;; Mode transition — DISCUSS → DIAGNOSE → EXECUTE state machine (R10)
;; =============================================================================

(def OperationalMode
  "Operational mode — the three-phase gate from mode-gate.flexiarg.
   DISCUSS: data gathering, information review (read-only).
   DIAGNOSE: analysis, pattern matching (read-only).
   EXECUTE: code changes, deployment (write-capable, requires approval)."
  [:enum :discuss :diagnose :execute])

(def ModeTransition
  "A validated mode transition event.
   R10 (mode-gate): transitions must be explicit with exit criteria.
   Valid paths: DISCUSS→DIAGNOSE, DIAGNOSE→EXECUTE (needs approval),
   EXECUTE→DISCUSS (exit with summary), Any→DISCUSS (reset/timeout)."
  [:map
   [:mode/from OperationalMode]
   [:mode/to OperationalMode]
   [:mode/actor :string]
   [:mode/at Timestamp]
   [:mode/exit-criteria {:optional true} :string]
   [:mode/approval-token {:optional true} :string]
   [:mode/summary {:optional true} :string]])

;; =============================================================================
;; Peripheral specs — capability envelopes (structural constraints)
;; =============================================================================

(def PeripheralId
  "Peripheral identifier — the five core peripherals."
  [:enum :explore :edit :test :deploy :reflect])

(def ToolSet
  "Set of tools available to a peripheral."
  [:set :keyword])

(def PeripheralScope
  "Scope constraint for a peripheral — what it can access."
  [:or
   [:enum :full-codebase :test-commands-only :git-push-only :session-log-only]
   [:map [:paths [:vector :string]]]])

(def PeripheralSpec
  "Definition of a peripheral — a constrained capability envelope.
   Constraints are structural, not behavioral: agents cannot accidentally
   exceed scope. Memory travels with session-id across hops.

   Derived from: futon3/docs/peripheral-spec.md"
  [:map
   [:peripheral/id PeripheralId]
   [:peripheral/tools ToolSet]
   [:peripheral/scope PeripheralScope]
   [:peripheral/entry [:set :keyword]]
   [:peripheral/exit [:set :keyword]]
   [:peripheral/context [:map-of :keyword :keyword]]])

;; =============================================================================
;; Hop protocol — session-id transfer across peripheral boundaries
;; =============================================================================

(def HopRequest
  "Agent's request to hop from one peripheral to another.
   Session-id is preserved across hops (futon3/docs/peripheral-spec.md §Hop Mechanics).
   :hop/exit-condition is the preferred way to specify why a peripheral is being exited.
   Falls back to :hop/context {:hop/exit-condition ...} for backwards compatibility."
  [:map
   [:hop/to PeripheralId]
   [:hop/reason :string]
   [:hop/session-id :string]
   [:hop/exit-condition {:optional true} :keyword]
   [:hop/context {:optional true} [:map-of :keyword :any]]])

(def HopResult
  "Result of a hop attempt — success means context was transferred."
  [:map
   [:hop/from PeripheralId]
   [:hop/to PeripheralId]
   [:hop/session-id :string]
   [:hop/at Timestamp]
   [:hop/success? :boolean]
   [:hop/context {:optional true} [:map-of :keyword :any]]])

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
   [:receipt/route {:optional true} :string]
   [:receipt/session-id {:optional true} :string]
   [:receipt/peripheral-id {:optional true} PeripheralId]
   [:receipt/fruit {:optional true} :any]])

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
;; Evidence landscape (R8, R9) — shared medium across timescales
;; =============================================================================

(def ClaimType
  "Type of claim made by an evidence entry.
   Extended beyond futon3's 5-type set to cover Corneli (2014) Table 24 entities."
  [:enum :goal :step :evidence :conclusion :question :observation
   :tension :correction :conjecture])

(def ArtifactRefType
  "Universal reference types for any artifact that can accumulate evidence."
  [:enum :pattern :mission :component :gate :session :agent :thread :evidence])

(def ArtifactRef
  "Universal reference to any artifact (Table 24's overloaded X)."
  [:map
   [:ref/type ArtifactRefType]
   [:ref/id :string]])

(def EvidenceType
  "Typed evidence event category (distinguishes timescale/function provenance)."
  [:enum :coordination :gate-traversal :pattern-selection :pattern-outcome
   :reflection :forum-post :mode-transition :presence-event :correction :conjecture])

(def EvidenceEntry
  "Primary evidence shape — all other evidence projections are derived from this.
   Entries are structured events (R9) attached to an ArtifactRef subject.
   Optional fields support reply-chains, forks, conjectures, and ephemera."
  [:map
   [:evidence/id :string]
   [:evidence/subject ArtifactRef]
   [:evidence/type EvidenceType]
   [:evidence/claim-type ClaimType]
   [:evidence/author :string]
   [:evidence/at Timestamp]
   [:evidence/body :any]
   [:evidence/tags [:vector :keyword]]
   [:evidence/pattern-id {:optional true} :keyword]
   [:evidence/session-id {:optional true} :string]
   [:evidence/in-reply-to {:optional true} :string]
   [:evidence/fork-of {:optional true} :string]
   [:evidence/conjecture? {:optional true} :boolean]
   [:evidence/ephemeral? {:optional true} :boolean]])

(def EvidenceQuery
  "Evidence store query parameters. All fields optional.
   include-ephemeral? defaults to false at the store layer."
  [:map
   [:query/subject {:optional true} ArtifactRef]
   [:query/type {:optional true} EvidenceType]
   [:query/claim-type {:optional true} ClaimType]
   [:query/since {:optional true} Timestamp]
   [:query/limit {:optional true} :int]
   [:query/include-ephemeral? {:optional true} :boolean]])

;; =============================================================================
;; Error shape (R4: loud failure)
;; =============================================================================

(def SocialError
  "Structured error — R4 (loud failure): errors surface at the layer that
   caused them. No silent catch-and-swallow. Every component can produce this.

   Maps to :error-response outputs in social-exotype.edn."
  [:map
   [:error/component [:enum :S-presence :S-authenticate :S-mode
                       :S-dispatch :S-validate :S-persist :registry :peripheral
                       :E-store :E-threads :E-validate :E-compact :E-default
                       :transport]]
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
   :ModeTransition      ModeTransition
   :PeripheralSpec      PeripheralSpec
   :HopRequest          HopRequest
   :HopResult           HopResult
   :DispatchReceipt     DispatchReceipt
   :CoordinationOutcome CoordinationOutcome
   :SessionRecord       SessionRecord
   :PatternLibrary      PatternLibrary
   :AgentRegistryShape  AgentRegistryShape
   :ClaimType           ClaimType
   :ArtifactRefType     ArtifactRefType
   :ArtifactRef         ArtifactRef
   :EvidenceType        EvidenceType
   :EvidenceEntry       EvidenceEntry
   :EvidenceQuery       EvidenceQuery
   :SocialError         SocialError
   :TypedAgentId        TypedAgentId})
