(ns futon3c.peripheral.war-machine-pilot-shapes
  "Minimal domain shapes for the War Machine Pilot peripheral.

   SPIKE-STAGE (M-war-machine-pilot VERIFY phase, 2026-05-24).
   Authored as the smallest viable CycleDomainConfig that:
     - passes `cycle/valid-domain-config?`
     - declares one observation phase (:observe)
     - declares one domain tool (:anchors-read) that reads wm-ui-anchors.edn
     - returns fruit naming the anchor count

   NOT YET implemented at this stage:
     - per-anchor consent-gate semantics (Pilot-I1 will land in INSTANTIATE)
     - write-action tools (:anchor-flip, :coherence-row-author, :pilot-action)
     - per-phase phase-sigil-tags
     - Agency registration mechanics

   This file's purpose: validate that the generic cycle.clj engine accepts
   a war-machine-pilot CycleDomainConfig without error. Reduces DR-V4 risk
   per M-war-machine-pilot.md VERIFY decision-log.

   Cross-refs:
   - DERIVE §Choice 2 (Mission peripheral as clone-from)
   - DERIVE §Choice 5 (writer-action-class direct write — DEFERRED to INSTANTIATE)
   - cycle/valid-domain-config? — the predicate this file aims to satisfy
   - mission_shapes.clj — the clone-from shape donor")

;; =============================================================================
;; Phases — minimal two-phase cycle
;; =============================================================================

(def phase-order
  "Two phases for the spike: :observe (read substrate) → :completed (terminal)."
  [:observe :completed])

;; =============================================================================
;; Tool operation kinds (:observe | :action)
;; =============================================================================

(def pilot-tool-operation-kinds
  "Tool classification. Phase 3 INSTANTIATE: substrate-write tools land here
   for the first time (`:anchor-flip`, `:coherence-row-author`, `:pilot-action`)
   gated by `:consent-gate-emit` per Pilot-I1.  See `substantive-tools` below
   for the closed-set declaration of write-action tools."
  {;; Pilot-specific observation tools
   :anchors-read         :observe
   :wm-api-query         :observe
   :playwright-probe-run :observe
   ;; Transport tools (emit signals to OTHER surfaces; non-substantive)
   :bell-emit            :action
   :heartbeat-emit       :action
   :walkie-psr           :action
   :walkie-pur           :action
   :walkie-par           :action
   ;; Consent-gate emitter (Phase 3 NEW; non-substantive — emits intent-handshake bell)
   :consent-gate-emit    :action
   ;; SUBSTANTIVE TOOLS (Phase 3 NEW; substrate-writing; Pilot-I1 applies)
   :anchor-flip          :action
   :coherence-row-author :action
   :pilot-action         :action
   ;; Cycle-control tools
   :cycle-begin          :action
   :cycle-advance        :action
   ;; Delegated standard tools (observe)
   :read                 :observe
   :glob                 :observe
   :grep                 :observe
   :bash-readonly        :observe})

(def substantive-tools
  "Closed-set declaration of substantive tools per Pilot-I1 (per DR-V2 2026-05-24).
   Any Command whose tool-id is in this set MUST cite a `:consent-gate-event-id`
   in its args; PilotBackend enforces structurally."
  #{:anchor-flip :coherence-row-author :pilot-action})

;; =============================================================================
;; Phase-allowed tools — what's permissible in each phase
;; =============================================================================

(def phase-allowed-tools
  "Phase tool gates.  Phase 3 INSTANTIATE: substantive write-action tools land
   in :observe (kept single-phase for v0 simplicity; the order-within-phase
   discipline — consent-gate-emit BEFORE substantive — is enforced by
   Pilot-I1 via args.:consent-gate-event-id citation rather than by phase
   transition)."
  {:observe   #{:read :glob :grep :bash-readonly
                :anchors-read :wm-api-query :playwright-probe-run
                :bell-emit :heartbeat-emit :walkie-psr :walkie-pur :walkie-par
                :consent-gate-emit
                :anchor-flip :coherence-row-author :pilot-action
                :cycle-advance}
   :completed #{}})

;; =============================================================================
;; Setup tools — available when no cycle is active
;; =============================================================================

(def setup-tools
  "Tools available before a cycle begins.  Phase 2: pilot can emit a setup-time
   bell (e.g., 'inhabiting' announcement) and query the WM API before deciding
   which cycle to begin."
  #{:read :glob :grep :bash-readonly
    :anchors-read :wm-api-query
    :bell-emit
    :cycle-begin})

;; =============================================================================
;; Required outputs per phase
;; =============================================================================

(def phase-required-outputs
  "What each phase must produce before advancing.
   :observe must produce :anchors-summary (the substrate-read fruit).
   :completed is terminal."
  {:observe   #{:anchors-summary}
   :completed #{}})
