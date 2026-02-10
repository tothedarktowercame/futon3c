# Mission: Agency Refactor

## Derivation

INSTANTIATE step of the social exotype derivation xenotype.

Prior:
- ARGUMENT.flexiarg (R1-R11) — requirements derived from agency + realtime patterns
- social-exotype.edn (8/8) — abstract wiring diagram verified standalone + composed
- compose-parallel merged to futon5 main (d5ae681)
- Bootstrap closure works (Part III of M-social-exotype)

## Scope In

- S-presence: Agent presence verification (R7)
- S-authenticate: Identity resolution (R6)
- S-dispatch: Message routing with receipts (R1, R2)
- S-persist: Session continuity (R8, R9)
- Agent registry: Core agent lifecycle (R3, R5)
- Social error handling (R4)

## Scope Out

- S-mode: Mode classification (R10) — belongs to M-peripheral-model
- S-validate: Coordination outcome validation (R5 lifecycle) — needs forum
- S-default: Default mode network — needs all other components first
- Forum service — M-forum-refactor
- Drawbridge routing — absorbed into S-dispatch, but HTTP/WS transport is later
- HTTP API — subsequent mission after core components work

## Source Material (futon3 → futon3c mapping)

| futon3 file | Lines | Maps to | Notes |
|-------------|-------|---------|-------|
| agency/registry.clj | 247 | futon3c/agency/registry.clj | Tests-first rewrite |
| agency/invokes.clj | 329 | futon3c/agency/invokes.clj | Keep Claude+Codex factories |
| agency/agents.clj | 149 | Absorbed into registry | Thin wrapper, not needed |
| agency/http.clj | 857 | futon3c/social/dispatch.clj (partial) | Only routing logic |
| agency/service.clj | 797 | futon3c/social/persist.clj (partial) | State persistence only |
| agency/codex_mirror.clj | 244 | Deferred | Not in social pipeline |

## Parts

### Part I: Registry + Shapes (Claude)

**Status:** Complete

:in  — futon3/agency/registry.clj (source), social-exotype.edn, ARGUMENT.flexiarg
:out — src/futon3c/social/shapes.clj
       src/futon3c/agency/registry.clj
       test/futon3c/social/shapes_test.clj
       test/futon3c/social/test_fixtures.clj
       test/futon3c/social/pipeline_test.clj (stub)
       test/futon3c/agency/invariant_test.clj
       test/futon3c/agency/registry_test.clj

What was done:
- Social pipeline Malli shapes for all boundary types
- Test infrastructure with shape-validated factories
- Registry rewritten with R1-R11 compliance from scratch
- Invariant tests ported from A0-A5 to R1-R11 (no EXPECTED FAIL markers)
- All tests pass

### Part II: S-presence (first Codex handoff)

**Status:** Complete (561da67)

:in  — src/futon3c/social/shapes.clj (READ-ONLY)
       test/futon3c/social/test_fixtures.clj (READ-ONLY)
       src/futon3c/agency/registry.clj (READ-ONLY)
       futon5/data/missions/social-exotype.edn (READ-ONLY, §S-presence)
:out — src/futon3c/social/presence.clj
       test/futon3c/social/presence_test.clj

Criteria:
- [x] Shape-validated: input is AgentConnection, output is PresenceRecord|SocialError
- [x] R7 (rendezvous-handshake): connection + explicit readiness = presence
- [x] Connection → presence pipeline boundary validated
- [x] 6 tests pass (5+ met)
- [x] No EXPECTED FAIL markers

Scope compliance: clean — two :out files created, no :in files modified.
Review note: registry-agent-exists? has a live-registry fallback to firm up in Part VI.

### Part III: S-authenticate (Codex)

**Status:** Complete (1d533f0)

:in  — src/futon3c/social/shapes.clj (READ-ONLY)
       src/futon3c/agency/registry.clj (READ-ONLY)
       src/futon3c/social/presence.clj (READ-ONLY, output shape)
:out — src/futon3c/social/authenticate.clj
       test/futon3c/social/authenticate_test.clj

Criteria:
- [x] R6 (typed identifiers): transport ID → typed agent identity
- [x] Identity → registry capability lookup
- [x] Input is PresenceRecord, output is AgentIdentity|SocialError
- [x] 6 tests pass (5+ met)
- [x] No EXPECTED FAIL markers

Scope compliance: clean — two :out files created, no :in files modified.
Review notes:
- No live-registry fallback (unlike S-presence): takes registry snapshot directly,
  clean I3 boundary. No integration concern here.
- `resolve-agent-id` cleanly handles R6: `:transport` → `:continuity`,
  `:protocol` → unresolvable. Correct — only transport and continuity resolve.
- Defensive output validation: constructed AgentIdentity validated before return.

### Part IV: S-dispatch (Codex)

**Status:** Complete (d86df0f + 40acbce)

:in  — src/futon3c/social/shapes.clj (READ-ONLY)
       src/futon3c/agency/registry.clj (READ-ONLY)
       src/futon3c/social/authenticate.clj (READ-ONLY, output shape)
:out — src/futon3c/social/dispatch.clj
       test/futon3c/social/dispatch_test.clj

Criteria:
- [x] R1 (delivery receipt): every message produces receipt or explicit failure
- [x] R2 (single routing authority): one authority per agent-id
- [x] Input includes ClassifiedMessage, output is DispatchReceipt|SocialError
- [x] 7 tests pass (5+ met)
- [x] No EXPECTED FAIL markers

Scope compliance: clean — two :out files created, no :in files modified.
Review notes:
- Dual registry check (snapshot + live): dispatch.clj lines 64 and 71 check the
  agent in both the registry snapshot AND the live registry (`reg/get-agent`),
  producing two distinct error codes (:agent-not-found vs :agent-not-registered).
  This is the same I3 boundary issue as S-presence. Part VI integration must
  resolve this: the snapshot should be authoritative, the live-registry check
  should be removed or gated. **This is now the second component with this
  pattern — fix consistently across S-presence and S-dispatch in Part VI.**
- `coerce-prompt` (lines 23-29) converts all non-string payloads to string via
  `pr-str`. Pragmatic for now — agents receive string prompts. But a structured
  map payload like {:type "standup"} becomes the string "{:type \"standup\"}",
  which is lossy. **When dispatch routes to agents that can handle structured
  payloads (e.g., Codex task specs), coerce-prompt must be replaced with a
  protocol that preserves payload structure.** Not blocking, but track it.

### Part V: S-persist (Codex)

**Status:** Complete (5f3ef6a)

:in  — src/futon3c/social/shapes.clj (READ-ONLY)
       src/futon3c/social/dispatch.clj (READ-ONLY, output shape)
:out — src/futon3c/social/persist.clj
       test/futon3c/social/persist_test.clj

Criteria:
- [x] R8 (authoritative transcript): one transcript is authoritative
- [x] R9 (structured events): events are structured, machine-parseable
- [x] Input flows from dispatch, output is SessionRecord|SocialError
- [x] 5+ tests pass (7 tests)
- [x] No EXPECTED FAIL markers

Scope compliance: clean — two :out files created, no :in files modified.
Review notes:
- `ensure-session-record` (lines 37-44) validates output shape and returns
  SocialError on internal failure — good defensive pattern, consistent with
  dispatch.clj.
- `update-session!` merges `:data` with `merge` (line 131) — derived snapshot
  is a simple overlay, not a reduce over events. R8 compliance holds because
  each state-update event contains the full update map, so the data snapshot
  is reconstructable from events alone.
- `list-sessions` (lines 143-154) returns empty vec for invalid agent-id
  rather than SocialError. Minor inconsistency with R4 (loud failure), but
  reasonable for a filter operation.

### Part VI: Integration (Claude)

**Status:** Complete

:in  — All component .clj files from Parts I-V
:out — test/futon3c/social/pipeline_test.clj (filled in)

Criteria:
- [x] Wire S-presence → S-authenticate → S-dispatch → S-persist
- [x] Pipeline integration test passes end-to-end (8 tests, 117 total)
- [ ] At least one proof-path from gate pipeline submission (bootstrap.clj pattern)
- [x] All R1-R11 invariant tests pass

Pipeline tests cover:
- Full 5-stage pipeline: connection → presence → identity → classify → dispatch → persist
- Session retrieval round-trip after persist
- Session update with authoritative transcript (R8: 2 events after update)
- Undelivered receipt rejection
- Multi-agent session listing and filtering
- Shape validation at every boundary
- Error propagation at correct pipeline stage (unknown agent, no readiness)

Remaining:
- Gate pipeline proof-path (bootstrap.clj pattern) — deferred to operational integration
- I3 snapshot enforcement and coerce-prompt clean-up tracked below

Integration clean-up (from Part II + Part IV reviews):

1. **I3 snapshot enforcement (S-presence + S-dispatch)**: Both components consult
   the live registry alongside the snapshot. `presence.clj` has a fallback in
   `registry-agent-exists?` that consults the live atom when input is not an
   `AgentRegistryShape` map. `dispatch.clj` checks the agent in both the snapshot
   (line 64) and the live registry via `reg/get-agent` (line 71). In the integrated
   pipeline, the constraint input MUST always be the snapshot (I3: slow constrains
   fast). To resolve: (a) ensure `verify` and `dispatch` receive the registry
   snapshot, never nil; (b) remove or gate the live-registry fallbacks so the
   pipeline boundary is clean; (c) resolve consistently across both components.

2. **`coerce-prompt` in S-dispatch**: `dispatch.clj` converts all non-string
   payloads to string via `pr-str` (lines 23-29). This is lossy for structured
   payloads. When the pipeline evolves to support structured agent communication
   (e.g., Codex task specs, evidence entries), replace `coerce-prompt` with a
   protocol that preserves payload structure. Not blocking, but track.

## Exit Conditions

- All R1-R11 invariant tests pass (no EXPECTED FAIL markers)
- Pipeline integration test: connection → presence → identity → dispatch → persist
- At least one proof-path from gate pipeline submission
- `clojure -X:test` passes cleanly

## R11 Enforcement (scope-bounded-handoff)

Each Part declares :in (read-only) and :out (writable). If a Codex handoff
modifies any :in file, that is a scope violation. This is the same structural
protection that caught the social-exotype.edn rewrite in futon5
feat/compose-parallel (a5ea168).

Constraint artifacts (exotypes, patterns, missions) are NEVER writable from
task timescale (I4 applied to agents).
