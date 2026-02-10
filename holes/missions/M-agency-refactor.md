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

**Status:** Ready for handoff

:in  — src/futon3c/social/shapes.clj (READ-ONLY)
       test/futon3c/social/test_fixtures.clj (READ-ONLY)
       src/futon3c/agency/registry.clj (READ-ONLY)
       futon5/data/missions/social-exotype.edn (READ-ONLY, §S-presence)
:out — src/futon3c/social/presence.clj
       test/futon3c/social/presence_test.clj

Criteria:
- [ ] Shape-validated: input is AgentConnection, output is PresenceRecord|SocialError
- [ ] R7 (rendezvous-handshake): connection + explicit readiness = presence
- [ ] Connection → presence pipeline boundary validated
- [ ] 5+ tests pass
- [ ] No EXPECTED FAIL markers

Specification (from social-exotype.edn §S-presence):
- Input: I-connections (:http-request) + I-registry (:config)
- Output: :xtdb-entity (PresenceRecord) or :error-response (SocialError)
- Function signature: `(verify connection registry) -> PresenceRecord|SocialError`
- Must: check agent exists in registry, verify readiness handshake
- Must NOT: modify registry or any :in file

### Part III: S-authenticate (Codex)

**Status:** Blocked on Part II

:in  — src/futon3c/social/shapes.clj (READ-ONLY)
       src/futon3c/agency/registry.clj (READ-ONLY)
       src/futon3c/social/presence.clj (READ-ONLY, output shape)
:out — src/futon3c/social/authenticate.clj
       test/futon3c/social/authenticate_test.clj

Criteria:
- [ ] R6 (typed identifiers): transport ID → typed agent identity
- [ ] Identity → registry capability lookup
- [ ] Input is PresenceRecord, output is AgentIdentity|SocialError
- [ ] 5+ tests pass
- [ ] No EXPECTED FAIL markers

### Part IV: S-dispatch (Codex)

**Status:** Blocked on Part III

:in  — src/futon3c/social/shapes.clj (READ-ONLY)
       src/futon3c/agency/registry.clj (READ-ONLY)
       src/futon3c/social/authenticate.clj (READ-ONLY, output shape)
:out — src/futon3c/social/dispatch.clj
       test/futon3c/social/dispatch_test.clj

Criteria:
- [ ] R1 (delivery receipt): every message produces receipt or explicit failure
- [ ] R2 (single routing authority): one authority per agent-id
- [ ] Input includes ClassifiedMessage, output is DispatchReceipt|SocialError
- [ ] 5+ tests pass
- [ ] No EXPECTED FAIL markers

### Part V: S-persist (Codex)

**Status:** Blocked on Part IV

:in  — src/futon3c/social/shapes.clj (READ-ONLY)
       src/futon3c/social/dispatch.clj (READ-ONLY, output shape)
:out — src/futon3c/social/persist.clj
       test/futon3c/social/persist_test.clj

Criteria:
- [ ] R8 (authoritative transcript): one transcript is authoritative
- [ ] R9 (structured events): events are structured, machine-parseable
- [ ] Input flows from dispatch, output is SessionRecord|SocialError
- [ ] 5+ tests pass
- [ ] No EXPECTED FAIL markers

### Part VI: Integration (Claude)

**Status:** Blocked on Parts II-V

:in  — All component .clj files from Parts I-V
:out — test/futon3c/social/pipeline_test.clj (filled in)

Criteria:
- [ ] Wire S-presence → S-authenticate → S-dispatch → S-persist
- [ ] Pipeline integration test passes end-to-end
- [ ] At least one proof-path from gate pipeline submission (bootstrap.clj pattern)
- [ ] All R1-R11 invariant tests pass

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
