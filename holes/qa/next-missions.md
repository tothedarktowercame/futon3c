# Tech Debt Assessment + Candidate Next Missions

Date: 2026-02-10

## Tech Debt Items (From Mission Reviews)

### 1. I3 Snapshot Enforcement (S-presence + S-dispatch)

Description:
- Both components consult the live registry alongside the snapshot; the integrated pipeline intends the registry snapshot to be the authoritative constraint input.

Evidence:
- `holes/missions/M-agency-refactor.md:200` through `holes/missions/M-agency-refactor.md:208`

Severity:
- Should-fix (before transport wiring). It does not break current tests, but it weakens the “constraints are slow inputs” boundary the exotype depends on.

Blocks:
- Transport layer and any “real” pipeline wiring that expects strict constraint snapshots.

### 2. Lossy `coerce-prompt` in S-dispatch

Description:
- S-dispatch currently converts non-string payloads to a string via `pr-str`, which is lossy for structured payloads.

Evidence:
- `holes/missions/M-agency-refactor.md:210` through `holes/missions/M-agency-refactor.md:214`

Severity:
- Cosmetic to should-fix depending on how quickly the system moves to structured payloads for agent invocation. It does not block the current “prompt-string” invocation model.

Blocks:
- Does not block transport, but blocks “structured message bus” evolution.

### 3. Exit Condition Inference Fragility in Hop Validation

Description:
- Hop validation infers exit conditions from `:hop/reason` substring matching. There is an explicit escape hatch via `:hop/context {:hop/exit-condition ...}`, but inference is ordering-dependent.

Evidence:
- `holes/missions/M-peripheral-model.md:151` through `holes/missions/M-peripheral-model.md:161`

Severity:
- Should-fix. Acceptable for early integration tests; risky for real user/system-driven hops.

Blocks:
- Does not block peripheral specs as data, but blocks reliable hop enforcement in production.

## Candidate Next Missions (2-3)

### Mission A: Snapshot Boundary Enforcement (I3 Hardening)

Scope:
- Remove or gate live-registry fallbacks at social pipeline boundaries and make snapshot usage explicit and testable. Define a clear separation between “registry snapshot” (constraint input) and “live registry” (runtime invocation authority), so dispatch/invocation can remain live without undermining constraint semantics.

Dependencies:
- Existing pipeline: `src/futon3c/social/presence.clj`, `src/futon3c/social/dispatch.clj`, integration tests in `test/futon3c/social/pipeline_test.clj`.

### Mission B: Structured Dispatch Payload Protocol

Scope:
- Introduce a structured payload protocol for dispatch/invocation so that the dispatch layer can route maps without stringifying. This likely requires either: (1) registry invoke-fn contract updated to accept a structured envelope, or (2) a typed “rendering” step that is explicit and reversible. Add tests to ensure no information loss for map payloads.

Dependencies:
- `src/futon3c/social/dispatch.clj`, `src/futon3c/agency/registry.clj`, plus any agent invoke factories.

### Mission C: Transport Wiring (HTTP + WebSocket) as Boundary Adapters

Scope:
- Implement thin adapters that accept external events (connection/message/mode transitions) and call the existing pure pipeline components, emitting `SocialError` on boundary failures. Keep transport logic out of core components; treat transport as adapters that produce/consume the shapes.

Dependencies:
- Social pipeline components (presence/auth/mode/dispatch/persist), plus evidence emission decisions (what gets appended to the evidence store).

### Mission D: PSR/PUR Mesh Peripheral (futon3a + futon3b)

Scope:
- Add a dedicated discipline peripheral for PSR/PUR operations so agents can select patterns (PSR), update outcomes (PUR), and emit PAR-compatible punctuation records inside a constrained capability envelope.
- Validate transport-native P-4 (explicit hop transition) and P-6 (interleaved streams) with live Emacs+IRC runs, including ALFWorld-informed scenarios.

Dependencies:
- `holes/missions/M-psr-pur-mesh-peripheral.md`
- futon3a query/search boundary (pattern retrieval)
- futon3b gate/evidence boundary (pattern usage updates)
