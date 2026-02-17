# Tech Debt Assessment + Candidate Next Missions

Date: 2026-02-10

## Tech Debt Items (From Mission Reviews)

### 1. I3 Snapshot Enforcement (S-presence + S-dispatch) — RESOLVED

Description:
- Both components consult the live registry alongside the snapshot; the integrated pipeline intends the registry snapshot to be the authoritative constraint input.

Resolution (2026-02-17):
- S-presence already returns `:registry-missing` error when registry is nil (fail-fast, not lenient).
- S-dispatch uses snapshot for routing decisions; live registry is only for runtime invocation.
- Added test for nil registry enforcement in presence_test.clj.
- No code changes needed — the boundary was already correctly enforced.

### 2. Lossy `coerce-prompt` in S-dispatch — RESOLVED

Description:
- S-dispatch previously converted non-string payloads to a string via `pr-str`, which was lossy for structured payloads.

Resolution:
- `coerce-prompt` was already fixed: passes structured data through, only converts nil to "". No lossy pr-str conversion.

### 3. Exit Condition Inference Fragility in Hop Validation — RESOLVED

Description:
- Hop validation previously inferred exit conditions from `:hop/reason` substring matching, which was ordering-dependent and fragile.

Resolution (2026-02-17):
- Removed substring inference entirely from `resolve-exit-condition` (renamed from `infer-exit-condition`).
- Exit conditions now require explicit `:hop/exit-condition` keyword (top-level or nested in `:hop/context`).
- All existing callers already used explicit exit conditions — no breakage.
- 475 tests pass.

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
