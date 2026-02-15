# Mission Notes: futon3c Codex Workstream

Date: 2026-02-14
Status: Active (updated after pulling `7d1a5d0`)

## Context

- Baseline branch head before sync: `4162a40` (`peripheral: add Codex parity for adapter last-mile wiring`).
- Current branch head after sync: `7d1a5d0` (`Add proof peripheral, bridge, mission specs, and scripts`).
- Existing mission docs indicate core peripheral bridge and transport adapters are complete.
- This note tracks Codex-specific integration and cleanup to merge cleanly with parallel Claude updates.

## Inspection Update (`7d1a5d0`)

1. The cross-repo bridge is now implemented.
   - `src/futon3c/bridge.clj` adds receipt conversion, gate submission, and sidecar recording.
   - This directly implements the earlier “social-to-task bridge” idea.

2. A sixth peripheral (`:proof`) is now in the core model.
   - Added in `resources/peripherals.edn`.
   - `PeripheralId` extended in `src/futon3c/social/shapes.clj`.
   - Registry wiring added in `src/futon3c/peripheral/registry.clj`.

3. Proof-domain implementation landed as first-class code.
   - `src/futon3c/peripheral/proof.clj`
   - `src/futon3c/peripheral/proof_backend.clj`
   - `src/futon3c/peripheral/proof_dag.clj`
   - `src/futon3c/peripheral/proof_shapes.clj`
   - `src/futon3c/peripheral/real_backend.clj`

4. Cross-repo triangle test exists and passes.
   - `test/futon3c/cross_repo_test.clj` exercises futon3c -> futon3b -> futon3a.

5. Scope-enforcement bugfixes landed.
   - `src/futon3c/peripheral/tools.clj`: path-scoped check now validates the path arg position rather than all string args.
   - `src/futon3c/peripheral/edit.clj`: edit/write path extraction now uses only the first argument as file path.

## Ideas To Carry Forward

1. Make the social-to-task bridge explicit in code, not only in mission prose.
   - Add a thin futon3c adapter that converts social/peripheral outcomes into a gate submission payload for futon3b.
   - Keep boundary semantics strict: typed shapes in, typed shapes out, loud failure on mismatch.

2. Keep agent parity at the adapter boundary.
   - Continue the `4162a40` direction: Claude and Codex event shapes normalize into the same peripheral action protocol.
   - Avoid separate execution logic per agent; differences should stay in translation-only layers.

3. Preserve evidence continuity across boundaries.
   - Ensure dispatch/peripheral evidence in futon3c can be linked to futon3b proof-path records without lossy transforms.
   - Prefer explicit correlation fields over inferred joins.

4. Prioritize invariant hardening that still appears as debt.
   - Snapshot-boundary enforcement (I3) and hop-exit fragility are high-value cleanup before broader operational rollout.

## Entry Points

### futon3c

- `src/futon3c/bridge.clj`
  - Typed bridge from DispatchReceipt to futon3b gate input and futon3a sidecar evidence.
- `src/futon3c/social/dispatch.clj`
  - Action-vs-coordination routing and peripheral dispatch receipt enrichment.
- `src/futon3c/peripheral/adapter.clj`
  - Claude/Codex tool-call normalization, prompt/instruction sections, exit detection.
- `src/futon3c/peripheral/registry.clj`
  - Chain orchestration and hop validation integration, now including `:proof`.
- `src/futon3c/peripheral/proof.clj`
  - Phase-gated proof peripheral runner.
- `src/futon3c/peripheral/proof_backend.clj`
  - Proof-domain tools and persistence/state semantics.
- `src/futon3c/peripheral/proof_shapes.clj`
  - Proof-domain shapes and transition constraints.
- `src/futon3c/peripheral/proof_dag.clj`
  - DAG checks and impact scoring.
- `src/futon3c/peripheral/real_backend.clj`
  - Real filesystem/command backend used by peripheral execution.
- `src/futon3c/transport/http.clj`
  - HTTP boundary into social pipeline.
- `src/futon3c/transport/ws.clj`
  - WS lifecycle/readiness boundary into social pipeline.
- `test/futon3c/cross_repo_test.clj`
  - End-to-end triangle verification across futon3c/futon3b/futon3a.
- `test/futon3c/social/dispatch_integration_test.clj`
  - End-to-end path with peripheral routing.
- `test/futon3c/peripheral/adapter_test.clj`
  - Codex/Claude adapter parity coverage.

### futon3b

- `src/futon3b/bootstrap.clj`
  - Existing submission path for futon3c tasks through gates.
- `src/futon3/gate/pipeline.clj`
  - Typed gate execution and rejection/persistence behavior.
- `data/missions.edn`
  - Active mission registry constraints used by G5 mission binding.

## Cleanup Plan

1. Mission-doc consistency pass.
   - Reconcile stale text in `M-peripheral-behavior.md` that claims Codex does not need adapter-level translation.
   - Align “five peripherals” language with current six-peripheral model where applicable.
   - Align counts/status notes with current test totals.

2. Boundary contract pass.
   - Define one typed submission envelope from futon3c outputs to futon3b `submit!`.
   - Add integration tests validating envelope construction and failure mapping.

3. Invariant hardening pass.
   - Implement snapshot-boundary enforcement in social pipeline paths still using live fallback behavior.
   - Replace fragile hop-exit inference with explicit exit-condition propagation where feasible.

4. Merge-stream safety pass.
   - Compare touched files across the two streams:
     - Mission specs (`holes/missions/*.md`)
     - Peripheral core (`src/futon3c/peripheral/*`)
     - Bridge path (`src/futon3c/bridge.clj`, `test/futon3c/cross_repo_test.clj`)
   - Resolve by invariant-preserving merge only (no special-case bypasses).

## Stream Merge Plan

1. Keep `7d1a5d0` as the integration base (it contains the newer structural information).
2. Rebase or replay any pending Codex-only edits onto this base with test-first checks.
3. Run full suite (`clojure -X:test`) after each conflict set (adapter/proof/bridge/docs).
4. Record conflict resolutions in mission notes with invariant rationale (why each resolution preserves global coherence).

## Stream-Merge Diff Report (Baseline `4162a40` → `7d1a5d0`)

### A. Runtime-Critical Additions (directly supports practical integration)

1. Cross-repo bridge (new):
   - `src/futon3c/bridge.clj`
   - `test/futon3c/cross_repo_test.clj`
   - Impact: first concrete `futon3c -> futon3b -> futon3a` execution path.
   - Merge stance: keep as primary integration route; extend, do not fork.

2. Proof peripheral stack (new):
   - `src/futon3c/peripheral/proof.clj`
   - `src/futon3c/peripheral/proof_backend.clj`
   - `src/futon3c/peripheral/proof_dag.clj`
   - `src/futon3c/peripheral/proof_shapes.clj`
   - `test/futon3c/peripheral/proof_test.clj`
   - `test/futon3c/peripheral/proof_backend_test.clj`
   - Impact: sixth peripheral with phase-gated cycle semantics and structured constraints.
   - Merge stance: keep intact; future edits must preserve phase and status invariants.

3. Real backend support (new):
   - `src/futon3c/peripheral/real_backend.clj`
   - `test/futon3c/peripheral/real_backend_test.clj`
   - Impact: makes peripheral execution operational against real files/commands.
   - Merge stance: keep; constrain destructive operations via existing scope/invariant regime.

### B. Model/Contract Changes (shared touchpoints with baseline work)

1. Peripheral set expansion:
   - `resources/peripherals.edn`
   - `src/futon3c/social/shapes.clj`
   - `src/futon3c/peripheral/registry.clj`
   - `test/futon3c/social/peripheral_spec_test.clj`
   - `test/futon3c/social/peripheral_test.clj`
   - `test/futon3c/peripheral/registry_test.clj`
   - Change: `5 -> 6` peripherals (`:proof` added).
   - Merge risk: medium (any assumptions hardcoded to five peripherals must be removed).

2. Adapter overlap with Codex parity work:
   - `src/futon3c/peripheral/adapter.clj`
   - Change: proof tool descriptions + mapping coverage extended; Codex parity path from `4162a40` preserved.
   - Merge risk: low-medium (shared file, mostly additive).

3. Scope enforcement bugfix:
   - `src/futon3c/peripheral/tools.clj`
   - `src/futon3c/peripheral/edit.clj`
   - Change: path checks now treat first arg as file path for edit/write flows.
   - Merge risk: low, high value (prevents false scope rejections and accidental scope leaks).

### C. Mission/Protocol Expansion (alignment docs for practical evaluations)

- Added:
  - `holes/missions/M-peripheral-phenomenology.md`
  - `holes/missions/M-peripheral-gauntlet.md`
  - `holes/missions/alleycat-scorecard.md`
  - `holes/missions/gauntlet-wiring.edn`
  - `holes/missions/par-alfworld-inhabitation-2026-02-13.edn`
- Impact: evaluation criteria are now explicit and operational (especially alleycat checkpoint grading).
- Merge stance: keep as normative guidance for pragmatic acceptance tests.

### D. Practical Gaps Remaining (for “both agents up with peripherals”)

1. Live Claude/Codex peripheral wrappers are still not ported in this repo.
   - Evidence: `scripts/` currently has proof/alfworld scripts only (no `fuclaude`/`fucodex` runtime wrappers).
   - Consequence: tests validate architecture, but day-to-day dual-agent operation still depends on external/manual orchestration.

2. Alleycat validation is currently documented + simulated strongly, but not yet fully transport-native.
   - `holes/missions/alleycat-scorecard.md` explicitly records limitations around live WS multiplexing and explicit hop transitions.
   - Consequence: we need a live run path that uses WS/dispatch/peripheral flow with both agents connected.

3. Background HTTP server-loop ClassCastException still appears during tests (non-failing).
   - Observed in `clojure -X:test` output while `transport.http-test` runs.
   - Consequence: does not currently break tests, but should be triaged before long-running practical sessions.

## Pragmatic Runway (Alleycat-Focused)

1. Establish dual-agent runtime path in futon3c.
   - Register both `:claude` and `:codex` agents with real invoke functions.
   - Ensure action-mode routing uses `:peripheral-config` and session continuity.

2. Bind alleycat checkpoints to executable integration tests.
   - Add an explicit test scenario that asserts checkpoint artifacts (spoke card flow, PAR structure, chat handoff references).

3. Promote the scorecard from retrospective doc to acceptance gate.
   - Treat `holes/missions/alleycat-scorecard.md` criteria as pass/fail for the next practical milestone.

4. Keep merge policy invariant-first.
   - Any change that weakens boundary, hop, or scope invariants is rejected even if it helps short-term demo velocity.

## Practical Readiness Check (Codex WS + Peripheral)

Date: 2026-02-14

- Added executable smoke script:
  - `scripts/codex_ws_alleycat_smoke.clj`
- Run command:
  - `clojure -M scripts/codex_ws_alleycat_smoke.clj`

### What it validates

1. Codex WS transport path comes up at `/agency/ws` with query-param identity.
2. Readiness handshake succeeds (`{"type":"ready_ack"}`).
3. Codex action message routes through peripheral dispatch (`route="peripheral/run-chain"`).
4. Codex default action peripheral is `edit` (`peripheral_id="edit"`).
5. Connected roster reflects lifecycle transitions:
   - Before close: `["codex-1"]`
   - After close: `[]`

### Result

- PASS on 2026-02-14.
- This establishes practical Codex readiness for real-time WS protocol + peripheral routing.
- Next integration step is dual-agent runtime (Codex + Claude simultaneously connected under the same session choreography).

## Practical Readiness Check (Dual-Agent WS Runtime)

Date: 2026-02-14

- Added executable dual-agent smoke script:
  - `scripts/dual_agent_ws_alleycat_smoke.clj`
- Run command:
  - `clojure -M scripts/dual_agent_ws_alleycat_smoke.clj`

### What it validates

1. Concurrent WS readiness for both agents:
   - Codex (`codex-1`)
   - Claude (`claude-1`)
2. Connected roster contains both after handshake:
   - `{:connected_agents [codex-1 claude-1], :count 2}`
3. Agent-default action routing is correct under live WS dispatch:
   - Codex action receipt: `route=peripheral/run-chain`, `peripheral_id=edit`
   - Claude action receipt: `route=peripheral/run-chain`, `peripheral_id=explore`
4. Coordination path remains direct invoke while both are connected:
   - Coordination receipt: `route=registry/invoke`
5. Disconnect lifecycle updates roster deterministically:
   - After codex close: `{:connected_agents [claude-1], :count 1}`
   - After both close: `{:connected_agents [], :count 0}`

### Result

- PASS on 2026-02-14.
- Practical dual-agent runtime gate is now green for WS + peripheral routing + coordination fallback.
- Next pragmatic step is to bind this to an explicit alleycat checkpoint sequence (artifact handoff assertions).

## Linode-Reachable Live Gate Config

Date: 2026-02-14

- Added live gate runner with external-Claude mode:
  - `scripts/dual_agent_ws_live_gate.clj`
- Added environment template for Linode/public WS routing:
  - `scripts/dual_agent_ws_live_gate.env.example`

### Local deterministic validation (completed)

- Command:
  - `FUTON3C_EXTERNAL_CLAUDE=false FUTON3C_PORT=47070 clojure -M scripts/dual_agent_ws_live_gate.clj`
- Result:
  - PASS (`codex -> edit`, `claude -> explore`)

### External Claude mode (for Linode)

1. Set `FUTON3C_PUBLIC_WS_BASE` to a host reachable from Claude on Linode.
2. Run:
   - `clojure -M scripts/dual_agent_ws_live_gate.clj`
3. Claude should connect as:
   - `claude-1` via `/agency/ws` using the printed URL.
4. If `FUTON3C_REQUIRE_CLAUDE_ACTION=true`, Claude must send:
   - WS frame type `message`
   - `msg_id=alleycat-claude-live-1`
   - string payload
   - `to=claude-1`
5. Gate passes only after observing Claude explore-route receipt.
