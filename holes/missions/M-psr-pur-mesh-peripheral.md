# Mission: PSR/PUR Mesh Peripheral (futon3a + futon3b)

Date: 2026-02-15  
Status: In progress (Codex-only live gate path landed)

## Latest Validation (2026-02-15)

Codex-only live validation is now runnable in one command:

```bash
./scripts/run_discipline_live_gate.sh
```

This performs:
1. Live WS readiness + action routing to `:discipline`.
2. Real-backend PSR->PUR->PAR cycle in `:discipline`.
3. Explicit hop `:discipline -> :reflect` with session continuity checks.
4. Artifact emission to `holes/qa/discipline-live-gate-*.edn`.
5. Automatic scorecard update at `holes/missions/alleycat-scorecard.md`.

Current state:
1. Gate A (PSR/PUR round-trip): implemented and passing in live harness.
2. Gate B (explicit transition): implemented and passing (`:discipline -> :reflect`).
3. Gate C (transport-native interleaving): partially covered (live WS routing proven; full multi-source interleave still pending).
4. Gate D (ALFWorld-triggered validation): pending.

## Why This Mission Exists

Three-way chat is now operational in futon3c (Joe + Claude + Codex, mention-gated,
shared continuity). The next practical step is to make collaboration discipline
first-class inside the coordination mesh:

1. Pattern selection and retrieval from futon3a (PSR side).
2. Pattern update and usage record back into futon3b evidence flow (PUR side).
3. Explicit session punctuation (PAR-compatible) so multi-agent runs stay coherent.

This mission defines a dedicated peripheral for that work so PSR/PUR behavior is
an inhabited capability envelope, not an ad-hoc instruction.

## Forum Port Implications (TN-forum-to-evidence-landscape)

The old futon3 Forum was a thread-first service and was abandoned without tests.
futon3c reframes this area as an Evidence Landscape where:

1. Evidence entries are primary.
2. Threads are projections over shared evidence.
3. Social/task/glacial events share one typed medium.

Therefore the `:discipline` peripheral must be designed as an Evidence Landscape
writer with typed PSR/PUR/PAR events. It should not recreate Forum-style
transport/service semantics inside the peripheral.

## Objective

Introduce a new discipline peripheral in futon3c that lets agents perform PSR/PUR
operations with explicit boundaries, typed records, and evidence traceability
across futon3a and futon3b.

This includes:
- typed evidence categories for discipline actions (`:pattern-selection`,
  `:pattern-outcome`, `:correction`, `:reflection`);
- canonical pattern identity (`:evidence/pattern-id`) across PSR->PUR flow;
- subject routing that enables projection (pattern-subject for pattern work,
  session-subject for session punctuation).

## Scope

In scope:

- Add a new peripheral profile for PSR/PUR discipline operations.
- Provide tools/routes for:
  - querying and selecting patterns (futon3a-facing behavior),
  - recording usage and post-run updates (futon3b-facing behavior),
  - producing PAR-compatible punctuation records for handoff.
- Add acceptance gates that directly test P-4 and P-6 in transport-native runs.

Out of scope:

- Full autonomous overnight orchestration.
- Replacing existing proof peripheral semantics.
- Any bypass of existing futon3c boundary invariants.

## Constraints

1. No workarounds: all behavior must preserve existing social/peripheral invariants.
2. One typed envelope across boundaries: no lossy prompt-only side-channel.
3. Evidence-first: each PSR/PUR action must be persisted as auditable records.
4. Session continuity required: transport pivots must preserve conversation identity.
5. No thread-first reimplementation: thread views must remain projections of
   evidence, never primary mutable state.

## Proposed Peripheral Contract (Draft)

Peripheral ID: `:discipline` (or renamed to `:psr-pur` after review)

Core operations:

1. `psr/search`
2. `psr/select`
3. `pur/update`
4. `pur/mark-pivot`
5. `par/punctuate`

Evidence mapping:
- `psr/select` -> evidence type `:pattern-selection` with pattern subject and
  canonical `:evidence/pattern-id`.
- `pur/update` -> evidence type `:pattern-outcome` linked to the selected pattern.
- `pur/mark-pivot` -> evidence type `:correction`.
- `par/punctuate` -> evidence type `:reflection` on session subject.

Draft output fields (minimum):

- `:discipline/session-id`
- `:discipline/agent-id`
- `:discipline/pattern-id`
- `:discipline/action` (`:psr/select`, `:pur/update`, etc.)
- `:discipline/rationale`
- `:discipline/evidence-refs`
- `:discipline/next-hop`

## Acceptance Gates

### Gate A: PSR/PUR Round-Trip

Pass when:

1. Agent performs `psr/search` + `psr/select` inside discipline peripheral.
2. Agent later performs `pur/update` using the selected pattern ID.
3. Evidence graph links selection and update via canonical `:evidence/pattern-id`.
4. Pattern-subject thread projection surfaces the PSR->PUR lifecycle.

### Gate B: P-4 Explicit Transition

Pass when:

1. Agent enters discipline peripheral through explicit hop.
2. Agent emits explicit exit condition (not implicit timeout/end-of-script).
3. Session continuity is preserved after return to parent context.

### Gate C: P-6 Interleaved Streams (Transport-Native)

Pass when:

1. Agent receives interleaved inputs from at least two live transports
   (for example Emacs + IRC).
2. Agent maintains correct PSR/PUR state under interleaving.
3. No cross-agent misrouting under mention-gated operation.

### Gate D: ALFWorld-Informed Validation

Pass when:

1. ALFWorld task event (or equivalent external-world signal) triggers PSR selection.
2. Outcome of that run produces PUR update in discipline peripheral.
3. Resulting PAR captures the pattern lifecycle and handoff state.

## Evidence Requirements

Minimum artifacts:

1. One transcript showing explicit hop in/out with session continuity.
2. One transcript showing interleaved transport messages and stable discipline state.
3. One structured PSR record and one PUR update record linked by pattern ID.
4. One PAR record that summarizes the cycle and next-hop recommendation.

## Implementation Entry Points

Likely files:

- `src/futon3c/peripheral/registry.clj`
- `src/futon3c/peripheral/adapter.clj`
- `src/futon3c/social/dispatch.clj`
- `resources/peripherals.edn`
- `test/futon3c/peripheral/*`
- `test/futon3c/social/*`

Integration boundaries:

- futon3a: pattern query/search adapter.
- futon3b: evidence submission/update adapter.

## Open Questions

1. Peripheral naming: `:discipline` vs `:psr-pur`.
2. Whether PSR and PUR should be a single peripheral mode or two coupled modes.
3. How strict the required schema should be before first live gate.

## Exit Criteria

Mission complete when:

1. Discipline peripheral exists with typed PSR/PUR operations.
2. Gates A-D pass in live runs with saved evidence.
3. Alleycat scorecard references this gate as completed or active with date and proof.
4. Discipline records align with Evidence Landscape semantics from
   `holes/technotes/TN-forum-to-evidence-landscape.md` (evidence-first,
   projection-friendly, no thread-first bypass).
