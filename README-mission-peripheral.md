# Mission Peripheral

This document summarizes what the Mission Peripheral in `futon3c` actually
does today, based on the code in `src/` and the tests in `test/`.

If this README disagrees with older mission notes, trust the code first.

## What It Is

The Mission Peripheral is the code-mission analogue of the Proof Peripheral.
It is not portfolio inference, and it is not mission control.

It manages one mission at a time as a constrained, evidence-emitting,
phase-gated cycle machine:

- Proof Peripheral: "work a proof problem through a disciplined cycle"
- Mission Peripheral: "work a code/development mission through the same kind
  of disciplined cycle"
- Mission Control: "observe the whole portfolio"
- Portfolio Inference: "infer what to work on next from portfolio state"

The implementation lives mainly in:

- `src/futon3c/peripheral/mission.clj`
- `src/futon3c/peripheral/mission_backend.clj`
- `src/futon3c/peripheral/mission_shapes.clj`
- `src/futon3c/peripheral/cycle.clj`

Related siblings:

- `src/futon3c/peripheral/proof.clj`
- `src/futon3c/peripheral/mission_control.clj`
- `src/futon3c/portfolio/*.clj`

## Core Shape

The Mission Peripheral is a thin wrapper around the generic cycle engine in
`cycle.clj`.

`cycle.clj` provides:

- start / step / stop lifecycle
- phase-gated tool access
- required outputs per phase
- evidence emission for start, each step, and stop
- optional state snapshots
- blackboard projection

The mission-specific wrapper in `mission.clj` supplies:

- the mission domain id `:mission`
- mission phase order and tool gates
- mission-specific setup tools
- mission-specific persisted state
- mission-specific evidence snapshots on `:mission-save`
- mission-specific "fruit" and exit context
- optional mission wiring autoconfiguration

## Mission State

The persisted mission state is defined in `mission_shapes.clj` as
`MissionState`. It contains:

- `:mission/id`
- `:mission/version`
- `:mission/spec`
- `:mission/obligations`
- `:mission/cycles`
- `:mission/failed-approaches`
- `:mission/updated-at`

The most important parts are:

- `MissionSpec`: title, success criteria, scope in/out, version history
- `Obligation`: DAG-tracked task with status, evidence class, artifacts, owner,
  and dependency/unlock edges
- `CycleRecord`: one run through the 9-phase machine against one blocker
- `FailedApproach`: append-only dead-end record

Mission state is persisted as EDN under:

```text
data/mission-state/<mission-id>.edn
```

## The 9-Phase Cycle

The Mission Peripheral uses the same 9 phases as the Proof Peripheral:

1. `:observe`
2. `:propose`
3. `:execute`
4. `:validate`
5. `:classify`
6. `:integrate`
7. `:commit`
8. `:gate-review`
9. `:completed`

Phases must be traversed in order. The cycle engine does not allow skipping.

Each phase has:

- a restricted tool set
- required outputs that must exist before `:cycle-advance`

Current required outputs are:

- `:observe` -> `:blocker-id`
- `:propose` -> `:approach`
- `:execute` -> `:artifacts`
- `:validate` -> `:validation-artifacts`
- `:classify` -> `:classification`
- `:integrate` -> `:rationale`, `:obligation-changes`, `:doc-artifacts`,
  `:hypergraph-plan`
- `:commit` -> `:saved?`
- `:gate-review` -> `:gates-passed`

Two details matter here:

- `:integrate` now has a DOCUMENT checkpoint. The code requires
  documentation artifacts and a hypergraph plan before the cycle can
  honestly move through the later gates.
- Reaching `:completed` clears the active phase/cycle and increments
  `:cycles-completed`.

## Tool Surface

### Setup tools

When no cycle is active, the peripheral allows a wider setup surface:

- `:mission-load`, `:mission-save`
- `:obligation-query`, `:obligation-upsert`
- `:dag-check`, `:dag-impact`
- `:mission-spec-get`, `:mission-spec-update`
- `:cycle-begin`, `:cycle-list`, `:cycle-get`
- `:failed-approach-add`
- `:evidence-query`, `:corpus-check`
- `:mission-wiring`, `:mission-doc-audit`
- `:read`, `:glob`, `:grep`, `:bash-readonly`

### Phase-gated runtime

Once a cycle starts, the active phase decides what is allowed.

Examples:

- `:observe` allows read/query tools and forbids writes
- `:execute` allows `:write` and `:bash`
- `:integrate` allows obligation updates, failed-approach recording, and
  mission-spec updates
- `:commit` allows `:mission-save`
- `:gate-review` allows `:gate-check` and doc audit

## Backend Behavior

`mission_backend.clj` is where most of the mission-domain logic lives.

It implements:

- mission-state load/save
- obligation query/upsert
- status validation and transition enforcement
- DAG acyclicity and impact scoring
- mission spec versioning
- cycle begin/advance/get/list
- failed-approach append-only recording
- corpus check
- mission wiring lookup
- mission doc audit

Important enforcement already in code:

- obligation status must be valid
- status transitions depend on evidence class
- `:assertion` evidence cannot directly justify `:done`
- failure reasons cannot be erased
- DAG acyclicity is checked
- failed approaches are append-only

The mission backend reuses the same DAG algorithms as the proof domain via
`proof_dag.clj`.

## Evidence Behavior

The Mission Peripheral is evidence-native.

The cycle engine emits:

- start evidence
- one evidence entry per step
- stop evidence

The Mission Peripheral also emits a mission snapshot on `:mission-save`.

That snapshot includes:

- mission id and version
- summarized obligations by status
- cycle count
- failed-approach count
- current phase
- current cycle id
- cycles completed

The tests also verify that emitted evidence forms a valid proof-thread style
chain and satisfies the same proof-tree invariants used elsewhere.

## Table 25 Tags

The Mission Peripheral auto-tags step evidence with phase-specific sigil tags.

Current phase-to-tag mapping includes:

- `:observe` -> information / perception
- `:propose` -> argumentation / intuition
- `:execute` -> software / written language
- `:validate` -> logic / concrete application
- `:classify` -> comprehension / self-discovery
- `:integrate` -> collaborative knowledge / organization
- `:commit` -> consistency / gradual accumulation
- `:gate-review` -> quality / constructive feedback

This is implemented in `mission_shapes.clj` and applied by the generic cycle
engine in `cycle.clj`.

## Wiring And Doc Audit

The current code adds two mission-specific helpers beyond the original
"proof generalized to code" story.

### `:mission-wiring`

If the context includes `:mission-id`, `mission.clj` tries to load a
per-mission wiring diagram from:

- `holes/missions/<short-name>-wiring.edn`
- `holes/missions/M-<short-name>-wiring.edn`
- `holes/missions/<mission-id>-wiring.edn`

If found, the wiring diagram is attached to the effective cycle config and can
also be queried explicitly.

### `:mission-doc-audit`

The backend can audit a mission markdown file and the guide document for:

- presence of GF headings
- GF ordering in the guide
- open honest intervals linked to sections
- silent section gaps

This is then fed into `:gate-check`.

## Gate Review

`gate-check` is not just a generic "are we done?" call. The current local gate
stack is:

- `:G5-scope`
- `:G4-evidence`
- `:GF-fidelity`
- `:G3-status`
- `:G2-dag`
- `:GD-document`
- `:G1-obligations`
- `:G0-commit`

Notable current gates:

- `:GF-fidelity` fails on mission-doc drift, missing GF sections, silent
  section gaps, or open honest intervals
- `:GD-document` requires `:doc-artifacts` and either hypergraph refs or an
  explicit deferred reason

When local gates pass, the backend also submits a receipt into the futon3b
pipeline via `bridge/submit-to-gates!`.

## Relationship To Mission Control And Portfolio Inference

The current split is:

- Mission Peripheral: individual mission execution and evidence discipline
- Mission Control: portfolio observation and review
- Portfolio Inference: AIF-style deliberation over portfolio observations

Mission Control reads the world at portfolio level.
Portfolio Inference reasons over that portfolio state.
Mission Peripheral is the lower-level work lane that generates part of the
evidence Mission Control later observes.

## What Is Real Today

Implemented and tested:

- generic cycle-machine integration
- phase gating
- persisted mission EDN state
- obligation DAG management
- mission spec versioning
- failed-approach recording
- evidence emission
- save-time snapshots
- proof-like evidence threading
- mission wiring lookup
- mission doc audit
- gate review with GF and DOCUMENT checks

## Current Rough Edges

The code is ahead of some older docs, and a few edges are still rough:

- `:evidence-query` is currently a stub in the mission backend. The tool
  exists for phase-gating purposes, but it returns empty results unless
  a richer integration is added.
- `resources/peripherals.edn` publishes the main mission tool set, but the
  code-level mission runtime also uses `:mission-wiring` and
  `:mission-doc-audit`.
- The mission peripheral is operational, but some surrounding docs still
  describe the earlier aspiration rather than the current implemented shape.

## Useful Files

- `src/futon3c/peripheral/mission.clj`
- `src/futon3c/peripheral/mission_backend.clj`
- `src/futon3c/peripheral/mission_shapes.clj`
- `src/futon3c/peripheral/cycle.clj`
- `test/futon3c/peripheral/mission_test.clj`
- `test/futon3c/peripheral/mission_backend_test.clj`
- `src/futon3c/peripheral/proof.clj`
- `src/futon3c/peripheral/mission_control.clj`
- `src/futon3c/portfolio/core.clj`

## Short Version

The Mission Peripheral is now a real, code-backed peripheral in `futon3c`.
It is a mission-specific instantiation of the same generic cycle engine used by
the Proof Peripheral, with persisted mission state, obligation DAGs,
phase-gated tools, save-time snapshots, doc-drift auditing, and gate-review.

It is not portfolio inference. It is the disciplined single-mission work lane
that sits below Mission Control and Portfolio Inference.
