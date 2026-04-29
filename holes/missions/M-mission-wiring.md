# Mission: Mission Wiring

**Date:** 2026-04-29
**Status:** IDENTIFY
**Owner:** Joe + Codex/Claude
**Cross-ref:** M-mission-control, M-war-machine, M-mission-peripheral,
M-live-geometric-stack, M-hyperreal-dictionary-planning
**Repos:** futon3c, futon3, futon4, futon1a, futon0

## Motivation

Mission documents now exist as real cross-stack control objects, but the
stack still treats them inconsistently.

Some surfaces are already correct on a **pull** basis:

- Mission Control can scan `holes/missions/M-*.md` across repos
- Arxana mission views can read that inventory
- the live AIF stack view can overlay mission status for missions that are
  already named in the cached stack model

But those are observational reads. They do not create a reliable **push**
mechanic from "mission file was written or updated" to "the rest of the stack
knows what changed."

The consequence is drift:

- saving a mission doc does not necessarily propagate into futon1a
- evidence snapshots for mission-doc changes are not guaranteed
- the War Machine can miss active work because the AIF model is not kept
  current against mission inventory
- watcher behavior is split between old whole-repo polling and newer
  per-file ingest
- lifecycle events like rename/delete remain ambiguous

This mission is about the wiring discipline that makes mission files first-class
live objects rather than prose that some tools happen to rescan.

## The Core Claim

Mission wiring should satisfy this invariant:

> No active mission may exist only as a file on disk or only as a pull-time
> inventory artifact.

For any live mission, the stack must be able to answer:

1. Was the mission file saved, changed, renamed, or deleted?
2. Was that change pushed into Mission Control as versioned evidence?
3. Was it projected into futon1a/substrate-2 as a mission-doc object?
4. Is it mapped into the AIF strategic model, or explicitly visible as
   unmapped residue?
5. Which operator surfaces should now show it?

The point is not that every mission must be promoted into the strategic spine.
The point is that every mission must be **accounted for**. Strategic mapping is
allowed to be selective; invisibility is not.

## Current State (Session Snapshot)

This session already established a partial push path:

- `multi_watcher.clj` now notices mission markdown saves on a per-file basis
- `ingest_one_file.clj` can push a single mission file to futon3c via
  `POST /api/alpha/mc/sync-mission`
- `mission_control_backend.clj` can parse one mission path and emit a
  versioned `:mission :sync :snapshot` evidence entry keyed by content hash
- the same ingest path can materialize a `code/v05/mission-doc` vertex in
  futon1a

This is a real improvement, but it does not yet settle the whole wiring
question.

What remains unresolved:

- old `watcher_daemon.clj` and new `multi_watcher.clj` are still split
- mission rename/delete semantics are not yet first-class
- the War Machine/AIF stack view still depends on the cached stack model for
  strategic visibility
- there is not yet a principled mapping layer from mission inventory into AIF
- propagation guarantees and failure reporting are not yet stated as a stack
  law

## What This Mission Produces

1. **Mission wiring contract**  
   A clear end-to-end contract from filesystem event to mission inventory,
   evidence, substrate projection, and strategic surfaces.

2. **Push-first lifecycle for mission docs**  
   Create/update/rename/delete semantics for `holes/missions/M-*.md`, not just
   opportunistic save handling.

3. **Watcher unification plan**  
   One coherent watcher path for mission-doc propagation, rather than a split
   between old whole-repo ingest and newer per-file ingest.

4. **AIF mapping discipline**  
   A rule that every active mission is either:
   - mapped into the AIF model, or
   - surfaced explicitly as `unmapped-mission` strategic residue

5. **War Machine integration**  
   Live strategic surfaces should consume the AIF-aware mission state rather
   than silently omitting newly active missions.

6. **Failure visibility**  
   If mission push fails at any stage, that failure should be inspectable
   rather than hidden behind later pull-based rescans.

## Scope In

- Mission-doc push propagation from filesystem save/update into Mission Control
- Versioned evidence for mission-doc state snapshots
- Projection of mission docs into futon1a/substrate-2
- Watcher architecture for mission-doc events
- Mission rename/delete semantics
- Mapping from mission inventory into the live AIF/War Machine view
- Surface-level accounting for unmapped active missions
- Pattern/generalization questions insofar as mission docs are the first
  worked example of broader stack wiring

## Scope Out

- Redesigning mission markdown syntax itself
- Collapsing all mission governance into the AIF spine
- Replacing Mission Control with War Machine, or vice versa
- Generic arbitrary-markdown ingestion
- Solving every other document type in this mission
- Autonomous mission creation or auto-prioritization policy

## Design Principles

### 1. Push beats pull for liveness

Pull-based rescans are still useful as audit and recovery mechanisms, but they
are not sufficient as the primary propagation path.

### 2. Evidence must be versioned by content

Mission save events are not just "latest status." They are a sequence of
snapshots. Duplicate content can dedupe; changed content must not overwrite
history silently.

### 3. Strategic visibility is selective, accounting is total

The AIF model should not be bloated by treating every mission as a spine node.
But every active mission must either map into strategy or appear as an explicit
gap in strategic mapping.

### 4. Delete and rename are not special cases

If mission docs are first-class objects, lifecycle semantics must include
removal and movement, not only create/update.

### 5. No hidden fallback success

If push propagation fails but later pull-based inventory still sees the file,
that does not count as success. The failure itself must remain visible.

## Work Packages

### WP1. Formalize the mission wiring contract

Write the contract in operational terms:

- source event
- normalized mission identity
- emitted evidence shape
- substrate projection shape
- strategic mapping state
- operator-visible surfaces
- failure states

The output is a concrete state-transition model, not prose alone.

### WP2. Unify watcher responsibility

Decide whether:

- `multi_watcher.clj` becomes the canonical mission-doc watcher, with the old
  watcher retired, or
- the old watcher is upgraded to delegate to the same per-file mission path

The invariant is one mission-doc event pipeline, not two half-overlapping ones.

### WP3. Add rename/delete semantics

Mission lifecycle support must cover:

- create
- update
- rename/move
- delete

This includes evidence shape, substrate updates, and Mission Control state.

### WP4. AIF mapping layer

Extend the live stack generator so it can distinguish:

- inventory missions
- mapped missions
- unmapped active missions

and expose that structure through the live stack API.

### WP5. War Machine / operator surface uptake

Update the consuming surfaces so the new AIF-aware mission accounting becomes
visible to operators.

Minimum expectation:

- a visible count/list of unmapped active missions
- no silent omission of new active missions

### WP6. Pattern generalization

Mission docs are the first worked example because they touch many surfaces. The
same wiring question may later apply to:

- patterns / `flexiargs`
- sorrys
- devmaps
- other first-class design documents

But that generalization should happen only after the mission-doc contract is
stable and tested.

## Derivation Path

1. **IDENTIFY** — this document
2. **MAP** — inventory the real event paths, pull paths, consumers, and
   failure cases
3. **DERIVE** — define the wiring contract and architecture choices
4. **ARGUE** — justify the watcher/AIF/surface split and the invariants
5. **VERIFY** — test create/update/rename/delete and strategic visibility
6. **INSTANTIATE** — land the remaining code and surface changes

## Source Material

| Source | Role |
|--------|------|
| `futon3c/holes/missions/M-mission-control.md` | portfolio and mission inventory semantics |
| `futon3c/holes/missions/M-war-machine.md` | strategic surface and AIF-facing synthesis |
| `futon3c/holes/missions/M-mission-peripheral.md` | mission object semantics and evidence discipline |
| `futon3/holes/labs/M-live-geometric-stack/` | push-based substrate and watcher discipline |
| `futon3/scripts/multi_watcher.clj` | current per-file push watcher |
| `futon3/scripts/ingest_one_file.clj` | mission-doc push bridge into futon3c + futon1a |
| `futon3c/src/futon3c/peripheral/mission_control_backend.clj` | mission parsing, inventory, sync evidence |
| `futon3c/src/futon3c/transport/http.clj` | mission sync transport |
| `futon3c/src/futon3c/aif/stack_generator.clj` | live AIF overlay and future unmapped-mission accounting |

## Concrete Questions This Mission Should Answer

1. What is the canonical mission identity across save, rename, and delete?
2. What event log should exist when a mission is renamed or removed?
3. Should Mission Control retain tombstones for deleted missions?
4. Which watcher path is canonical?
5. What does the live stack API owe the War Machine about active but unmapped
   missions?
6. How should Arxana and other mission surfaces distinguish inventory presence
   from strategic mapping?
7. Which parts of this contract are mission-specific, and which should later
   become a generic document-wiring pattern?

## Completion Criteria

This mission is complete when all of the following are true:

1. Saving a mission file causes an inspectable push event, not just eventual
   pull visibility.
2. Rename/delete semantics are implemented and tested.
3. There is one coherent watcher path for mission-doc wiring.
4. Mission docs have versioned evidence snapshots and substrate projection.
5. The live AIF stack view exposes mapped vs unmapped active missions.
6. War Machine / operator-facing mission surfaces no longer silently miss
   active missions that are present in inventory.
7. The resulting contract is explicit enough to reuse as the model for other
   first-class document types later.
