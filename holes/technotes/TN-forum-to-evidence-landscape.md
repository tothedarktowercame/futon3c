# TN: Forum → Evidence Landscape Port Status

Date: 2026-02-15
Context: Status check on the futon3 Forum feature and its port to the
futon3a/3b/3c rewrite.

## futon3 (original): Forum as a Service

The Forum in futon3 was a collaborative proof tree system for multi-agent
communication. Three files, 776 lines total:

- `forum/service.clj` (343 lines): thread/post model with 5 claim types
  (goal, step, evidence, conclusion, question), proof tree structure via
  `:in-reply-to`, pattern linkage via `:pattern-applied`, EDN persistence
- `forum/http.clj` (295 lines): REST API (create thread, list threads,
  reply, get tree) + WebSocket streaming endpoint
- `forum/ws.clj` (138 lines): WebSocket transport with filtering

Status: 8 commits, no tests, ClassCastException on first WS test. Abandoned.

## futon3c (rewrite): Evidence Landscape

The Forum has been **reframed as the Evidence Landscape** — a more general
shared medium for accumulating evidence across all three AIF timescales
(social, task, glacial). Threads are *projections* of evidence entries,
not the primary structure.

Design grounded in Corneli (2014) Table 24 (PlanetMath 3.0 entity model).

### What's built and tested

**Evidence shapes** (`social/shapes.clj`):
- 10 claim types (original 5 + observation, tension, correction,
  conjecture, plus the original 5)
- EvidenceEntry: subject, type, claim-type, author, timestamp, body,
  pattern-id, session-id, in-reply-to, fork-of, conjecture?, ephemeral?,
  tags
- EvidenceType: coordination, gate-traversal, pattern-selection,
  pattern-outcome, reflection, forum-post, mode-transition,
  presence-event, correction, conjecture
- ArtifactRef: universal references to patterns, missions, components,
  gates, sessions, agents, threads, evidence

**Evidence store** (`evidence/store.clj`, 332 lines):
- Append-only with shape validation
- Reply-chain integrity (in-reply-to must reference existing entry)
- Fork integrity (fork-of must reference existing entry)
- Ephemeral entry support with compaction
- Query filters: by subject, type, claim-type, timestamp range

**Thread projection** (`evidence/threads.clj`, 219 lines):
- Groups evidence entries into proof trees by shared subject/goal
- 7 invariants enforced: tree validity, root invariant, no orphans,
  claim ordering, author tracking, monotonic timestamps, entry count
- Fork navigation (branches recombine)
- Conjecture lifecycle (open → confirmed/refuted)
- Pattern mining from closed threads

**Tests** (all passing):
- `evidence/store_test.clj`: append-only, reply-chain, fork integrity,
  ephemeral handling
- `evidence/threads_test.clj`: 7 proof-tree invariants
- `evidence/integration_test.clj` (480+ lines): end-to-end scenarios
  covering forks, conjectures, pattern mining, ephemeral compaction,
  front-page queries

### What's NOT ported yet

| futon3 component | Status | Notes |
|-----------------|--------|-------|
| `forum/http.clj` (REST API) | Deferred | HTTP endpoints not yet built |
| `forum/ws.clj` (WebSocket) | Deferred | Real-time transport not yet built |
| Persistence backend | Deferred | Currently in-memory atoms; XTDB vs EDN undecided |
| Forum bridges (bb/TS) | Deferred | Separate peripheral concern |
| L1 observer | Deferred | Glacial-timescale evidence scanning |

### Migration mapping

| futon3 | futon3c | Change |
|--------|---------|--------|
| `forum/service.clj` | `evidence/store.clj` + `evidence/threads.clj` | Reframed: threads are projections of evidence |
| 5 claim types | 10 claim types | Added observation, tension, correction, conjecture |
| Posts | EvidenceEntry | Added pattern linkage, session tracking, fork, conjecture, ephemeral |
| Thread-first | Evidence-first | Threads are views, not primary structures |
| MUSN events | Multi-timescale entries | Social, task, glacial all write to same landscape |

## futon3a, futon3b: No forum work

- **futon3a** (pattern search sidecar): no forum code, focused on query
  infrastructure
- **futon3b** (gate pipeline + library evolution): no forum code, but
  consumes evidence through proof-path events and tension tracking

## Bottom line

The hard part — data model, proof-tree invariants, multi-timescale
integration — is done and tested in futon3c. What remains is the
transport/presentation layer (HTTP + WS) and persistence backend.

Mission docs: `holes/missions/M-forum-refactor.md`,
`holes/qa/M-forum-refactor-qa.md`.
