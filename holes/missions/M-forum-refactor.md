# Mission: Evidence Landscape

## Derivation

INSTANTIATE step of the social exotype derivation xenotype.

Prior:
- ARGUMENT.flexiarg R8 (authoritative-transcript), R9 (structured-events)
- social-exotype.edn §S-validate (lines 179-190): coordination outcome validation
- social-exotype.edn O-coordination-evidence: typed proof-path entries
- realtime/authoritative-transcript.flexiarg: single authoritative event log
- realtime/structured-events-only.flexiarg: structured events, not free-text
- **social/evidence-landscape.flexiarg**: evidence landscape as durée
- futon3/forum/ (3 files, 776 lines): source material with known defects

## Why This Mission Exists

The forum in futon3 was abandoned after 8 commits (4846339 through a346d27) with:
- No tests (S1: verify-before-compose violated)
- No invariant tests (S2: tension-before-code violated)
- ClassCastException on first WS test, no exit recorded (S3: explicit-exit violated)
- M-forum-organization.md created but never evaluated against success criteria

The deeper problem: futon3 treated the forum as a service bolted onto agency.
The evidence-landscape.flexiarg identifies a structural reframe: the evidence
landscape is the shared medium through which all three AIF timescales accumulate
experience. It provides durée — continuous lived duration — not just event storage.

The forum is NOT just chat. It is the evolutionary landscape where all evidence
lives: PSRs record selections, PURs record outcomes, PARs record reflections,
gate traversals record validations, coordination records capture real-time events.
Every artifact has a biography. Threads are projections (groupings of entries
sharing a goal), not the primary structure.

## What This Mission Produces

1. **Evidence shapes** — Malli schemas for EvidenceEntry, ArtifactRef, EvidenceQuery
2. **Evidence store** — CRUD for evidence entries with invariants, tested
3. **Thread projection** — proof-tree views on evidence (grouping entries by goal/subject)
4. **S-validate integration** — coordination outcomes validated against accumulated evidence
5. **Evidence emission** — all timescale events as typed evidence entries

## Scope In

- Evidence data model: EvidenceEntry as primary shape, ArtifactRef for universal subject references
- Evidence store: create/read/query evidence entries with claim-type validation
- Thread projection: group evidence entries into proof trees by shared goal
- Proof-tree invariants: tree structure validity, claim-type ordering (on projections)
- Evidence query: by subject, by type, by time range, by pattern linkage
- S-validate wiring: accumulated evidence feeds coordination outcome validation
- Front page query: aggregated recent activity across subjects (what L1 observer reads)

## Scope Out

- HTTP API: REST endpoints — subsequent mission after data layer works
- WebSocket transport: Java-WebSocket or http-kit WS — subsequent mission
- Forum bridges: Babashka/TypeScript bridges — separate concern (peripheral)
- Agency dispatch endpoint: POST /forum/thread/:id/dispatch — needs agency HTTP
- Persistence backend: in-memory atoms for now; XTDB or EDN files decided later
- L1 observer logic: reads from evidence landscape, but scanning/mining is glacial timescale

## Conceptual Model

From evidence-landscape.flexiarg:

**Structural model:**
- Social timescale writes: coordination events, delivery receipts, presence records, mode transitions
- Task timescale writes: gate traversals, task submissions, validation outcomes
- Glacial timescale writes: pattern canonizations, tension records, library evolution events
- L1 observer reads: scans accumulated evidence for recurring tensions and successful patterns
- The loop closes through the medium, not through direct connections between timescales

**Design principles (PlanetMath + Stack Exchange precedent):**
- The primary shape is EvidenceEntry (typed event with subject, provenance, claim-type, pattern linkage)
- Every artifact gets evidence attached (PlanetMath model: every object has a discussion)
- Threads/proof-trees are projections (groupings of entries sharing a goal), not the primary structure
- The "front page" is aggregated recent activity across subjects
- Surface structures (Q&A, proof trees, timelines) are views on the same evidence

## Source Material (futon3 → futon3c mapping)

| futon3 file | Lines | Maps to | Notes |
|-------------|-------|---------|-------|
| forum/service.clj | 343 | futon3c/evidence/store.clj | Reframed: evidence store, not forum service |
| forum/http.clj | 295 | Deferred | HTTP layer, not in scope |
| forum/ws.clj | 138 | Deferred | WS transport, not in scope |

The 343-line service.clj contains the thread/post model. The reframe keeps the
proof-tree logic but makes EvidenceEntry the primary shape, with threads as a
projection layer on top.

## Key Design Decision: Evidence-First, Not Forum-First

futon3 built the forum as a service. futon3c recognizes the evidence landscape
as the connective tissue of the entire AIF loop. The sequence:

1. Evidence shapes (EvidenceEntry as primary, ArtifactRef for universal subject references)
2. Evidence store with invariant tests
3. Thread projection layer (proof-tree views on evidence entries)
4. S-validate integration
5. Then HTTP (later mission)
6. Then WS (later mission)

This follows verify-before-compose AND the durée insight: the evidence store
is the foundation, not the forum surface.

## Parts

### Part I: Evidence Shapes (Claude)

**Status:** Ready

:in  — src/futon3c/social/shapes.clj (extend with evidence shapes)
       library/social/evidence-landscape.flexiarg (READ-ONLY, design reference)
       futon3/src/futon3/forum/service.clj (READ-ONLY, source reference)
       futon5/data/missions/social-exotype.edn (READ-ONLY, §S-validate)
:out — src/futon3c/social/shapes.clj (ADD evidence shapes, preserve existing)
       test/futon3c/social/shapes_test.clj (ADD evidence shape tests)

New shapes:
- `ClaimType`: enum [:goal :step :evidence :conclusion :question :observation :tension]
- `ArtifactRef`: {:ref/type [:pattern :mission :component :gate :session :agent :thread],
   :ref/id :string}
  (universal reference to any artifact in the system)
- `EvidenceEntry`: {:evidence/id, :evidence/subject ArtifactRef, :evidence/type keyword,
   :evidence/claim-type ClaimType, :evidence/author, :evidence/at,
   :evidence/pattern-id (optional), :evidence/session-id (optional),
   :evidence/body, :evidence/in-reply-to (optional), :evidence/tags}
  (primary shape — all other evidence types are subtypes or projections)
- `EvidenceQuery`: {:query/subject (optional), :query/type (optional),
   :query/claim-type (optional), :query/since (optional), :query/limit (optional)}
- `EvidenceType`: enum [:coordination :gate-traversal :pattern-selection :pattern-outcome
   :reflection :forum-post :mode-transition :presence-event]
  (distinguishes evidence from different timescales)

Relationship to existing shapes:
- ForumPost is an EvidenceEntry with :evidence/type :forum-post
- ProofPathEntry is a projection of EvidenceEntry for MUSN compatibility
- PSR/PUR/PAR are EvidenceEntry subtypes (:pattern-selection, :pattern-outcome, :reflection)
- GateTraversal events are EvidenceEntry with :evidence/type :gate-traversal

### Part II: Evidence Store (Codex handoff)

**Status:** Blocked on Part I

:in  — src/futon3c/social/shapes.clj (READ-ONLY)
       test/futon3c/social/test_fixtures.clj (READ-ONLY)
       library/social/evidence-landscape.flexiarg (READ-ONLY, design reference)
       futon3/src/futon3/forum/service.clj (READ-ONLY, source reference)
:out — src/futon3c/evidence/store.clj
       test/futon3c/evidence/store_test.clj

Evidence store with:
- Shape-validated inputs/outputs on all operations
- Append-only evidence entries (evidence accumulates, never deleted)
- Query by subject (ArtifactRef), by type, by time range, by claim-type
- Claim-type validation (structural rules on what can reply to what)
- Event emission as typed maps for subscriber notification
- In-reply-to chain integrity (no orphan replies, referenced entry must exist)
- No persistence to disk yet (in-memory atoms) — persistence strategy decided later

Function signatures:
```clojure
(defn append! [{:keys [subject type claim-type author body pattern-id session-id in-reply-to tags]}]
  ;; -> {:ok true :entry EvidenceEntry} | SocialError
  )

(defn query [evidence-query]
  ;; -> [EvidenceEntry]
  )

(defn get-entry [evidence-id]
  ;; -> EvidenceEntry | nil
  )

(defn get-reply-chain [evidence-id]
  ;; -> [EvidenceEntry] (ordered ancestor chain)
  )

(defn recent-activity [{:keys [limit since]}]
  ;; -> [EvidenceEntry] — the "front page"
  )
```

Criteria:
- [ ] Shape-validated: all inputs/outputs checked against evidence shapes
- [ ] R8 (authoritative-transcript): evidence store is the authority
- [ ] R9 (structured-events): entries have mandatory type + claim-type, events are typed maps
- [ ] R4 (loud failure): no silent nil returns, SocialError on all failures
- [ ] Append-only: evidence entries cannot be modified or deleted
- [ ] Reply-chain integrity: in-reply-to references valid existing entries
- [ ] 8+ tests pass (CRUD + query + invariants + events)
- [ ] No EXPECTED FAIL markers

### Part III: Thread Projection + S-validate (Codex handoff)

**Status:** Blocked on Part II + M-agency-refactor Part IV (S-dispatch)

:in  — src/futon3c/social/shapes.clj (READ-ONLY)
       src/futon3c/evidence/store.clj (READ-ONLY)
       src/futon3c/social/dispatch.clj (READ-ONLY, from M-agency-refactor)
:out — src/futon3c/evidence/threads.clj
       src/futon3c/social/validate.clj
       test/futon3c/evidence/threads_test.clj
       test/futon3c/social/validate_test.clj

Two components, sharing evidence as the foundation:

**Thread projection (threads.clj):**
- Group evidence entries into proof trees by shared subject/goal
- Proof-tree invariants (tree validity, root invariant, claim ordering) operate on projections
- Thread is a view: {:thread/id, :thread/subject ArtifactRef, :thread/goal EvidenceEntry,
  :thread/entries [EvidenceEntry], :thread/participants, :thread/status}
- Pattern mining: extract patterns from completed threads (closed goals with conclusions)

Function signatures:
```clojure
(defn project-thread [store subject-ref]
  ;; -> ThreadProjection | nil — group all entries for a subject into a tree
  )

(defn thread-status [thread-projection]
  ;; -> :open | :closed | :stalled
  )

(defn thread-patterns [thread-projection]
  ;; -> [{:pattern-id :keyword :applied-count :int :success? :boolean}]
  )
```

**S-validate (validate.clj):**
- Input: S-dispatch output + I-patterns (config) + evidence query results
- Validates coordination produced a well-formed outcome
- Output: CoordinationOutcome (already in shapes.clj) or SocialError
- R5 (bounded-lifecycle): transient resources have deterministic bounds

Function signature:
```clojure
(defn validate-outcome
  "Validate a coordination outcome against accumulated evidence and patterns.
   Returns CoordinationOutcome on success, SocialError on failure."
  [dispatch-result patterns evidence-store]
  ...)
```

Criteria:
- [ ] Thread projection groups evidence entries correctly
- [ ] Proof-tree invariants hold on projections (all 7 below)
- [ ] R5 (bounded-lifecycle): validates coordination completeness
- [ ] Uses accumulated evidence (not just thread state) for validation
- [ ] Output conforms to CoordinationOutcome shape
- [ ] 10+ tests pass (5+ threads, 5+ validate)
- [ ] No EXPECTED FAIL markers

### Part IV: Integration (Claude)

**Status:** Blocked on Parts II-III

:in  — All component files from Parts I-III
:out — test/futon3c/evidence/integration_test.clj

Criteria:
- [ ] Append evidence → project thread → post proof steps → validate outcome end-to-end
- [ ] Evidence entries from different timescales coexist (coordination + gate traversal + reflection)
- [ ] Front page query returns recent activity across subjects
- [ ] Pattern mining extracts patterns from threads with conclusions
- [ ] Evidence events notify subscribers (in-memory, no transport)
- [ ] Wire into pipeline_test.clj if S-dispatch exists

## Proof-Tree Invariants (Applied to Thread Projections)

These invariants apply to thread projections, not to evidence entries directly.
A thread projection groups entries sharing a subject into a proof tree.

1. **Tree validity**: Every entry's `:in-reply-to` references an existing entry in the same projection
2. **Root invariant**: Thread's goal entry exists and has `:claim-type :goal`
3. **No orphans**: Every entry in a projection belongs to exactly one thread
4. **Claim ordering**: `:conclusion` can only reply to `:step` or `:evidence`, not to another `:conclusion`
5. **Author tracking**: `:thread/participants` is the union of all entry authors
6. **Monotonic timestamps**: Entry timestamps within a projection are non-decreasing
7. **Entry count consistency**: `:thread/entry-count` equals actual entry count for that projection

## Relationship to Other Missions

- **M-agency-refactor**: S-dispatch output feeds S-validate input. S-validate is
  Part III of this mission but depends on S-dispatch from M-agency-refactor Part IV.
  Both share shapes.clj as the contract boundary. Social pipeline components
  (S-presence, S-authenticate, S-dispatch) write evidence entries to the landscape.

- **M-peripheral-model**: Peripheral hops and mode transitions write evidence entries.
  A forum-agent peripheral operates within the evidence landscape. The peripheral
  spec defines the structural constraints; the evidence store provides the data operations.

- **social-exotype.edn**: S-validate has edges from I-patterns and S-mode output,
  produces O-coordination-evidence. This mission implements the S-validate node
  and the O-coordination-evidence output. The evidence landscape is WHERE
  O-coordination-evidence lives — not a separate store, but the shared medium.

- **futon3b gate pipeline**: Gate traversals write evidence entries. The evidence
  landscape is the bridge between social (futon3c) and task (futon3b) timescales.
  PSRs, PURs, PARs are all evidence entries.

- **Glacial timescale (L1 observer)**: The L1 observer reads from the evidence
  landscape to detect recurring tensions and successful patterns. The front page
  query is what both the L1 observer and humans see.

## Exit Conditions

- Evidence shapes validate all entry/query/artifact-ref types
- Evidence store passes invariant tests (append-only, reply-chain integrity)
- Thread projections pass proof-tree invariant tests (all 7 above)
- Front page query works (recent activity across subjects)
- Pattern mining works on closed threads
- All new tests pass, existing tests unaffected
- `clojure -X:test` passes cleanly
