# Mission: Forum Refactor

## Derivation

INSTANTIATE step of the social exotype derivation xenotype.

Prior:
- ARGUMENT.flexiarg R8 (authoritative-transcript), R9 (structured-events)
- social-exotype.edn §S-validate (lines 179-190): coordination outcome validation
- social-exotype.edn O-coordination-evidence: typed proof-path entries
- realtime/authoritative-transcript.flexiarg: single authoritative event log
- realtime/structured-events-only.flexiarg: structured events, not free-text
- futon3/forum/ (3 files, 776 lines): source material with known defects

## Why This Mission Exists

The forum in futon3 was abandoned after 8 commits (4846339 through a346d27) with:
- No tests (S1: verify-before-compose violated)
- No invariant tests (S2: tension-before-code violated)
- ClassCastException on first WS test, no exit recorded (S3: explicit-exit violated)
- M-forum-organization.md created but never evaluated against success criteria

The forum is NOT just chat. Posts are proof steps, threads are proof trees,
claim-types (:goal, :step, :evidence, :conclusion) are the inference structure,
and pattern-applied fields record which patterns were used. This is the
mechanism by which social-timescale coordination becomes task-timescale evidence.

## What This Mission Produces

1. **Forum shapes** — Malli schemas for Thread, Post, PostTree, ForumEvent
2. **Forum service** — thread/post CRUD with proof-tree invariants, tested
3. **S-validate integration** — coordination outcomes validated against forum evidence
4. **Proof-path emission** — forum events as typed O-coordination-evidence entries
5. **Forum event streaming** — subscriber notification (data layer, not transport)

## Scope In

- Forum data model: Thread, Post, PostTree (proof tree structure)
- Forum service: create/read threads and posts with claim-type validation
- Proof-tree invariants: tree structure validity, claim-type ordering
- Event emission: typed forum events for subscriber notification
- MUSN event conversion: post → proof-path-compatible event
- Pattern mining: extract patterns from completed threads
- S-validate wiring: forum evidence feeds coordination outcome validation

## Scope Out

- HTTP API: REST endpoints for forum — subsequent mission after data layer works
- WebSocket transport: Java-WebSocket or http-kit WS — subsequent mission
- Forum bridges: Babashka/TypeScript bridges — separate concern (peripheral)
- Agency dispatch endpoint: POST /forum/thread/:id/dispatch — needs agency HTTP
- Pin/frontpage: UI concerns, add when HTTP layer exists
- Forum client CLI: scripts/forum-client.clj — separate tool

## Source Material (futon3 → futon3c mapping)

| futon3 file | Lines | Maps to | Notes |
|-------------|-------|---------|-------|
| forum/service.clj | 343 | futon3c/forum/service.clj | Core: CRUD, proof-tree, events, patterns |
| forum/http.clj | 295 | Deferred | HTTP layer, not in scope |
| forum/ws.clj | 138 | Deferred | WS transport, not in scope |

The 343-line service.clj contains the data model. The 433 lines of HTTP + WS
are transport — they should be built on top of tested data layer, not alongside.

## Key Design Decision: Data Layer First

futon3 built service + HTTP + WS simultaneously with no tests. The
ClassCastException happened because WS was composed before the data layer
was verified. futon3c reverses this:

1. Forum shapes (Malli contracts)
2. Forum service with invariant tests
3. S-validate integration
4. Then HTTP (later mission)
5. Then WS (later mission)

This follows verify-before-compose: each layer tested before the next is added.

## Parts

### Part I: Forum Shapes (Claude)

**Status:** Ready

:in  — src/futon3c/social/shapes.clj (extend with forum shapes)
       futon3/src/futon3/forum/service.clj (READ-ONLY, source reference)
       futon5/data/missions/social-exotype.edn (READ-ONLY, §S-validate)
:out — src/futon3c/social/shapes.clj (ADD forum shapes, preserve existing)
       test/futon3c/social/shapes_test.clj (ADD forum shape tests)

New shapes:
- `ClaimType`: enum [:goal :step :evidence :conclusion :question]
- `ThreadStatus`: enum [:open :closed]
- `ForumPost`: {:post/id, :post/thread-id, :post/author, :post/timestamp,
   :post/body, :post/claim-type, :post/pattern-applied, :post/in-reply-to, :post/tags}
- `ForumThread`: {:thread/id, :thread/title, :thread/author, :thread/created,
   :thread/updated, :thread/goal, :thread/root-post-id, :thread/post-count,
   :thread/participants, :thread/tags, :thread/status}
- `ForumEvent`: {:event/type, :thread-id, :post ForumPost}
  (used by subscriber notification and proof-path emission)
- `ProofPathEntry`: {:event/type :forum/post, :post/id, :post/thread-id,
   :post/author, :post/claim-type, :post/pattern-applied, :timestamp}
  (MUSN-compatible event for O-coordination-evidence)

### Part II: Forum Service (Codex handoff)

**Status:** Blocked on Part I

:in  — src/futon3c/social/shapes.clj (READ-ONLY)
       test/futon3c/social/test_fixtures.clj (READ-ONLY)
       futon3/src/futon3/forum/service.clj (READ-ONLY, source reference)
:out — src/futon3c/forum/service.clj
       test/futon3c/forum/service_test.clj

Rewrite service.clj with:
- Shape-validated inputs/outputs on all operations
- Proof-tree structure invariants (tree is valid, no orphan posts)
- Claim-type ordering validation (:goal must be root, :conclusion only in reply-to)
- Event emission as typed ForumEvent maps
- MUSN event conversion (post→musn-event) as ProofPathEntry
- Pattern mining (thread-patterns, successful-thread-patterns)
- No persistence to disk yet (in-memory atoms, same as futon3) — persistence
  strategy decided when we know if it's XTDB or EDN files

Function signatures:
```clojure
(defn create-thread! [{:keys [title author body goal tags]}] -> {:ok true :thread :post} | SocialError)
(defn create-post! [{:keys [thread-id author body in-reply-to claim-type pattern-applied tags]}] -> {:ok true :post} | SocialError)
(defn get-thread [thread-id] -> ForumThread | nil)
(defn get-thread-posts [thread-id] -> [ForumPost])
(defn get-post-tree [thread-id] -> PostTree | nil)
(defn post->proof-path-entry [post] -> ProofPathEntry)
```

Criteria:
- [ ] Shape-validated: all inputs/outputs checked against forum shapes
- [ ] R8 (authoritative-transcript): thread state is the authority, no projections
- [ ] R9 (structured-events): posts have mandatory claim-type, events are typed maps
- [ ] R4 (loud failure): no silent nil returns, SocialError on all failures
- [ ] Proof-tree invariants: tree structure valid, no orphans, claim ordering
- [ ] 8+ tests pass (CRUD + tree + invariants + events)
- [ ] No EXPECTED FAIL markers

### Part III: S-validate Integration (Codex handoff)

**Status:** Blocked on Part II + M-agency-refactor Part IV (S-dispatch)

:in  — src/futon3c/social/shapes.clj (READ-ONLY)
       src/futon3c/forum/service.clj (READ-ONLY)
       src/futon3c/social/dispatch.clj (READ-ONLY, from M-agency-refactor)
:out — src/futon3c/social/validate.clj
       test/futon3c/social/validate_test.clj

S-validate component from social-exotype.edn:
- Input: S-dispatch output + I-patterns (config)
- Validates coordination produced a well-formed outcome
- Output: CoordinationOutcome (already in shapes.clj) or SocialError
- R5 (bounded-lifecycle): transient resources have deterministic bounds

Function signature:
```clojure
(defn validate-outcome
  "Validate a coordination outcome against forum evidence and patterns.
   Returns CoordinationOutcome on success, SocialError on failure."
  [dispatch-result patterns forum-evidence]
  ...)
```

Criteria:
- [ ] R5 (bounded-lifecycle): validates coordination completeness
- [ ] Uses forum thread state as evidence for validation
- [ ] Output conforms to CoordinationOutcome shape
- [ ] 5+ tests pass
- [ ] No EXPECTED FAIL markers

### Part IV: Integration (Claude)

**Status:** Blocked on Parts II-III

:in  — All component files from Parts I-III
:out — test/futon3c/forum/integration_test.clj

Criteria:
- [ ] Create thread → post proof steps → validate outcome end-to-end
- [ ] Proof-path entries emitted for all forum operations
- [ ] Pattern mining extracts patterns from closed threads
- [ ] Forum events notify subscribers (in-memory, no transport)
- [ ] Wire into pipeline_test.clj if S-dispatch exists

## Proof-Tree Invariants (New — futon3 had none)

These are the invariants that should have existed before the WS layer:

1. **Tree validity**: Every post's `:in-reply-to` references an existing post in the same thread
2. **Root invariant**: Thread's `:root-post-id` exists and has `:claim-type :goal`
3. **No orphans**: Every post belongs to exactly one thread
4. **Claim ordering**: `:conclusion` can only reply to `:step` or `:evidence`, not to another `:conclusion`
5. **Author tracking**: `:thread/participants` is the union of all post authors
6. **Monotonic timestamps**: Post timestamps within a thread are non-decreasing
7. **Post count consistency**: `:thread/post-count` equals actual post count for that thread

## Relationship to Other Missions

- **M-agency-refactor**: S-dispatch output feeds S-validate input. S-validate is
  Part III of this mission but depends on S-dispatch from M-agency-refactor Part IV.
  Both share shapes.clj as the contract boundary.

- **M-peripheral-model**: Forum bridges are peripherals. A forum-agent peripheral
  operates within the forum data model. The peripheral spec defines the
  structural constraints; the forum service provides the data operations.

- **social-exotype.edn**: S-validate has edges from I-patterns and S-mode output,
  produces O-coordination-evidence. This mission implements the S-validate node
  and the O-coordination-evidence output.

## Exit Conditions

- Forum shapes validate all thread/post/event types
- Forum service passes proof-tree invariant tests (all 7 above)
- Proof-path entries conform to ProofPathEntry shape
- Pattern mining works on closed threads
- All new tests pass, existing tests unaffected
- `clojure -X:test` passes cleanly
