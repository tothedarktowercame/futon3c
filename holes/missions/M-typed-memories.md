# M-typed-memories — memories as typed dialogue acts over the futon1b hypergraph

Date: 2026-07-22
Status: **DERIVE** (chartered at IDENTIFY and MAP completed 2026-07-22 —
the Claude pipeline chased into 9 hypergraph stages, §MAP; DERIVE opened
same day as a **prototype ladder** at Joe's direction: MVP + upgrade path,
not a monolithic build — see §Prototype ladder). **P0 LANDED + ACCEPTED
2026-07-22 evening** (zai-3 a94A06 session, 4 typed memories through the
tool end-to-end). One position staged for ARGUE (§Staged for ARGUE).
**Retrieval strategy added 2026-07-23** — pattern-conditioned recall via
hyperedge `:end` query (already exists in futon1b), wired into
`psr_search`/`psr_select`; see §Retrieval gap and strategy. Sequence
ahead: DERIVE (P1 retrieval wiring) → ARGUE → VERIFY → INSTANTIATE.
**Shared-regime Phases 2 and 3 ACCEPTED live 2026-07-23** — after the Phase 1
conjunctive bounded
`end + type`, compact cross-domain projection, explicit domains, and common
use-receipt seam, fresh `zai-6` completed pattern-conditioned
search → select → full-body recall → cited use → separately witnessed Lean
outcome. Phase 3 then added bitemporal challenge/supersession/retraction,
correction and retrieval-to-use latency, bounded repeated trials, lexical
memory-supported candidate construction, and idempotent PUR replay. Three
fresh Lean trials closed with independently witnessed outcomes; see
`M-shared-memory-control-build-test` for the audit table. This closes this
mission's P1 PSR wiring and lifecycle rung; shared-regime Phase 4 is the
dark-only WM consumer.

Cross-reference:
- `holes/missions/M-shared-memory-control-build-test.md` — authoritative
  cross-domain promotion gates from shared-store contract through Zaif
  mathematics trials, dark WM cascades, strategic ablations, and bounded live
  use. Phases 1–3 are accepted live; Phase 4 is next.
- `holes/missions/M-typed-bells.md` — the **prototype**. Typed the coordination
  channel (bells) with IATC performatives so ArSE populates by construction.
  This mission is the same move one layer in: type the *epistemic* channel
  (what an agent learns) so the memory substrate populates by construction.
- `holes/tickets/T-typed-bell-arse-write-async.md` — WATCH item on the typed-bell
  write path (synchronous ArSE write on a hot request thread). The memory write
  path is in the same hazard class; whatever discipline resolves one should
  resolve the other.
- `futon1b/zai_memory_1b.clj` — the proven XTQL read seam (D-11.i) +
  FTS5 text search (D1). The read side of this mission builds on it.
- `futon1b/futon1b_graph.clj` — entities/relations/hyperedges over XTDB 2
  (`:hx/id :hx/type :hx/endpoints :hx/props`), gate stack + rescue ladder
  (`put-verified!`), retraction (`:hyperedges` ∈ `retractable-tables`, L142).
- `futon1b/API-CONTRACT.md` — including the **bounded-page extension dated
  2026-07-22** (keyset cursors, read-admission permit, 503
  `:expensive-read-busy`) — the hardening work in flight; see "Related ongoing
  work" below.
- **`futon1b/TN-futon1b-memory-incident.md`** — the 2026-07-22 :7073
  memory-pressure brown-out (cgroup `memory.high` throttle purgatory;
  predecessor: the 07-13 OOM-zombie) AND the Codex hardening pass deployed
  the same day (14:04–14:11 BST) in response: cursor pages, four-permit
  pgwire budget, two-permit corpus-read admission, `:store-timeout` /
  `:store-unreachable` taxonomy (no longer misreported as `:shape`), the
  Emacs evidence outbox with stable ids + replay, `MemoryHigh` 7G→10G,
  vitality timer. **Not closed** — week-long steady-state acceptance
  observations outstanding (TN §Still open). Any memory issue hit during
  this mission should be checked against the TN's diagnostic signature
  before being diagnosed fresh.
- `holes/notes/retrieval-strategy-pattern-conditioned-recall.md` —
  **Retrieval strategy (2026-07-23)**: pattern-conditioned recall via the
  existing futon1b `?end=` hyperedge query. Five rungs (pattern endpoints
  on memories → enrich `psr_search` → `psr_select` as retrieval trigger →
  record memory use in PUR → boot projection). Key finding: the query
  primitive already exists; the gap is wiring, not infrastructure. P1
  acceptance test: a fresh Lean problem surfaces a pattern via
  `psr_search`; selecting it returns one of the a94A06 memory IDs; the
  agent applies it and cites that ID.
- `futon2/holes/missions/M-wm-strategic-mission-selection.md` — the same
  substrate extended to strategic mission selection. The WM and Zaif runners
  should share the memory contract, store, endpoint retrieval, provenance,
  retraction, and use/outcome receipts; domain endpoints and witnesses differ.
  Mathematics runs become a cheap test tier for the common mechanism, not
  evidence that a WM-specific policy or outcome model is correct.
- Prior art: **Corneli, Martin, Murray-Rust & Pease 2017** (IATC) — the
  performative vocabulary (`assert challenge agree define retract suggest
  query answer`) already carried through DERIVE/ARGUE/VERIFY once by
  M-typed-bells. Memories are its third instantiation (bells = agent↔agent,
  ArSE = Q&A threads, memories = the agent's dialogue with its past self).

## HEAD (Joe's framing, 2026-07-22)

**An agent cannot deliberately write a memory — and the fix should be
hypergraph-native, typed, and bitemporal.**

1. **The Zai XTQL memory model isn't wired up.** The read side is rich (six
   memory-family tools); the write side does not exist. When a Zai agent
   realizes mid-proof that a strategy fails and another works, the realization
   is captured only as `:turn-round` self-talk — raw material, not memory.
2. **Plan: use XTQL to make memories hypergraph-native.** Evidence and the
   hypergraph live in the same XTDB 2 store; a memory should be a typed
   hyperedge whose endpoints reach the evidence it distills, the patterns it
   is about, the subject artifacts, the session, the mission — so retrieval is
   one XTQL pipeline, not app-level joins.
3. **ArSE / M-typed-bells was already a prototype of this kind of thing.**
   Same gap-shape ("the raw stream is captured; the act is not"), same answer
   (recover illocutionary force as first-class typed, ref-carrying records;
   populate by construction, not by later mining), same spec (IATC 2017).
4. **Bitemporality as a way to look at learning-over-time.** XTDB 2's
   valid-time/system-time split makes belief-state-as-of-T, belief diffs
   between two moments ("what was learned in this interval"), and the
   correction lag (when a belief stopped being true vs when the agent noticed
   — the prediction-error latency of the learning loop) *queryable objects*.

## One substrate, several domains (2026-07-23)

The memory system is not specific to theorem proving. Zaif runners and the War
Machine should write to and retrieve from the same hypergraph substrate using
the same typed, bitemporal, provenance-bearing contract. A mathematics memory
and a WM memory differ through explicit `domain`, subject, mission/session,
pattern, evidence, and witness endpoints—not through separate storage theories.
Shared storage therefore permits cross-domain retrieval proposals while keeping
the evidential boundary inspectable.

Zaif runners can be understood as the nomadic workers of this architecture:
they move among problem domains and leave typed, revisable traces rather than
requiring a new memory stack in each territory. The immediate experimental
benefit is a cost ladder. Frequent mathematics runs can exercise memory
realization, write durability, pattern-conditioned recall, citation/use
receipts, cascade composition, correction, and supersession before the same
mechanisms are exposed to heavier WM clicks. What transfers is evidence about
the mechanism. Claims about WM mission value, transitions, safety, or
preferences still require independent WM-domain observations and may not
self-certify from mathematics success.

## The gap (verified 2026-07-22)

Tool inventory in `src/futon3c/agents/zai_api.clj`:

- **Six read-only memory tools** (`memory_search`, `tool_history`,
  `evidence_graph`, `pattern_memory`, `recent_coordination`,
  `mission_context`) — zai_api.clj:179-220, grouped as
  `memory-family-tool-names` (L222) for the M-custom-harness §8.4 ablation
  conditions.
- **No `memory_record` / `memory_write` tool.** The only agent-initiated
  writes are the pattern-discipline trio `psr_select` / `pur_update` /
  `par_punctuate` (L155-178), shape-constrained to pattern
  selection/outcome. Everything else is automatic: `persist-turn-start!`
  (L673), `persist-round!` (L684), `emit-bug-records!` session-end sweep
  (L702).
- The asymmetry: `memory_search` already filters by `type` / `claim_type` /
  `author` / `tags` / `since` / subject ref — the store is queryable by
  exactly the dimensions a deliberate memory would want, but nothing
  agent-driven writes entries typed for that purpose.

## Live specimen: zai-1's APM run on a94J06 (the "naive solution" capture)

zai-1 produced a fresh APM solution as this mission was chartered:
problem **a94J06** (Putnam 1994 A6, unit-circle contour integral with ζ̄ —
"looks like Cauchy but isn't"), dispatched by zai-2 through the Agency bell
path (job `invoke-1784725075981-12-a2d75b10`, manual one-sample bypass of
the cron quota gate per Joe) precisely so the self-talk would land in the
evidence store. **Completed same day: zero sorries, zero errors, three
theorems, apm-lean commit `6bbe1d7`** — so the specimen is a *closed* proof
episode with its full (gappy — see below) evidence trail. Full session narrative + the
staged motivating examples: `holes/technotes/TN-zai2-session-notes-2026-07-22.md`.
A bounded query (`GET 127.0.0.1:7073/api/alpha/evidence?author=zai-1&limit=3`)
shows both layers of today's capture:

- **Structured decision records exist**: `:zaif :arm-choice` entries
  (`:evidence/type :coordination`, `:evidence/claim-type :step`) with full
  G-term breakdowns — e.g. `e-9ef2ddba-fc8c-499e-80aa-96355c6ed9af`
  ("zaif v0 chose retrieve: retrieve 0.772, act 0.000, ask 0.150, yield
  0.000", round 28). The *decision* layer is already typed and queryable.
- **Epistemic realizations are prose blobs**: the adjacent `:turn-round`
  entry (same turn `zai-turn-ffce17d3`) contains: "✘ Several issues to fix …
  `mul_comm` on `(ζ-z)⁻¹ * ζ⁻¹` gives `ζ⁻¹ * (ζ-z)⁻¹` ≠ `(ζ*(ζ-z))⁻¹`.
  Need `mul_inv_rev` instead." That is a completed learning act — strategy
  refuted, replacement identified, reason given — stored as untyped text.

The contrast IS the mission: the harness types what the *scheduler* did
(arm choices) but not what the *agent came to believe*. The upgrade path
from the second layer to typed, queryable memories — "who types it, when,
into what shape" — is a key framing issue (below).

## Related ongoing work (dependencies to sort and check)

1. **futon1b hardening (Codex, deployed 2026-07-22; documented in
   `TN-futon1b-memory-incident.md`).** The TN records both the incident that
   forced it (the 13:20–13:37 brown-out — during which **at least one zai
   transcript entry was lost and a live operator turn aborted**) and the
   full hardening pass (cursor pages, permit budgets, failure taxonomy,
   Emacs outbox, capacity + vitality monitoring) with its validation runs.
   This helps: memory queries will live under a read-admission regime that
   has already killed the unbounded-scan class of failure (the 8,882-doc
   /10MB recall of 2026-07-11, the ~19s limit=1000 scans). **We must review
   that work before building on it** (author ≠ reviewer — the TN's
   validation section is the starting checklist, and its §Still open items
   are unclosed by design), and check it specifically against the
   write-async hazard class (`T-typed-bell-arse-write-async`): the memory
   write path must not put a heavy synchronous write on a hot coordination
   thread. The lost-transcript fact also sharpens the mission's stakes:
   until realizations are deliberately persisted (and durably delivered),
   brown-outs silently delete learning events.
2. **Topology to verify at MAP.** Observed today: TWO JVMs — the futon3c
   serving JVM (pid 532781: :7070, :6768) and a separate futon1b store JVM
   (pid 653094: **:7073**). :7074 (the `futon1b_backend.clj` `default-url`)
   is not listening. Meanwhile `zai_memory_1b.clj`'s header says it "can
   never be loaded in-process" in the futon3c JVM (futon1a XTDB 1 coordinate
   clash) — but futon1a was retired in the I-0 unification (2026-07-14), so
   that constraint has likely dissolved. MAP must pin down: where does the
   XTQL memory seam actually run (in-process vs bridged), and which port is
   canonical. Do not design against the stale note or the stale default.
3. **futon1bi as the text-index substrate (exists; nearly working).** Joe's
   working assumption (2026-07-22): the futon1bi text-indexing features are
   needed to upgrade the wrong-shape finding into a learning event — and
   futon1bi already exists and may not need much improvement to actually
   work. Assessment against the code: futon1bi (`futon1bi/src/futon1bi/
   text_index.clj`, 336 lines + 119-line oracle test) is a complete
   spec-driven extraction of the live D1 sidecar — candidate pre-filter +
   store-truth re-check, rebuildable derived index, RS-3 staleness bound,
   `search` complete with `:index-as-of` stamp. Deferred items are the HTTP
   wrapper / soak / tombstones — and the HTTP wrapper becomes moot if the
   memory seam runs in-process (dependency 2 above). Role split by
   distillation path: **write-at-realization does not need it to create
   memories** (and retrieval-by-text already works — the live sidecar
   pr-strs `:turn-round` bodies into the index, futon1b_text.clj:70-76);
   **retroactive mining is where it is the instrument** (a second index
   spec extracting `:text` from turn-round bodies, rather than tokenizing
   pr-str'd EDN). Its oracle harness doubles as a VERIFY tool (retrieval
   checked against exhaustive scan, not eyeballs). Known knob for MAP:
   `unicode61` with no tokenchars splits `mul_inv_rev` into three tokens —
   fine for phrase queries, but `tokenchars` for identifier-heavy corpora
   is a one-line spec fix worth making deliberately.
4. **zai-1's APM output as evaluation corpus.** When the run completes, the
   captured evidence is the "initial naive solution" against which the
   upgrade is judged: for each `:turn-round` realization, what typed memory
   *should* have been written, and would `memory_search`/XTQL have retrieved
   it at the moment a later turn needed it?

## Key issues to frame (for MAP/DERIVE)

- **Who types, and when.** Two candidate paths, not mutually exclusive:
  (a) a `memory_record` tool the agent calls at the moment of realization
  (populate by construction — the M-typed-bells answer); (b) retroactive
  distillation of `:turn-round` transcripts into typed memories (mining).
  M-typed-bells' lesson favors (a); the zai-1 corpus lets us measure what
  (a) would have to capture.
- **Shape: prose/structure split.** Body of a memory as an append-only
  `:evidence` entry (gets FTS5 indexing, authorship, provenance — evidence
  `-delete!` is a no-op); structure as a retractable hyperedge
  (`:hx/type` in the `:memory/*` family, endpoints = evidence + pattern +
  subject + session + mission, roles in `:hx/props` since endpoints are a
  flat vector). Immutable provenance under revisable belief; XTDB 2
  bitemporality then gives belief-as-of-T for free.
- **Role vocabulary = IATC performatives.** `assert` (finding), `retract`
  (supersession), `challenge` (contradiction), `agree` (confirmation),
  `suggest`, `define`. Not a new type system — the third instantiation of
  the one M-typed-bells verified. Open sub-question: supersession as
  retract-and-replace vs a second hyperedge referencing the first (the
  bitemporal learning queries need whichever preserves the correction event).
- **Typing is (act × kind), two small vocabularies.** The comparison
  survey (TN Part 3) shows memory content-kind (feedback/reference/
  project/user, per the working Claude file-memory system) is orthogonal
  to the IATC act: a memory is e.g. assert × feedback. And the
  math-vs-programming domain contrast moves three design points:
  **validity horizons** (math memories are ~timeless; ops memories like
  "store is on :7073" are valid-until-changed — bitemporality is
  load-bearing for ops memories specifically), **subjects** (library/
  domain entities vs infrastructure entities — the endpoint vocabulary
  must span both), and **sharing topology** (ops memories pay off across
  agents in one environment — the `run_readonly` rejection hit both zai
  agents in one day; math memories pay off across problems). The Claude
  system's transfer table (one file/fact → entry+hyperedge; MEMORY.md
  index → boot-time bounded projection; `[[dangling links]]` → endpoints
  to named-but-unwritten nodes; Why/How-to-apply → structured body slots
  where "how to apply" is a retrieval predicate) is in TN Part 3 — with
  one strict improvement in transfer: file overwrite destroys revision
  history, XTDB2 supersession keeps belief-as-of-T.
- **Type registry.** `:hx/type` is deliberately NOT auto-registered
  (futon1b_graph.clj header); `:memory/*` types with endpoint-type checking
  (`hyperedge-type-inhabited?`, L451) require an explicit registration step.
- **Write-path discipline.** Same class as
  `T-typed-bell-arse-write-async`: agent-facing memory writes must be
  either cheap+bounded or queued off the request thread. `put-verified!`
  (gate stack + rescue ladder + read-back) is the write primitive; its cost
  on the dispatch path needs measuring, not assuming. **Durability under
  store degradation is part of this issue**: the 07-22 brown-out lost a zai
  transcript entry outright (TN §Incident summary). The hardening pass's
  Emacs evidence outbox (stable evidence id fixed before delivery, disk
  spool, replay on transport/5xx, `failed/` for operator inspection) is
  prior art for exactly this — a `memory_record` whose id is fixed at
  creation can be referenced by later turns even while delivery is pending.
  A memory that can be silently dropped during the store's bad hour is not
  a memory.
- **Query discipline.** Hyperedge type-queries are heavy — always bounded
  (`&limit`; the bounded-window cache in `hyperedges-query` exists because
  of this). Memory retrieval rides the new keyset-cursor/admission regime.
- **Ablation integrity.** M-custom-harness §8.4 conditions strip the
  memory-family tools; a `memory_record` tool must join
  `memory-family-tool-names` so `:files`/`:none` conditions remove the
  write along with the reads, or the comparison leaks.
- **Learning-over-time as VERIFY material.** Candidate VERIFY criteria
  (to be fixed at ARGUE/VERIFY, noted here as direction): (1) an agent
  writes a memory and a later session retrieves it by endpoint AND by text;
  (2) a superseded memory answers correctly both "what do we believe now"
  and "what did we believe as-of T"; (3) on the zai-1 APM corpus, at least
  one real `:turn-round` realization round-trips into a typed memory that
  XTQL retrieves in a context where the naive capture would not have
  surfaced it.

## Staged for MAP (2026-07-22)

Methodological note (Joe): MAP surveys what exists in the landscape — and
what exists in a single session *is* now part of the landscape, because
sessions land in the evidence store. The zai-2 session that dispatched the
a94J06 specimen (read over zai-1's transcript live, buffer
`*zai-repl:zai-2*`) has been distilled into
`holes/technotes/TN-zai2-session-notes-2026-07-22.md`; MAP should open with
its four motivating examples:

1. **Round-17 strategy narration** — capability-relevant reasoning
   (strategy, Mathlib API mapping, the `integral_congr` subtlety) IS
   captured in free text.
2. **The mul_comm→mul_inv_rev self-correction** — a completed learning act
   as untyped prose (the canonical memory_record candidate).
3. **Forward-narration unevenness** (zai-2's sharpest finding): agents
   narrate "now I'll try X", not "X failed because Y, so Z" — so transcript
   mining inherits lucky, uneven retrospection; write-at-realization (or
   prompts that ask for retrospection) is what makes learning events
   reliable.
4. **Session contamination as a provenance argument**: the a94J06 bell
   landed in a persisted session (prior turn: zai-1 building the
   M-zaif-harness dual-constant recorder — the very instrument that then
   recorded its own arm-choices) — a memory formed in a mixed session
   needs typed subject/session endpoints to say which work it belongs to.

The post-completion survey of `*zai-repl:zai-1*` (TN Part 2) added four
more, at least two of which move the design:

5. **The existing deliberate write path failed in practice**: `psr_select`
   and `pur_update` both errored with "Invalid evidence shape" during real
   work; the agent retried, then abandoned the PUR — the outcome record was
   silently lost. `memory_record` must treat shape-fit feedback and retry
   ergonomics as first-class.
6. **The specimen corpus itself has holes**: four turns died mid-proof to
   "ZAI transcript persistence was rejected" (+1 EOF, +1 stalled compile;
   ~6 manual "continue" interventions by Joe). The corpus we intend to
   mine is already incomplete — durability is not hypothetical.
7. **The route story is A → B → A′**: partial fractions abandoned, direct
   evaluation tried and dead-ended, partial fractions re-adopted with the
   `‖z‖ ≠ 1` hypothesis fix. Only a typed retract/re-assert chain makes
   A′ ≠ A legible to a later reader; a transcript miner sees oscillation.
8. **Within-session recurrence**: the `rw`-direction confusion recurred
   three times in one session — memory pays off before the session ends,
   not only across sessions. (TN Part 2 also catalogs 8 concrete
   memory candidates spanning tactic behavior, API absence, notation
   matching, and proof architecture — the `:memory/*` family should not
   assume one granularity.)

Sibling-mission wiring (from the same session): zai-2's three-layer framing
— capture (exists) / **deliberate memory formation (missing — this
mission)** / distillation into capability (M-apm-capability-ratchet) — with
M-zaif-harness as the learning consumer. Joe's chain: a recorded memory
"would become a capability later." The memory-model question that seeded
this charter was itself aborted once by the 13:2x brown-out
(TN-futon1b-memory-incident §Incident summary) before being answered — the
mission's stakes, enacted on its own origin.

## MAP — the Claude pipeline, chased and recreated (opened 2026-07-22)

Method (Joe): the Claude harness file-memory system is a working
write-at-realization pipeline in production. Chase it stage by stage;
recreate each stage in hypergraph format for Zai. Each stage below gives
(a) how Claude does it, (b) the hypergraph recreation, (c) what exists in
code today. Grounding: everything cited as existing was verified in-session
2026-07-22.

### Stage 1 — Boot recall (the index in context)

(a) `MEMORY.md` — one line per memory (`- [Title](file.md) — hook`) — is
loaded into context every session; content never lives in the index; when
the index outgrows its budget, verbose hooks are compacted to an archive
file (observed: `archive_memory_index_2026-07-03.md`).
(b) **The index is a query, not an artifact** (registry-as-projection): a
boot-time bounded XTQL projection over the `:memory/*` family — columns
(name, hook/description, kind, act, at) — delivered in the zai boot packet
beside `mission_context`. Compaction is free (it's `limit` + ordering);
"archive" is a no-op because the store is the archive and the projection is
the budget.
(c) Exists: `mission_context` as the model for boot-time composed
orientation; keyset/bounded-window discipline (hardening pass). Missing:
the memory projection itself.

### Stage 2 — Realization (when to write)

(a) Claude writes at the moment of realization, guided by trigger classes
(user feedback, confirmed approach, project state, external pointer).
(b) `memory_record` available every turn; plus a nudge wired where the
realization already *surfaces*: marks.clj already parses `✘`/`💡` from
narration into `:self-correction`/`:self-idea` tags — the realization
detector partially exists, and a self-correction mark is precisely a
memory-candidate event. (Whether the nudge is prompt-contract text or a
tool-result hint is a DERIVE choice; the zai-1 corpus says unprompted
retrospection is uneven — Part 1 Example 3.)
(c) Exists: marks recognizer; prompt-contract precedent (the PSR/PUR
instruction block in the zai system prompt). Missing: the tool.

### Stage 3 — Curation gate (before writing)

(a) Claude checks for an existing memory covering the fact → updates
rather than duplicates; deletes wrong memories; refuses derivable facts;
converts relative dates to absolute.
(b) Build the gate into the tool round-trip: `memory_record` runs a
near-duplicate probe (subject-endpoint match + FTS text) and returns
candidates with a confirm/supersede/new choice when matches exist —
curation as an interactive contract, not agent virtue. Derivable-fact
discipline becomes: store the *link* (endpoint to the `:turn-round`
distilled, `in-reply-to` to prior memory), never a copy. Dates: server
stamps `:evidence/at`; author/session stamped server-side (charter
principle).
(c) Exists: `memory_search` filters + D1 FTS (and futon1bi as the clean
substrate). Missing: the probe wired into a write flow.

### Stage 4 — Write (the shape)

(a) One file = one fact; frontmatter name/description/type; body with
`**Why:**` / `**How to apply:**` for feedback; `[[links]]` liberally,
dangling allowed.
(b) One memory = **one evidence entry + one hyperedge**:
- Evidence entry (append-only): body = the fact, with structured slots
  `{:why ... :how-to-apply ...}` (PAR precedent: maps not strings);
  `:evidence/type :memory`, claim-type = the IATC act.
- Hyperedge `:hx/type :memory/<act>` (`:memory/assert`, `:memory/retract`,
  `:memory/challenge`, …), endpoints = [the entry, subject(s), the
  distilled `:turn-round`(s), session, mission], roles + kind
  (feedback/reference/project/user) + name-slug + hook in `:hx/props`.
- Dangling `[[link]]` → an endpoint naming a not-yet-existing node: legal
  (XTDB doesn't enforce referential integrity) and *useful* — a standing
  query for future memory formation.
(c) Exists: `append!` (store.clj:112), `put-verified!` (gate stack +
read-back), retractable `:hyperedges`. Missing: `:memory/*` types
registered (explicit step — `register-types!` exists; `:hx/type` is
deliberately not auto-registered) and the endpoint-role vocabulary fixed.

### Stage 5 — Supersession & repair

(a) Claude edits the file in place (destroying revision history) and
deletes refuted memories.
(b) Supersede = new version at the same identity (or `:memory/retract`
edge referencing the old — DERIVE decides which; the bitemporal learning
queries need whichever preserves the correction event). Delete-wrong =
retraction with valid-time end. **Strict improvement in transfer**:
belief-as-of-T survives; the correction lag (valid-time end vs system
time of the retract) becomes the queryable learning-latency object from
the HEAD.
(c) Exists: bitemporal XTDB 2; hyperedge retraction with cache
invalidation. Missing: memory-level semantics defined on top.

### Stage 6 — Recall-time verification (consumer side)

(a) Recalled Claude memories arrive marked "reflects what was true when
written — verify before relying on it."
(b) Already the envelope's frame string ("recorded, not necessarily
current") — and valid-time makes it mechanical: the current-belief
projection excludes superseded memories *by construction*; as-of queries
retrieve past belief deliberately rather than accidentally.
(c) Exists: frame string in `zai_memory_1b.clj`. Missing: current-vs-as-of
query pair over the `:memory/*` family.

### Stage 7 — Delivery durability (no Claude analog needed; Zai needs it)

Claude writes to local disk — durable trivially. Zai writes cross a store
that browned out today and ate four turns of this mission's own specimen.
Recreation: the Emacs-outbox pattern (id fixed at creation, spool, replay
on 5xx, `failed/` for operator inspection) applied to `memory_record`, and
the write kept off hot dispatch threads (T-typed-bell-arse-write-async
class).

### Ready/missing table

| Pipeline stage | Substrate status |
|---|---|
| Boot projection (index-as-query) | MISSING (model exists: `mission_context`) |
| Realization detection | PARTIAL (marks.clj `✘`/`💡` tags live) |
| Curation probe in write flow | MISSING (parts exist: `memory_search` + FTS) |
| Write primitive | EXISTS (`append!`, `put-verified!`) |
| Write tool surface | MISSING — and shape ergonomics are a proven hazard (psr/pur "Invalid evidence shape" failures) |
| `:memory/*` type registration | MISSING (mechanism exists: `register-types!`) |
| Supersession semantics | PARTIAL (bitemporal store + retraction exist; semantics undefined) |
| Recall staleness discipline | EXISTS (frame string) → improved by valid-time queries |
| Durable delivery | PRIOR ART (Emacs outbox) — not applied to this path |
| Ablation integrity (§8.4) | RULE STATED (memory_record must join `memory-family-tool-names`) |

### Stage 8 — Scale: compression is a context response, not a storage need (added 2026-07-22)

(a) Claude-side, the memory corpus is compressed/compacted because recall
*is* context loading: the index must fit the window, so hooks get
shortened, verbose entries archived, the corpus held small. The budget is
the context window, and it is applied to storage because storage and
recall surface are the same artifact.
(b) The recreation decouples them. **Two budgets, not one**: a per-turn
context budget that applies only to projections (always bounded — Stage 1),
and a storage budget that is effectively unbounded (append-only log; TBs
are not out of the question as design headroom). The anti-spam mechanism is
not compression but **facetization**: memories attach to facet nodes, and
retrieval scopes by facet.
- **Facets are entities, not enum values.** A facet ("mathematics",
  "complex-analysis", "futon1b-ops") is a node in the graph; a memory's
  hyperedge carries an endpoint to its facet(s); facet nodes form their own
  hierarchy edges. This makes the taxonomy *data*: "mathematics" can
  ultimately be split per domain by adding children and re-pointing (or
  sub-typing) memberships — facet refinement is an ordinary graph
  operation, and bitemporality applies to the taxonomy itself ("what
  facets existed as-of T").
- **Projection budgets go per-facet**: the boot packet takes top-k per
  relevant facet (facets reachable from the clocked mission/task) rather
  than top-k global — the corpus can grow without the boot surface
  growing. FTS stays as the cross-facet net for what scoping misses.
- **Facets are also the bounded-query partition key**: the heavy
  hyperedge-type-window problem (always `&limit`; the materialized window
  cache) gets naturally smaller windows when queries are facet-scoped.
(c) Exists: the type registry has parent inference (`register-types!`,
`infer-parent` from keyword namespace) for one hierarchy level; entities +
relations support facet-as-node with arbitrary depth today; keyset pages +
compact projections (hardening pass) are the scale-compatible read
discipline; the FTS sidecar is a candidate prefilter by design, so text
search cost tracks candidates, not corpus. Honest caveat: *today's
deployment* is a 4G-box JVM that browned out at GB scale this morning —
TB-scale is a statement about the architecture (append-only log, bounded
pages, projections; XTDB 2's remote-object-storage growth path), not about
lucy. Possible convergence to note for DERIVE: a per-domain facet tree is
close kin to the capability star-map's domain partition (pca3-v1) — one
taxonomy could serve both memories and the capability ratchet.

### Stage 9 — Empty stars, hungers, and the growth engine (Joe, 2026-07-22)

Adopted for prototyping: unify the facet tree with the capability-domain
partition, and **demonstrate the memory-to-capability pipeline** — it is
what makes facets meaningful rather than decorative.

The observation that seeds the engine: capability stars begin as
**empty/unfilled capacities, specifically designated as worth having**.
This memory model can represent that natively, because it already has the
primitive: Stage 4's dangling endpoint (a named-but-unwritten node as a
standing query) — now given normative force. An empty star is an IATC
`query`-act memory (kind: preregistered hypothesis / preference) whose
body is a standing retrieval predicate: *"run these sessions and scan for
new information that might fall into this attractor basin or help grow
this preferential-attachment cascade."* One entry, three identities:

1. **A memory** — typed, bitemporal, retrievable like any other.
2. **A capability-in-potentia** — an empty star in the star-map sense,
   designated worth having before any evidence exists.
3. **An engine component** — its predicate runs against incoming material,
   recruiting memories into its basin.

Engine mechanics (sketch for DERIVE, not commitment):
- **The Stage 3 curation probe doubles as the attachment engine.** Every
  new memory written is matched not only against near-duplicates but
  against *open standing queries*; hits add memory→star edges. Basins
  with mass recruit more — via matching and via per-facet projection
  presence (preferential attachment).
- **Fill threshold → the ratchet.** When a star's basin crosses its
  designated bar, promotion to capability-packet distillation
  (M-apm-capability-ratchet's input). "A memory becomes a capability
  later" is then a movement within one faceted graph.
- **Time-to-fill is the third bitemporal learning quantity** (designated-at
  vs filled-at), alongside correction lag and belief-as-of-T.

Precedents already live in the ecosystem: the Z3a preregistration
discipline (`z3-prereg.md` — hypotheses declared before evidence; same
shape, star declared before memories); and in AIF terms the empty stars
are prior preferences over future capability-observations — the belly —
with the scan as preference-directed epistemic foraging. Note the zaif
controller already prices a `:retrieve` arm in its G-terms: standing
queries are the natural feeder for what that arm retrieves.

Guard (per the gamification-guard discipline): attachment mass must
*measure* evidence accumulation, never become a score to chase — the fill
threshold is an evidence bar, not a target metric.

### Loop architecture (Joe, 2026-07-22): inner = using, outer = reflecting

**Inner loop** (fast, within-task): learning to *use* memories — the boot
projection arriving as prior, `memory_search` at need, and above all
*applying* a surfaced memory mid-round. **Outer loop** (slow, between
tasks): post-hoc reflection — retrospective punctuation, librarian
organization, star tending, facet gardening. The two close through each
other: the outer loop's product (organized corpus, curated projections)
is the inner loop's prior; the inner loop's exhaust (transcripts,
realizations, corrections) is the outer loop's observations. Memory is
the coupling medium of a closed two-loop system — the standard AIF
hierarchy (inner = inference under a model; outer = model revision)
instantiated on the corpus.

Consequences worth holding:
- **`par_punctuate` is already the inner→outer handoff moment** — the
  retrospective memory turn is punctuation doing outer-loop work; the
  PAR detach/reattach model ("bridges social and task timescales") had
  this slot waiting.
- **The meters split by loop**: outer-loop health = the three bitemporal
  meters; inner-loop health = retrieval-hit-at-need (did the M5-class
  memory surface at zai-3's step-back?) and applied-citation (the "cite
  its id when you rely on it" discipline, already in the zai prompt,
  makes inner-loop use *observable* — a cited memory id in a turn-round
  is the inner loop leaving evidence).
- **The prototype ladder ascends inner → outer**: P0–P1 build the inner
  loop (write + recall/use); P2–P4 and the librarian build the outer
  (supersession, facets, stars, organization).
- Read-side burden is asymmetric by design: the harness makes *surfacing*
  passive (projection), but *applying* stays agent behavior — "learning
  to use memories" is a genuine inner-loop skill, measurable via
  citations, and the nudge 2×2 (q3) has a read-side analog.
- **Abandonment markers (Joe, 2026-07-22, watching zai-3 restart the
  a94A06 proof ~a dozen times):** "Let me try a completely different
  approach" / "Let me take a step back" are *lexical abandonment
  markers*, same class as ✘/💡 — and lexical markers are load-bearing
  precedent (WM full-loop: hole-count is lexical). Concrete item
  (P1-adjacent, small): extend marks.clj with a `:self-restart` /
  route-abandonment recognizer, so every restart is tagged in the
  transcript as it lands — the tagged set IS the outer loop's
  learning-opportunity feed for that session (each restart should yield
  a retract/assert pair: what was abandoned and why; what replaced it).
  Not all restarts are equal: principled pivots with stateable reasons
  are memory-worthy; cosmetic re-tries dedup away; and a restart with NO
  stateable reason is itself diagnostic — it marks exactly where a
  surfaced memory would have helped most.
- **Restart count as a Continual-Improvement meter**: restarts-per-proof
  is cheap, lexical, per-session — the natural inner-loop efficiency
  meter ("it would be very exciting to see it improve"). Expected
  signature of improvement: restart count declining across same-facet
  problems as the memory corpus grows, with memory-id citations
  appearing at the decision points where restarts used to be.
  Gamification guard applies in full: restarts are a meter, never a
  target — an agent optimizing the count (by not narrating restarts, or
  by avoiding legitimate pivots) corrupts both the meter and the corpus.
  Volume check from live data: a94J06 yielded 8 candidates, a94A06 ~a
  dozen restarts — the librarian/facet scale rationale is confirmed, not
  hypothetical.

### Open questions handed to DERIVE

1. Supersession mechanics: same-id new version vs `:memory/retract` edge
   (constraint: preserve the correction event for the learning-lag query).
2. Recall push vs pull: is the boot projection enough, or should
   turn-start inputs (task text digest) drive an automatic FTS candidate
   probe into the boot packet?
3. The nudge — now a 2×2, not a binary (Joe's dual-team proposal,
   2026-07-22): **who writes** (solver | scribe) × **when** (at-realization
   | retrospective). Solver×at-realization = the prompt-contract nudge;
   solver×retrospective = session punctuation; scribe×concurrent = a
   second agent (zai-4-style) consuming the solver's live transcript from
   the evidence store (turn-rounds land in near-real-time; marks.clj
   ✘/💡 tags are the trigger feed — the scribe is a transcript-consumer,
   NOT an in-session shoulder-watcher) and writing memories with explicit
   distill ids (no @current-round needed — the scribe can cite exact
   evidence ids, dissolving F3 for its path); scribe×retrospective = the
   mining path with an agent behind it. Rationale for the scribe (Joe):
   the write burden competes with task flow — step-back moments are
   exactly when the solver's working memory is saturated; "more relaxing
   not to have to think about the write side." The read side stays with
   the solver either way (P1's boot projection makes it mostly passive).
   Caution against scribe-only: the solver knows WHY it abandoned a route;
   the scribe sees only what was narrated, so scribe memories inherit the
   forward-narration unevenness AND risk plausible-but-wrong why-clauses
   (a reconstructed rationale is worse than an absent one, per claim
   discipline) — scribe memories should carry a :witnessed provenance
   role (author = scribe, distills → solver's rounds) and modest
   confidence. Cost note: a concurrent scribe doubles gated zai quota per
   session; cheaper variants = event-triggered (wake on ✘ marks only) or
   retrospective batch. Clean next experiment when quota allows: a scribe
   pass (zai-4) over the SAME a94A06 session job-2 just punctuated —
   solver-retrospective vs scribe-retrospective on identical material.
4. Shape ergonomics: what payload does the agent *naturally* produce, and
   does the tool accept it first-time (the psr/pur failure class)?
   Candidate: run the a94J06 memory-candidate catalog (TN Part 2, 8 items)
   through the proposed shape on paper before any code.
5. Retrieval scoping: which memories surface for which agent (ops
   memories are environment-scoped, math memories capability-scoped —
   TN Part 3). Stage 8's facet mechanism is the candidate answer shape.
6. Facet representation and refinement: facet-as-entity graph vs type-
   registry namespaces (Stage 8 argues entity); membership semantics when
   a facet splits; per-facet projection budgets (k per facet, and who
   sets k). Stage 9 resolves the unification question in principle (one
   faceted graph serving memories and capabilities) — DERIVE owes the
   concrete join to pca3-v1.
7. Standing-query representation and cadence (Stage 9): how is the
   predicate stored (executable XTQL/FTS query vs facet-match spec vs
   prose evaluated by the curating agent); when does matching run
   (at-write via the curation probe, batch scan over sessions, or both);
   who sets the fill threshold and what promotion to the ratchet
   concretely emits; how the guard is enforced (attachment mass as
   evidence bar, not chased metric).
8. **The Librarian role (Joe, 2026-07-22 — supersedes the scribe as the
   interesting second agent).** Writing scales linearly with sessions;
   organization scales superlinearly with the corpus (dedup is pairwise,
   facet strain grows with mass, contradiction detection is cross-memory)
   — so solver-retrospective may suffice for the WRITE side, and the
   dedicated role belongs on the ORGANIZE side. Librarian duties = the
   ongoing practice of what P1–P4 define as operations: (a) dedup/merge
   across sessions and agents (the run_readonly lesson will be recorded
   twice — merge via agree-edges or canonical+variants); (b) facet
   gardening (split/merge/create facet nodes as mass accumulates — P3's
   acceptance op as routine); (c) staleness patrol (re-verify :volatile?
   memories against live state, retract falsified ones — bitemporal
   hygiene); (d) contradiction detection (challenge edges between
   memories); (e) star tending (batch attachment matching, fill-threshold
   judgment, ratchet promotion — Stage 9's engine steward); (f) hook and
   projection curation (bodies are append-only but hooks/roles live in
   retractable hyperedge props — the librarian improves the retrieval
   surface without touching the record); (g) quality review of incoming
   memories against the fixture bar (author ≠ reviewer, again). IATC
   signature: the solver's act is `assert`; the librarian's are `agree`,
   `challenge`, `retract`, and above all `define` — the act vocabulary
   partitions naturally across the two roles. Division of epistemic
   labor: solver writes (ground truth of intent — no reconstructed
   why-clauses), librarian organizes after the fact (needs no access to
   intent). Cadence: batch/periodic (nightwatch-style) over bounded
   projections — cheap, no always-on second agent; becomes necessary
   around P2–P3 corpus sizes. Prior art to consult: E-futon1a-archivist
   (the archivist role, same shape one store back).
   **AIF framing (Joe, 2026-07-22): the librarian is a War Machine loop
   instance whose manifold is the memory hypergraph.** Component-wise, not
   metaphorical: observations = corpus state (duplicate pairs, facet
   strain, stale :volatile? entries, contradiction candidates, unfilled
   stars); preferences C = the organized corpus — and Stage 9 already made
   the stars prior preferences, so the belly IS the librarian's C;
   policies = the librarian acts (merge, split, retract, challenge,
   define); EFE selects among them — **staleness patrol is epistemic
   foraging** (re-verifying a volatile memory is an uncertainty-reducing
   action, pure epistemic value); **contradiction between memories is
   prediction error** in the corpus generative model (the challenge edge
   is the error signal, resolution is model update); **facet splitting is
   structure learning** (expanding the model's state space when
   accumulated mass warrants). The three bitemporal meters (correction
   lag, belief-as-of-T, time-to-fill) are the loop's sensory channel —
   which makes the Continual-Improvement gate (§Staged for ARGUE) and the
   WM loop one apparatus, not two. Organizing the corpus is also **niche
   construction** — the librarian modifies the epistemic niche every
   future agent inhabits (M-aif2 scoping applies). Practical hooks: the
   zaif controller's G-term arm pricing is a candidate policy selector;
   the WM overnight apparatus / nightwatch regime is the natural cadence
   host. Positions the eventual librarian charter as a joint child of
   M-typed-memories and M-war-machine-wiring.

## Staged for ARGUE — "Continuous Integration" → "Continual Improvement" (Joe, 2026-07-22)

Parked here for the ARGUE phase, not argued yet: this apparatus could allow
replacing "Continuous Integration" with **"Continual Improvement"** as the
standing gate discipline — **if we can't evidence learning, we're probably
doing the wrong thing.** CI demands that every change pass the tests;
Continual Improvement would demand that sustained work *evidence learning*
(memories formed, corrections recorded, stars filling — the three
bitemporal quantities are the meter). The ARGUE round owes: the
IF/HOWEVER/THEN/BECAUSE form of this position, what it would gate in
practice (sessions? missions? runners?), and its failure modes (the
gamification guard applies with full force to any learning *metric* made
into a gate).

## Prototype ladder — DERIVE structure (2026-07-22)

Rationale (Joe): theory has gone as far as it usefully goes — 9 stages,
many features, many invariants, **almost zero examples**. The route is an
MVP and an upgrade path, not a monolithic build (consistent with
POC-plus-follow-on discipline and with the thematics: the memory system
should itself grow by accretion, not arrive fully formed).

**The upgrade-path constraint shapes the MVP**: P0 must be *minimal in
surface but right in shape* — later rungs attach via new edges and new
queries, never via schema rework of what P0 wrote. Concretely: P0 writes
to the real store (bitemporality latent from day one), P0's hyperedge
already carries the full endpoint set (subject, distilled turn-round,
session, mission), and P0's act/kind vocabulary is the real one even if
only `assert` is enabled.

### P0 — "a Zai runner can write a memory" (the minor-seeming core)

- One tool: `memory_record` (act = `assert` only), writing one evidence
  entry + one `:memory/assert` hyperedge via existing `append!` /
  `put-verified!`; author + session server-stamped; `:memory/assert`
  registered via `register-types!`; tool joins `memory-family-tool-names`
  (§8.4 ablation intact).
- **Examples-first discipline** (the almost-zero-examples problem): before
  any code, hand-write the 14 catalogued candidates (TN Part 2's 8 math +
  Part 3's 6 ops) in the proposed payload shape. They are the fixture
  suite AND the shape-ergonomics test (DERIVE q4): if the shape can't
  express the catalog naturally, the shape is wrong — fix it on paper.
- Read side: **no new surface** — existing `memory_search` (type filter +
  FTS) must already retrieve P0 memories, or P0 shape is wrong.
- P0 invariants: I-M1 write is verified (read-back) and returns the id;
  I-M2 identity server-stamped, never agent-supplied; I-M3 ablation modes
  exclude the tool; I-M4 write cost bounded on the dispatch path (measure
  `put-verified!`; outbox/queue NOT in P0 — but the id-fixed-at-creation
  property IS, so durability can be added without changing the contract);
  I-M5 first-try shape acceptance against all 14 fixtures (the psr/pur
  "Invalid evidence shape" lesson as a test, not a hope).
- P0 acceptance: a real zai session records ≥1 memory unprompted-by-
  operator; `memory_search` retrieves it by text and by filter; the entry
  cites its distilled `:turn-round`.
- **Fixtures WRITTEN (2026-07-22)**:
  `holes/labs/M-typed-memories/p0-fixtures.edn` — all 14 (M1–M8 math,
  O1–O6 ops), payload contract at top, store-side expansion shown once,
  and five shape findings the writing forced: **F1** subject multiplicity
  (payload `:subjects` vector; first = primary entry subject, all become
  endpoints — no schema change); **F2** the kind boundary rule
  (why-from-failure → `:feedback`, documented contract → `:reference`;
  goes in the tool description); **F3** agents cannot know their own
  evidence ids — `:distills` gets an `"@current-round"` sentinel resolved
  server-side (demanding ids would recreate the psr/pur failure class);
  **F4** `:volatile?` hint for valid-until-changed memories (O4/O5; P2's
  natural first retract candidates); **F5** negative finding — nothing
  needed an `:act` field, the A→B→A′ story fit in a `:why`, confirming
  act-fixed P0 (retract belongs to P2, when a *later* session contradicts).

### P1 — recall + curation (memory becomes *used*)

Pattern-conditioned recall, not a global popularity projection. The task and
mission supply R1/R2 context; `psr_search` constructs the bounded R6 candidate
pattern set; each candidate is enriched with compact memories attached to that
pattern as warrants and counterexamples (ids, hook, provenance, volatility,
and attachment confidence). `psr_select` returns the selected pattern with the
top relevant memory bodies and records the memory ids actually used. A small
boot projection may remain as a bounded fallback, but is not the principal
retrieval strategy: a memory that is merely searchable or globally recent is
not operationally remembered.

The write-flow curation probe still returns near-duplicate candidates with
confirm/supersede/new, and never blocks the write path. Attachments are
revisable graph claims: FTS/embedding nearness may propose them, while use and
independently witnessed outcomes update their R3/R7 precision. The solver's
write or citation is an R9-bounded assertion, not R16 evidence. At the slow
R15 timescale the Librarian tends attachments and may propose R17 pattern
splits/merges from accumulated episodes; at the fast timescale the solver only
retrieves and acts.

Invariants: candidate and memory projections are bounded + cheap (compact
columns first); every R6 inclusion/exclusion is reason-bearing; retrieval
returns both supporting and challenging evidence; selection/use receipts cite
memory ids; no memory is promoted by frequency alone. Acceptance: on a fresh
problem, the relevant pattern enters the R6 set, selecting it surfaces a memory
written in an earlier session, the solver applies and cites that memory, and an
external result records whether the application worked. The a94A06
`field_simp` denominator-normalization memory is the first designated specimen.

### P1b — the mint lane (memory becomes a new pattern)

Pattern-conditioned recall must be able to say **none of the above**. When the
bounded R6 set has no adequate pattern, record a reason-bearing policy-hole and
the memories that expose it. That bundle is the input to a pattern proposer,
which emits a catalogue-shaped candidate: IF/HOWEVER/THEN/BECAUSE, triggering
contexts, supporting and challenging memory ids, nearest existing patterns,
and a predicted distinction/outcome on which a trial could fail.

This is model expansion, not BMR: R17's reduction equations can merge or prune
an expanded model but cannot invent a new state. The candidate enters R6 at a
broad, explicitly unearned prior. Embedding nearness, one eloquent memory, or
raw attachment count may nominate it but may not promote it. It earns standing
only through use in a pattern cascade and independently witnessed outcomes;
failures revise, merge, or retire it without deleting the episode record.

Fast/slow split: the solver may deposit the policy-hole and suggest the
candidate while working; the Librarian checks catalogue shape, overlap, and
counterevidence before promotion. Acceptance: a task for which every existing
R6 candidate is rejected produces one traceable candidate pattern; it is tried
on a later task, the use cites its source memories, and the external result
changes its status or precision. No operator or agent declaration alone can
promote it.

### P2 — supersession + the bitemporal meter

`retract`/supersede enabled (mechanics per DERIVE q1); current-vs-as-of
query pair; **correction lag measurable** on a real correction.
Acceptance: one memory superseded by a later session; both "what do we
believe now" and "what did we believe as-of T" answer correctly; the
correction event survives. (Fixture O4 — the :7073/:7074 topology memory,
marked `:volatile?` — is the designated first supersession: the wiring
work in flight will falsify it.) **Candidate at P2+, per Joe on F3
(2026-07-22): rebuild PSR/PUR/PAR on this substrate** — the
pattern-discipline records are memory-shaped (selection/outcome/
punctuation as typed acts about patterns), a rebuild would unify the
write paths and retire the "Invalid evidence shape" failure class at its
source; needs P2's update/outcome semantics first.

### P3 — facets (scale organization)

Facet-as-entity nodes; memories attach; per-facet top-k boot projection;
facet-scoped search. Acceptance: one facet split ("mathematics" → domains)
performed as a pure graph operation with no memory rewrites.

### P4 — stars + the engine (memory-to-capability demonstrated)

One empty star declared (candidate: "circle-integral proof technique in
Mathlib" — 8 catalogued memories already belong to its basin, making the
first demonstration retroactively honest); curation probe doubles as
attachment engine; fill threshold; promotion handoff to
M-apm-capability-ratchet; **time-to-fill measurable**. Acceptance: the
declare → record → attach → fill → promote arc runs end-to-end on real
session material.

### Build protocol notes

- Each rung is a Codex-packet with its acceptance bar; the ladder
  structure — which rung, what shape, what invariants — stays with the
  Claude owner. **P0 packet WRITTEN**:
  `holes/CODEX-HANDOFF-p0-memory-record.md` (delivery per Joe: file
  passed directly, no bell; Claude-owner review after the code lands).
- **P0 LANDED + REVIEWED (2026-07-22, Codex `b9b3e32`, author≠reviewer
  PASS).** All 14 fixtures accepted unmodified (fixture file untouched by
  the commit); focused tests re-run by reviewer (memory-write-test 10/208/0;
  zai-api-test's 2 failures confirmed pre-existing drift from
  `b5b5d02`/`57dd416`); kondo 0 errors / parens OK re-run; live read-back
  verified at both claimed ids (server honors client `:hx/id`; props
  flatten to `:prop/*`); `:memory`+`:memory/assert` registered (parent
  `:memory`). First deliberate memory in the store is Codex's meta-memory
  of the tool's own contract (`e-4efe26fd…`). One review fix applied by
  owner (uncommitted): `subjects` now schema-required (+minItems 1) —
  the runtime demanded it, so the schema says so; live-reloaded via
  drawbridge. **Open acceptance remainder**: (1) a real zai session
  records a memory unprompted (the live write was harness-context);
  (2) FTS live retrieval (blocked by `:expensive-read-busy` at
  implementation time); (3) entry-write 686ms on the dispatch path —
  measured per I-M4, watch under the hardening acceptance week.
- **P0 ACCEPTANCE MET (2026-07-22 evening, a94A06 run, zai-3).** A real
  zai session recorded 4 typed memories through the tool, in-session,
  agent-authored end-to-end (ids e-6bcbb51e/e-d9b1739f/e-94028b3f/
  e-3c8277ed; entries + hyperedges verified; session stamped). BUT the
  road there found and fixed a live P0 bug the fixtures could not see:
  **the tool ctx carried a nil session-id on the real dispatch path**
  (tool-opts' `session-id-atom` vs the round loop's `sid` — two identity
  sources), and EvidenceEntry rejects present-but-nil keys → all 8 of
  zai-3's first attempts failed "did not conform to shape" with no
  usable detail. Fixed same evening (owner, live-reloaded, uncommitted):
  `:session-id-fallback` threaded from `sid` at the call site;
  memory_write never assocs nil identity keys. **Lesson for the fixture
  discipline: I-M5 tested payload shape; the ctx wiring needed its own
  live-path fixture** — add one at P1. Degradation observed under opaque
  errors: zai-3's later retries drifted to invalid ref types
  ("topic"/"artifact") — error messages must name the failing field
  (boundary detail improvement queued). Recovery replays created
  near-duplicates (4 replayed + 3 edge-less orphans beside the agent's
  canonical 4) — append-only store, so these are the librarian's first
  real dedup case, not deletions.
  **Post-acceptance retrieval gap identified (2026-07-23):** the 4
  canonical memories are stored but not operationally retrievable —
  no pattern endpoints, `psr_search` doesn't see the evidence store,
  no boot projection. See §Retrieval gap and strategy for the P1 fix
  (pattern-conditioned recall via the existing `?end=` hyperedge
  query).
- **Condition comparison (DERIVE q3 data, one day):** zai-1 a94J06
  (no tool): 0 recorded, ≥8 candidates in self-talk. zai-3 main turn
  (tool visible, no nudge): 0 recorded despite ~12 restarts. zai-3
  punctuation turn (explicit retrospective request): 8 attempts / 4
  distinct memories — recorded (after the harness fix), including the
  M5-class field_simp/ring lesson, the npowRec defeq workaround, a
  correctly-`:volatile?` polyrith reference, and one restart captured
  as an abandonment memory with its why. **Coverage: 4 of ~12 restart
  opportunities → the retrospective condition works but is lossy;
  write-at-realization (nudge) condition still untested — requires
  agent re-registration (registered invoke-fn closures predate the
  prompt edit).**
- Rungs ship in order; a rung's acceptance must hold before the next
  starts (Continual-Improvement dogfood: each rung should *evidence* that
  the previous rung's memories are being formed/used on real work).
- The hardening-review dependency (TN-futon1b-memory-incident) gates P0's
  I-M4 measurement, not P0's design.

## Retrieval gap and strategy (2026-07-23, zai-3 at Joe's direction)

### The gap, confirmed live

After P0 acceptance (4 typed memories in the store), **none of the 9
`:memory/assert` hyperedges has a pattern endpoint.** Verified by live
query (`GET /api/alpha/hyperedges?type=memory%2Fassert`): endpoints are
`[entry, Mathlib, apm/a94A06, session]` — no `:pattern` ref. The
consequence chain:

- `psr_search` queries the futon3a notions-index TSV, not the evidence
  store. It returns pattern candidates with no knowledge of attached
  memories.
- `memory_search` queries evidence by type/author/tags/subject — but the
  agent must independently decide to call it with a good query.
- The boot packet has no memory projection.
- **Therefore a saved memory surfaces only if the agent independently
  calls `memory_search` with the right query at the right decision point.**
  That is storage, not operational memory.

### Key infrastructure finding: the query primitive already exists

`hyperedges-query-uncached` in `futon1b_graph.clj:507` has a dedicated
`:end` branch (line 568) that unnests `:hx/endpoints` and filters by
exact match:

```
GET /api/alpha/hyperedges?end=Mathlib&limit=2
→ returns all hyperedges with "Mathlib" in :hx/endpoints ✓
```

**If memory hyperedges had pattern endpoints, retrieval by pattern would
be one HTTP call.** The infrastructure for pattern-conditioned recall
already exists; the gap is wiring, not new code.

### P1 architecture: pattern-conditioned recall (Joe's framing)

The key move is **pattern-conditioned recall**. A memory remains a
concrete episode or fact; a pattern becomes its reusable retrieval handle.
When a later task causes a pattern to surface via `psr_search`, its
attached memories should arrive automatically — the solver should not
need to remember that `memory_search` exists.

```
current task
    ↓
psr_search → candidate pattern (with memory hooks)
    ↓
psr_select → pattern + full bodies of top 3 attached memories
    ↓
agent applies and cites evidence id
    ↓
pur_update carries :memory_ids (retrieval-to-application observable)
```

Five rungs (full details in `holes/notes/retrieval-strategy-pattern-conditioned-recall.md`):

| Rung | What | Effort | Dep |
|------|------|--------|-----|
| 1 | Pattern endpoints on memories (hyperedge backfill) | Small: 1 XTDB tx | — |
| 2 | Enrich `psr_search` with attached memory hooks | ~10 lines in `tool-psr-search` | 1 |
| 3 | `psr_select` returns full memory bodies | ~10 lines in `tool-psr-select` | 1 |
| 4 | Record memory use in PUR (`:memory_ids` param) | Small | 2–3 |
| 5 | Boot projection of recent memories | Medium | — |

**Minimum viable: Rungs 1+3.** Pattern endpoints on the 3 a94A06 math
memories (all under `math-formalization/tactic-algebra-interference`) +
`psr_select` querying `?end=<pattern-id>` and returning bodies. ~1 day.

### Pattern → memory mapping for the a94A06 corpus

| Memory | Pattern |
|--------|---------|
| `e-6bcbb51e` field_simp poly denom | `math-formalization/tactic-algebra-interference` |
| `e-d9b1739f` HasDerivAt.comp npowRec | `math-formalization/tactic-algebra-interference` |
| `e-3c8277ed` expand composed antiderivative | `math-formalization/tactic-algebra-interference` |
| `e-94028b3f` polyrith unavailable | (none — volatile reference, not a proof technique) |

Three memories cluster under one pattern — the right umbrella: each is a
concrete instance of "tension between algebraic truth and tactic
mechanics."

### P1 acceptance test (Joe's criterion, made concrete)

1. A **fresh Lean problem** is dispatched to a zai agent.
2. `psr_search` returns `tactic-algebra-interference` with `e-6bcbb51e`
   as an attached memory hook.
3. `psr_select` returns the **full body** of `e-6bcbb51e` — including the
   fix (normalize `u²-u+1` to `1-u+u²` before `field_simp [hq]`).
4. The agent **applies** the fix and **cites** `e-6bcbb51e` in its
   `pur_update :memory_ids`.

Until this works end-to-end, the system does not have memory in the
practical sense.

### Two complementary processes

- **Memory → pattern** (librarian/outer loop): classify concrete memories
  under reusable patterns. The Stage 3 curation probe doubles as the
  attachment engine — every new memory matched against open standing
  queries (Stage 9).
- **Pattern → memory** (agent/inner loop): ordinary pattern selection
  automatically retrieves relevant experience. The agent is already
  instructed to `psr_search` → `psr_select` → `pur_update`; making
  memories arrive at `psr_select` means retrieval happens at the
  decision point where the agent has committed to applying the pattern.

### Open implementation questions

1. **Hyperedge update for endpoint addition**: existing hyperedges are
   XTDB documents; the update path is `xt/submit-tx` with a new document
   version at the same `:xt/id`. The `invalidate-hyperedge-query-cache!`
   call (line 597) must fire after any mutation. Retraction also available
   (`:hyperedges` ∈ retractable-tables).
2. **The `:type` + `:end` `cond` issue**: `hyperedges-query-uncached`
   uses `cond`, so `:type` takes precedence over `:end`. For
   pattern-conditioned recall, use `:end` only (returns hyperedges of all
   types with that endpoint). Post-filter by type in Clojure if needed.
3. **Pattern entities vs dangling endpoints**: pattern ids like
   `math-formalization/tactic-algebra-interference` become dangling
   endpoints in the hypergraph — legal in XTDB (no referential integrity
   enforcement). The `?end=` query matches by string equality, so the
   pattern id string IS the endpoint string. No entity pre-registration
   needed (Stage 4's dangling-endpoint principle, now operationalized).
4. **Volatile memories and patterns**: `e-94028b3f` (polyrith
   unavailable, kind: reference) should NOT need pattern attachment —
   reference memories are retrieved by boot projection or direct search.
   The `kind` distinction handles this naturally.

## Out of scope (this mission)

- Turning `FUTON3C_TYPED_BELLS` ON (C-adopt remains Joe's call in
  M-typed-bells) — though shared IATC vocabulary raises its value.
- Retroactive mining of the full historical transcript corpus — the mission
  proves the typed write/read loop; bulk distillation is a follow-on.
- Cross-agent memory federation (lon/chi sites).
