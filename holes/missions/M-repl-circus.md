# M-repl-circus: the Circus as hooks around the shared REPL

**Status:** OPEN — MAP (ungated 2026-11-07, Joe + codex-7).
**Needs:** M-formal-arxana (artifact: record types); E-gcc-schema (artifact: the GCC definition, futon4 `holes/excursions/E-gcc-schema.md`). Recorded by claude-13, 2026-11-10, from Joe: "it also depends on M-formal-arxana working and a GCC being defined that they can write into"; futon4 `holes/labs/M-formal-arxana/DERIVE-repl-circus-needs.md`.

The IDENTIFY → MAP gate, [E-evidence-cleanup](../excursions/E-evidence-cleanup.md),
closed in futon3c `1ccde6689`.  Its prevention follow-ups are also live in
futon1b (`4f43c51`, `fd4efb2`): retired derived families are rejected at the
evidence boundary and current evidence contracts are audited by owner, shape,
bytes, and rate.  MAP may now proceed without reopening the cleanup.

**Related:** [M-giraffe](M-giraffe.md) (the loop 驴 → 象 → 狗 → 馬 → Giraffe → War
Machine, §"Giraffe in futonic logic"), [M-象-2000](M-象-2000.md) (象 turn analysis;
P7b is where `:artifact/weak-activation` came from),
[M-formal-agency](M-formal-agency.md) (feedback reaches every participant).

**One-line:** State what each Circus animal hooks onto, what it gives the
participants, and where its output lives. Evidence holds what participants said
and sent. The animals' outputs are references attached to those acts, or records
in their own derived stores.

## Goal: the Circus talks through the graph (Joe, 2026-11-05)

The Circus interoperates by **graph writes and graph queries**, not by storing JSON inside
the evidence store. Even when such JSON is read, the reader has to parse bodies to make sense
of it. The animals write nodes and edges into the GCC, in XTDB, and read each other through
XTQL queries over it.

The GCC is meant to be read from many places:
- retrieving relevant patterns for operator turns (驴);
- priority items in Giraffe;
- related missions and pattern-based methods;
- M-the-perfect-crime hotspots and misconstruals in the War Machine;
- especially, fast construction of cascades in the War Machine and in a future mathematics tutor.

futon4 Arxana, as a view into XTDB, is the existing prototype of this kind of reading.
Parts already in place:
- futon1b's hyperedge API (`/api/alpha/hyperedge(s)`, `futon1b_server.clj:1034,1103`; `hx_*` sidecar tables);
- WebArxana reading it (`webarxana/server/interest_network.clj`);
- `peripheral/memory_recall.clj:175`, which already finds memories through `:memory/assert`
  hyperedges and is one working case of the pattern.

This design depended on E-evidence-cleanup through IDENTIFY.  That dependency is
now discharged; MAP starts from its clean evidence/derived-store boundary.

## Decided: where derived output goes (claude-8, 2026-11-05, on Joe's go for step 2)

- **Output that relates things goes in the GCC as typed hyperedges** through futon1b's
  `/api/alpha/hyperedges` batch route (`futon1b_server.clj:1034`, `write-hyperedges-batch!` :221).
  Endpoints are ids (evidence, pattern, mission, memory, file path), never copied text.
  This covers 驴 activations, 象 readings, 馬 cards, Giraffe relationship reviews, and the
  agent→file "touched" facts that inbox_zero currently mines out of transcript text.
  `:memory/assert` is the precedent.
- **Scheduler and operational state goes to its own XTDB documents, not evidence**, with its
  readers repointed. Examples: the promise lifecycle (the obligations API reads it) and the work queues.
- **Telemetry that no one reads is not stored as data.** A process log is enough until a
  reader is named (`written-never-read`). Tickle's scan/page/escalation records are the example.
- Order of work: by **live** volume, and only after DESIGN-RECORDS gives the writer's stated
  purpose (Joe's "why" criterion). Dormant writers (zero records on 11-04/05, e.g. `tickle-1`,
  `probe`, `ratchet`) are cleaned up last; changing them now has no effect on the landscape.

## Bearing on the Cascade Live thesis (Joe, 2026-11-05)

If this works, it is the first large piece of progress on the apex question
(`holes/excursions/apex-thesis-narration-2026-11-02.md:33`): "an account of itself structured
enough that the work can steer itself, so that activity becomes inference rather than motion".
Clean evidence plus a GCC that is written and queried as a graph are preconditions for that.
They are not the inference itself, which still needs rules. M-ways-of-working is already
writing such rules in its own terms:
- the intermediate theses IT-1..IT-8 (`holes/excursions/intermediate-theses.json`). For example,
  IT-2: "A closure without an independent witness is a reading, not a completion". IT-5: the
  War Machine selects only from what survives IT-1..IT-3.
- the probes of the ways of working (intents-are-tracked, feedback-reaches, …).
- its chained-claim sweep, which joins an agent's final claim to Joe's next-turn stance. That
  join runs over evidence parts 1 and 2 (turns and replies), which this cleanup makes findable.

As GCC queries, these become inference rules evaluated over the graph. They stop being
judgments made by reading JSON or prose. Writing them that way is a MAP-stage task shared
with M-ways-of-working.

## Origin

Joe searched the Evidence Landscape for a word from his own turn on 11-04
("hearo") and got 32 hits in the default 24-hour window. None of them was the
turn itself. It had aged out of the window, while copies of its text kept being
written by derivative writers. A full count for one day (11-04 05:45Z to 11-05
05:45Z, live FTS sidecar): 13,483 records and about 53 MB, against 90 records and
0.05 MB of operator turns. Joe called it stop-the-line. The working note is
`~/notes/stop-the-line/2026-11-05-evidence-derivatives.md`. The model below was
agreed in that conversation (claude-8 session `534939ec-0109-441a-9221-336a495667c6`).

## The evidence model (agreed)

**Evidence** is what a participant said or sent, recorded once, at the moment it
was said:
1. Operator turns.
2. Agent replies. Interleaved by session, turns and replies form a lab notebook.
3. Inter-agent dispatches (a Codex handoff, a bell) as sent, and their replies as
   returned, linked by job-id or in-reply-to. Keeping these is what lets
   feedback reach every participant and lets intent satisfaction be traced
   across a dispatch tree. Continuations and bellbacks are this, seen from the
   receiving session. Park resumes are already authored `parked-resume`, not
   `joe` (302 against 99 in one day, checked 11-05).

**Not evidence:** anything that could be deleted and recomputed from evidence
plus the sources (pattern library, missions, memory). This covers turn analysis
and intent marks, retrieval results, observer summaries, seat prompt copies,
agent tool-step traces, and scheduler state such as promise lifecycles.

## The Circus

The trigger does not decide whether an animal's output is evidence. Its role
does: the animals observe or assist acts, and what they produce could be
recomputed.

| Animal | Hooks onto | Gives | Output should live |
|---|---|---|---|
| 驴 donkey | operator turn (post-hook) | relevant patterns | metadata on the operator turn (pattern ids, scores) |
| 象 elephant | operator turn | proforma reading, intent marks | derived store keyed by turn evidence-id |
| 狗 dog | agent turn: pre-hook recall, post-hook save | relevant memories, proposed memories | metadata on the agent reply (memory ids, scores) |
| 馬 horse | agent reply | 無門関: tension resolved / re-raised / dropped | derived store |
| Giraffe | 馬's tensions + pattern cascade (second derivative) | proposed tickets, relationship reviews | derived store |
| 猴 monkey | timer (the operator-focus reader; seat `猴-1`, renamed from `zai-1` 2026-11-05) | operator focus | derived store |

狗 is to the agent as 驴 is to the operator (Joe, 2026-11-05). The code calls 狗
"act" (M-giraffe loop line; "驴 works while 狗 does", `xiang/lu_rerank.clj:303`).
This mission makes 狗 the agent's memory hook.

### 驴 and 狗 lines: visible, pushed, and followable

- Both lines are given to the agent as input. 驴's already is (`Prompt: pattern ~…`,
  `transport/http.clj:5365`). Both are also displayed in the shared REPL, as a
  1–2 line entry like `continuation:` and `system:`.
- In evidence they are stored as **metadata on the act they belong to**, never as
  separate records. Checked 11-05: today's operator-turn records carry no pattern
  metadata (`pattern_id` empty). 驴's hits survive only in the separate
  artifact-activation record, which also copies the whole prompt.
- **Spreading activation** (Joe): each item in a line is a pointer the agent can
  follow in one call, not a vague sense of familiarity. Each item carries a
  handle (memory slug → `m40_history` or file; pattern id → library file) and a
  short reason it fired. The next ring out is the GCC neighbourhood: a memory's
  `[[links]]`, a pattern's cascade.

### The giant connected component

The GCC of patterns, missions, turns, sessions and memories is a derived graph in
its own store. Its turn and session nodes are evidence-ids; its edges come from
evidence and the library, including the 驴 and 狗 metadata. It can be rebuilt
from scratch, which is also what makes incremental upkeep checkable. This is the
store the Circus feeds, not the Evidence Landscape.

**GCC schema: 1.0 done, 1.1 TBD (Joe, 2026-11-10).** Schema 1.0 is futon4
`holes/excursions/E-gcc-schema.md` (Parts A–C, claude-1, futon4 `23a44da`). It
names the record types written by 驴, 象, 馬 and Giraffe. The types for 狗 and 猴
are left open on purpose (Joe: "I'd vote that we explicitly *don't* close that
in E-gcc-schema in order to prove that the schema is extensible"). Adding them
here is schema 1.1, and it should need no change to what 1.0 defines.

### The control layer (discussed, not yet written up)

Joe, 2026-11-10: the Circus should refer to the control layer, which has been
discussed in claude-13's REPL (2026-11-09) and recorded only in parts, in
E-gcc-schema §12 ("Links as witnesses; 'eventually' as control"). Its
elements, in the words used then:
- **CTRL items.** A small fixed set of moves an agent can always make on the
  shared graph, each with a defined effect and receipt: name (propose
  `[[X]]`), cite, ask (bell or whistle), wait (park), hand back (a warrant),
  mark (a reply mark). Joe: "maybe CTRL maps to what I was talking about with
  XTQL; so the query language gets mapped to a simple control language."
- **Query becomes control.** A link `[a ? c]` is a query ("is a linked to
  c?"), and a link record is the witness that answers it. Joe: "If we think
  across time, this becomes a kind of 'control problem', i.e., a links to c
  'eventually'." An unanswered query with a horizon is a want. The War
  Machine is the controller that picks the action expected to produce a
  witness, and park is the bounded "eventually" for one job.
- **Instances already running:** War Machine tokens (locator = query,
  observation = witness, bounded by T), `[[X]]` awaiting a referent, parks,
  typed holes and Giraffe `needs` edges with `met` computed.

For the Circus this means the animals' writes are witnesses or open queries
in the GCC, and the agents' interface to the graph is the CTRL set. A
write-up (where it lives, which verbs, which receipts) is owed.

### Written-never-read is feedback that reaches no one (Joe, 2026-11-05)

`written-never-read` (R0, `diagramprover/wiring.clj`) is the opposite of M-formal-agency's
"feedback reaches every participant": a record nobody reads gives nothing back to anyone.
狗 is one way earlier work reaches an agent. A memory of this discussion could come
back later as `狗: don't write spam to the evidence store`. So every Circus output needs a
named reader: the agent through 狗 or 驴 lines, Joe through the REPL, or the GCC. A
derived record without one fails the shape below.

## Shape (for MAP)

One shape, six instances: *a hook on a participant act that produces references
to other nodes; the references attach to the act as metadata, or go to a derived
store keyed by the act's evidence-id; the hook writes no evidence of its own.*
The instances are the six animals above.

## Acceptance checklist (IDENTIFY)

- [ ] For each animal: which GCC node and edge types it writes, and which XTQL queries it reads
  others through. No animal reads another's output by parsing evidence bodies.

- [x] E-evidence-cleanup has closed (gate to MAP).  Closure: `1ccde6689`;
  live admission defences: futon1b `4f43c51`, `fd4efb2`.
- [ ] Each animal's trigger, input, output and output store are confirmed against
  the code, not only against this table. 狗's current code role and 猴's seat are
  the least certain.
- [ ] The dispatch boundary is written down: which invocations are dispatches
  (part 3 of the model) and which are Circus machinery.
- [ ] A per-turn 狗 recall exists in prototype, showing the 1–2 line form, with
  handles that resolve in one call.
- [ ] CLAUDE.md's minus40 section is rewritten to treat 驴/狗 items as entry
  points. Do this only once 狗 lines actually arrive.

## Open

- Where the derived stores live in XTDB (one store per animal, or one store with
  typed records), and how the GCC reads them.
- Whether 象's per-turn analysis keeps its current file store
  (`~/.emacs-graph/session-turn-analysis`) or moves.
- Prompt durability. The invoke ledger is in `/tmp`, so seat prompts are only
  kept because they are copied into evidence. Once those copies stop, prompts
  need a durable home outside evidence.
