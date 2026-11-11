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

Corrected against the code on 2026-11-11 (MAP:
`holes/labs/M-repl-circus/MAP-animals-2026-11-11.md`, kimi-1, `3108d2c26`).

The trigger does not decide whether an animal's output is evidence. Its role
does: the animals observe or assist acts, and what they produce could be
recomputed.

| Animal | Hooks onto | Gives | Output should live |
|---|---|---|---|
| 驴 donkey | operator turn, at `:recorded` (before the reply; `xiang/lu_rerank.clj:300`) | relevant patterns | metadata on the operator turn (`:lu_ranked`, `.patterns.json`) |
| 象 elephant | operator turn | proforma reading, intent marks | derived store keyed by turn evidence-id |
| 狗 dog | operator turn: recall at `:recorded`, use check and bury at `:settled` (`agency/dog_provider.clj`) | relevant memories on the agent's Prompt line; buried rulings | metadata on the operator record (`:dog_recall`, `:dog_use`, `:dog_buried`); buried memories under `~/.claude/projects/狗/memory` and minus40 :7093 |
| 馬 horse | agent reply: reading at `:ported`, occurrence at `:aligned`, observer opt-in (`ma-enabled.txt`) | 無門関 tensions; per-act alignment; pattern verdicts | `:alignment`, `:pattern_verdicts` on the record; observation rows at :7073 |
| Giraffe | two things: relationship-review candidates at `:settled`; a batch reread/priority pipeline (`giraffe/run.clj`, `reread_worker.clj`, no scheduler found) | proposed tickets, relationship reviews | review entries at :7073; run directories |
| 猴 monkey | intended: timer (seat `猴-1`). **Not in the code** (MAP, 2026-11-11: no reference in src/scripts/dev; last evidence a 429 on 11-07) | operator focus | none yet |

**Pattern occurrence moves from 馬 to 猴 (Joe, 2026-11-11).** "猴's primary
job is ascertaining the operator focus: so finalising the cascade *and
locating it within the GCC* would achieve that." Why it moves: the per-turn
馬-sonnet whistle reads a chain window that often exceeds its per-call
budget (of the last 71 jobs on 11-11: 58 done, 10 "exceeds per-call
budget", 3 "no text"), and a failed read is stored as "no reader", which the
stepper drew as void (frame 179). 馬 read 40 of claude-13's 179 turns. 猴,
on a timer, reads a frame once its reply has landed, can split a large
window, retries failures, and writes the same `pattern_verdicts` shape. 馬
keeps the tensions on the agent reply. Open: whether locating a turn's
cascade in the GCC needs graph embeddings (Joe: "I suspect the answer is
yes").

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

### 狗: recall before the agent's turn, use checked after it

Joe's words, gathered 2026-11-11 from the evidence store:
- 10-24: "We might want to designate the actual worker as 狗 … 狗, the
  actual operative agent, should have some requirements placed upon it,
  related to the use and storage of memories"; "rather than just using
  standard Claude memories, you would try to use the m40 memories instead".
- 11-05: "狗 is to agent as 驴 is to operator. So, 驴 could retrieve relevant
  patterns derived from the operator turn as a post-hook, whereas 狗 would
  retrieve relevant memories for the agent as a pre-hook"; shown as "a brief
  1-2 line memory recall similar to "continuation:" or "system:" lines",
  "pushed to the agent", stored "as metadata … on yours".
- 11-11: "it needs both pre-hook to retrieve any relevant memories (it could
  either read the operator text patterns or wait for the 驴 patterns) and
  also a post-hook to see whether any of these memories were actually
  used/useful … When 猴 is ready a pre-computed operator focus could help
  guide "relevant"."

What exists (11-11): minus40's SessionStart brief injects memories per
session, not per turn (24 of claude-13's 188 frames carry an `injected`
list); its PostToolUse hook checks memory writes for near-duplicates; the
frames record explicit m40 calls (`memory.calls`). `xiang/lu_memory.clj`
is 驴 writing its verdicts into minus40, not 狗 reading.

Shape: the **pre-hook** is a prompt-line provider (`agency/prompt_line.clj`,
beside 驴's `pattern ~…` facts) that adds `memory ~<slug> (<why>)` items,
each a handle resolvable in one call; the recalled slugs are kept as
metadata on the agent's reply. The **post-hook** reads each recalled
memory against what the turn did (cited, opened, applied) and records
used / not used: a prediction ("this memory is relevant") and its
observation, as in E-gcc-schema §16 items 11–13, so retrieval can learn
from it. Relevance first from the operator's text (fast), then 驴's
patterns, later 猴's operator focus.

The post-hook has a second half (Joe, 11-11): "'post-' was not only going
to judge whether they were used (which is interesting) but also "bury" new
memories that are useful (i.e. in the sense of a dog's bone)". So after a
turn 狗 both marks each recalled memory used or not, and buries what the
turn established that a later turn would want, checked against minus40's
near-duplicates first so a bone already buried is added to rather than
buried twice. Decided (Joe, 11-11): a buried memory is written at once, its status
saying it was buried by 狗 and not yet used; the use check promotes it
(the ladder of E-gcc-schema §16 item 14: proposed → accepted → attested).
Empirical question (Joe, 11-11): how each model family uses the memory
store today. "Zai is "obliged" to look and doesn't actually find the memory
store that useful … claude uses its memory routinely and seems to find it
very useful. Codex … has its own memory system which I think is
provider-side".

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

## Closure: the full lifecycle (Joe, 2026-11-11)

Joe: "we should go through the full mission lifecycle. It makes sense that
the components are built, what's needed for DERIVE etc. is to understand how
they fit together." Most animals already run (驴 with sidecar reindexing,
象's turn records, the per-turn cascade, 狗's recall, use check and burying);
the remaining work is understanding their composition, then verifying it.

- **MAP.** Each animal's trigger, input, output, store and readers confirmed
  against the code, with the dispatch boundary (claude-13's packet
  circus-05 to kimi-1, `holes/labs/M-repl-circus/MAP-animals-2026-11-11.md`).
  GCC record types for 狗 and 猴 as schema 1.1.
- **DERIVE.** How the animals fit together: the data flow from operator turn
  to War Machine, what each animal reads of another's output (through the GCC,
  not by parsing evidence bodies), and the control layer (CTRL moves, query as
  control), which is owed as a write-up.
- **ARGUE / VERIFY.** The two invariants below as checks: coverage (core.logic,
  every operator turn has its records) and stepper attachment (reazon).
  Coverage comes first, because the other animals' results rest on it.
- **INSTANTIATE.** 猴 takes pattern occurrence from 馬 (packet circus-04, on
  hold); CLAUDE.md's minus40 section rewritten for 狗 lines.
- **Outside closure, as excursions:** whether 狗 helps (claude-6, 2026-11-11,
  at Joe's request: "some analytics that we could use to say whether 狗 is
  helping after a while"), overlap modes, E-question-log, futon7
  E-futon-questions, the XTDB home of derived stores, prompt durability.

## DERIVE (draft, claude-13, 2026-11-11, from the MAP note)

**How the animals fit together today.** They do not read each other through
the GCC. They hang off one chain of turn-service events on the operator's
象 turn record (`~/.emacs-graph/session-turn-analysis/turn-X.json`), and each
reads what earlier animals left on that record:

1. `:recorded` (record written): 驴 reranks patterns (`:lu_ranked`); 狗 starts
   its recall, seeded from 驴's patterns (`:dog_recall`); the 小象 draft; 象's
   analysis seat is dispatched.
2. `:reading` (象's reading published: `:acts`, `:sentences`, analysis.json).
3. `:ported` (ports computed from the reading): 馬 reads the reply against
   the acts (`:alignment`).
4. `:aligned`: 馬-occurrence matches patterns against the turn
   (`:pattern_verdicts`); the per-turn cascade is drawn from these.
5. `:settled` (reading and reply both present): the evidence packet to :7073;
   Giraffe's relationship-review candidates; 狗's use check and bury
   (`:dog_use`, `:dog_buried`).

So the shared store in practice is the turn record, ordered by these events,
plus :7073 for what must be evidence. Three things follow:

- **The GCC has no production writer.** 驴's `[:pattern/proposal 1]`, 象's
  `[:interpretation 1]`, 馬's `[:pattern/occurrence 1]` and Giraffe's
  `[:needs 1]` are decided or built on fixtures; none is written live. Since
  every animal's output is already on the record, the smallest composition is
  one projector that rebuilds GCC records from the turn records (the GCC
  "can be rebuilt from scratch"), rather than six writers. The animals keep
  writing the record; the GCC is a projection of it, and the readers move to
  the GCC one at a time.
- **猴 is the missing stage after `:settled`.** It is meant to read a settled
  frame, finish its cascade and place it in the GCC. With a projector, 猴
  reads the GCC rather than parsing records, and its output (operator focus)
  is a GCC record (schema 1.1). This is where circus-04 fits.
- **Outputs without a reader.** 狗's three fields reach the Prompt line and
  the E-dog-helps report but not the stepper. Giraffe's priority projection
  has no reader found. Each needs a named reader or should stop.

**What VERIFY checks.** Coverage: every operator chat turn at :7073 has a
record that reached `:settled` within a stated interval, with each animal's
field present or typed absent (core.logic over the records). Attachment: one
stepper per REPL (reazon). The order of events above is what coverage checks.

**Open for DERIVE.** Whether the projector runs at `:settled` or in batch;
the GCC record types for 狗 (recall, use, buried) and 猴 (focus); where
Giraffe's batch pipeline is scheduled, if anywhere; the CTRL write-up.

## Acceptance checklist (IDENTIFY)

- [ ] For each animal: which GCC node and edge types it writes, and which XTQL queries it reads
  others through. No animal reads another's output by parsing evidence bodies.

- [x] E-evidence-cleanup has closed (gate to MAP).  Closure: `1ccde6689`;
  live admission defences: futon1b `4f43c51`, `fd4efb2`.
- [x] Each animal's trigger, input, output and output store are confirmed against
  the code, not only against this table. Done 2026-11-11: MAP note `3108d2c26`;
  the table above is corrected from it (five mismatches). 狗's current code role and 猴's seat are
  the least certain.
- [x] The dispatch boundary is written down (MAP note §7: dispatches are invoke
  jobs; machinery is turn-service listeners plus the 象-N, 驴-haiku and
  馬-sonnet calls they make; told apart by the record's `:origin`,
  `:analysis_dispatch` and the evidence writer): which invocations are dispatches
  (part 3 of the model) and which are Circus machinery.
- [x] A per-turn 狗 recall exists in prototype (live 2026-11-11, `74e644f33`;
  handles are project/slug, one file each), showing the 1–2 line form, with
  handles that resolve in one call.
- [ ] CLAUDE.md's minus40 section is rewritten to treat 驴/狗 items as entry
  points. Do this only once 狗 lines actually arrive.

## Two invariants for the Circus (Joe, 2026-11-11)

Raised when the 象 stepper sat at frame 126 of claude-13's session
(ed69fb38) for eight hours. The stepper was current with its records; the
records had stopped. agent-chat's large-buffer bound (`3b6c9bc6a`, 11-04)
switched `session-mode-turn-tags-mode` off past 1 MB, and the capture,
an Emacs advice on `agent-chat--start-turn`, is gated on that mode, so 34 of
Joe's turns to claude-13 (and claude-9's) went unrecorded with no warning.
Fixed in `1e786e2dd` (painting dropped, capture kept); the invariants below
are what would have caught it.

1. **Attachment (Emacs side, Reazon).** Joe: "there's the *象 stepper*
   buffer which should be attached exactly to one REPL at a time, otherwise
   I guess if we're modelling the steps for two or more buffers they should
   be *象 stepper:claude-13* or some such indexed referent. But that's a
   layout and attachment invariant (which could be enforced with reazon, as I
   have similar sidebuffer setups in other experiments)." Precedent:
   `emacs/agent-chat-invariants.el` (reazon group); the stepper's buffer name
   is `turn-stepper-buffer-name` in `emacs/turn-stepper.el`, one buffer today.
   Its companion check: the attached stepper shows that REPL's newest record.

2. **Coverage (JVM / Agency side, core.logic).** Joe: "The invariant that
   the REPL circus runs on all turns whether or not there is a stepper
   attached is a JVM / Agency side invariant that should be, correspondingly,
   enforced with core.logic". Stated over the evidence store: every operator
   chat turn (author joe, event chat-turn, futon1b :7073) has a 象 turn record
   within a stated interval; the violations are listed, never silently
   dropped. Capture lives in Emacs today, which is why an Emacs display
   change could stop it; `scripts/operator_turn_capture.py` already
   reconciles turns typed outside the recording Emacs from the evidence
   store, so the remedy for a violation exists. Home: `futon3c.logic`
   beside `xiang.clj` (a flat fact base of acts, valid and system time).
   Witness case: the 34 turns of 2026-11-10 17:39 → 11-11 02:00.

## Open

- Overlap modes as factor precision: futon2
  `holes/tickets/T-overlap-modes-as-factor-precision.md`, taken on as an
  excursion of this mission (Joe, 2026-11-11: "#7 could be tagged onto
  M-repl-circus as an Excursion there, even if that's an anachronism").
  Finding so far: the reading is absent from the cascade code, not
  contradicted (futon2 `49cfb6b3c`). Next: live Circus turns whose record is
  trusted (invariant 2 above), read as mini-cascades, with houseboat, coop
  and submarine ("I really feel cooped up in here") as the first tests.
- Joe's questions as questions: turns tagged `claim-type: question` logged,
  linked to their answers and surfaced while open, in the manner of ArSE.
  Excursion [E-question-log](../excursions/E-question-log.md) (Joe,
  2026-11-10).
- Whether 狗 is helping: outcomes, a recalled-but-withheld arm, and the
  metrics with their source fields. Excursion
  [E-dog-helps](../excursions/E-dog-helps.md) (Joe, 2026-11-11).
- Where the derived stores live in XTDB (one store per animal, or one store with
  typed records), and how the GCC reads them.
- Whether 象's per-turn analysis keeps its current file store
  (`~/.emacs-graph/session-turn-analysis`) or moves.
- Prompt durability. The invoke ledger is in `/tmp`, so seat prompts are only
  kept because they are copied into evidence. Once those copies stop, prompts
  need a durable home outside evidence.
