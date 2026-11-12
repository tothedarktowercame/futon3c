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
`holes/labs/M-repl-circus/MAP-animals-2026-11-11.md`, kimi-1, `ae1a4c632`).

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

## DERIVE (claude-13 with Joe, 2026-11-12, from the MAP note)

Joe, 2026-11-12: "one projector rebuilding/extending the GCC from turn
records is *one* crucial direction for DERIVE, though another lens on this
that is important is that Giraffe priorities should be kept up to date
routinely. These are closely linked b/c if we continuously update the GCC,
we can also continuously update Giraffe priorities."

### How the animals fit together today

They do not read each other through the GCC. They hang off one chain of
turn-service events on the operator's 象 turn record
(`~/.emacs-graph/session-turn-analysis/turn-X.json`), and each reads what
earlier animals left on that record:

1. `:recorded`: 驴 reranks patterns (`:lu_ranked`); 狗 starts its recall,
   seeded from 驴's patterns (`:dog_recall`); the 小象 draft; 象's analysis
   seat is dispatched.
2. `:reading`: 象's reading published (`:acts`, `:sentences`, analysis.json).
3. `:ported`: 馬 reads the reply against the acts (`:alignment`).
4. `:aligned`: 馬-occurrence matches patterns (`:pattern_verdicts`); the
   per-turn cascade is drawn from these.
5. `:settled`: the evidence packet to :7073; Giraffe's relationship-review
   candidates; 狗's use check and bury (`:dog_use`, `:dog_buried`).

The shared store in practice is the turn record, plus :7073 for what must be
evidence. No GCC record type (驴 `[:pattern/proposal 1]`, 象
`[:interpretation 1]`, 馬 `[:pattern/occurrence 1]`, Giraffe `[:needs 1]`) has
a production writer.

### The composition: two continuous projections

**1. The GCC projector.** One projector, run at `:settled`, reads the settled
turn record and extends the GCC with that turn's records (§16 of E-gcc-schema:
occurrences, gluing computed, `[:cascade/reading 1]`). A full rebuild from all
turn records must give the same graph; that equality is the projector's check.
The animals keep writing the record and stay independent of each other; the
GCC is a projection of the records, not a seventh writer racing the six.

**2. Giraffe priorities, kept current from the GCC.** Giraffe has two parts
(`giraffe/pass.clj`, `giraffe/priority.clj`):
- the *pass* orders a work pool (missions, excursions, tickets with their
  state groups) by readiness over `needs` edges (closure, artifact,
  alternative);
- the *priority projection* bands that order by a focus relation received
  from outside (focus / associated / elsewhere). Its docstring: "it does not
  infer focus."

Today the pass reads an edges file and nothing schedules it, and the priority
projection has no reader (MAP note). With the projector, both inputs come from
the GCC: `needs` edges and item states are GCC records, and the focus relation
is 猴's output. So each GCC update can recompute the pass and the projection.
That keeps priorities current without a separate schedule.

**猴's place follows.** 猴 reads the GCC after a turn settles, locates the
turn's cascade in it, and writes the operator focus (a schema 1.1 record).
That focus is the input Giraffe's priority projection waits for. The flow
closes: operator turn → 驴, 象, 狗, 馬 on the record → `:settled` → GCC
projector → 猴 focus → Giraffe pass and priorities → the War Machine (and Joe)
as readers of the priorities.

### Two JVMs (Joe, 2026-11-12)

Joe: "13 JVMs is 11 more than are needed, in my opinion. It is starting to
sound like "GCC" mostly means decluttering our JVM multiverse and putting them
into 1x futon3c and 1x futon1b." On 11-12 Zone ran 13 (about 39 GB):
futon3c (:7070), futon1b (:7073), minus40's store (:7093, a second futon1b),
the formal Arxana demos F1–F6 (:7085–:7090), the code↔docs demo (:7091), the
workplan demo (:7094), the Arxana demo (:7084, a child of the futon3c JVM),
and giraffe-reread (a fresh JVM every 5 minutes from a timer enabled on
11-06, though its unit files say "NOT INSTALLED OR STARTED"). The GCC is one
graph, so its consolidation is also a consolidation of processes: every
service as routes and scheduled tasks in the futon3c JVM (I-0), every store's
data in futon1b. Order: minus40's store first (Joe: "memory migration is the
priority"); giraffe-reread as an in-JVM scheduled task like the watchers; the
Arxana demos as routes in futon3c with their data in futon1b, with their
owners. Joe, 11-12: "it's the Arxana demos that I thought of as "trial
basis". Those need to all be merged into the 1x futon3c and 1x futon1b that
we have discussed." Inventory: claude-6,
`holes/labs/M-repl-circus/JVM-inventory-2026-11-12.md` (in progress). Until a
merge is cut over, the demo processes are left running and unsignalled. Then
futon1b's heap doubles (Joe, 11-12: "a much better route is to 2/13 the JVMs
then 2x the memory for futon1b, since we are still far ahead on memory in
that case"); the eleven JVMs use about 21 GB today, so a 16 GB futon1b
still leaves Zone with more free memory than now. "If it *happens* that some
aspect of this needs to be "hydrated" into datascript, that route remains
open."

Inventory (claude-6, `holes/labs/M-repl-circus/JVM-inventory-2026-11-12.md`,
`a976ee9c6`): the nine Arxana processes all run the canonical futon3c code and
serve the same `/arxana/v1` routes, differing only in store and fixture; eight
start a private embedded futon1b (about 9 GB together); live futon1b already
registers `/api/alpha/arxana/command` and `/query`.

| JVM | Data | In use | Fold | Effort |
|---|---|---|---|---|
| giraffe-reread | SQLite queue + EDN, no XTDB | every 5 min | scheduled task on its own executor | small |
| Arxana demo :7084 | mock, in memory | unknown | mount in-process, or retire | small |
| F1–F6 :7085–:7090 | in-memory XTDB from pinned inputs | **retired 11-12** (Joe: already QA'ed and accepted) | loaders under the `/arxana/v1` mount, if ever wanted again | – |
| code↔docs :7091 | same | **retired 11-12** | same | – |
| workplan :7094 | own durable XTDB store, 130 MB, 24 post-load commands held nowhere else | daily | same mount, reload into futon1b, replay post-load writes | medium to large |

**Retired 2026-11-12:** F1–F6 and code↔docs (seven JVMs; Joe: "I believe I
already QA'ed and accepted the Arxana demos, really they should have been
retired then"); recorded in futon4 `holes/missions/M-jvm-arxana.md`. Zone went
from 13 JVMs to 6. Pending Joe: :7084 (the code-server demo on :8097 uses it)
and the workplan store on :7094 (data of record, migrate first). The Giraffe
packet is held until the JVM consolidation is sorted out (Joe, 11-12).

**giraffe-reread is not folded as a schedule** (Joe, 11-12: "if the GCC is
continuously up to date, then we don't need a scheduled reread, and we can
have an invariant that says "Giraffe is always up to date""). Each 5-minute
cycle today polls for drift: a mission or excursion document whose digest
differs from the one its landed reading was made from gets a re-read request,
dispatched to a reader seat; the landed reading updates Giraffe's `needs`
edges (`giraffe/reread_cli.clj`, `reread_queue.clj`, `reread_worker.clj`).
Event-driven instead: the multi-watcher already sees every change to holes/
documents, so a change mints the re-read request (debounced, as the pattern
index does); a landed reading, like a settled turn, updates the GCC, and each
GCC update recomputes Giraffe's pass and priorities. The invariant, checked
with the others in VERIFY: every mission and excursion document's current
digest has a landed reading or a pending request younger than a stated bound,
and Giraffe's priorities were computed after the newest reading and the newest
GCC change. (A scheduled-task packet to codex-7 was cancelled for this.)

Order: giraffe-reread (event-driven, as above); the `/arxana/v1` mount proved on F1 (with a durable
artifact provider, since the only one is in memory); workplan; F2–F6 and
code↔docs; :7084. The futon4a README's "never a live store" rule changes at
the same cut-over (Joe's 11-12 statement supersedes it).

### What VERIFY checks

- **Coverage** (core.logic): every operator chat turn at :7073 has a record
  that reached `:settled` within a stated interval, each animal's field present
  or typed absent, and its GCC records present.
- **Projection** : incremental GCC equals rebuilt GCC; Giraffe priorities are
  no older than the newest settled turn.
- **Attachment** (reazon): one stepper per REPL.

### Open for DERIVE

- **Where the GCC lives: a research question** (Joe, 2026-11-12: "I think
  *maybe* a sidecar makes the most sense, but this remains a kind of research
  question or dilemma. We could go back to the old Datascript idea from
  futon1 for example, so that it "lives" in XTDB but is fast to access and
  use"). The candidates:
  - *SQLite sidecar*, as for the pattern index, the warrant index and the
    entity sidecar columns: a file, fast, rebuildable; graph queries
    (connectivity, gluing, readiness over `needs`) are recursive SQL.
  - *Datascript in the JVM, mirrored to XTDB*, as futon1's `app.store` did
    (futon1 `apps/graph-memory/src/app/store.clj`: Datascript working copy,
    append-only journal and snapshots, `sync-to-xtdb!`,
    `hydrate-from-xtdb!`): Datalog over the graph in memory, durable in XTDB.
    Costs: heap in the shared futon3c JVM, and reload hazards for in-memory
    state.
  - *XTDB alone* (futon1b :7073): durable, but slow per query and that heap is
    already near full.
  One fact narrows it: the GCC is a projection that can be rebuilt from the
  turn records, so its store needs speed more than durability. A durable copy
  saves rebuild time; it does not hold anything that would otherwise be lost.
  What would settle it: build the same GCC (all settled turn records, the
  pattern library, missions) in SQLite and in Datascript in a separate
  process, then measure size, full-rebuild time, one turn's incremental
  write, and three queries the readers need (猴: locate a turn's cascade;
  Giraffe: readiness over `needs`; the coverage check).
  **Use-case survey** (Joe: "maybe the main issue here is how it'd be
  *used*"; kimi-1, `holes/labs/M-repl-circus/GCC-use-cases-2026-11-12.md`,
  `c93c00d84`, 16 uses). Most uses are point lookups and one- or two-step
  neighbourhoods from known ids (Mojira and stepper views, 狗's recall inside
  its 100 ms Prompt-line budget, CTRL's "is a linked to c"). Next come
  reachability queries over small subgraphs, answered within a turn
  (猴 locating a cascade by shared elements and its connected component,
  Giraffe's readiness over `needs`, turn → question). Whole-graph checks and
  as-of queries are rare and offline. Writes are small per-turn appends by
  one writer; the graph is at most a few hundred thousand edges.
  Recommendation: a SQLite sidecar, with Giraffe's readiness staying in
  Clojure and reading edges from it; graphqlite (colliery-io, a SQLite
  extension adding Cypher and graph algorithms; MIT; v1.0.0 on 2026-11-10;
  JVM loading unconfirmed) if the gluing and reachability queries are painful
  as recursive SQL; XTDB as a later durable mirror if as-of becomes a real
  reader. XTQL as the CTRL language is a separate question from the store:
  CTRL queries are rare enough that a slower language on top could serve.
  **Direction (Joe, 2026-11-12), superseding the sidecar recommendation
  unless the XTQL measurements argue otherwise.** "Before we build or spike
  anything we should get more clear on the use cases, write some real XTQL
  that works against the current system ... We already have patterns,
  missions, turns, memories, etc., available via XTDB. So whatever "else" is
  needed from a GCC pretty much needs to work with that." And: "The
  "Connected" part of GCC is important b/c we need to be able to do inference
  about the various different aspects." So the GCC's added records live with
  the records they connect, in futon1b, and inference runs over one graph.
  Two consequences:
  - **Memories move into futon1b.** Joe: memories "should probably be
    migrated into the same store so that we don't have multiple unneeded
    JVMs". minus40's store (`minus40-store.service`, :7093) is a second
    futon1b instance; its readers and writers (minus40's ingest hook and MCP
    tools, the SessionStart brief, 狗's recall and bury) would point at :7073.
  - **futon1b's heap.** It runs with `-Xmx8g` (drop-in `heap.conf`, raised
    from 4g on 10-28) and by design runs near its limit; samples on 11-12
    show 3.0–5.5 GB used and no full GCs. Joe offered to double it ("we could
    2x its heap and still have plenty of RAM left"); Zone had 163 GB available
    on 11-12. Joe, 11-12: "If we're already at 8GB on futon1b and not
    pushing the limit there after all then we don't need to 2x it right
    away. But this does raise the interesting point that if we give XTDB a
    lot of memory, Datascript is probably less relevant b/c we'd presumably
    be able to have whatever parts of the database we need to query in
    memory after all." So the Datascript option's one advantage, speed from
    holding the graph in memory, can come from XTDB's own caches given heap.
  - **Order (Joe, 11-12): "memory migration is the priority."** Discovery:
    kimi-2, `holes/labs/M-repl-circus/MEM-migration-discovery-2026-11-12.md`.
  **Real XTQL per use case** (kimi-1, `holes/labs/M-repl-circus/GCC-xtql-2026-11-12.md`,
  `dd04fa305`; two queries rerun by claude-13). What today's futon1b answers:
  point lookups and one-step neighbourhoods, fast (a CTRL "is a linked to c"
  triple lookup in tens of ms; a node's relations in under 200 ms). What it
  does not: reachability (components, cones, gluing stay in Clojure, as
  Giraffe's pass already is), nested-map predicates (post-filter), and
  whole-table aggregates (past a 10 s deadline). What is missing, which is
  the GCC's content:
  - new derived records: occurrences, elements and cascade readings (§16);
    `needs` edges (none in `:relations`); 猴's focus; wants with locator,
    horizon and observation; each turn's lifecycle and per-animal fields
    (turn records are disk files, not in XTDB, which blocks both the coverage
    check and the projector); memory nodes and links (the migration);
    question ↔ turn and citation links;
  - joins that need only an id recorded: live turn evidence ids ↔ the older
    `:operator-turn` entities; hyperedge endpoint strings ↔ entity ids;
    valid time from `evidence/at` at ingest (as-of today means insert time,
    which the memory migration must preserve, see its review).
  So the GCC is mostly a projector writing those derived records into
  futon1b, with reachability computed in Clojure over what XTQL fetches, and
  counts kept as derived records rather than aggregated per query.
- The record types for 狗 (recall, use, buried) and 猴 (focus): schema 1.1.
- How the pass's work pool and `needs` edges become GCC records (today:
  Giraffe run files from holes/tickets edges).
- Who reads the priorities: the War Machine's selection, Joe's view (Mojira),
  or both.
- Cost: a pass per settled turn, or debounced like the pattern-index watcher.
- The CTRL write-up.

## Acceptance checklist (IDENTIFY)

- [ ] For each animal: which GCC node and edge types it writes, and which XTQL queries it reads
  others through. No animal reads another's output by parsing evidence bodies.

- [x] E-evidence-cleanup has closed (gate to MAP).  Closure: `1ccde6689`;
  live admission defences: futon1b `4f43c51`, `fd4efb2`.
- [x] Each animal's trigger, input, output and output store are confirmed against
  the code, not only against this table. Done 2026-11-11: MAP note `ae1a4c632`;
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
