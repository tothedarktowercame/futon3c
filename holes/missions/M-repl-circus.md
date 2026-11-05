# M-repl-circus: the Circus as hooks around the shared REPL

**Status:** OPEN — IDENTIFY (2026-11-05, Joe + claude-8). Progress to MAP is gated
on [E-evidence-cleanup](../excursions/E-evidence-cleanup.md).

**Related:** [M-giraffe](M-giraffe.md) (the loop 驴 → 象 → 狗 → 馬 → Giraffe → War
Machine, §"Giraffe in futonic logic"), [M-象-2000](M-象-2000.md) (象 turn analysis;
P7b is where `:artifact/weak-activation` came from),
[M-formal-agency](M-formal-agency.md) (feedback reaches every participant).

**One-line:** State what each Circus animal hooks onto, what it gives the
participants, and where its output lives. Evidence holds what participants said
and sent. The animals' outputs are references attached to those acts, or records
in their own derived stores.

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
| 猴 monkey | timer (the operator-focus reader; seat currently `zai-1`, to be renamed `猴-1`) | operator focus | derived store |

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

- [ ] E-evidence-cleanup has closed (gate to MAP).
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
