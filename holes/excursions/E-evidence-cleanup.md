# E-evidence-cleanup: take derived records out of the Evidence Landscape

**Status:** OPEN (2026-11-05, Joe + claude-8). **Gates** [M-repl-circus](../missions/M-repl-circus.md)
IDENTIFY → MAP: there is no point developing the Circus further while its output
swamps the evidence it reads.

**Stop-the-line finding** (Joe, 2026-11-05). Working note:
`~/notes/stop-the-line/2026-11-05-evidence-derivatives.md`.

## The finding

`M-x my/evidence-landscape-search` for "hearo" (a word from Joe's 11-04 turn)
returned 32 hits in the default 24-hour window. None of them was the turn. The
original (`emacs-a7db4b8f30c532f98faca17f6f408589`, 2026-11-04T03:34:29Z) had aged out
of the window, while derivative writers kept writing copies of its text into new
records up to 22 hours later. Searching all time finds the original among 58 hits.

## Measured (live FTS sidecar `futon1b/migration-store-21/fts5-evidence.db`, 11-04 05:45Z to 11-05 05:45Z)

| Writer | Type | Records | MB | What it stores |
|---|---|---|---|---|
| all | | 13,483 | ~53 | |
| `joe` | operator turns | 90 | 0.05 | the turns themselves |
| claude-14 | `:promise/*` lifecycle | ~1,400 (202 promises × ~7) | ~27 | scheduler state |
| `futon3c/artifact-activation` | `:artifact/weak-activation` | 601 | 6.6 | invoke prompt (≤64 KB) + MiniLM pattern hits |
| `wm-author` | `:coordination :step` | 962 | 2.2 | Zai transcript of the WM author seat: in a 200-record sample, 167 `:turn-round`, 13 `:turn-start`, 2 `:turn-commits` (writer `futon3c.agents.zai-api`); 18 tagged `:clock-decision` |
| 象-1 | `:coordination :step` | 556 | 2.0 | Kimi seat transcript, full prompt at turn start |
| `zai-1` (猴) | `:coordination :step` | 348 | 1.0 | operator-focus reader transcript, re-quotes batches of turns |
| `xiang-turn-service` | `:memory` | 82 | 0.9 | turn records with "what happened" summaries |
| 馬 | `:observation` | — | — | summaries of zai-1's turns (`scripts/ma_observe.py`) |
| Giraffe | relationship-review proposals | — | — | one per M-/E-/T- reference in a settled turn (`holes/labs/giraffe-observation-series/README.md` Step 1) |
| agent seats | `:coordination :step` | — | — | tool-step traces, "result-preview" copies of replies |

## Not bad ideas, misplaced ones (Joe)

`:coordination` steps and `:artifact/weak-activation` are reaching for what
M-repl-circus describes. They are badly placed and badly shaped, not pointless.
For each writer, keep what it computes and change where it puts it:

- **artifact-activation.** Keep the pattern hits; they are 驴's output. Stop storing the
  prompt text. The hits become metadata on the operator turn, or a derived record keyed
  by turn evidence-id.
- **Seat transcripts (象-1, 猴).** These are Circus machinery, not dispatches. Their
  prompts reference turns by evidence-id instead of copying them, and they live
  outside evidence.
- **馬, Giraffe, 象 turn records.** Move to derived stores keyed by evidence-id.
- **Agent `:step` traces.** The reply is the evidence. Steps are operational logs.
- **Promise lifecycle.** Scheduler state: one record per promise, outside evidence.
- **`wm-author :coordination`.** Read 11-05: the same Zai transcript writer, here logging every tool round of the War Machine author seat. The WM's dispatch to the seat and the seat's reply are evidence (part 3). The rounds between them are a tool-step trace. The 18 `:clock-decision` records need a separate look.

## Method: a diagramprover flow map (M-diagramprover R0, 2026-11-05)

Clocked under M-diagramprover. R0 (`src/futon3c/diagramprover/wiring.clj`) ingests boxes
with `:reads`/`:writes` and reports `written-never-read`. Model each evidence writer and
each evidence reader as a box, with wires typed by evidence type/author/tag. A record type
whose only reader is generic text search has no reason recorded in code, which is the
"why is this stored" question asked mechanically. A type with readers has its reason
in those readers, and they are what has to move with it. R4 (`causal/*`) is for the
later step: a causal model of how the GCC should be built from evidence.

## Step 1 result: R0 flow map (codex-12, `6d6882a89`; reviewed by claude-8 2026-11-05)

`holes/labs/E-evidence-cleanup/FLOWMAP.md` and `evidence-flowmap.edn`. `written-never-read`:
artifact-activation, claude-N `:step` traces (`scripts/claude_ws_invoke_bridge.clj:243`),
Giraffe/象 relationship-review proposals, and the 象-1 transcript. Writers that have readers:
- the zai-1 transcript, read by 馬;
- the wm-author transcript, read by its own transcript bug scan (`zai_api.clj:1697`);
- the promise lifecycle, read by the obligations API (`agency/obligations_reader.clj`);
- 馬, read by the `xiangma_port_export.py` and `xiang_turn_check.py` scripts;
- xiang-turn-service, read by `xiangma_port_export.py`.

Review notes:
- Checked: the commit touches only these two files. All eight known writers are present.
  `zai_api.clj:1697`, `operator_focus.clj:228`, `claude_ws_invoke_bridge.clj:243` and
  `promise_history.clj:86` say what the rows claim. Volume matches `ev_attr` (artifact-activation
  595, wm-author 962; the 601 earlier came from a join on `ev_fts_content`).
- **Coverage is partial.** Only 10 writers are boxes. The broad rows (tickle and the work queues,
  logic/*, peripherals, scripts, bells/whistles) are listed but not modelled, so the 4 findings
  are a lower bound. The table itself says the tickle/work-queue writers have "none found beyond
  generic search", but they cannot appear in the output.
- `read-never-written` on `:evidence/obligation`, `:evidence/pattern-outcome` and
  `:evidence/dispatch-graph` means reader and writer field names were not reconciled.
  For promises, the "has a reader" conclusion comes from the table, not from the run.
- A reader that is itself a Circus animal (馬 reading zai-1) or the writer reading itself
  (the wm-author bug scan) explains why the record is kept, but it does not justify keeping it
  in evidence. Writer and reader move together to a derived store.
- The promise lifecycle has a real outside reader (the obligations API), so it is the clearest
  case of state that belongs in its own store with that reader repointed.

- **Coverage rerun** (codex-12, `20d1ee7dc`, reviewed by claude-8). All writer families are now boxed,
  and the orphan reader wires are reconciled (`read-never-written` = only `:generic-search`).
  `written-never-read` rose from 4 to 40 fields. Two reasons not to read 40 as "40 kinds of spam":
  (1) only 7 readers are boxed. Readers listed in FLOWMAP.md (inbox_zero, ways-of-working-probe,
  turn_frames, scope_view_data, …) are not modelled, and **id-based reads are invisible to a filter
  search**: `peripheral/memory_recall.clj:149,175` reads memory evidence by id via `:memory/assert`
  hyperedges, so `:evidence/memory` is probably read. The 40 is now an upper bound.
  (2) Bells, whistles and dispatches are in the 40, and under the evidence model they ARE evidence.
  For them, being unread means feedback reaches no one (M-formal-agency). It does not mean
  they should be removed. Reader coverage is out to codex-12 (`invoke-1793897693931-13040-a1646c6f`).

- **Reader coverage rerun** (codex-12, `3cad28042`, `7b24f4849`; reviewed by claude-8). Filtered,
  session-scoped and by-id readers are now boxed, including the `:memory/assert` hyperedge join
  (`memory_recall.clj:139-184`, plus three apm stores). That join removes the memory, gate-memory
  and math-memory wires from the list. `written-never-read`: **30**. Of those, participant acts:
  `bell-arrival`, `scheduled-dispatch`, `test-bell`. Derived/operational: the rest (tickle-*, logic-*,
  the work queues, peripherals, artifact-activation, relationship-review, zaif-arm-choice).
  Review: agent `:step` and the transcripts dropped off the list only because two **session-wide**
  readers fetch everything in a session window. What they keep differs:
  - `ways-of-working-probe.py` keeps only the final assistant row per operator turn
    (`join_operator_replies`, :89). For it, steps and transcripts are pagination load, not input.
  - `inbox_zero/infer_adapters.clj:125-140` scans every body for mentions of a file path. It does
    use transcript text, to infer which files a seat touched. That is a real need served by
    text-mining JSON, and in the GCC it is an agent→file edge.
  So "read" here overstates use for the first reader and is accurate for the second.

- **Design records** (codex-12, `c843fb523`; reviewed and one row corrected by claude-8). This answers
  Joe's "why": `holes/labs/E-evidence-cleanup/DESIGN-RECORDS.md`. Most derived writers have **no
  stated reason for using evidence**. Where a purpose is stated, it says what the record is for
  (mining input, a watchdog, a tracer), not why evidence is the place for it. The only writer with a
  stated reason to be in evidence is the promise lifecycle (M-象-2000 P2a/P2b): the promise state
  lives in `/tmp` and is lost at boot, so its transitions were written to evidence to make that
  state rebuildable. The need is real, a durable event log; evidence is the wrong home for it.
  Review: `235d96cfe` and `d9dd89c9b` check out. The promise row had cited the 09-28 outbox commit
  and missed the 09-27 introducing commit `e8a4d1014` and its purpose; corrected in place.

- **Promise lifecycle move, discovery** (codex-12, `ae87cd9e8`; reviewed by claude-8): `PROMISE-HISTORY-MOVE.md`.
  - Destination: immutable typed hyperedges (`promise/history-event`), with an adapter that keeps
    the chain checks and ordered reads. futon1b has no route for an ordered event log; its
    entities are upserts.
  - Migration in five steps, none of which leaves a reader without data: dual write, dual-read
    comparison, repoint readers, switch outbox acknowledgement, stop the evidence writes.
  - Volume: about 7 rows per promise, because the ready inbox and delivery state change
    separately. The bytes come from copying the full record and payload into every row.
  - Checked: `obligations_reader`, `http.clj:11170` (`tags=promise-history&type=promise/woken`)
    and `ways-of-working-probe` are present. `/tmp` authority and "rebuild == live" are kept.
  - Step 1 (dual write) dispatched to codex-13.

- **Promise move step 1, dual write** (codex-13, `d775f31f2`; reviewed by claude-8, fix `ecbd715bb`).
  Outbox rows go to evidence and to an immutable `promise/history-event` hyperedge (new
  `agency/promise_log.clj`). A row is acknowledged only after both readbacks match. The bad-case
  test (hyperedge store down → row kept, recovered on the next drain) is good. Its stubs could
  not show the real store, so it was checked live: a probe edge written through futon1b reads
  back `same-edge? true`, and the probe was retracted afterwards.
  **Defect found in review:** futon1b does not store nil-valued keys. A first-in-chain row has
  `:history/predecessor nil`, so it never matched its stored copy. Pre-existing effect: 13 rows
  stuck in the outbox since 10-28 and 16,249 `:conflict`s. d775f31f2 sent fresh appends through
  the same comparison, which would have stuck every new `park-made`. Fixed by comparing present
  keys only. The new test fails on the old comparison (2 failures) and passes on the fix. 4
  namespaces: 46 tests, 200 assertions, 0 failures. Live-loaded both namespaces from master about 17:10Z. Verified live: the 13 stuck rows and 2 new
  ones drained by 17:13Z (outbox 0, written +15), `:conflict` stopped at 16,288, and
  `promise/history-event` hyperedges are readable in futon1b. Drains run at roughly 4–6 s per row
  (the hyperedge POST dominates), which is enough for about 2,400 rows a day. Watch for bursts.

- **Promise move step 2, dual-read comparison** (codex-13, `53e77f85c`; reviewed by claude-8, cutover
  instant corrected in `c10953dd8`). `promise_log/read-history` (bounded, paginated, current or as-of)
  and `promise_history_compare/compare-history`. Evidence stays the authority. First live run (17:17Z;
  evidence since 17:00Z against the whole log): **0 mismatched** over the rows present in both. The 10
  evidence-only rows are 17:10:50–17:11:04Z, written before the reload took effect, so the cutover
  constant moved from 17:10 to 17:12Z. The 13 log-only rows are the 10-28 rows that were stuck and are
  now drained (outside the evidence window). Chain gaps on both sides are window edges. Only a handful
  of rows are common so far, so a timer park (`park-32c37866-…`, +3 h) reruns it over a larger
  window before step 3 repoints readers.

## Step 2 progress

- **artifact-activation stops copying text** (codex-13, `f75fb0fff`; reviewed by claude-8 2026-11-05).
  The body keeps `:text-sha256`, the hits, cited and weak lists, and the retrieval descriptor.
  `:text` and `:text-truncated?` are gone. Replay of legacy records that still hold `:text` returns
  `:existing` and does not conflict, because `comparable-entry` ignores the two retired keys.
  The bad-case test feeds 11 KB plus a marker; under the old 64 KB cap that marker would have
  been stored, so the test fails on the old code. Ran `artifact-activation-test` once (first
  run of a new test): 11 tests, 40 assertions, 0 failures. Live-loaded from master
  16:53Z; the running JVM's `activation-record` returns a body without `:text`. Checked in
  the store: the 5 records written 16:56–16:58Z have `:text-sha256` and no `:text` (about 2.7 KB each).
  It still writes to evidence. Moving the hits onto the operator turn as metadata waits on
  M-repl-circus's store decision.

## Live case: automated prompts filed as Joe's turns (claude-14, 2026-11-05)

claude-14 was assembling Joe's apm-lean operator prompts as a public "making of"
(`~/notes/apm-making-of/SURVEY.md`). Park wakes, `Continuation:`, `REVIEW CHECKLIST`,
`PRECONDITION: awaited`, supervision and build packets are stored as author `joe`, role
user, with a turn-id, and no field separates them from typed turns. About 485 of 1,552
turns clocked to M-apm-* open with an automated header.

Checked by claude-8 (sidecar, author `joe`, by opening header): none before July; 23 in
July; about 611 in August; about 263 in September; 132 in October, all on or before
10-01T20:40Z. The `parked-resume` author starts at 10-01T20:49Z (6,027 records since), and
no header-matched prompt is filed as `joe` after it. So the writer was fixed on 10-01, but
the July–October back catalogue is still mislabelled, and the header test is incomplete
(claude-14 found prompts it misses). Correction (claude-14, checked by claude-8): the origin field already exists.
Chat-turn docs carry `evidence/origin` with `:kind` operator | harness | agent | unknown from
2026-09-27T18:38Z on. Among author-`joe` sidecar records: Sept (from 09-27) 631 operator, 505
harness; Oct 2,956 operator, 156 harness; Nov 432 operator, 0 harness. (claude-8's first count
read 4 and 2 harness: its regex looked for `:kind` within 80 characters of the origin key and
missed maps that list `:attributed-author "joe"` and `:actor "parked-resume"` first.) The
sidecar's author column holds the attributed author, so harness turns were indexed as `joe`
until 10-01T20:49Z. So after 10-01 the two signals agree. The open problem is the ~19,000
Joe-authored turns before 09-27, which have no origin. claude-14's rule classifier (`~/notes/apm-making-of/classify.py`, method in SURVEY.md),
scored on 4,670 labelled turns: 646/654 harness caught, 1/4,016 operator flagged (itself a
wake carrying an operator label). Pre-09-27 accuracy rests on a 60-turn hand sample, not
re-sampled since its last rule fix. No labels written to the store. Step 4 (existing records) now includes
whether that classifier's labels are written back, and where: an attribute on the existing turn
(evidence is immutable) or a derived store keyed by evidence-id.

## Steps (each a small handoff, with review between)

1. **Discovery, per writer.** Find the design record that put it in evidence and what
   it argued, and every consumer that reads these records back. Say plainly where no
   such record exists. Do not change code in this step.
2. **Redirect, one writer per handoff.** Start with artifact-activation, which is
   the smallest change for the largest share of copies.
3. **Search.** Once writers stop copying, check that a search for a phrase from an
   operator turn returns the turn itself. Decide whether the 24-hour default window
   should apply to when a record was written or to when its source was said.
4. **Existing records.** Decide what happens to the copies already written. Evidence
   is immutable, so this needs Joe's call: leave them, filter them out of search, or
   migrate them.

## Exit criteria

- [ ] No writer copies operator-turn or reply text into a new evidence record.
- [ ] Every derived writer listed above either writes to a derived store or attaches
  references to the act it belongs to.
- [ ] `my/evidence-landscape-search` for a phrase from a recent operator turn shows
  that turn, and shows no copies of it.
- [ ] Per-day record count and volume measured again and compared with the table above.
- [ ] Decision recorded on the copies already written.
