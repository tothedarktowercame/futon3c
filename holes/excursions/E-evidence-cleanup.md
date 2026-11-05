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
