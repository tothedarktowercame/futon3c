# Proposal: one reading per item, links and placement together (2026-11-06)

**This proposal changes a War Machine contract**:
`futon2/holes/labs/wm-contract/SPEC-placement-adapter-2026-11-02.md` names
`A1-placements.tsv` and `classes.tsv` as the placement source. It goes to Joe
before anything is built.

Author: claude-6, from codex-7's discovery report ONE-READING-1
(job invoke-1793979474294-13963-7d1a7caa). Joe's request, 2026-11-06: a
catch-up mechanism from nothing placed to everything placed and prioritised,
kept up to date continuously, and inspectable by him.

## What exists

- **Link reading (Giraffe).** One record per item: status, purpose, edges,
  unresolved. Judged mechanically by the generation's `check_edges.py`
  (pins, vocabulary, verbatim quotes in a six-line window). Kept current by
  the re-read queue: a changed source or a new item queues a reading
  (`reread_queue.clj` `reconcile!`). Working since 44b794f2a and ddd207ae2.
- **Placement reading (ant reads A1).** What it is, open or not, way, wing,
  action, where the need goes, finish, beneficiary, confidence
  (`holes/excursions/ant-prompts/A1-place.md`). Run once, 2026-11-02..04;
  108 rows in `ant-reads/A1-placements.tsv`. Nothing checks a row's fields
  or citations (`placement_adapter.clj:43,67` splits the TSV by header).
  Nothing queues a placement when an item appears or changes.
- **Priority.** Giraffe orders ready items by unblocks, then cone
  (`pass.clj`), written to each run's `pass.edn` (and `priority.edn` with
  focus input) under `~/code/storage/giraffe/<run>/`. The WM then scores
  cascades over those facts by expected free energy (`outer_score.clj`) and
  writes its choice and runner-up to the run record.

## Populations (codex-7's counts, 2026-11-06)

| set | count |
|---|---:|
| distinct M/E/T stems anywhere under `futon*/holes` | 1,279 |
| of those inside the Giraffe corpus | 771 |
| Giraffe corpus members (retained, incl. now-closed) | 801 |
| currently open in Giraffe's census | 574 (330 read, 244 unread) |
| placement rows across the three sheets | 509 ids |
| placed, open, work, with a way (usable by the WM) | 53 |

Joe's "about 2,000" is the older commons-map draw pile (2,683 stems, 2,174
unplaced), which counted worktree copies and labs material. Deduplicated and
restricted to open, live items, the backlog is 574.

## Proposal

1. **One record.** Add a required `placement` object to the Giraffe reading
   record: what, status, ways (up to two), wing, action, need_goes_to (when
   the action passes the need on), finish, beneficiary, confidence, each
   cited claim carrying the same EVIDENCE shape the edges use.
2. **One checker.** Extend the generation's `check_edges.py` with the
   placement vocabularies (ways from the declared ways of working, wings
   from A1-place.md, actions) and the evidence check. It checks shape,
   vocabulary and quotations; whether the way or wing is right stays a
   reviewer's judgement, as A1 intended.
3. **The worker prompt follows the checker** as it does now; no second copy.
4. **Placement consumer.** `placement_adapter` reads the placement
   projection of the pinned Giraffe generation instead of the TSV. The
   existing 108 rows seed it: they are kept as the starting placement for
   their items until a checked reading replaces them.
5. **Catch-up and upkeep are the same queue.** Mark every open item that
   lacks a placement as needing a reading; the queue drains them (about 11
   an hour on the current two readers, so 574 items in roughly two days).
   After that, a changed or new file queues its own reading, links and
   placement together.
6. **Out of scope:** closed items and requisitions. A1 has special rules for
   requisitions; Giraffe excludes them. They stay in a separate history lane.

## What Joe will be able to inspect

A live section on Cascade Live, replacing the 2026-11-02 snapshot sections
"Preparing what can be selected" and "The 47 review rows": one row per open
item with read state, placement (way, wing, action, finish), Giraffe
readiness and rank, its links in and out, and why the WM excludes it if it
does. Data from the current generation and the latest `pass.edn`; nothing
recomputed on the page.

## Build order (small handoffs, each reviewed)

1. Checker: placement fields and vocabularies, with the bad cases tested.
2. Seed: project the 108 TSV rows into the generation as starting placements.
3. Queue: mark open items without a checked placement as needing a reading.
4. Adapter: read placements from the generation (contract change, after Joe).
5. Page: the live Giraffe section on Cascade Live.

## Questions for Joe

- Is the 574 open, live items the right scope, or should closed and lab
  items be placed too?
- May the placement-adapter contract change from the TSV to the checked
  generation?
