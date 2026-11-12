# The Giraffe store: what it is, who feeds it, and what "always up to date" would take

claude-6 for claude-13 and Joe, 2026-11-12. Read-only discovery for M-repl-circus
§DERIVE ("feed the Giraffe store"; "Giraffe is always up to date"). Nothing was
changed: no timer, queue, corpus or run directory was touched, and nothing was
loaded into a shared JVM. Checks made: file listings and sizes; a read-only
SQLite open of the queue; the user journal for `giraffe-reread.service`; two
GETs on futon1b :7073; one separate `giraffe.run --check-fresh` process (which
writes nothing) to time the adapter. Items marked **unconfirmed** were not
checked to the end.

Two corrections to the packet's premises:

- **The timer is not held.** `giraffe-reread.timer` fired every five minutes
  throughout this discovery (cycles at 01:22, 01:27, 01:32, 01:37Z).
- **The edges JSON is no longer the pass's edge source** (§1.4).

## 1. Inventory

| Part | Location | Holds | Source or derived | Size | Growth | Deleted or compacted |
|---|---|---|---|---|---|---|
| Run directories | `~/code/storage/giraffe/<run-id>/` | `input.edn`, `pass.edn`, `manifest.edn`, `excluded.edn`, `rejected.edn`, `report.md`, `work-clusters.json`, sometimes `priority.edn` | derived, all of it | 1.3 GB, 188 runs since 10-27 | 8.2 MB per run; 13–15 runs a day on 11-10 and 11-11 (about 110 MB a day); 43 runs / 290 MB on 11-06 | never (no delete in `giraffe/run.clj`) |
| Readings corpus | `~/notes/wm-next/giraffe-reads-2026-11-04/corpus` → `corpus.generations/<digest>/` | `batch-*.jsonl` (645 reading records), `members.json` (826 members: id, path, sha256), `check_edges.py` and its vocabularies | **source**: agent readings | 594 MB in 441 generations; one generation is about 1.3 MB | one full copy per landed reading: 18–20 a day lately, 186 on 11-06 | never: `delete-tree!` is applied to the staging copy only (`corpus_generation.clj:194,206`) |
| Re-read queue | `~/.local/state/futon3c/giraffe-reread/requests.sqlite` | table `reread_requests`, key (target_id, source_sha): 593 rows: 386 landed, 145 failed, 61 superseded, 1 pending | operational state | 340 KB (+ three `.bak` copies from 11-06/07) | 7–41 rows a day since 11-08 | rows are never removed |
| Mint batch state | same directory, `mint-batch.edn` | pending landings, last mint time and result | operational state | 126 bytes | – | overwritten in place |
| Second-reading outcomes | `~/code/storage/giraffe-second-reading/` | outcomes of one repair round on 11-06 | derived | 68 KB | none since 11-06 | – |
| Edges file | `futon3c/holes/missions/giraffe-mission-edges.json` | 41 hand-reviewed mission edges, last written 10-28 (`be4fcb128`) | source, now unused (§1.4) | 118 KB | none | – |
| Item documents | `holes/missions`, `holes/excursions`, `holes/tickets` in 14 repos (`missions/default-repos`) | the missions, excursions and tickets themselves | **source**: items and their states | – | – | – |
| Shadow demands | `futon3c/data/file-change-hygiene/giraffe-shadow-demands.jsonl` | 183 debounced "recompute Giraffe input" demands since 11-05, each with a freshness probe result | derived, observation only | 134 KB | 3–43 a day | append-only |
| Relationship reviews | futon1b evidence, ids `e-xiang-relationship-<sha1>`; resolutions on the turn record (`:relationship_resolutions`) and as `e-xiang-relationship-resolution-<sha1>` | per-turn proposals that the operator's text concerns work other than the clocked target | derived proposals; a resolution is a **source** (operator verdict) | **unconfirmed** count (§3) | – | – |

### 1.1 The newest run (20261112T003524.498Z-1591e88b)

503 open items, of which 487 have a current reading and 16 are new and unread
(excluded from the graph, listed in `excluded.edn`); 0 have a changed source
awaiting re-read; 324 corpus members are no longer open. The pass reports
487 open, 405 ready, no cycles, 77 theories glued. The edge count was not
extracted (**unconfirmed**).

### 1.2 Priorities

`priority.edn` is written only when a focus relation is supplied
(`run.clj:259`). 28 of the 188 runs have one, the last on 11-07. Every run
since records `:priority-projection {:status :absent :reason
:focus-relation-input-absent}`. So there are no current priorities, and
Giraffe's output today is the readiness order alone.

### 1.3 Source and derived, in short

Source: the item documents; the reading records (one per item, pinned to the
document's sha256); operator resolutions of relationship reviews; the focus
relation (absent). Everything else is derived and can be rebuilt: run
directories, older corpus generations, shadow demands.

### 1.4 The edges JSON

`--edges` is still a required argument (`run.clj:551`) and the file is still
read (`missions.clj:201`, `tickets.clj:188`). In the supported mint
(`--source missions-excursions --readings-root …`,
`reread_mint.clj:73`, `wm_click.sh:234`) its edges are then replaced by the
edges of the checked readings: `(assoc-in [:input :edges] (:edges reading))`
(`excursions.clj:155`). Nothing has written the file since 10-28. The `needs`
edges now come from the reading records, each converted by
`readings.clj:51 convert-edge` after `check_edges.py` accepts the record.

## 2. Writers and readers

| Part | Writers | Trigger | Readers |
|---|---|---|---|
| Run directory | `run.clj:231 write-run!`, via `run.clj:535 -main` | (i) the timer cycle's mint: `reread_cli.clj:119` → `reread_mint_batch.clj:28 step!` → `reread_mint.clj:62 supported-mint!`, which starts a second JVM; minted when readings have landed and either 30 minutes have passed or the queue is idle. (ii) a War Machine click that finds the field stale: `futon2/scripts/wm_click.sh:234`. (iii) by hand | War Machine outer loop: `full_loop_runtime.clj:1437 find-giraffe-run` (newest verified run), `outer_observation.clj:224–227` (reads `manifest.edn` and `pass.edn`), `focus_receipt.clj:311`; `wm_click.sh:192,258` (`--check-fresh`); the watcher's shadow probe `watcher/multi.clj:1377–1411` → `run.clj:499 current-missions-freshness`; `reread_cli.clj:49 health` |
| `priority.edn`, `work-clusters.json`, `report.md` | same `write-run!` | same | no code reader found in futon2 `src`/`scripts`, futon3c `src`/`scripts`/`emacs`, futon4 `dev`, futon0 `scripts` (one grep pass). `report.md` is read by people. Mojira: no reference found |
| Corpus generation | `corpus_generation.clj:215 publish-generation!` (copy the tree, replace one record, validate, swap the symlink) | `reread_worker.clj:249 run-once!` when a reader seat returns a record matching its claim | `readings.clj:79 consume` (from `reread_cli.clj:32 authority-snapshot` every cycle, and from `run.clj:459 build-adapter-result` on every mint and probe); `second_reading.clj:26` |
| Queue | `reread_queue.clj:117 reconcile!` (pending, superseded), `:156 claim!`, `:192 settle!` | the timer cycle (`reread_cli.clj:69 run-once!`); readers are the seats in the unit's environment file (zai-2, kimi-1; one new item per cycle) | `reread_cli.clj:49 health`; the cycle itself |
| Shadow demands | `file_change/shadow_ledger.clj` from `watcher/multi.clj:1646–1675` | a watched change to a task document (`demand_projector.clj:18 task-definition?`), debounced with a maximum wait | none: the comment at `multi.clj:1645` says it "never invokes Giraffe, the registry, or a dispatcher" |
| Relationship review | `xiang/turn_service.clj:449 relationship-review-evidence-entries`, appended at `:567` | a turn reaching `:settled` | the resolve route (`transport/http.clj:12522` → `turn_service.clj:645 resolve-relationship!`); no Giraffe code reads them |

One cycle of the timer, in order: rebuild the census of all 14 repos and
re-check every reading (`authority-snapshot`); reconcile drift into the queue;
one reader per seat; mint if due.

## 3. What the Circus feeds Giraffe, per animal

| Animal | Output at the turn | Reaches a Giraffe input today? |
|---|---|---|
| 驴 | `:lu_ranked` patterns on the turn record | no |
| 象 | reading (`:acts`, `:sentences`), clocked target | only through the relationship-review proposal below |
| 狗 | `:dog_recall`, `:dog_use`, `:dog_buried` | no |
| 馬 | `:alignment`, `:pattern_verdicts` (tensions) | no. The mission table's "proposed tickets" has no writer: no code in `xiang/` or `agency/` turns a 馬 tension into a ticket or a proposal (one grep pass) |
| Giraffe at `:settled` | relationship-review proposals, as evidence at :7073; operator resolutions on the turn record and as evidence | no. The pass reads items and reading edges only; a resolved relationship ("this work needs that work") is written where no pass reads it |
| 猴 | operator focus | no writer exists (MAP note), so the priority projection has had no input since 11-07 |

So nothing a settled turn produces reaches the pass or the priorities. The
only live path into Giraffe is: a document under `holes/` changes → (up to
five minutes later) the timer notices the digest differs → a reader seat
re-reads it → the reading lands in the corpus → the next mint.

Count of relationship-review entries at :7073: **unconfirmed**. Two GETs on
`/api/alpha/evidence` with `tags=` and `author=` filters returned zero rows
with `:scanned 0`, which reads as a filter I did not match, not as an empty
set; I made no further probes. No turn record under
`~/.emacs-graph/session-turn-analysis/` contains `relationship_resolutions`,
so no review has been resolved.

## 4. Real time: the smallest event-driven design

### (a) A changed document mints its own re-read

The watcher already does the first half. `demand_projector.clj:26 project`
turns each watched change to a task document into a pending Giraffe demand,
debounced, and `due-demands` (`:81`) emits it; today the sink only appends to
the shadow ledger. The smallest change is a second sink that, for each path
in `:changed-paths`, computes that one document's sha256 and state group and
upserts one queue row (`reread_queue.clj:93 ensure-pending!` with the
supersede rule beside it), without rebuilding the census of 14 repos. A new
row wakes a reader on an executor of its own (the reader call blocks for
minutes); the timer goes.

What this needs that does not exist: an index, held in the futon3c JVM, from
item id to (path, sha256 of the document its reading was made from), built
once from the item and reading records and updated by the two events below.

### (b) Landed readings and settled turns update Giraffe's inputs

What moves into futon1b as GCC records:

| Record | Identity | From | Replaces |
|---|---|---|---|
| item | item id (`M-…`, `E-…`, `T-…`) | the watcher, per changed document: repo, path, sha256, declared status, state group | the per-cycle census |
| reading | (item id, source sha256), with the checker sha and the certificate hash | the reader's accepted record | a `batch-*.jsonl` line and the `members.json` row |
| `needs` relation | (from item, to item, class ∈ closure, artifact, alternative), pinned to the reading that asserted it | `readings.clj:51 convert-edge`; and an operator resolution of a relationship review whose verdict is a dependency | the `:edges` of `input.edn`; GCC-xtql note UC3 found zero `:needs` rows |
| re-read request | (item id, source sha256), state, claimant, lease | the sink in (a) | the SQLite row |
| focus relation | per operator, valid-time | 猴, when it exists | the `--with-retained-focus` argument |
| pass result, priorities | one current record each (earlier versions stay in XTDB history) | (c) | `pass.edn`, `priority.edn` in a new directory per run |

A new reading for an item is a new version of that item's reading record and
a replacement of its `needs` relations, so the 441 full-copy corpus
generations (594 MB) are no longer made: history is the store's.

What stays a file: the item documents (the source), `check_edges.py` and its
vocabularies (code), and a run directory written only as an export when a
reader still wants one. The War Machine does: `outer_observation.clj:224`
refuses a run without `manifest.edn` and `pass.edn`, and
`wm_click.sh` already mints one itself when its probe says stale. So the
smallest step keeps `write-run!` for that caller and stops the re-read path
from writing directories; moving the War Machine onto the pass record is a
separate change in futon2.

The settled turn contributes two things: a resolution of a relationship
review becomes a `needs` relation (or a typed "not a dependency"), and 猴's
focus becomes the focus relation. Both are small projector outputs at
`:settled`. 馬's tensions as proposed tickets need a writer first (§3).

### (c) Pass and priorities recomputed incrementally

The pass itself is a function of items and `needs` relations
(`pass.clj:316`), over about 500 open items. The cost today is in building its
input, not in the pass (§5). With items and relations held as records and
mirrored in memory in the futon3c JVM, each change to an item or a relation
recomputes the pass in process and replaces the one pass record. A change to
the focus relation recomputes only the priority projection
(`priority.clj:13`, a sort of the pass order). No directory is written.
Whether the pass needs to be made incremental within itself, or can simply
be rerun whole per event, depends on its run time over the in-memory input,
which I did not measure separately (**unconfirmed**; see §5).

### The invariant: "Giraffe is always up to date"

All three clauses reuse digests the code already computes.

1. **Readings are current.** For every open item, with D the sha256 of its
   document now: a reading with source sha D exists, or a request for
   (item, D) is pending or claimed and younger than bound B1. Digests: the
   item's `:source-pin :sha256` against the reading's `source.sha256`
   (the comparison `excursions.clj:82 reading-drift` makes). Data for B1:
   of 75 requests landed since 11-08, the median time from request to
   landing was 11 minutes and the longest 144 minutes.
2. **The pass is current.** The pass record's input digest equals the digest
   of the current items and `needs` relations (the `input-sha256` of
   `run.clj:440 check-freshness`), or the pass is younger than the newest
   item or relation change by no more than bound B2 (the debounce plus one
   pass).
3. **Priorities are current.** The priority record was computed from the
   current pass record and the newest focus relation.

Against today's data: clause 1 fails for 16 open items that have no reading
(the unit allows one new item per cycle) and 145 requests sit in `failed`
(many withdrawn in bulk by the operator on 11-07, so "failed" needs a typed
split between withdrawn and to-retry before the clause can be checked);
clause 2 held at the 00:35Z mint and failed again by 01:30Z (shadow ledger:
the probe computed a different input digest after two mission documents
changed); clause 3 cannot hold while no focus relation exists.

## 5. Cost

| What | Measure | Source |
|---|---|---|
| One timer cycle | CPU: median 137 s, 90th percentile 244 s, maximum 867 s; 15 of 506 cycles above 8 minutes (the cycles that mint, which start a second JVM) | journal, 11-10 to 11-12 |
| One timer cycle, wall | median 196 s, 90th percentile 574 s | journal, from 11-11 12:00Z |
| The timer in total | 11.2 CPU-hours a day; 1.1 GB peak memory per run | journal |
| Freshness probe inside the futon3c JVM (adapter build and digest; no JVM start) | wall: median 108 s, 90th percentile 226 s, over 183 probes | shadow ledger, started-at to completed-at |
| Freshness probe as its own process | wall 168 s; CPU 126 s user + 64 s system; 1.0 GB peak; result `fresh: false` (adapter build, pass and digest together) | this discovery, `/usr/bin/time` |
| Request to landed reading | median 11 min, maximum 144 min (75 landings since 11-08) | queue |

Reading of these:

- Building the input costs about two minutes even in a warm JVM, and the
  timer builds it at least once per cycle (twice when it mints). Where the
  two minutes go is **unconfirmed**; the candidates are the checker, which
  is started as one `python3` process per reading record
  (`readings.clj:29–39`, 645 records), and the census reading documents in 14
  repos. The 64 s of system time in my probe, a third of its CPU, is what
  several hundred process starts would look like.
- Event-driven recomputation with the present adapter is already cheaper
  than the timer: the watcher's debounce produced 183 demands in eight days
  (about 23 a day; 43 on the busiest day), so about one CPU-hour a day
  against 11. It does need the debounce it has.
- Per settled turn (about 77 operator turns a day) a two-minute rebuild in
  the shared JVM is not affordable, and is not needed: a settled turn changes
  at most a relation or the focus, so with the inputs held as records it
  costs one pass or one sort.

## Not confirmed

- The number of relationship-review entries at :7073 (§3).
- The edge count of the newest run, and the pass's own run time apart from
  the adapter.
- Where the adapter's two minutes go.
- That nothing outside the greps reads `priority.edn`, `work-clusters.json`
  or `report.md` (one grep pass over the directories named in §2).
- Whether Mojira reads anything of Giraffe's (no reference found; I did not
  locate Mojira's code to check from its side).
