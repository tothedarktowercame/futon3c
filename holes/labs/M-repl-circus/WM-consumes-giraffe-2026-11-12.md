# How the War Machine consumes Giraffe (today), and what the reorganised store would change

zai-1 for claude-13 and Joe, 2026-11-12. Read-only discovery: no clicks, no
runs, no WM state changes, no writes to :7073 (no timed-q reads were needed —
everything below comes from code, run directories, and the wm-run records on
disk). Companion to `GIRAFFE-store-2026-11-12.md` (§2 names the War Machine
as the only code reader of the run directories). Sources: futon2
`src/futon2/aif/` and `scripts/wm_click.sh` at HEAD 2026-11-12; the newest
selection records under `futon2/data/wm-runs/` (2026-11-07, the most recent
clicks that exercised the outer loop).

## 1. Exactly what the War Machine reads from a run

### 1.1 Which run, and the verification before any read

- `full_loop_runtime.clj:1437 find-giraffe-run` — takes `:outer/giraffe-run-dir`
  if given (verified), else the **newest directory under
  `/home/joe/code/storage/giraffe`** (name-descending, i.e. the timestamped
  run id) that passes `verified-giraffe-run?` (`:1427`): `manifest.edn`
  parseable, its edge source path under `holes/missions/` and no
  tickets-index (so a *missions-giraffe* run, `:1421`), and
  `futon3c.giraffe.run/verify-run` returns `:ok?`.
- `outer_observation.clj:224–227` `giraffe-field` — refuses the click
  (`:giraffe-run-unverified` / `:giraffe-run-incomplete`) unless
  `manifest.edn` and `pass.edn` are both files. This is the refusal line the
  packet cites.

### 1.2 Files, fields, and where each field lands in selection

| File | Fields read | Read at | What it changes in selection |
|---|---|---|---|
| `manifest.edn` | `:run-id`; `:files` row for `input.edn` (sha pin); `:reading-drift :drift :reading-source-changed` (id set); `:priority-projection` (status+sha, via `readiness-priority-source`, `full_loop_runtime.clj:319`; today always `:absent :focus-relation-input-absent`, last real one 11-07) | `outer_observation.clj:229–249`; `focus_receipt.clj:305 read-graph-input`; `outer_menu.clj:136–139` | run identity pins (`:giraffe/run-id`, `:manifest-sha256`); drift ids become **exclusions** (`:reading-source-changed`); the priority projection would become the readiness-priority source — it never resolves today |
| `pass.edn` | `:order` rows, each `:id :status :cone :unblocks :rank :rank-basis :graph-terms :completion :dependency-reading` (whole-row select-keys, `outer_observation.clj:292`) | `outer_observation.clj:230–233` | the **field**: order preserved as enumeration; `:status` gates the kernel's readiness steps and drives exclusion typing (`:completion-definition-not-found`, `:unread-new`); `:unblocks`, `:rank`, `:cone` are the kernel's ordering steps (`:by-unblocks` `:by-rank` `:by-cone`) and its G-scoring discovery-gain terms (`outer_score.clj:142–149, 211, 268–286`); `:rank-basis` labels the brief row (`:computed-before-reading` note, `outer_select.clj:78–119`); the row's `:dependency-reading :record-pin :sha256` is the per-item **reading pin** whose absence refuses (`:giraffe-row-pin-unavailable`, `outer_observation.clj:288–290`); `:graph-terms` name the producer of the ranking terms (`outer_score.clj:102`) |
| `input.edn` | the needs/edges graph + graph parameters (`:graph-hop-bound`, `:graph-link-kinds`, `:graph-link-bases`) — only after re-verifying its sha against the manifest row | `focus_receipt.clj:305–335` (`read-graph-input`, memoised per run dir), used by `giraffe-graph-path` (`:338`) | the **receipt's justification path**: nearest declared relation row from the chosen target over the pinned graph (`:407, :503`) — why-this-pick evidence, not selection input |
| `excluded.edn`, `rejected.edn`, `report.md`, `priority.edn` (when present), `work-clusters.json` | — | — | **not read by any War Machine code**. The WM builds its own exclusions (§2) and never sees Giraffe's |

Where the pins surface downstream: the observation carries
`:giraffe/run-id`, `:pass-sha256`, `:input-sha256`, `:manifest-sha256`
(`outer_observation.clj:311–321`) into the selection record and the Lean
kernel pins (`:kernel`, `:reading`, `:graph` — `outer_menu.clj:136–139`,
`outer_select.clj:442`, `full_loop_runner.clj:10607–10643`), and
`outer_argue.clj:83–139` argues over the same `:giraffe` fact block
(`giraffe.cone/unblocks/rank`, prerequisites) when the kernel is contested.

### 1.3 One recent click's receipt

`futon2/data/wm-runs/selection-record-2026-11-07-wm-click-70f995eb-….edn`
(the last date with exercised clicks; chain-index 1, `:status :chosen`):

- consumed run `20261107T075606.546Z-c0a9ec59`, `:giraffe/pass-sha256
  "eb9af32f95c94f30b8f4d984f10801af9809d6ed7cb75e371105908e6fd0f4f4"`;
- field counts in the record: `:counts {:read 454, :unread 362}`;
- typed exclusions present in the record: `unread-new` 588 occurrences,
  `reading-source-changed` 360, `completion-definition-not-found` 164,
  `target-under-edit` 0;
- example fact blocks: chosen-adjacent row `{:rank 1, :status :ready,
  :unblocks 2, :cone 2, :rank-basis :after-reading, :prerequisites []}` and
  the eventual pick `"M-action-cost-modelling"` whose row carries
  `{:rank 324, :status :undetermined, :reason :prerequisite-unresolved,
  :prerequisites [nil nil nil], :evidence {:line 4, :path
  "…/holes/missions/M-action-cost-modelling.md", :quote …}}` — note the pick
  is **not** the rank-1 ready item: the kernel ordered by its own steps over
  the facts (unconfirmed which step won this click; the kernel-input block
  is in the record for replay).

## 2. What it does NOT use, and what it re-derives

Not used (present in runs, no futon2 reader — matches GIRAFFE-store §2's
grep pass): `priority.edn` (the readiness-priority source reads the
manifest's projection, which has been `:absent` since 11-07; the file
itself is written only when a focus relation exists), `work-clusters.json`,
`report.md`, `excluded.edn`, `rejected.edn`, readiness *reasons* as Giraffe
states them (the WM re-types them, below), and the `needs` edges' `met`
status as a notion — Giraffe does not compute `met` today (M-giraffe WM-2);
the WM sees readiness only as the pass's `:status` strings.

Re-derived by the War Machine itself:

- **The open population** — from its own registry authority
  (`registry-entries`/`open-entries`, `outer_observation.clj:238–243`), not
  from Giraffe. The pass's order is only the *enumeration order*; open
  tasks with no pass row are appended (`:244–247`).
- **Exclusions** — re-typed per task from registry + pass + manifest drift:
  `:reading-source-changed`, `:completion-definition-not-found`,
  `:unread-new`, `:target-under-edit`, or the declaration's
  `:source-absent` (`:252–292`), each validated and pinned. Giraffe's own
  `excluded.edn` is never consulted.
- **Reach** — `task-reach` maps the pass's `:status` to a reach value
  through an *empirical kit* pinned as
  `:giraffe-status-readiness-prior-v1` (`:137–145`): the WM, not Giraffe,
  decides what "ready/blocked" means for contact.
- **Ordering and G** — the kernel's status gates and `:by-unblocks` /
  `:by-rank` / `:by-cone` steps (`outer_score.clj:133–149`) plus
  discovery-gain from `:unblocks` (`:211, :278`) run over the facts; the
  pass's own order is not the selection order.
- **Dependency paths** — `giraffe-graph-path` walks `input.edn` itself,
  applying its own hop bound and link-kind/basis filters
  (`focus_receipt.clj:338–345`), rather than asking Giraffe for a path.

## 3. Staleness

The click, not the WM runtime, decides freshness — at click preparation
(`wm_click.sh:175 giraffe_field_freshness`, once per launch, guard at
`:164`):

1. **Probe** (`:192–198`): run `futon3c.giraffe.run --check-fresh`, which
   rebuilds the adapter input from the same sources and compares its sha to
   the newest *verified* field run's `input.edn` sha
   (`run.clj:449 check-freshness`; `newest-field-run` `:430` re-verifies).
   `fresh: true` ⇒ fire on that run (`:208–209`).
2. **Stale ⇒ mint once** through the supported `-main` (`:232–243`), with a
   fast path: if the mint's printed input sha equals the probe's, no
   re-probe (`:245–254`); else full re-probe and record what the field
   actually is (`:256–270`).
3. **Every failure is a FINDING, not a stop** (`:199–231`): probe throws
   (population drift is daily), a click already running, a dirty futon3c
   checkout (minted git-sha would not resolve), or a failed mint — in each
   case the click **continues on the newest verified field** and records its
   id. `GIRAFFE_MINT=0` skips the gate entirely (hermetic tests).
4. Inside the loop, `find-giraffe-run` re-verifies whatever is newest at
   selection time; a run that fails verification is skipped silently in
   favour of an older one. `previous-giraffe-observation`
   (`full_loop_runtime.clj:1544`) reuses a prior observation only when its
   `:giraffe/run-id` matches the current run — a stale-field continuation
   still gets a typed `:previous-giraffe-run-absent` path (`:1616`).

So "fresh enough" = *the pinned input digest of the newest verified run
equals a freshly computed one*, checked once per launch, degraded to "newest
verified" on any failure. There is no age bound on the run itself beyond
that digest comparison.

## 4. Against WM-1..WM-5

- **WM-1 (every eligible item read)** — *not met today*: the newest run
  (§1.1 of GIRAFFE-store) has 16 unread-new open items, and the click's
  receipt above shows 588 `unread-new` exclusion rows against 454 read.
  Reorganised: readings as records keyed (item, source sha) with
  supersession make "current reading or a request younger than B1" a query
  over records (GIRAFFE-store §4b); event-driven freshness (§4a) replaces
  the one-new-item-per-cycle timer. **Read:** an XTQL query over item +
  reading records (or the sidecar view "current reading per item"), the
  same data `reading-drift` now derives per mint.
- **WM-2 (readiness before selection)** — *partially met*: the pass's
  `:status` does gate the kernel, but readiness is a pass-internal string
  with no `met` computation, and `:prerequisite-unresolved` rows (see the
  pick above) still reach the top. Reorganised: `needs` relations with
  `met` computed as a derived record (GCC load design, "Recomputed" row)
  give a Definition of Ready the WM can gate on directly. **Read:** the
  `met` column of the needs-relation records / a sidecar readiness view —
  replacing the empirical `giraffe-status-readiness-prior-v1` mapping.
- **WM-3 (tickets can be selected)** — *not met*: tickets enter only as
  pass rows via the readings source (`include-tickets` when
  `readings-root`, `run.clj:482`), and the WM's registry is
  missions-shaped; ticket wants found on the fly have no locator. 
  Reorganised: tickets as substrate entities (a named substrate fix in the
  GCC load design — "tickets (155 …) have no entities and no writer") with
  wants as records. **Read:** entity lookup by external id `T-…`, then the
  item's wants records — no file exists for this today.
- **WM-4 (the choice can say why; a recorded pass and priority)** — *half
  met*: the pass is pinned (`:run-id`, `:pass-sha256`, `:input-sha256`,
  `:manifest-sha256`) and cited, which satisfies "recorded, not
  recomputed". But **nothing reads Giraffe's priorities** — the priority
  projection has been `:absent` since 11-07 and no 猴 writer exists, so the
  readiness-priority source always refuses absent
  (`full_loop_runtime.clj:326–328`). Reorganised: the pass and priority as
  one current record each in XTDB history (GIRAFFE-store §4b table); the
  party orderings become focus-relation records. **Read:** the current pass
  + priority records by valid time (the record replaces both `pass.edn`
  and `priority.edn`).
- **WM-5 (dropped items leave the field with their reason)** — *not met*:
  the WM re-derives exclusions per click from drift/under-edit/declarations
  (`§2` above); Giraffe has no dropped state and its `excluded.edn` is
  unread. Reorganised: dropped as a state on the item entity (state groups
  computed from the entity's status, per the substrate-fix list), so the
  exclusion survives the click. **Read:** the item entity's status/state
  group column — today's `:registry/non-open` select
  (`outer_observation.clj:303`) already points the way.
- **Item → question links** (the L-lens, but WM-visible worth): no reader
  today. Reorganised: model-made `serves` links as loaded judgements; a
  sidecar view "questions an item serves" would let the brief cite them.

## 5. The smallest change to read the reorganised store instead

Keep every contract the WM already enforces; move only where the bytes come
from. Concretely, one seam: **`giraffe-field`'s four inputs** (verified run
dir, manifest, pass, input-graph) become one "current pass record" read,
with pins preserved.

1. In futon3c, beside `write-run!`: a projector that writes the pass result
   as the one current record (items+needs digest as its input pin, run id,
   per-row facts including `:dependency-reading` pins) whenever the pass is
   recomputed — GIRAFFE-store §4c already plans this; it keeps `write-run!`
   for the WM until the futon2 side lands.
2. In futon2, replace `find-giraffe-run` + `giraffe-field`'s file reads
   (`outer_observation.clj:224–333`) with a reader of that record (an XTQL
   query on :7073, or the same bytes exported to one file the existing
   shape can parse — the file route needs no new transport in the click).
   The record must carry the manifest facts the WM pins today:
   `:reading-source-changed` drift set and the input digest, so
   exclusions and staleness stay typed. `read-graph-input` in
   `focus_receipt.clj` is the second, smaller seam (the needs graph); it
   can keep reading exported `input.edn` pinned by the record's digest.
3. `wm_click.sh`'s staleness gate then inverts: instead of probe-mint (the
   click mints when stale), it *asks* whether the current pass record's
   input digest equals the digest of current items+needs (GIRAFFE-store's
   clause 2) and fires a finding when not — the mint becomes the store's
   job (event-driven), not the click's.

**What changes in a click:** the pins the kernel cites become record ids +
valid time instead of file shas (same values, different address); a click
no longer spends ~75 s minting when the field is stale; exclusion typing
stops depending on the run's manifest drift field and reads the item
records' own state; nothing in G, ordering, reach, or the receipt path
changes — those all consume the same per-row facts.

Unconfirmed, marked above: which kernel step won the 11-07 pick (replayable
from the record's kernel-input); the ticket path's exact registry shape
(no exercised ticket selection found in the 11-07 records).
