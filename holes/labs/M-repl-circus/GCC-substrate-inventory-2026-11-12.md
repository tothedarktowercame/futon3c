# GCC substrate inventory — how holes/ documents already live in futon1b

kimi-1 for claude-13 and Joe, 2026-11-12. M-repl-circus §DERIVE ("the
substrate inventory would be good to start with"). READ-ONLY throughout:
:7073 GET routes only (`/api/alpha/entities`, `/api/alpha/entity`,
`/api/alpha/hyperedges`, all bounded, limit ≤ 200, cursor-paged), plus file
reads. No writes to any store; nothing signalled. futon3c is PUBLIC: ids,
counts and code locations only.

Inputs: Giraffe run `~/storage/giraffe/20261112T181136.142Z-9883ce72/`
(newest at read time: 662 items in `input.edn`, 489 in `excluded.edn`;
the "826" figure is the corpus members.json population, not this run's
input), workplan slice `~/notes/cascade-live-outline/arxana/all-items.edn`
(323 `[:workplan/item 1]` records), all 1,200 `mission/doc` and 600
`excursion/doc` entities paged from :7073 (200/page, `after` cursor;
both totals came out exact multiples of the page size — re-verified by
the cursor terminating, not by a server total, which the route does not
return).

## 1. Kinds → entities

| holes/ kind | Entity type | Id scheme | external-id | Writer (file:line) | Trigger | Props kept current |
|---|---|---|---|---|---|---|
| Mission `holes/missions/M-*.md` (also bare `holes/M-*.md`) | `mission/doc` | `<label>/mission/<slug>` e.g. `futon3c-d/mission/repl-circus` (label = watch-root label; slug = lowercased, non-alnum→`-`) | `M-<slug-source>` e.g. `M-repl-circus` | `futon3c/watcher/file_ingest.clj:929` `ingest-mission-doc!` (entity at :998, source `mission-doc-watcher`) | multi-watcher file event on the md (pattern `multi.clj:70`) | `mission/id, title, status, repo, phase, date, owner, summary, cross-refs, code-paths, mtime, psrs, purs` — full header parse |
| Mission (same file) | `mission/doc` | `<repo>-d/mission/<slug>` via `mission-doc-id` (`mission_scope_ingest.clj:298`; repo derived from path, label differs from watcher's for futon4/futon6 — see §2) | `M-…` | `futon3c/scripts/mission_scope_ingest.clj:1755-1767, 1912-1926` (source `mission-scope-tree`) | watcher scope lane per changed M/E/C doc (`multi.clj:74-77`, off-cycle drainer), and manual runs | only `mission/id`, `mission/path` — **no status/phase/title** |
| Excursion `holes/(missions/\|excursions/)E-*.md` | `excursion/doc` | **UUID** `:entity/id`; `:entity/name` = `<label>/excursion/<slug>` | `E-…` | `file_ingest.clj:1037` `ingest-excursion-doc!` (entity at :1048, source `excursion-doc-watcher`) | watcher file event (pattern `file_ingest.clj:48`) | `excursion/id, title, status, parent-mission`, `provenance/repo, provenance/path` |
| Excursion (spurious second node) | `mission/doc` (!) | `<label>/mission/e-<slug>` e.g. `futon3c-d/mission/e-dog-helps` | `E-…` | `mission_scope_ingest.clj` via the scope lane — the stem pattern `multi.clj:74` matches E- stems, so excursion docs get scope-tree ingested as if missions | scope lane | `mission/id`, `mission/path` only |
| Campaign `holes/(campaigns/)?C-*.md` | **no entity** (verified: 0 `campaign/doc` on :7073) | — | — | watcher routes C- docs to the scope lane only (`file_ingest.clj:1500` → `:campaign-doc-scopes`); no `ingest-campaign-doc!` exists | — | — |
| Ticket `holes/tickets/T-*.md` (and bare `holes/T-*.md`) | **no entity** (0 `ticket/doc`) | — | — | nothing: watcher md patterns (`multi.clj:70-83`) match only M-/E-/C-; T- files are watched only for extension-based code lanes | — | — |
| Lab (`holes/labs/**`) | **no entity** (0 `lab/doc`) | — | — | nothing | — | — |
| Problem (evidence-store problems like a96J05) | **no entity** in substrate-2 (0 `problem/doc`); they live in the evidence store, not under holes/ | — | — | — | — | — |
| A- algorithms | none found as a holes/ kind in this census; no `algorithm/doc` entities | — | — | — | — | — |
| Essays (`essay-home` files) | entities written by `file_ingest.clj:1104` `ingest-essay!` (types from the essay ns) | per-essay | — | watcher | — | per essay ns |
| Sorry registries (edn) | hyperedges only (`code/v05/sorry`), no item entity | — | — | `file_ingest.clj:1074` | watcher | — |

Two writers create `mission/doc` entities for the **same** mission under
**different ids** whenever the scope-ingester's path-derived label differs
from the watcher label: futon4 watcher label is `futon4-elisp-d`, the
ingester mints `futon4-d`; futon6 watcher is `futon6-py-d`, ingester mints
`futon6-d`. 89 mission external-ids are duplicated this way (plus a
case-folding pair: `populate-substrate-2.D3-slice2-design` legacy vs the
slug-lowered `populate-substrate-2-d3-slice2-design`). There are also 22
legacy `mission-doc/m-N` entities (external-id `M-1`…, nil source, from an
older scope-ingest shape) and UUID-keyed `mission/doc` entities from the
watcher's early T-9e form.

## 2. Coverage

Resolution rule: Giraffe/workplan item id == entity `:entity/external-id`
(exact string).

| Population | Items | Resolved | Unresolved | Unresolved breakdown |
|---|---|---|---|---|
| Giraffe run input | 662 (348 M, 159 E, 155 T) | **474 (72%)** | **188** | 155 T (kind not ingested at all); 26 futon7 M (see below); 7 futon1b M (futon1b is **not a watch root** — only in `sweep-roots`, `watcher/roots.clj:41-46`) |
| Workplan slice | 323 (170 M, 134 E, 19 T) | **280 (87%)** | **43** | 19 T (kind not ingested); 20 M + 4 E, all futon7 or futon1b except `M-xtdb-22x-benchmarking` (futon1b too) |

Causes, classified:

- **Kind not ingested**: all 155 Giraffe T- items, all 19 workplan T-
  (tickets); campaigns likewise have no entity (none in these two
  populations' unresolved sets, but the gap is structural).
- **Repo not watched**: futon1b (`M-xtdb-fast`, `M-evidence-landscape-index`,
  `E-SO-to-XT`, `E-futon1b-fairness-etc`, `E-xtdb-plan-cache-heap`,
  `E-xtdb-types-cleanup`, `M-xtdb-22x-benchmarking`).
- **futon7 anomaly (UNCONFIRMED cause)**: futon7 IS a watch root
  (`watcher/roots.clj:38`, label `futon7-d`) and its holes hold `M-*.md`
  files, and 9 `futon7-d` excursion entities exist — but **zero** futon7
  mission entities. Either futon7 mission ingest errors or the watcher is
  not delivering futon7 mission events. 26 Giraffe M's + most workplan
  unresolved M's are this one cause.
- **Scratch-repo census**: 131 further Giraffe-run non-resolving ids come
  from `futon2-retraction-p1` / `futon2-declared-effect-wiring` worktree
  paths in the census (counted in the excluded/supplementary population,
  not watchable repos).
- Renamed/deleted: none observed among resolved ids; no evidence of
  renames in the unresolved set beyond the case-folding dup in §1.

## 3. What already hangs off item entities

Sampled with `/api/alpha/hyperedges?end=<id>` (bounded):

| Item entity | Edges fetched | Types |
|---|---|---|
| `futon3c-d/mission/repl-circus` | 44 | `mission-scope/loose-section` 24, `clock/clocked-on` 11, `mission-scope/capability-scope` 3, `mission-scope/pattern` 3, `mission-scope/eightfold-phase` 1, `mission-scope/map-item` 1, `code/v05/mission-doc` 1 |
| `futon3c-d/mission/giraffe` | 130 | `mission-scope/source-material` 57, `mission-scope/loose-section` 36, `mission-scope/pattern` 14, `code/v05/file→mission` 9, `mission-scope/eightfold-phase` 6, `mission-scope/capability-scope` 4, `code/v05/mission-doc` 1, `clock/clocked-on` 1, `mission-scope/map-item` 1, `mission-scope/plain-argument` 1 |
| `futon3c-d/excursion/dog-helps` (name endpoint; the entity's UUID id is NOT the edge endpoint) | 2 | `code/v05/excursion-doc` 1, `clock/clocked-on` 1 |

Notes: excursion edges key on the **name** string, not the UUID entity id
— point lookups must use the name. Also present in the type vocabulary
(from the ingester): `mission-scope/nesting`, `mission-scope/relates-to`,
`mission-scope/operator-gate`, `mission-scope/psr|pur`,
`code/v05/related-mission`, `code/v05/excursion→parent-mission`,
`edge/renamed-to`. `:xiang/*` scope-edge types: **not sampled**
(unconfirmed whether any have item endpoints).

GCC load-design record kinds vs what exists:

| GCC record kind | Exists in substrate under another name? |
|---|---|
| readings | **No** — genuinely new (Giraffe corpus JSONL only) |
| needs | **No** — `mission-scope/relates-to` is doc-declared scope, not a Giraffe `needs` edge; genuinely new |
| serves | **No** — new |
| names | **No** — new (workplan lexicon side only) |
| mentions | **No** as item links — `clock/clocked-on` is agent-session↔mission, the nearest thing; turn-mentions are new |
| state group | **Partially** — see §4: the value lives split across watcher props and Giraffe's derivation; no stored record unifies it |
| pending level | **No** — new |

## 4. State: where an item's state lives, and whether the two agree

Two parallel derivations from the same Status line:

- **Substrate**: entity props `mission/status` (and `mission/phase`),
  written by `ingest-mission-doc!` from the header parse
  (`file_ingest.clj:955-967`). Scope-ingester `mission/doc` entities carry
  **no status at all** (only id+path).
- **Giraffe**: `:state-group` ∈
  backlog/unstarted/started/completed/cancelled, derived at census time
  (`giraffe/missions.clj:76-84`: live? + `status->group` rule table at
  :44-56; `giraffe/excursions.clj`, `giraffe/tickets.clj` for the other
  kinds). Computed, never stored anywhere durable.

The 5 demo items (`~/storage/gcc-preview/index.json`):

| Item | Giraffe state-group | Substrate entity status | Agree? |
|---|---|---|---|
| M-repl-circus | started | `in-progress` (phase map) | yes, after vocab mapping (started ↔ in-progress) |
| M-giraffe | started | `in-progress` (phase instantiate) | yes, same mapping |
| T-pattern-search-reads-stale-index | unstarted | **no entity** | n/a — no substrate state |
| E-gcc-schema | (excluded from run, `excluded.edn`) | `open` (excursion/doc) | n/a; vocab mismatch open vs five-group |
| M-formal-arxana | (excluded from run) | `in-progress`, phase document (watcher entity); scope-tree entity carries nil | the two entities for one item disagree in completeness |

Also observed outside the five: M-apm-demonstration is `:cancelled` in
Giraffe but its entity still says `in-progress` — a stale watcher prop or
a status-line edit the watcher has not re-ingested (**unconfirmed**
which). E-dog-helps' excursion entity says status `unknown`.

## 5. Recommendation

| GCC record kind | Verdict |
|---|---|
| item | **Attach to the existing entity** — the watcher/scope `mission/doc` and `excursion/doc` entity IS the item. Id mapping needed: item id ↔ entity via `external-id` (exact today); canonical key should be `M-repl-circus ↔ futon3c-d/mission/repl-circus`. Two fixes first: (a) collapse the dual-writer duplication (89 external-ids with 2 entities; futon4/futon6 label mismatch, legacy `mission-doc/m-N`, UUID missions) onto one id per external-id, or the GCC inherits the split; (b) excursion entities need a stable readable id (`<label>/excursion/<slug>`) instead of UUIDs, matching what the edges already use |
| tickets / campaigns / labs | **New writer needed** — extend the watcher md patterns to `holes/tickets/T-*.md` and emit `ticket/doc` (id `<label>/ticket/<slug>`, external-id `T-…`); same for `campaign/doc`. Labs: decide whether a lab is an item at all before writing anything |
| futon1b, futon7 repos | **Ops fix, not schema**: add futon1b to watch-roots (or accept its holes as unwatched); investigate futon7's missing mission ingest (9 excursions, 0 missions — smells like a per-repo failure, unconfirmed) |
| readings | **New writer** (loader over the Giraffe corpus; then event-driven per landed reading). Attach to item entity via external-id; pin source path+sha256 as the B1 artifact revision |
| needs | **New writer** from accepted readings; endpoints are item external-ids, so edges resolve onto existing entities |
| serves, names, pending level | **New writers** (workplan side); attach to the same item entity |
| mentions | **New writer** (turn projector); item endpoint = the entity |
| state group | **Attach to the existing entity**: one writer (the watcher, which already parses the header) should also derive and write the five-group value as a prop, replacing Giraffe's census-time re-derivation as the of-record source; Giraffe then reads the prop. This also fixes the M-apm-demonstration-class staleness by construction (one derivation, written on change) |
| pass/priority/focus receipts | New record types, per the earlier review; not entities |

Overall: the B1 instinct is confirmed by the inventory — for M- and E-
items the substrate entity already exists, is kept current by a live
writer, and already carries the hyperedge traffic. The GCC load is mostly
"point new record kinds at existing entity ids", plus three gap-writers
(tickets, campaigns, futon1b/futon7 coverage) and one dedup ruling.
