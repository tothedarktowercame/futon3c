# Promise history read populations

Discovery date: 2026-11-05. No live-store unbounded query was used.

## What each reader actually needs

| reader | current implementation | minimum population |
|---|---|---|
| `agency/obligations_reader.clj:41-62,96-103` → `obligations.clj` | Pages every row tagged `promise-history`, then every outcome row. `obligations-as-of` filters time, checks every chain, groups by promise and finds the creation row (`obligations.clj:139-164`). | For a current answer: complete chains for promises whose projected obligation is still open/overdue/unchecked for the requested agent, plus creation + decisive release/outcome/check rows for closed checkable promises retained in `:ignored` (`:165-177`). It does **not** need unrelated agents' chains. For arbitrary `?at=`, it needs chains visible at that instant or a snapshot at/before T plus subsequent events. |
| `agency/obligations.clj` | Pure projection; explicit releases, fulfilled/lapsed/check verdicts determine status (`:48-117`). Broken chains are incomplete, never inferred (`:9-11,143-164`). | Creation plus all state-changing rows needed to validate continuity, and relevant outcome rows. Latest row alone is insufficient: release role/basis and fulfilment observations can be earlier than the latest transition. |
| `agency/answer_population.clj:75-92` | Validates that an answer declared/read the exact promise-history and promise-outcome filters. | The same bounded, pinned population used by obligations. It needs the population manifest and completeness facts, not a repository-wide history payload. |
| `transport/http.clj:12759-12775` `/api/alpha/obligations` | Calls the whole-history obligations reader for one agent. | Agent-scoped current projection, or agent-scoped as-of chains when `at` is supplied. |
| `transport/http.clj:11165-11207` operator-turn source | Filters only `type=promise/woken`, tag `promise-history`, and one session. | Woken rows for that session only; no other lifecycle rows. |
| `agency/promise_outcome.clj:247-270` `sweep!` | Reads the complete history tag, then keeps creation types with a machine-evaluable criterion, once per promise. | Creation row (or a current due-work projection) for promises with a criterion not yet finally evaluated. It does not need ready/lease/ack or completed unrelated chains. |
| rebuild / P2b | Exact replay requires lossless snapshot edits, ready/lease/ack, contiguous sequence and predecessors (`holes/missions/M-象-2000.md:893-910`; `promise_history.clj:250-311`). | At boot: a complete current-state checkpoint plus all later transitions. Without checkpoints, complete chains for every promise represented in the authoritative state. It does not require indefinitely retaining already-terminal chains merely to rebuild current `/tmp`. |
| `scripts/ways-of-working-probe.py:76-87` | Pages `promise-history` since a 24-hour cutoff. | Its explicit time window only. It measures process behavior, not current scheduler reconstruction. |
| `agency/promise_log.clj:106-129` | Repeats the evidence reader's repository-wide 20,000-row cap over `:promise/history-event`. | This is not a consumer requirement; it is an adapter limitation. Repointing unchanged would reproduce the failure after backfill. |

## “Open” and measured population

Scheduler closure is represented by `:promise/released` for parks and
`:promise/followup-terminal` for followups. `obligations.clj:9-11,53-87` makes
an important distinction: any plain release ends an unchecked wait, but only
an explicit creditor/debtor release discharges a checkable debt. Fulfilled,
lapsed and fulfilment-check rows can close or make an *obligation* overdue,
although they are outcome rows rather than history-chain terminators. Budget
and deadline expiry trigger terminal handling and are normally accompanied by
release (`parked_on.clj:585-592`); counting them as terminal independently is a
conservative scheduler-state variant.

Read-only query method:

```python
c = sqlite3.connect(
  'file:/home/joe/code/futon1b/migration-store-21/fts5-evidence.db?mode=ro',
  uri=True)
rows = c.execute('''
 select a.id,a.type,a.at,cast(d.doc as text)
 from ev_attr a join ev_doc d using(id) join ev_tags t using(id)
 where t.tag=':promise-history' order by a.at,a.id''').fetchall()
# Extract :history/promise-id from the Transit document, group by promise id,
# and mark a group closed if it contains released or followup-terminal.
```

Measured result: **50,001 rows**, **7,305 promise chains**, **7,110 closed**,
and **195 open** by the release/followup-terminal definition. If standalone
budget/deadline expiry is also treated as terminal, **7,117 closed / 188
open**. The supplied 50,267 count includes rows appended after this read and/or
the store's current edge; the population conclusion is unchanged. All queried
rows yielded a promise id. This count is a chain-state census, not the smaller
set of open *checkable obligations* after agent/status/outcome projection.

## Failure interval and callers

The same ordered sidecar query shows row 20,000 at
`2026-10-15T00:36:31.939Z`; from then onward the reader's 20 pages × 1,000 cap
necessarily trips `truncated!` (`obligations_reader.clj:37-62`). Thus current
obligations reads have been structurally failing since 2026-10-15, subject only
to the exact timing of the next request.

Production callers found:

* the 象 client obligations pane,
  `packages/xiang-client/widget/main.ts:103` and its model at `model.ts:162`;
* the public route at `transport/http.clj:12759-12775` (also proxied in
  `packages/xiang-client/deploy/Caddyfile:28`);
* tests in `test/futon3c/transport/obligations_http_test.clj:47-88`;
* design/read probes in `holes/labs/M-象-2000/P18-0-answer-population-discovery.md:116`.

No Emacs production caller was found. No monitor, alert, or fallback keyed to
`:truncated-input` was found. The UI fetch is the only live consumer located,
so failure could remain a pane error without stopping Agency work. The cleanup
excursion now records the incident (`holes/excursions/E-evidence-cleanup.md:169`).

## Recommended population and serving shape

The smallest common solution is **not** a time-windowed global scan. It is:

1. a per-promise current-state document/projection containing promise id,
   agent/beneficiary, creation payload, current scheduler state, latest
   sequence/head, terminal status, criterion/outcome summary and checkpoint
   time; and
2. immutable `:promise/history-event` edges fetched per promise endpoint only
   when continuity, audit, replay, or historical projection needs them.

Current obligations can select current-state documents by agent and status,
then fetch complete chains only for the roughly 195 open candidates (and any
closed checkable rows intentionally returned in `:ignored`). Outcome sweeping
selects due unevaluated current states. Operator-turn source uses a direct
session/type index for woken rows. Ways-of-working retains its bounded time
window. Rebuild starts from the latest durable checkpoint and replays only
later per-promise edges.

As-of reads require a checkpoint whose valid time is at or before T, plus each
selected promise's edges through T with both valid/system axes pinned. A sole
mutable “latest” document cannot answer historical queries truthfully; either
version current-state documents or retain periodic immutable checkpoints.

Trade-off: this adds and maintains a small projection/checkpoint authority in
exchange for eliminating O(total-history) reads. Per-promise edge fetches cost
more requests for broad historical audits, but current UI/API reads scale with
open promises rather than 50,000+ events. A time-window alone is simpler but
incorrect for old still-open promises and for reconstruction across the window
boundary.
