# Promise lifecycle move — discovery

## Authority and invariant

Authority remains the atomically replaced `/tmp` park/followup state. The
durable log exists to reconstruct it after boot, not to supersede it:
`agency/promise_history.clj:1-2`. The adopted design says pending rows live in
the authoritative file's `:history-outbox`, are removed only after exact
readback, and “Authority does not change” (`holes/missions/M-象-2000.md:529-533`).
The consumer invariant is exact: “rebuild == live”; the first format was
rejected because it omitted state and ordering (`M-象-2000.md:893-903`).

## Writers and record contract

`promise_history/prepare-entry!` (`promise_history.clj:86-111`) allocates the
deterministic id `promise-history:<uuid [promise-id sequence]>`, type equal to
the transition, claim `:step`, agent subject/author, event time, tag
`:promise-history`, optional session, and an origin stamp. Its body merges the
complete promise record with:

* `:history/format 3`;
* lossless `:history/payload-edn` containing `{:record rec :changes changes
  :predecessor ...}`;
* `:history/writer-id`, process submission `:history/sequence`, normalized
  `:awaiting`;
* contiguous `:history/promise-id`, `:history/promise-sequence`, and
  `:history/predecessor {:sequence :id :type}` (`:73-108`).

There are two emission paths. Old-path `record!` queues an asynchronous append
and outcome evaluation (`:113-133`). Transactional `stage!` freezes a row into
the authoritative state's `:history-outbox` before that state is persisted
(`:137-146`). `drain-now!` orders pending rows by promise sequence, appends,
accepts a duplicate only after `get-entry*` and `same-history-row?`, persists
the chain head, removes the row from the state outbox, persists state again,
and evaluates outcomes (`:164-198`). `drain!` coalesces retries to one queued
drain per state and never blocks the authoritative mutation (`:199-224`).

Transition producers are `agency/parked_on.clj:175-245,291,399-407,458-491,
544-592` (park, ready enqueue/lease/ack/requeue, dependency termination,
wake/release, budget/deadline), and `agency/followup_queue.clj:62-69`
(enqueued/requeued/dequeued/terminal). `promise_history/capture!` emits explicit
park/followup store-change maintenance transitions for otherwise unconsumed
snapshot edits (`promise_history.clj:234-248`).

`promise_outcome.clj` is a second writer. It emits deterministic
`:promise/fulfilment-check` rows tagged `[:promise-outcome
:fulfilment-check]`, containing promise/source ids, criterion, due/checked
times, verdict, references and optional inability reason (`:104-151`). It emits
`:promise/fulfilled` or `:promise/lapsed` tagged `:promise-outcome`, containing
beneficiary/deadline, criterion, job observation, basis and truth flag
(`:174-228`). Both do exact by-id readback on collision. These are observations
about fulfilment, not reconstruction transitions (`:1-9`).

## Readers

| consumer | filter/use |
|---|---|
| `agency/obligations_reader.clj:41-62,96-103` | pages tag `promise-history`; separately tag `promise-outcome`, then restricts to fulfilled/lapsed/fulfilment-check; supports current and two-axis as-of reads. |
| `agency/obligations.clj:48-74,193-200` | groups creation/lifecycle/outcome rows; interprets releases, fulfilment, lapse, checks, broken chains and orphan checks. |
| `agency/answer_population.clj:75-92` | declares the required filters exactly: history tag, and outcome tag plus three outcome types. |
| `transport/http.clj:11170-11210` | reads promise-history by tag/type for the HTTP promise projection. |
| `agency/promise_outcome.clj:247-270` | complete tag query of retained history; selects creation rows and evaluates due criteria even after `/tmp` deletion. |
| `scripts/ways-of-working-probe.py:76-87` | bounded pagination by `tags=promise-history` and time. |
| `promise_history/check-chains` (`:261-311`) | verifies format, origin, contiguous sequence, predecessor id/type and pending gaps. |
| rebuild/P2b tests and implementation | `M-象-2000.md:893-910` requires lossless payload, every reconstructive transition and contiguous predecessors; tests under `test/futon3c/agency/promise_history_test.clj`, `parked_on_test.clj`, `followup_queue_test.clj`, `obligations*_test.clj`, and `promise_outcome_test.clj` pin these properties. |

## Existing futon1b destinations

Futon1b exposes generic entity POST/GET/list routes
(`futon1b_server.clj:952,1546-1550`; `futon1b_graph.clj:532,587-598,664`).
Entities are identity/upsert documents and latest-by-identity projections. They
do not naturally express an immutable per-promise transition sequence.

It also exposes hyperedge POST, exact GET, and bounded type/end queries
(`futon1b_server.clj:880,1536-1556`; `futon1b_graph.clj:1033,1433`). A promise
event can be an immutable hyperedge with stable id, type
`promise/history-event` or `promise/outcome`, endpoints for promise and agent,
and props holding the current lossless payload/link. This is the closest
existing route: it has exact readback, temporal storage and type/endpoint
selection.

What is missing is material: no existing route enforces contiguous
`promise-sequence`, predecessor agreement, uniqueness per `[promise-id,
sequence]`, or returns a globally complete ordered event scan with the current
tag-query contract. Those checks must remain in the adapter/readers, and the
route needs bounded pagination/as-of behavior verified before cutover. The
futon1b `hx_edge`/`hx_node` sidecar is an index for those hyperedges, not a
separate write authority.

## Migration sequence without a reader gap

1. **Introduce the operational adapter and dual write.** In
   `promise_history.clj` and `promise_outcome.clj`, retain the existing
   state-file outbox but drain each fixed row to both evidence and the new
   promise hyperedge/document adapter. Remove it only when both exact readbacks
   match. Pin with a test that one staged row survives restart and reaches both
   stores with identical payload/link.
2. **Add dual-read comparison.** Add a promise-log reader beside
   `obligations_reader.clj`; compare complete populations and chain-check
   results while returning the evidence result. Pin current and historical
   reads plus a missing-middle transition.
3. **Repoint reconstructive and outcome readers.** Move
   `promise_outcome/sweep!`, rebuild/P2b, obligations reader, HTTP projection,
   answer population, and ways-of-working probe to the operational API. Pin
   “rebuild == live”, due outcome after `/tmp` deletion, and the existing
   obligation population manifest.
4. **Switch the outbox acknowledgement.** Once every reader uses the new log,
   make exact operational readback sufficient for removing an outbox row;
   evidence remains a temporary shadow write. Test operational success with
   evidence unavailable.
5. **Stop evidence writes.** Remove only the shadow append after a measured
   comparison window. Keep deterministic ids and chain checks unchanged. A
   regression test must assert zero evidence-boundary calls while reconstruction
   and obligation/outcome tests remain green.

## Volume driver and reconstructive minimum

The common seven-row lifecycle is not seven independent snapshots: creation
(`park-made`), ready enqueue, lease, ack, dependency termination, wake, and
release are each recorded because the ready inbox and delivery state mutate
separately (`parked_on.clj:175-245,399-407,458-491`). This explains the observed
roughly seven rows per promise and the repeated large payload.

For exact reconstruction, creation, ready enqueue/requeue/lease/ack,
dependency-arrival/termination, budget/deadline expiration, wake/release, and
followup queue transitions are needed because each changes authoritative state;
the lossless `changes` payload is what makes the result exact
(`M-象-2000.md:893-910`). Store-change maintenance rows are needed only when
they carry edits not consumed by a semantic transition
(`promise_history.clj:234-248`). Fulfilment checks, fulfilled/lapsed outcomes,
writer/submission diagnostics, and repeated full record projections are not
needed to rebuild scheduler state. They serve obligation/outcome audit or
writer diagnostics and can be stored separately or derived. The major byte
redundancy is copying the full record and its EDN payload into every lifecycle
row; the migration must preserve information, ordering and exact readback, not
that duplication.

## Decided: step 3 serving shape (claude-8, 2026-11-05, after PROMISE-READ-POPULATION.md)

Readers stop scanning whole-tag history. Two stores in futon1b, both hyperedges:

1. **`:promise/state`, one edge per promise**, stable id `promise-state:<promise-id>`, rewritten on
   every transition with `:hx/valid-time` = the transition's event time. futon1b's batch writer
   puts same-id docs as new XTDB versions (`futon1b_server.clj` ~253, `put-docs` with
   `:valid-from`), so as-of reads come from XTDB's own valid/system axes; no separate checkpoint
   store. Endpoints: the promise id, `agent:<debtor>`, `agent:<creditor>` when known, and a status
   endpoint `promise-status:open` or `promise-status:closed`. Current obligations then query
   `end=promise-status:open` (~195 rows today) instead of 50,000 history rows.
   Props: status, head sequence, head evidence id, terminal transition, creation payload
   summary, fulfilment criterion, outcome summary.
2. **`:promise/history-event`** (already dual-written) stays the audit chain, fetched per promise
   by endpoint when continuity or replay needs it.

The state edge is a projection of the history edges, so "state == fold(history)" is the test,
alongside "rebuild == live". Backfill: state edges for all 7,305 promises, and history edges for
the open promises' chains that predate 17:12Z (~200 chains). Then repoint readers in the order of
PROMISE-READ-POPULATION.md, obligations first, since its API has failed since 2026-10-15.

Handoffs: (a) pure fold + state-edge writer in the drain path; (b) backfill; (c) repoint
obligations_reader; (d) the remaining readers one at a time.
