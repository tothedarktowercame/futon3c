# P5 narrowed: fulfilled and lapsed observations

Implements Joe's 2026-09-27 narrowed decision. Park/followup delivery, wake,
release and cache deletion retain their existing behavior. Two additive types
observe the P1 criterion: `promise/fulfilled` and `promise/lapsed`.

`job-terminal-ok` reads the actual invoke ledger's state; `done` is the successful
terminal classification. Dependency payloads and the fact of waking are never
criterion evidence. Each outcome names its promise, source history record,
criterion, beneficiary/deadline when supplied, observation time, and all job
fields read (`state`, `finished-at`, `terminal-code`, job id). Evidence event time
is evaluation time; XTDB valid time remains insertion time.

Prose is explicitly not machine-evaluable and never automatically fulfilled or
lapsed. Absent/pruned jobs and failed reads are unknown, not false. A known false
criterion after the P1 absolute deadline lapses. `deadline-ms` is still only the
park liveness backstop and is not substituted for the P1 deadline. A job finished
after the deadline proves lateness even if polling missed the boundary: it may
have both lapsed and fulfilled observations. `outcome-basis` distinguishes that
retrospective lateness from a currently false criterion; it does not erase lapse.

## Execution and durability

- The ordered background history writer evaluates criteria after successful
  transition writes, including wake/release and followup transitions. It also
  checks creation/dependency changes; only a true criterion can fulfil.
- The existing 30-second parked deadline timer queues a coalesced background
  outcome sweep. The hook is inside `parked-on/sweep-deadlines!`, so the existing
  daemon sees it after reload; no second timer or restart is required.
- The sweep reads retained `promise-history` creation records, including parks
  already deleted from /tmp. It refuses incomplete paginated scans and records
  errors for pre-repair records whose exact criterion cannot be reconstructed.
- Writes use the existing evidence boundary with a harness origin stamp and a
  deterministic id per `(promise id, outcome type)`. Before append, the durable
  backend is checked. Duplicate append races are verified against the existing
  record. No local success flag is needed across restarts. Failed writes remain
  retryable; they do not interrupt delivery. Counts are exposed under
  `GET /api/alpha/parked` → `promise-history.outcomes`.
- Outcomes use `promise-outcome` tags and do not consume snapshot-edit sequence
  numbers. P2b replay and P17 explicitly distinguish these observations from
  transition chains, preserving state comparison and constraint behavior.

This does not repair gaps in P2a history, restore pruned job evidence, change
/tmp authority, or introduce any new authority decision. A retained creation
record and readable job evidence are necessary for recovery after restart.

## Validation

All seven namespaced runs passed: promise-outcome (5 tests / 30 assertions),
promise-history (7/44), parked-on (19/83), followup-queue (6/24), promise-record
(5/45), promise-replay (5/47), history-constraints (6/25): **53 tests / 298
assertions, no failures/errors**. Existing tests used isolated temporary store
paths. clj-kondo: zero errors/warnings; check-parens: OK.

The bad case runs the real park/history/boundary code: wake dependency succeeds,
criterion job fails, wake fires, no fulfilled record; after release deletes the
cache, the retained-history sweep emits only lapsed. Other tests cover successful
job evidence, prose, absent jobs, followups, late success, fresh backend instances
without local state, and P2b/P17 compatibility with the new records.

## Live receipt (2026-09-27)

Reloaded additively from canonical master. A temporary local peripheral invoked
`sha256sum` on the evaluator source through the real `/api/alpha/invoke` route:
job `invoke-1790539177774-25638-6b49179b`, terminal ledger state `done`, finished
`2026-09-27T20:00:16.029037999Z`. Its HTTP result was consumed and its delivery
receipt recorded; the probe peripheral was deregistered afterward.

Park `park-28f489cc-7a0f-436b-8d6f-1eaf0ef545d8` had that job as both dependency
and criterion, beneficiary Joe, next-day P1 deadline, zero resume budget.
GET :7073 evidence for session `p5-live-codex4-20260927` returned exactly one
`promise/fulfilled`: `promise-outcome:23177258-7e68-37df-9e09-e313dc449480`.
Its job observation records `done`, the finish timestamp and criterion job id;
its origin is harness/promise-outcome. The original transition chain contains
park-made, dependency-terminated, budget-exhausted. Thus fulfilment does not
require (and is not manufactured by) a wake. GET /parked confirms no residual park.
Repeated evaluation increments existing rather than written (observed written=1,
existing=3). Raw receipt: `/tmp/p5-live-evidence.json`.

The timer also encounters legacy pre-repair promises carrying criterion metadata.
It refuses them with `Incomplete pre-repair promise`, increments the visible
failure count, and emits no inferred outcome for them. Journal inspection confirms
these refusals account for the observed sweep failures; the successful probe is
format 3. Repairing those historical gaps remains outside this packet.
