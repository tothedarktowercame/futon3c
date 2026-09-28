# P8-0 — fulfilment-check discovery

Read-only discovery, 2026-09-28. P8 requires one due-time check for every
criterion-bearing promise; wake and dependency termination are not criterion
evidence (`BUILD-PLAN-象2000.md:156-160`).

## What P5 does now

`promise-outcome/evaluate!` accepts only a `:job-terminal-ok` criterion whose
`:machine-evaluable?` is true (`promise_outcome.clj:53-66`). It reads the job
through `promise-job-lookup`, retaining only state, finish time and terminal
code (`:18-22,63-65`). `decide` writes `:promise/fulfilled` whenever the known
job state is `done`; after an explicit deadline it writes `:promise/lapsed` if
the criterion is false or the job finished late (`:32-51`). A late success can
therefore have both immutable observations.

The sweep reads the complete retained `:promise-history`, rejects a partial
read, selects creation rows with criteria, refuses pre-format-3 payloads, and
evaluates each independently (`:102-117`). Deterministic IDs are derived from
promise ID and outcome type; an identical stored record is reused and a
conflict fails (`:29-30,67-99`). Removed `/tmp` promises remain eligible.

The daemon calls `parked-on/sweep-deadlines!` every 30 seconds
(`transport/http.clj:1440-1469`). That queues `sweep-outcomes!`, but the latter
permits only one background sweep per five minutes because its full history
read was measured at about 20 seconds (`promise_history.clj:201-225`;
`parked_on.clj:491-497`). Promise creation, wake/release, dependency and
followup transitions also cause asynchronous evaluation, as recorded in the
namespace contract (`promise_outcome.clj:1-9`). Thus deadline detection is
automatic but may be about five minutes late; the verdict still compares with
the promised deadline.

P5 silently produces no record for prose, a missing job, or an unknown job
state: `decide` returns `[]` (`promise_outcome.clj:37-51`), and `evaluate!`
does not run for prose (`:58-60`). These skips increment neither a typed skip
counter nor a durable record. Exceptions, including malformed legacy payloads
and lookup/read failures, increment only aggregate `:failed` and print stderr;
the per-entry catch lets the sweep continue (`:23-27,113-117`). A partial
history read fails the whole sweep loudly (`:107-108`).

## Due rule

Use this total rule:

1. With `:deadline`, the check is due at that instant.
2. Without a deadline, `:job-terminal-ok` is due when its referenced job is
   first observed in any terminal state (`done`, `failed`, `error`,
   `cancelled`, `timeout`, `deduped`, or `rejected`). A missing or nonterminal
   job is not due yet.
3. A non-machine criterion without a deadline is due at promise creation. Its
   check says unable-to-determine because no future machine event can make the
   configured evaluator decide it.

This avoids inventing a deadline while ensuring every accepted criterion has
a determinate trigger. The scheduler should continue polling pending
job-backed promises; creation itself schedules the prose check.

## Smallest complete design

Add one `:promise/fulfilment-check` evidence record per promise, with
deterministic ID `promise-check:<UUID(promise-id)>`. Its body contains
`:promise-id`, `:source-evidence-id`, `:criterion`, `:due-at`, `:checked-at`,
`:verdict` (`:fulfilled`, `:unfulfilled`, or `:unable-to-determine`),
`:refs` (the exact job/source records read), and optional `:unable-reason`.
The closed unable set for v1 is:

- `:unsupported-criterion` — including prose;
- `:job-not-found` — at an explicit deadline;
- `:job-state-unknown` — a returned state outside the accepted vocabulary;
- `:job-read-failed`;
- `:history-read-failed` or `:history-incomplete`;
- `:invalid-promise-record`.

At a deadline, known `done` by the deadline is fulfilled; a known contrary
state, including a wake paired with a failed job, is unfulfilled. Missing or
unreadable evidence is unable, never unfulfilled. Without a deadline, a
terminal `done` is fulfilled and another terminal state is unfulfilled.

Refactor P5's pure observation/decision code so P8 and existing outcome writes
share it. One scheduled pass writes the due check first and keeps P5's
`:promise/fulfilled` / `:promise/lapsed` compatibility observations. A restart
reads the deterministic check ID and verifies its source, criterion and due
instant before treating it as existing.

The check is an immutable due-time judgment. A later real outcome does not
delete or rewrite an earlier unable check: both remain, and the later outcome
is the newer fact. This preserves what could be known at the due time while
allowing a transient lookup failure or late completion to be represented.

## P9

`obligations/promise-row` currently appends the sentinel `:outcome-unknown`
when a deadline passed without fulfilled/lapsed evidence
(`obligations.clj:48-75`). P9 should ingest check records and replace that
sentinel with the check ID. `:unfulfilled` remains overdue; unable remains
overdue with an `:unable-to-determine` fact and reason; fulfilled closes the
debt as completed. A later P5 fulfilled outcome can change the current status
to completed/completed-late while both record IDs remain in `:facts`.

## First implementation packet

Extend `promise_outcome.clj` rather than add a second sweep:

1. Extract a pure observation classifier and due predicate.
2. Write the deterministic `:promise/fulfilment-check` for every due criterion.
3. Preserve the existing fulfilled/lapsed writes from the same observation.
4. Extend P9's pure projection to consume checks.

Tests: deadline fulfilled; deadline known-false unfulfilled; prose unable;
missing job at deadline unable; job without deadline checked on terminal and
not while running; wake plus failed criterion yields unfulfilled; write/store
read failures are typed; two post-restart sweeps retain exactly one check;
P9 uses the check ID and no longer emits `:outcome-unknown`. A scheduler test
advances the existing periodic tick past due with no direct/manual evaluator
call and waits for the background writer.

## Spot checks

After drafting I re-read (1) `promise_outcome.clj:46-51`, confirming that a
failed job produces lapse only after a deadline and never from wake, and (2)
`promise_history.clj:203-223`, confirming the effective outcome cadence is
five minutes even though the park daemon ticks every 30 seconds.
