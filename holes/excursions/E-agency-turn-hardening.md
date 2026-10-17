# E-agency-turn-hardening: turns that collide, leak, or wait on dead jobs

Date: 2026-10-16. Owner: claude (terminal session, not on the roster).
Status: O1-O3 fixed 2026-10-17 (ce669debf), live-loaded; two of them still
want a live occurrence to confirm (see "Live evidence"). Joe (2026-10-16):
"these are enough issues that we need to make an E-agency-turn-hardening.md
note and come back to them later"; (2026-10-17) "I'm not sure we have
enough live evidence to do so but we can at least try."

## How it started

Joe reported codex-68 "stuck and giving me strange errors in the REPL". It
was not stuck. At 04:02:34 the work-orders nag (job
`invoke-1792123353472-15118-4f79a825`) started a `codex exec resume` on
codex-68's thread. Joe's turn, sent three times (04:02:42, 04:03:54,
04:04:03), each started a second `codex exec resume` on the same thread, and
Codex refused every one:

    thread-store conflict: thread 01a13631-5f17-7f42-8f73-cf13841eaab9
    already has an active writer (code -32600)

The nag finished at 04:05:38. Joe's turn never reached codex-68. Looking into
it turned up the other items.

## Fixed

### F1. 象 turn polls leaked one Emacs buffer each (0217d3e22)

`session-mode--xiang-request` (emacs/session-turn-analysis.el), async
branch: bare `url-retrieve`, response buffer never killed. The turn poller
calls it every 20 s, up to 225 times per turn. The main Emacs had 1,317
`" *http 127.0.0.1:7070*"` buffers (8.3 MB). url-retrieve can also call back
twice for one request, which forked the poll chain: `turn-BmUsd5` had three
live poll timers and 426 leaked buffers, against a 225-poll cap.

Fix: route through `futon-url-retrieve` (one callback, 20 s deadline), kill
the buffer after the callback. ERT test
`session-turn-analysis-jvm/async-request-kills-buffer-and-calls-once`, which
fails on the old code. Live Emacs: 1,358 buffers killed, 5 duplicate timers
cancelled; count stayed 0 afterwards.

### F2. A turn behind a dead analysis job was polled to exhaustion (a7978107e)

The 03:20 Agency restart killed four turn-capture analysis jobs (Zai seats)
with `worker-lost-on-restart`. Their records stayed `requested`, and Emacs
kept polling them. Two causes:

- `tr/job-outcome` reads the public job view, which shows a finished job
  with delivery pending as `"delivering"`. A turn-capture job keeps that
  state for good, so the reaper classed it as `:running`. It now reads the
  finished state from the job's last terminal event.
- Scheduled reaps live in memory (`:schedule!`), so the restart dropped them.
  `turn-view` (GET `/api/alpha/xiang/turns/:id`) now reaps a `requested`
  record once, scheduling nothing more.

Tests in `turn_record_test` and `turn_service_test`
(`reading-a-turn-settles-it-behind-a-dead-job`, which fails without the
change). Live-loaded from master; turn-BmUsd5, turn-DE6bT1, turn-2q44Qt read
`failed` ("recovered on startup") on the next GET.

## Open on 2026-10-16, fixed 2026-10-17 (ce669debf)

### O1. Codex REPL turns do not queue behind a running turn

Both REPLs send turns through `/api/alpha/invoke-stream`. Claude seats queue
there (`agency/agent_pouch.clj` with `agency/turn-queue`). Codex seats have
no such queue, so any turn arriving while the seat is mid-turn (a nag, a
bellback, Joe) starts a second `codex exec resume` and Codex refuses it. Joe
(2026-10-16): "The Claude REPL just queues turns behind the others that are
executing." That is the behaviour wanted: a Codex turn waits for the seat,
reported as queued, and runs after.

**Cause, found 2026-10-17:** the premise above was wrong. Codex seats do
have the queue: `/invoke-stream` REPL turns, bells, auto-bellbacks and
park resumes all go through `turn-queue/accept-async!` (the auto-bellback
path was moved there after a 2026-06-26 two-pipelines incident). The
work-order nudge, added later, did not: `enqueue-work-order-bell!`
submitted to the shared `invoke-executor`, so it ran beside the queue.
**Fix:** it joins the recipient's queue like the others. Test
`a-work-order-bell-joins-the-recipients-turn-queue`.

### O2. A job queued before a restart is never started

`invoke-1792120765266-15088-513138be` (turn-yE50Ay's analysis, zai-1) was
accepted at 03:19:25, a minute before the restart, and was still `queued`
at 04:15. zai-1 was idle and invoke-ready (`queued-jobs 1`) and had run
other work since (last active 03:54).

**Cause:** the turn queue is durable, but an entry's processor is an
in-memory closure. At boot `resume-pending-drainers!` popped the restored
entry, `drain!` called the missing processor (nil), caught the exception
and marked the entry failed. The invoke job was never told, and could not
be: bell entries were queued under a random id, not their job id.
**Fix:** bell, auto-bellback, park-resume and nudge entries carry their job
id as their id; `drain!` hands a processor-less entry to an orphan handler,
which http registers (`fail-orphaned-invoke-job!`) to fail the job as
`turn-lost-on-restart`. Failing, not re-running: the caller's delivery
closure is gone too, and a re-run nobody hears back from is worse than an
honest failure. Revisit if lost turns turn out to matter. Tests
`an-entry-without-a-processor-is-handed-to-the-orphan-handler`,
`a-restored-turn-without-processor-fails-its-job`.

### O3. Nothing settles a requested turn without a poller

F2 settles a turn when someone asks about it. A turn nobody polls (Emacs
restarted, the poller ran out of tries) stays `requested` behind a dead job.
**Fix:** `sweep-requested!` reaps the newest 500 `requested` records with
a job id once each, scheduled 120 s after the service starts (after the
boot drain has failed any orphans). Test
`the-boot-sweep-settles-requested-turns-behind-dead-jobs`.

## Live evidence (2026-10-17)

- O2, by hand: the stranded zai-1 job was failed through
  `fail-orphaned-invoke-job!`; `turn-yE50Ay` then read `failed` ("queued
  turn's processor did not survive a restart"). The automatic path (boot
  drain → orphan handler) has not run live yet: it needs a restart with a
  queued turn. **Check after Joe's next restart:** no invoke job left
  `queued` that is absent from `turn-queue/queue-view`.
- O1 has not been seen live: it needs a nudge to land while a REPL turn is
  pending. **What to look for:** the REPL shows "queued #1 behind
  <work-orders job>" instead of the "already has an active writer" error.
- O3 runs at the next service start (a restart, or a reload that recreates
  the service).
- Tests: the touched namespaces show 32 failures and 1 error that are
  identical on unmodified HEAD (inbox, auto-bellback typed bells, job
  timeout, delivery conformance: 404s and ledger-flusher NPEs, which look
  like cross-namespace isolation). Not caused by this work; not yet looked at.

## Also seen

- Joe's confused turn was recorded and analysed three times (turns 168,
  169, 170 of the codex-68 session), once per refused send. The analysis
  seat noticed and reused its reading. Once O1 queues turns, a resend should
  not be needed.
- The poll budget (`session-mode-turn-jvm-poll-tries` 225 × 20 s, 75 min) is
  long, and was only needed because the server never answered `failed`.
  Revisit once O2/O3 are done.
