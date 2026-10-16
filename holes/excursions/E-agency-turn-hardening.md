# E-agency-turn-hardening: turns that collide, leak, or wait on dead jobs

Date: 2026-10-16. Owner: claude (terminal session, not on the roster).
Status: parked. Joe (2026-10-16): "these are enough issues that we need to
make an E-agency-turn-hardening.md note and come back to them later."
Two items are fixed (below); three are open.

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

## Open

### O1. Codex REPL turns do not queue behind a running turn

Both REPLs send turns through `/api/alpha/invoke-stream`. Claude seats queue
there (`agency/agent_pouch.clj` with `agency/turn-queue`). Codex seats have
no such queue, so any turn arriving while the seat is mid-turn (a nag, a
bellback, Joe) starts a second `codex exec resume` and Codex refuses it. Joe
(2026-10-16): "The Claude REPL just queues turns behind the others that are
executing." That is the behaviour wanted: a Codex turn waits for the seat,
reported as queued, and runs after.

To decide: share `turn-queue` with Codex seats, or queue in the Codex invoke
path. Server-side Clojure; live-load from master, no restart needed unless
the queue state needs one.

### O2. A job queued before a restart is never started

`invoke-1792120765266-15088-513138be` (turn-yE50Ay's analysis, zai-1) was
accepted at 03:19:25, a minute before the restart, and was still `queued`
at 04:15. zai-1 was idle and invoke-ready (`queued-jobs 1`) and had run
other work since (last active 03:54). `recover-inflight-jobs`
(transport/http.clj) keeps queued jobs on purpose, "must remain activatable
after restart", but nothing activates them. Find what drains a seat's queue
and why it skips a pre-restart job.

### O3. Nothing settles a requested turn without a poller

F2 settles a turn when someone asks about it. A turn nobody polls (Emacs
restarted, the poller ran out of tries) stays `requested` behind a dead job.
A startup pass over `requested` records with a job id, reaping each once,
would close this. Check the sweep in `turn_service` before adding one.

## Also seen

- Joe's confused turn was recorded and analysed three times (turns 168,
  169, 170 of the codex-68 session), once per refused send. The analysis
  seat noticed and reused its reading. Once O1 queues turns, a resend should
  not be needed.
- The poll budget (`session-mode-turn-jvm-poll-tries` 225 × 20 s, 75 min) is
  long, and was only needed because the server never answered `failed`.
  Revisit once O2/O3 are done.
