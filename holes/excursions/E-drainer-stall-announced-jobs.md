# Excursion: E-drainer-stall-announced-jobs — announced jobs sit `queued` and are never drained

**Date:** 2026-08-22
**Status:** RESOLVED (cause established 2026-08-22 14:20, ams-claude-fable). Neither
hypothesis below was it — see "Resolution" at the end. Driver fix:
futon3c-frame18-control `Activate announced jobs through /invoke`.
**Repo:** futon3c — `transport/http.clj` (announce + `build-invoke-response`),
`agency/turn_queue.clj` (`drain!`, `acquire-drain!`/`release-drain!`),
`agency/agent_pouch.clj` (warm pouch / M-kangaroo LRU).
**Spawned from:** APM library-lane bring-up (claude-14, 2026-08-22). The lane dispatches
frame seats through `POST /api/alpha/invoke/announce`, so a stalled drain stops the whole
apparatus — and it stops the countdown machinery too, which uses the same path.

## HEAD (one line)
**Two jobs announced after 13:44 have sat `queued` indefinitely with only an `accepted`
event, while jobs announced minutes earlier ran to completion.** `reap` considers them
healthy (`reaped: 0`), both seats report `invoke-ready? true` with live session ids, and the
box is otherwise busy — so this is not a dead server, it is specific jobs no drainer takes.

## Evidence

Job ledger, ordered by creation (`GET /api/alpha/invoke/jobs`):

```
13:02:59  f9045569047055-solver      done    [accepted running done delivery-recorded]
13:38:53  codex-12                   done    [accepted running prompt done delivery-recorded]
13:43:29  sink-proof-20260822        done    [accepted running prompt text tool_use done]
13:43:58  sink-proof-bell-20260822   done    [accepted running prompt text tool_use done]
13:44:47  codex-8                    queued  [accepted]                     <-- stuck
13:51:24  f9045569047055-solver      queued  [accepted]                     <-- stuck
```

Roster at the time of the stall — both stuck seats look healthy:

```
codex-8                status=idle  session=01a01962-97b  invoke-ready?=true  last-active=10:44:00
f9045569047055-solver  status=idle  session=01a02991-420  invoke-ready?=true  last-active=13:51:19
```

`POST /api/alpha/invoke/jobs/reap` returns `{"ok":true,"reaped":0}` — the queue does not
regard either job as stale, so whatever timeout reap uses is not the mechanism here.

The box is not wedged: `codex-10` and `claude-13` were both active during the stall.

## Hypothesis A — cold pouch (I think this one is more likely)

The two jobs that stalled belong to seats with no recent activity: `codex-8` last active
10:44 (three hours), `f9045569047055-solver` last active 13:02 (49 minutes). The two jobs
that ran immediately before, `sink-proof-*`, were on agents freshly registered minutes
earlier and therefore warm.

If a seat's warm pouch has been LRU-evicted (M-kangaroo,
`FUTON3C_KANGAROO_MAX_WARM`), and the drain path assumes a live pouch rather than spawning a
cold one, the job would be enqueued, accepted, and then never picked up — exactly the
observed signature, with no error anywhere because nothing failed.

Note `f9045569047055-solver` RAN at 13:02 and stalled at 13:51 with no configuration change
to the seat in between. Its pouch going cold in that window is consistent.

**How to check:** whether `turn_queue/drain!` (or `acquire-drain!`) can start a cold seat, or
whether it requires an existing pouch; and whether a pouch existed for these two agent ids at
13:44 and 13:51.

## Hypothesis B — the stream-sink change (correlated, but I doubt it)

`43e69be0 "Record stream events for announced invokes"` (2026-08-22 13:44:07) installs an
invoke event sink around the direct-invoke boundary in `build-invoke-response`, restoring the
previous sink in a `finally`. The stall begins 40 seconds after that commit.

Reasons to doubt it despite the correlation:
- The two `sink-proof-*` jobs at 13:43:29 and 13:43:58 ran to completion **under that code**,
  which had already been reloaded into the serving JVM (they carry the `text`/`tool_use`
  events the change adds — that is what it was proving).
- `clear-invoke-event-sink!` is genuinely defined (`agency/registry.clj:1496`), so the
  obvious "references a missing fn" failure is not present.
- The change is 24 lines and confined to one boundary.

The commit time is when the change was COMMITTED, not when it was loaded — it was loaded
before the 13:43 proofs. So "everything after the commit is broken" overstates it.

**How to check:** whether the sink is left installed for a seat after an invoke that never
reached the `finally`, and whether a stale sink can block a subsequent drain for that agent.

## Why it matters beyond the lane

`live_proof_phases` and `live_learning_phases` both dispatch via `announce`, so this affects
every APM frame, not just the library lane. Separately on 2026-08-22 it was found that
`/api/alpha/invoke/activate` does not exist (404, absent from the route table) while both
drivers POSTed to it and gated on a 202 — the work ran anyway because announce enqueues and
the drainer dispatches. That is fixed in futon3c-frame18-control `67010bc7`, and it means the
drainer is now the ONLY thing standing between an announced frame phase and a running agent.

## What is NOT the problem

- Not agent registration: both seats are registered, `invoke-ready? true`, with session ids.
- Not the reaper: `reaped: 0`.
- Not a dead JVM: other agents active throughout; serving JVM up since Aug 21 08:36, never
  restarted.
- Not `announce` itself: it returns 202 `{:ok true, :accepted true, :state "queued"}` and the
  job appears in the ledger with an `accepted` event.

## Reproduction

```bash
curl -s -X POST localhost:7070/api/alpha/invoke/announce -H 'Content-Type: application/json' \
  -d '{"agent-id":"<a seat idle for hours>","prompt":"Reply with exactly: probe-ok.",
       "surface":"emacs-repl","caller":"<you>"}'
# then poll; the job stays queued with only [accepted]
curl -s localhost:7070/api/alpha/invoke/jobs/<job-id>
```

Contrast with the same announce to a freshly-registered seat, which runs.

## Open questions for whoever picks this up

1. Can `drain!` start a seat whose pouch has been evicted, or does it silently skip?
2. Is there a per-agent drain lock that can be acquired and not released, and would that
   present as `queued` rather than `draining`?
3. Should `announce` refuse, or at least warn, when the target seat has no drainable pouch?
   A job that is accepted and then never run is the worst available failure mode: the caller
   believes it dispatched.
4. Should `reap` treat a long-`queued`-never-`running` job as stale? It currently does not.

## UPDATE 2026-08-22 ~14:20 — the stall cleared itself, and that refines the diagnosis

Both stalled jobs eventually resolved WITHOUT intervention, and the pattern that emerged
narrows this considerably:

```
13:44:47  codex-8                queued  [accepted]                                  <-- STILL stuck
13:51:24  f9045569047055-solver  done    [accepted running prompt done delivery-recorded]
14:09:35  codex-8                done    [accepted running prompt done delivery-recorded]
14:17:39  f9045569047055-solver  queued  [accepted]                                  <-- in flight, normal
```

Two facts that kill Hypothesis B and weaken Hypothesis A as stated:

1. **A later job on the SAME agent ran fine while the earlier one stayed stuck.** codex-8's
   13:44:47 job is still queued; its 14:09:35 job completed normally. So this is not the
   agent being undrainable, and not a per-agent lock held forever — it is one specific job
   orphaned in the queue while the agent continues to serve.

2. **The solver's 13:51:24 job completed** roughly 25 minutes after being announced, with a
   full event set including the `prompt`/`text` recording from 43e69be0. So the stream-sink
   change does not block draining; a stalled job under it still runs. Hypothesis B should be
   closed unless new evidence appears.

Revised reading: an announced job can be **orphaned** — accepted into the ledger but dropped
by whatever selects the next job for an agent — while the agent remains healthy and later
jobs for it drain normally. The long delay (25 min) before the solver's job ran also suggests
the drain is being triggered by something intermittent rather than promptly on enqueue.

This makes open question 4 the sharp one: `reap` currently does NOT treat a job that has been
`queued` for 35+ minutes and never `running` as stale, so an orphaned job stays in the ledger
forever and the caller waits on a job that will never run. The 13:44:47 codex-8 job is a live
specimen — please inspect it before reaping it.

## Live state at time of writing

One job orphaned (`codex-8` from 13:44:47 — left in place deliberately as a specimen). The
library-lane siege is RUNNING: driver `bg-1787406671690-2` alive, solver round 1 committed
`b20f2ce` "identify equator homology" and round 2 committed `1f21394` "promote quotient to
TopCat pushout", with a third round queued at 14:17:39. apm-lean trunk untouched at
`2f9048c` (still dry-run).

## Resolution (2026-08-22, ams-claude-fable)

**There is no drainer stall. `announce` never dispatches.** `handle-invoke-announce`
(`transport/http.clj`) calls `create-invoke-job!` and returns 202 — it writes a ledger row in
state `queued` and touches neither `turn-queue/accept-async!` nor any executor. The row is run
by a *second* call carrying the same `job-id`, which `create-invoke-job!` reuses instead of
duplicating (`http_test.clj: invoke-announce-job-is-reused-by-direct-invoke`). That second call
is what the IRC bridge does (`ngircd_bridge.py`: `_announce_invoke` then `/api/alpha/invoke`
with `job_id`), and what the frame drivers' `activate-fn` was.

What broke it: `futon3c-frame18-control 67010bc7` replaced `activate-fn` with a GET, on the
stated premise that "announce already returns 202 … and the per-agent drainer dispatches from
that queue". The drainer drains the *turn-queue*; announce never puts anything in it. From that
commit on, every announced frame job was accepted and never run. That is why the stall starts
at 13:44 — the first dispatch after the driver change — and why `reap` sees nothing stale and
both seats look healthy: nothing failed, nothing was ever asked to run.

Why the drivers were posting to `/api/alpha/invoke/activate` at all: that route exists on
`feature/lane-effects` (`d6f9ec2c Add durable set-alight activation boundary`, 2026-08-21) and
never reached `master`, which is what the :7070 JVM serves. So activate 404'd, the driver
reported `:live-job-activation-failed`, and the 13:02 jobs ran only because something else
posted `/invoke` with their job-ids (their delivery note is `http-direct-response`).

Two dead ends checked so nobody re-walks them:
- **Hypothesis A (cold pouch):** no. `drain!` never sees these jobs; pouch warmth is irrelevant.
- **Hypothesis B (stream-sink commit):** no. Correlated only because it landed minutes before
  the driver change.
- **`/api/alpha/bell` with the announced `job-id` is NOT an activation** under
  `FUTON3C_TYPED_BELLS=true` (set in the serving JVM's environment): `handle-bell` finds the
  existing job and answers `202 {reused? true}` without enqueueing. Verified live on job
  `invoke-1787407775126-138-f354535f` (codex-8): announce → queued 8 s; bell → `reused? true`,
  still queued after 120 s; `/invoke` with the job-id → `running` → `done`, result `probe-ok.`

Fix (frame18-control): `runtime/activate-job!` POSTs `/api/alpha/invoke` with the ticket's
job-id on a daemon thread (the server runs the turn whether or not the client keeps the socket;
`/invoke` blocks for the whole turn) and confirms activation by polling the job out of
`queued`. Idempotent on a running/terminal job. All five activate sites use it.

Live state after the fix: job `…-137-ad9b4afc` (f9045569047055-solver) re-dispatched by hand
through the same `/invoke` path with its prompt rebuilt from the persisted ticket
(`data/apm-lane/f9045569047055/solve.edn :active :request` → `live-proof-phases/prompt`), so
the running siege driver picks it up as designed. Job `…-136-5ab069c3` (codex-8, caller
claude-14) cannot be replayed from the ledger — **the ledger does not persist prompts** —
claude-14 has to re-send it. The still-running `/tmp/dryrun.clj` JVM has the *old* driver code
loaded; its next dispatch will stall again until it is restarted on the fixed code.

Answers to the open questions: (1) moot — `drain!` is never involved; (2) no such lock; (3) yes
— announce should at minimum say in its 202 that a follow-up `/invoke` with this job-id is
required, and it should honour `:mode` (it ignores it today, so announced solve jobs land as
`brief` and skip the work-mode no-execution check); (4) yes — a job `queued` with no
`running` event for longer than any drainer poll interval is not healthy and `reap` should say
so. (3) and (4) are `master` changes to the live server and are left for a deliberate reload.

## Recommendation: run the APM drivers inside the Agency JVM (2026-08-22, ams-claude-fable, for the lane owner)

This failure needed two JVMs on two branches. The driver (`clojure.main /tmp/dryrun.clj`, run
from `futon3c-frame18-control` on `feature/lane-effects`) spoke HTTP to a server running
`master`. That is the only way "POST to a route that exists in my own source tree" can 404,
and the only way a driver can be "fixed" (67010bc7) by reasoning about server behaviour it
cannot see. Each process did its job correctly by its own lights, so nothing logged; the
excursion author spent the afternoon inside the server's drainer because that was the JVM
that was visible. Joe's framing: *extra JVMs = more problems.*

**Proposal.** Merge `feature/lane-effects` into `master` and run the phase drivers
(`live_proof_phases`, `live_learning_phases`, `live_preflight_runtime`, `live_promotion`,
`live_solver_rounds`, `library_lane_*`) in-process in the Agency JVM:

- `announce-fn` / `activate-fn` become direct calls to the job-ledger + invoke path
  (`create-invoke-job!` and the function behind `build-invoke-response`, or a small
  public `dispatch-job!` in `transport/http.clj` that does both). The announce/activate
  two-phase handshake exists for an *external* caller (the IRC bridge) that must reserve a
  job-id before it can promise anything on its surface; an in-process driver has no such
  gap, and the whole class of "accepted but never run" disappears because there is no seam
  for it. `d6f9ec2c`'s `/api/alpha/invoke/activate` then becomes unnecessary rather than
  unported.
- `job-fn` reads the ledger directly instead of `GET /invoke/jobs/<id>`; `persist-fn` is
  unchanged (the EDN state files under `data/apm-lane/<frame>/` are fine and are what lets a
  frame resume across restarts).
- The driver loop (today the body of `/tmp/dryrun.clj`) becomes a function the operator
  starts from the REPL or via one endpoint, not a script that spawns a JVM.

**What it costs / what to check before the restart.**
- A driver bug can now take down the server. Keep the driver loop on its own thread with
  the same catch-all the turn-drainers use.
- `feature/lane-effects` must merge cleanly; `master` has none of `src/futon3c/apm/` today, so
  expect the merge to be mostly additive, with the real conflict surface in
  `transport/http.clj` (activate handler + route) and `http_test.clj`. Do a
  `git merge --no-commit` dry run first.
- `f981f441` (`runtime/activate-job!`, `/invoke` on a daemon thread + confirm-by-poll) is the
  HTTP-era fix. It stays correct for any driver that remains out-of-process, and is the
  thing to delete once the drivers are in-process.
- While the drivers are still a separate JVM: the running `/tmp/dryrun.clj` has the OLD
  `activate-fn` loaded and must be restarted on `f981f441` before its next dispatch, or it
  stalls again.

**Server-side hygiene to fold into the same reload (small, `master`):**
1. `handle-invoke-announce` honours `:mode` (it ignores it; announced solve jobs land as
   `brief` and skip the work-mode no-execution check).
2. The announce 202 says a follow-up `/invoke` with this job-id is required — or, once the
   drivers are in-process, announce is bridge-only and says so in its docstring.
3. `reap` treats a job `queued` with no `running` event for longer than any drainer poll
   interval as unhealthy. Today it is the single worst failure mode available: the caller
   believes it dispatched.
