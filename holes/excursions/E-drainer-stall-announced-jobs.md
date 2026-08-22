# Excursion: E-drainer-stall-announced-jobs — announced jobs sit `queued` and are never drained

**Date:** 2026-08-22
**Status:** IDENTIFY. Reproduced, evidence captured, cause NOT established. Two hypotheses
below, one of which I consider more likely but did not prove. Handing off rather than
guessing further.
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

## Live state at time of writing

Two jobs still queued (`codex-8` from 13:44:47, `f9045569047055-solver` from 13:51:24). The
library-lane siege driver `bg-1787406671690-2` is alive and polling; it will pick the solve up
the moment the job drains, so nothing is lost by leaving them. apm-lean trunk untouched at
`2f9048c`.
