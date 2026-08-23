# Excursion: E-apm-drainer — `announce` is bookkeeping, not dispatch

**Date:** 2026-08-22
**Status:** IDENTIFY. Code path traced, leading explanation stated with evidence, one
contradicting datapoint recorded honestly (see "What does not fit").
**Repo:** futon3c — `transport/http.clj` (`handle-invoke-announce`, `create-invoke-job!`,
`build-invoke-response`), `agency/turn_queue.clj` (`accept-and-drain!`, drainer v2 thread).
**Spawned from:** E-drainer-stall-announced-jobs. Joe's question, which is the right one:
*"I don't understand why we'd spend time waiting around???"*

## HEAD (one line)
**`POST /api/alpha/invoke/announce` writes a ledger record and returns
`202 {:ok true, :accepted true, :state "queued"}` — and, as far as I can trace, asks nobody
to run it.** The job executes later only if something *else* independently invokes that
agent and adopts the pending job-id. So an "accepted" job is not scheduled work; it is a
reservation waiting for a coincidence. The waiting is not queue latency. It is the absence
of a dispatch.

## The trace

`handle-invoke-announce` validates the payload, confirms the agent is registered, then calls
`create-invoke-job!` and returns 202. `create-invoke-job!` does exactly one thing:

```clojure
(defn- create-invoke-job!
  [{:keys [requested-job-id agent-id prompt caller surface ...]}]
  (update-invoke-jobs-ledger!  ;; <-- writes a ledger record. That is all.
    ...))
```

Searching the whole announce handler for `accept-and-drain!`, `drain`, or any turn-queue
call finds only the docstring of `repl-through-queue?` — no code. Nothing enqueues a
turn-queue entry; nothing notifies a per-agent drainer thread.

Contrast the BELL path, which does dispatch:

```clojure
(defn accept-and-drain! [entry process-fn]
  (let [{:keys [status entry waiter]} (accept! (assoc entry :process-fn process-fn))]
    (when-not (= :deduped status)
      (drain! (:to entry) process-fn))
    @waiter))
```

and drainer v2, a per-agent daemon thread that `.wait`s on a monitor and drains when
**notified** or on timeout. A job that never entered the turn queue never notifies anything,
so no drainer wakes for it.

## Observed behaviour, which fits

```
13:44:47  codex-8                queued  [accepted]        <-- still queued 45m later, never ran
13:51:24  f9045569047055-solver  done                      <-- ran ~25 min after announce
14:09:35  codex-8                done                      <-- SAME agent, later job, ran fine
14:17:39  f9045569047055-solver  queued  [accepted]        <-- waited 13m+
14:29:xx  codex-13               running                   <-- queued -> running in ~1 min
```

Under "announce reserves, something else dispatches", all of this is one phenomenon:
- codex-8's 13:44 job is orphaned because no further invoke of codex-8 adopted it.
- codex-8's 14:09 job ran because it WAS an actual invoke.
- The solver's rounds run at irregular multi-minute delays because they run whenever the next
  invoke happens to occur.
- codex-13 was fast because its job was a real invoke, not a reservation.

The delay is not proportional to load. It is proportional to how long until someone else
invokes that agent — which for a dedicated frame seat that nobody else talks to is unbounded.

## Why this matters more than anything else in the APM stack

`live_proof_phases/run-live!` and `live_learning_phases/run-live!` — every APM frame phase,
countdown and library-lane alike — dispatch by calling `announce`. They then polled
`/api/alpha/invoke/activate`, which **does not exist** (404, absent from the route table);
that was fixed in futon3c-frame18-control `67010bc7` by confirming the job instead. But
removing a broken activation gate does not supply a working one. If announce does not
dispatch, nothing in the current APM path does.

That reframes the frame history. Every campaign in `data/apm-campaigns/` cleared preflight,
solve and verify, and **no campaign has ever completed a student attempt**. A dispatch
mechanism that fires only when some unrelated invoke happens to land is an excellent
explanation for a pipeline that advances erratically and stalls at the phase where nobody is
independently poking the agent.

Throughput consequence for the library lane: a solver round costs ~4 minutes of actual work
and 13-25 minutes of waiting. Against a 50-round budget the waiting dominates by roughly 5:1,
so unattended operation is gated by this and not by any of the lane machinery.

## What does not fit — recorded rather than smoothed over

The solver's FIRST job (13:02:59) ran essentially immediately: created 13:02:59, finished
13:07:05, and 4 minutes is about the length of the solve itself. Under a strict reading of
"announce never dispatches", that job should have waited too.

Possibilities I did not resolve:
- Something in the same `run-live!` call adopted it promptly (but `/invoke/activate` 404s).
- A drainer timeout wake happened to land immediately.
- `announce` does enqueue via a path I did not find, and the real bug is narrower — e.g. the
  notify is missed when the agent has no warm pouch, so it waits for the next timeout wake.

That last one would also explain the irregular multi-minute delays, and it is compatible with
everything above. **Whoever picks this up should settle this first**, because it decides
whether the fix is "make announce dispatch" or "fix a missed notify".

## Recommendations, in order

1. **Decide the contract for `announce`.** If it is a reservation, it must not answer
   `{:accepted true}` — that word is why every caller in the APM stack believes it dispatched.
   If it is meant to dispatch, it must enqueue and notify.
2. **Give the APM drivers a dispatch that actually dispatches.** The bell path
   (`accept-and-drain!`) already has the property required: single-writer, durable,
   reply-routed, and it *runs the turn*. `live_proof_phases` and `live_learning_phases` want
   that, or a direct `/api/alpha/invoke`.
3. **Make an undispatched job visible.** `reap` does not treat queued-and-never-running as
   stale, so an orphan sits in the ledger forever. `*active-agents*` now flags `queued!` past
   five minutes (futon3c `0a299680`) as a stopgap, but the server should know.
4. Only then measure siege throughput. Any round-count or duration budget taken now is
   measuring the dispatch gap, not the solver.

## Specimen

The orphaned job `codex-8` created 13:44:47 on 2026-08-22 was deliberately left in place
rather than reaped. It is a live example of an accepted job that will never run. Please
inspect before clearing.
