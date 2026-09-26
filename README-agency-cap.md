# The invoke cap — retired for bells (2026-08-02)

*(This document began as a stopgap note after a live loss: job
`invoke-1785165682706-208-826bd944`, 2026-07-27. The architectural repair it
called "the open gap" landed on 2026-08-02 and is described below. The original
failure is kept because it is the thing the design has to keep preventing.)*

## The failure that started it

A bell was dispatched to `codex-7` carrying a long compute packet (local
causal-state reconstruction over a spacetime field). It ran for **31m52s**
(16:21:23 → 16:53:15 BST) and then:

```
state: failed
mode:  brief
result: (empty)
```

No error, no partial result, no summary on the completion bell. The parked
caller woke with `(no summary)` and had to reconstruct what happened from file
mtimes in the working tree.

**About half the work had actually been done** — the expensive reconstruction
had written its outputs to disk before the kill — but nothing was committed and
nothing was reported. Had the caller trusted the bell payload, that work would
have looked like a total loss.

## Why raising the number never worked

The cap was raised from 30 to 60 minutes on 2026-08-02 as a stopgap, and it
moved the cliff without removing it. The reason is that the cap was never one
number. Six layers could independently end a turn, and the **innermost kill
always won**:

| layer | file | old behaviour |
|---|---|---|
| async job supervisor | `transport/http.clj` | soft cap 35m → `overrun`; hard ceiling `2×cap` = 70m → terminal + worker interrupt |
| registry invoke | `agency/registry.clj` | `deref` then `future-cancel` — result abandoned |
| WS invoke | `transport/ws/invoke.clj` | 60m default; on timeout **deleted the pending promise**, so a late `resolve!` returned `false` and the reply was dropped |
| codex process | `agents/codex_cli.clj` | `.destroyForcibly` at 60m |
| whistles | `social/whistles.clj` | 60m default |
| IRC relay | `dev.clj` | its own 60m hard timeout |

And the compounding defect: **the caller's `--timeout-ms` never reached the
worker.** `make-codex-invoke-fn` captured `:timeout-ms` from
`CODEX_INVOKE_TIMEOUT_MS` at *registration* time, not per call. So the per-bell
number only parameterised the layers that *give up waiting* — it could not
extend anything. `agency_send.py`'s 4-hour `BELL_DEFAULT_TIMEOUT_MS` was
therefore **placebo on the codex route**: it bought patience in the outer
supervisor while the inner process was destroyed at 60m. Binding constraint was
`min(60m destroy, 70m ceiling)` regardless of what the caller passed.

That is why "we fixed the 30-minute cap" was true and unhelpful more than once.

## The policy now

**Wall clock is an SLA signal, not evidence of stuckness.** A bell that is
still streaming events is working. Long research and coding turns are supposed
to be long, and a turn that emits nothing for a while may simply be computing.

- **Bells have no default wall-clock termination.**
- The soft cap (35m, `FUTON3C_JOB_CAP_MS`) still fires, but it is
  **observational**: the job moves to `overrun`, stays non-terminal, remains
  pollable, and finalises normally when its result arrives.
- A turn ends only on:
  - **explicit cancellation** — `POST /api/alpha/invoke/jobs/:id/cancel`
  - **confirmed process/transport death**
  - **an operator-configured ceiling** — `FUTON3C_JOB_CEILING_MS`, unset by
    default; `0` also means "none"
  - shutdown
- **Whistles keep a strict synchronous deadline**, because a caller is blocked
  on them — but the deadline detaches rather than kills. `POST
  /api/alpha/whistle` returns `504` with a `job-id` and `status-url`, and the
  turn keeps running.
- **One layer is the lifecycle authority**: the durable job supervisor
  (`supervise-invoke-future!`). WS invoke, the registry, and the codex adapter
  interpret a nil/non-positive timeout as unbounded and do not independently
  destroy workers.

### Cancelling a job

```bash
curl -s -X POST localhost:7070/api/alpha/invoke/jobs/<job-id>/cancel \
  -H 'Content-Type: application/json' \
  -d '{"caller":"joe","reason":"superseded"}'
```

This takes the terminal transition first (so the cancellation wins the
single-finalizer race and the job records `cancelled` / `operator-cancelled`,
not `failed`), then kills the agent's process tree and interrupts the
supervising worker. Cancelling an already-terminal job is a `409` and never
rewrites its outcome.

### Restoring a bound

Both knobs read a **System property first, then the environment**, so they can
be changed on the live JVM over Drawbridge without a restart (I-0):

| setting | default | meaning |
|---|---|---|
| `FUTON3C_JOB_CAP_MS` | 35m | soft cap → `overrun` (observational) |
| `FUTON3C_JOB_CEILING_MS` | *unset* | hard ceiling → terminal. `0` = none |
| `FUTON3C_CODEX_PROCESS_TIMEOUT_MS` | *unset* | per-process bound; overrides the caller. `0` = unbounded |

`FUTON3C_CODEX_PROCESS_TIMEOUT_MS` is read **per call**, deliberately: agents
registered before the change closed over the old value, and this is the lever
that reaches them without re-registration.

### Per-call timeouts now reach the process

`make-invoke-fn` (both `codex_cli` and the `dev.clj` wrapper) accepts a third
arity:

```clojure
(invoke-fn prompt session-id {:timeout-ms 5400000})
```

`registry/invoke-agent!` selects it by a **structural arity probe**
(`declares-arity?`), not by catching `ArityException` from the call — an
exception-driven probe cannot distinguish "this fn takes fewer args" from "the
body threw ArityException", and retrying in the second case dispatches the turn
twice.

### Late results are harvested, not discarded

- `registry/invoke-agent!` no longer calls `future-cancel` at a deadline. That
  interrupted the JVM thread but did **not** kill the codex child, leaving a
  live orphan writing files with nobody listening. It now **detaches**: the
  result map carries `:detached? true`, the lane stays `:invoking` until the
  turn really finishes, and the real completion releases it.
- `ws/invoke.clj` keeps its pending entry after a timeout. A late `resolve!`
  now returns `true` and routes the payload to a late-result handler
  (`set-late-result-handler!`) instead of dropping it.

### Confirmed transport death has to actually fire

Removing the WS deadline would otherwise trade a lost result for a permanently
wedged lane: a caller blocked on a socket that dies has no clock left to rescue
it. So eviction, `unregister!`, and `unregister-current!` now deliver
`disconnected-result` (`{:error :ws-disconnected}`) to every caller still
waiting, and `invoke!` re-checks registration after enqueuing to close the race
where the send itself triggers the eviction. This is the half of "no default
termination" that is easy to forget: an unbounded wait is only safe when every
*real* end-of-turn signal is wired.

**`:detached? true` means the turn is still running.** It is not "no work
happened".

## Corollary for the caller (unchanged, still true)

A job that reports a terminal failure is **not** proof that no work happened.
Check the working tree before deciding to re-dispatch:

```bash
git -C <repo> status --porcelain
find <repo> -newermt '<dispatch time>' -not -path '*/.git/*' -type f
```

Mind the clock skew: job records are **UTC** (`...Z`), while `ls` and `find`
report **local time**. During BST that is a one-hour offset, easily large
enough to make genuine in-window work look like it predates the dispatch.

## Dispatch ergonomics

`agency_send.py` defaults `--kind bell` to 4 hours (`BELL_DEFAULT_TIMEOUT_MS`).
With the ceiling retired that number is now only a soft-cap hint, not a
survival requirement. The lesson that produced it still stands and is worth
keeping:

> **An opt-in fix that must be remembered on every call, under time pressure,
> is not a fix. The default is the fix.**

Prefer the bundled dispatch-and-park:

```bash
python3 futon3c/scripts/agency_send.py --from <your-id> --to <codex-N> --kind bell \
  --park --park-deadline 6000 --park-payload "<review checklist>" --surface headless
```

Set the park deadline longer than any soft cap so the deadline wake is a
genuine backstop rather than firing while the job is legitimately running.

## Still open

- **The warm-pouch route keeps a 60m default** (`agency/agent_pouch.clj`).
  `read-turn-with-timeout` now *supports* unbounded (nil/non-positive), but the
  default is unchanged on purpose: the pouch is a persistent stdio protocol
  where abandoning a read leaves an unconsumed `result` that shifts every later
  turn one behind — the desync `drain-pending!` exists to repair. Removing the
  bound there needs a pouch-level cancel first.
- **The WS late-result handler is a hook with no default wiring.** Late replies
  are no longer dropped at the transport, but nothing yet attaches them back to
  the originating job ledger entry. The mapping from `invoke-id` to `job-id`
  does not exist.
- **The IRC relay keeps its own hard timeout** (`dev.clj`,
  `FUTON3C_RELAY_INVOKE_HARD_TIMEOUT_MS`). It is a different surface with a
  human waiting in a channel, so a bound is defensible — but it is not derived
  from the supervisor and remains a seventh independent number.
- **Silence-based liveness is not wired.** `codex_cli` already emits stream
  events and tracks `:last-output-at`; a watchdog that alerts (or reaps) on
  *silence* rather than elapsed time would be a much better stuckness signal
  than any clock. This is the natural next step, and the thing that makes an
  unbounded default safe rather than merely correct.
