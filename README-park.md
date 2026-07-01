# Parked-on continuations — an agent parks its REPL turn and self-resumes

*(E-repl-continuations; see `holes/excursions/E-repl-continuations.md` for the full
IDENTIFY→INSTANTIATE derivation.)*

## The problem

Agents kept saying *"I'll get back to you once X completes"* and then going **silent**
until Joe manually re-engaged them. There was no way for an agent to park its own turn
on dispatched work (a bell to another agent, a computation, N cluster replies) and
**resume itself, in place, in the REPL buffer, the moment the work lands**.

## The model (within-turn park)

To the operator it is **one turn**. Under the hood:

```
 agent works … POSTs /api/alpha/park on its deps … ends its turn   (segment A)
        │
        ▼  (deps complete server-side)
 backend joins the deps → assembles a resume prompt → hands it to the buffer
        │
        ▼  (Emacs poller delivers it)
 the buffer re-runs the agent with the joined results     (segment B, the continuation)
        │
        ▼
 continuation absorbs A's flair → one turn, totaled time, single divider above input
```

The park/resume is an **implementation detail**: the segments render as a single turn —
cooking time **totaled**, one divider, unified output.

## Components

### Backend — the durable engine + wiring

- **`agency/parked_on.clj`** — the pure, disk-backed continuation engine (NOT substrate-2,
  so it survives pouch teardown with zero :7071 write, like `turn_queue`/`invoke-jobs-ledger`):
  `park!` (+ reconcile-on-park), `note-completion!` (dep-keyed JOIN release, atomic single-fire
  via `swap-vals!`), `rehydrate!` (boot reconcile), `sweep-deadlines!` (deadline backstop). It is
  autobell-back generalised: n=1 caller-keyed → n≥1 dep-keyed, with a durable payload + a runaway
  budget. Unit-tested via `parked_on_test.clj` (the five VERIFY cases).
- **`transport/http.clj`** wiring (flag-gated `FUTON3C_PARKED_ON`, default OFF):
  - `parked-on-notify!` — hot-path hook in `finalize-invoke-job!` (the single completion
    chokepoint), async on `invoke-executor`: a job's terminal state folds into any parked-on
    join awaiting it.
  - `parked-resume!` — on a completed join: for a **buffer** surface (`emacs-repl`) it broadcasts
    `park-ready` over the agency WS **and** pushes the assembled prompt to a **ready-inbox**;
    for a **headless** surface it enqueues a normal turn (server-side).
  - Endpoints: `POST /api/alpha/park`, `GET /api/alpha/parked/ready` (poll-and-consume the inbox),
    `GET /api/alpha/parked` (outstanding parks).

### Emacs — the buffer receiver

- **`emacs/claude-repl-park.el`**:
  - An **async** poller (`url-retrieve`, never blocks the UI — a synchronous GET here is what
    forced `C-g`) that polls the ready-inbox per repl buffer and, when idle, resumes in place.
  - `resume-in-buffer` — injects the assembled prompt at the managed input position and sends it
    through the *normal* turn machinery, so the continuation **streams in natively**, attributed
    to `continuation:` (via `agent-chat-user-speaker`), not the operator.
  - **Flair absorption** — before injecting, it deletes the parked segment's turn-end flair and
    carries its elapsed forward (`agent-chat--accum-elapsed`), so the final segment renders one
    totaled flair. Race-free (retroactive; no dependency on the async park lifecycle).
- **`emacs/agent-chat.el`** — `agent-chat-finish-turn!` emits the flair with any carried-forward
  elapsed and records `agent-chat--last-flair-elapsed` for the next continuation to absorb.

## Delivery: polling, with WS as a latent fast-path

The buffer **polls** the ready-inbox (async, non-blocking). A `park-ready` WS broadcast is *also*
wired for when the shared agency WS (`futon-agency-ws`, `/agency/ws`) is connected — it isn't
today (an incomplete cutover), so polling is the working path; the WS path activates for free once
that route is sorted.

## Usage

```
# enable (default OFF)
export FUTON3C_PARKED_ON=1            # or: (System/setProperty "FUTON3C_PARKED_ON" "true")

# an agent parks its turn on deps it dispatched (job-ids), with a continuation payload:
curl -X POST localhost:7070/api/alpha/park -d '{
  "agent":"claude-1", "session":"<sid>", "surface":"emacs-repl",
  "awaiting":["<bell/job-id>", ...],
  "deadline-ms":<t>, "payload":"<what to do when the join completes>"}'
```

The boot hook (`start-parked-on!`) runs `rehydrate!` + a 30s `sweep-deadlines!` daemon (NOT the
Arxana Clock). The Emacs side auto-enables on load of `claude-repl-park.el`.

## Finalize-once (defer the parked segment)

A unified turn finalizes **exactly once**. When a segment parks, its turn-end
finalization is **deferred**: no flair, no clock read, no turn-evidence emit — it only
banks its elapsed (`agent-chat--accum-elapsed`) and output (`agent-chat--accum-text`).
The continuation is the final segment and finalizes once: one totaled flair + one
unified-output evidence record.

- **Detection is race-free even for a fast dep:** `GET /api/alpha/parked` returns
  `:more-pending = (outstanding park OR a ready resume already in the inbox)`, so the whole
  window from `park!` through poller-delivery reads "more coming". `agent-chat-finish-turn!`
  takes a `continued` flag; the streaming done-handler decides it once and both defers the
  evidence emit and passes it to `finish-turn!`.
- **Flair placement:** the turn-end clock sync is kept *synchronous* on purpose — its bounded
  in-RAM read spins the event loop just enough to settle the buffer so the flair lands after
  the response. (Async'ing it drifted the flair above the response; the real multi-second
  freeze was the *poller*, which is async.)

## Status / remaining

- ✅ Engine + 5-case tests; hot-path hook; endpoints; buffer poller + resume-in-place;
  `continuation:` attribution.
- ✅ Unified turn: single totaled flair (finalize-once), unified-output embedding (the final
  turn-evidence covers the whole turn — no partial-segment residual, since the parked segment
  defers its emit).
- ✅ Non-blocking: async poller; synchronous clock read doubles as the flair settle-point.
- ◻︎ WS fast-path activation (a `park-ready` broadcast is wired; blocked on the `/agency/ws`
  connector being live).
