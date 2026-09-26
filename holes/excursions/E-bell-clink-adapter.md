# Excursion: E-bell-clink-adapter — a pull delivery lane for CLI seats ("clink" = CLI Link)

**Date:** 2026-08-26
**Status:** BUILT, awaiting a restart to finish. P1 (`f6462999`) + P2 (`66858f5a`) by
codex-20, review fixes `dd3a9e61` and `3f842893`. The pull lane is live and was
demonstrated end-to-end on 2026-08-26; the ack endpoint is written and tested but is a
NEW ROUTE, so the running JVM cannot serve it until Joe restarts. See "What was built"
at the end. Originally written at Joe's request after a live bifurcation during the f41
watch.
**Repo:** futon3c — the delivery path: `agency/agent_pouch.clj` (`spawn-pouch!`),
`transport/http.clj` (handle-bell + auto-bellback), the roster
(`GET /api/alpha/agents`), and `scripts/agency_send.py`.
**Spawned from:** the f41 frame watch (`TN-opus-F41-analysis.md`), where claude-12 belled
codex-10 twice from a CLI seat, received nothing, reported "no replies" to Joe — and the
replies existed, were `state done`, and had been consumed by a headless fork of the same
session.
**Sibling:** `E-crossed-bells.md`. That note is about *threading*; this one is about
*delivery*. See "Three separate problems" below.

## HEAD (one line)
**A CLI seat has no place to put an inbound message except inside a second copy of itself.**
Agency's only delivery mechanism is `spawn-pouch!` running `claude --print --resume <sid>`,
so delivering to an interactive seat forks the session. The fix is not a better push: it is
a **pull lane** — write the payload to an inbox the seat already watches, and require a
consumption ack that only the seat can emit.

## Three separate problems, often conflated

| | question | status |
|---|---|---|
| routing | does the bell reach the right agent? | solved — durable queue, reply-routes, per-agent serialization |
| threading | can the agent tell a reply from a new request? | `E-crossed-bells` — designed, not built |
| **delivery** | **is there somewhere to put it that does not create a second inhabitant?** | **this note** |

Routing being transactional is what makes the third problem invisible: the job says
`accepted`, then `done`, and every transport-level indicator is green while the recipient —
the actual interactive session — never sees anything.

## What was measured, 2026-08-26

claude-12 was an Agency-registered agent whose session Joe quit and resumed in a CLI. The
registration survived the move, so the roster's `session-id` and the live terminal's session
id are the same uuid — which is the whole hazard in one line:

    roster claude-12 session-id:  c77bb7c4-a680-4105-90dc-d84aaf8cc108
    terminal session (scratchpad/transcript path): c77bb7c4-a680-4105-90dc-d84aaf8cc108
    live process: claude --print ... --resume c77bb7c4-a680-4105-90dc-d84aaf8cc108

Two bells sent with `--from claude-12` produced three `auto-bellback-*` jobs, all
`state done`, all consumed by that forked process. The fork did real work — it took
codex-10's pushback, corrected TN §16 (`769ac7d0`) and §17 (`3447ebda`), and independently
re-ran a fingerprint the terminal was running concurrently. Nothing collided. What was lost
was an hour of the terminal reasoning from a §16 that had already been corrected, and a
duplicated measurement.

**Note the trigger.** The recorded rule was "do not park from a CLI seat." The terminal
followed it — declined to park, polled job state by hand — and forked anyway, because
`--from <cli-seat>` records the mesh edge that routes the *completion bellback* back to that
seat, and delivery to a CLI seat is a fork whether or not a park is involved. Parking is one
trigger, not the trigger.

## Why this does not reopen the 2026-08-20 ruling

Joe then: *"'agent identity is singular' was invented very long ago for exactly this reason.
Bifurcations are a known hazard... leaving the possibility to create bifurcations 'live' is
not the answer."* The conclusion recorded was that silent non-delivery is **safer** than
delivery, because delivery means forking a live session.

That conclusion is about `spawn-pouch!`, which is the only delivery mechanism there is. It
says: do not make *spawn-delivery* work for CLI seats. A pull lane is a different object —
it never starts a process, so it never creates a second inhabitant of one identity. I-1 and
I-3 hold: one agent, one session, inhabiting its peripherals rather than delegating them to
a clone. The proposal is not "make bifurcation work"; it is "remove the reason to bifurcate."

## The design

**The inbound channel already exists, and it is not Agency's.** A running interactive session
has a live watcher path — the harness's Monitor / task-notification mechanism. An
`inotifywait -m` on a directory turns each arriving file into a notification in the terminal
within seconds. Agency does not need to invent delivery for CLI seats; it needs to stop
trying to, and write a file the session's own watcher is already looking at.

1. **Roster marks the lane.** A seat registers with `delivery: :inbox` (pull-only).
   `spawn-pouch!` is never called for such a seat — no exceptions, or this becomes a second
   way to fork rather than a way to avoid forking. The roster row must make the lane visible
   so nothing dispatches to it expecting push semantics.
2. **Bell writes a file.** `POST /api/alpha/bell` to a pull-only seat writes the payload to
   `~/.claude/agency-inbox/<agent-id>/<job-id>.json` and sets job state `delivered`.
3. **`delivered` and `done` are different states.** `done` must mean *consumed*, and only the
   seat can say so. This is the crux, not a detail — see the criterion below.
4. **The seat acks on consumption.** It reads the file, acts, and POSTs an ack (or moves the
   file to `consumed/`), which flips the job to `done`. An unread bell is then visibly
   unread.
5. **Staleness is alarmed.** Age-of-oldest-unconsumed per seat is the one number that catches
   a seat that died or stopped polling. Without it a pull seat degrades silently, which is
   the failure mode this note exists to remove.

## The criterion this rests on

Tonight produced the same rule at three layers — the APM witness standard (`:used-ids` is a
self-report; a use counts when the committed artifact carries the memory's fingerprint), the
§9 rule (an id an agent mints for something it claims to have written is a CLAIM until the
artifact is read back), and this:

> **A protocol needs a receipt that the obligated party could not have produced without
> discharging the obligation.**

`accepted` and `done` fail it — Agency emits both by itself, without the recipient doing
anything. A fingerprint passes it: an identifier cannot appear in the artifact unless someone
wrote it there. A consumption ack passes it: only the seat that read the file can emit it.
That is the reason to require the ack rather than just watching the directory, and it is
what makes this an adapter rather than a convention.

## Relation to typed bells

The typed-bell contract (`type=query|answer|assert|…`, optional `ref`) already types the
illocutionary act, which is the same instinct one layer up. What it does not yet do is check
that a declared obligation was discharged: nothing computes *queries with no answer on the
ref*. That is the coordination-layer counterpart of "unwitnessed uses," a number that has
been computed for memories (0 unwitnessed of 35, `NOTE-fingerprint-audit-2026-08-25.md`) and
never once for bells. Worth running before building anything — it would say how large this
problem actually is.

## What exists today (stopgap, no Agency change)

- An unregistered CLI can already **send**: `agency_send.py` just POSTs. A monitoring seat
  that never registers cannot be forked, and can dispatch work today. It only lacks a return
  path.
- The **pull half** needs nothing new: arm a Monitor on a directory and name that path in the
  packet ("leave your reply at `<path>`"). This is the per-packet convention the terminal
  adopted after the incident. The adapter's value is making it the supported lane instead of
  something each sender must remember.

## Risks / open questions

- **Two delivery mechanisms.** A seat could look reachable on the roster while its inbox goes
  unread — the same shape as `:regulator/status` reading `:running` for nine minutes while
  ticks were frozen (`TN-spec-delta` §15). The ack and the staleness number are what keep the
  roster row honest; shipping without them would be worse than not shipping.
- **Who registers as pull-only?** If Joe's second-CLI monitoring seat never registers at all,
  bells to it are `agent-not-found`. Registering it pull-only is what gives it a return path
  without giving it a fork surface — so pull-only registration is the point, not a detail.
- **Migration.** claude-12 is currently registered push-capable with a live terminal on the
  same session id. Either it moves to pull-only or it should be deregistered; leaving it as
  is means every future bellback forks it.
- **Does the ack need to be in-band?** A file move is simplest and needs no endpoint, but then
  job state lives in the filesystem rather than the ledger. An explicit
  `POST /api/alpha/invoke/jobs/<id>/ack` keeps the ledger authoritative. Undecided.

## What was measured before building (2026-08-26)

From the 1,450-job invoke ledger:

- **The typed-bell obligation check proposed above is degenerate.** One `:query` in the
  entire ledger, zero `:answer`s (796 `:request`, 653 untyped). n=1 says nothing about
  size, so "queries with no answer on the ref" was not worth building on.
- **The delivery side carries the events.** 57 bells addressed to `claude-cli` — an
  unregistered CLI seat — every one `failed / agent-not-found`, all on 2026-08-24 between
  08:30 and 13:09. That is the no-return-path cost, measured. Separately, 24 auto-bellbacks
  to claude-type seats, all `state done`.
- **Every one of the 108 jobs to claude-type seats carries the same self-issued receipt:**
  `{:status "delivered", :note "bell-job-ready"}`, written by Agency at dispatch. This is
  the criterion failing in the data rather than in the argument.
- claude-12 received two further auto-bellbacks at 10:20 and 10:22 on 2026-08-26 — after
  this note was written — and was still registered push-capable on session-id
  `c77bb7c4-…`, the same uuid as the live terminal. The hazard was open while the note
  describing it sat in the repo.

## What was built

- `:agent/delivery-mode` (`:push` / `:inbox`) on the agent record, persisted through
  `roster_store` and the restore path so a pull-only seat cannot come back push-capable
  after a restart.
- The no-spawn guard lives in `reg/invoke-agent!`, not in the bell handler. Every dispatch
  path funnels through it via `invoke-agent-with-session-recovery!`, including
  `enqueue-auto-bellback!`, which bypasses `handle-bell` entirely and is the path that
  actually forked claude-12. A guard in the handler alone would have left the measured
  defect open.
- `futon3c.agency.inbox` writes `<root>/<agent-id>/<job-id>.json` with a tmp-then-rename
  so a watcher never reads a truncated file, and embeds `ack-url` so the file is
  self-describing to a reader that knows nothing about Agency.
- `delivered` as a non-terminal job state. Three sites define "terminal" and two of them
  do it as the complement of `{queued, running, overrun}`, so a naive `delivered` would
  have been born terminal and unackable. They still disagree with the explicit allow-list
  in `terminal-invoke-state?`; unifying them is a separate change.
- `POST /api/alpha/invoke/jobs/:id/ack`, the consumption receipt. Records
  `inbox-consumed-ack` — deliberately distinguishable in the ledger from Agency's own
  `bell-job-ready` — appends an `acked` event, and moves the file to `consumed/`.
- Age-of-oldest-unconsumed per seat on `GET /api/alpha/agents`, and as an aggregate with
  the owning agent-id on `/health`, folded over the in-memory ledger with no filesystem
  scan.
- `scripts/clink-inbox.py` — `list` / `read` / `ack` for a CLI seat, reading `ack-url`
  out of the payload rather than reconstructing it.

### The finalize race, found in review

`finalize-invoke-job!` decided whether ITS call had performed the terminal transition by
reading an atom that the function passed to `swap!` had `reset!`. `swap!` re-runs that
function on CAS contention, and a discarded attempt's side effect has already happened —
so a losing caller came away holding the winner's flag. Two callers could both believe
they finalized: two auto-bellbacks, and, once the ack existed, two consumption acks.
This is very likely the `reaper-and-supervisor-race-finalizes-once` flake that showed two
bellbacks once during P2 and did not reproduce. Fixed in `3f842893` by reading the
transition from `swap-vals!`: the caller that saw the job non-terminal in the
ledger-before value is the one that committed, and only one caller can.

## Live acceptance, 2026-08-26

`claude-clink-1` was registered pull-only from a live CLI session (an ordinary HTTP POST),
a directory watcher was armed on its inbox, and codex-20 was asked to send it one real
bell.

- The bell landed as `~/.claude/agency-inbox/claude-clink-1/invoke-1787744055336-1458-986e7491.json`
  and the watcher surfaced it in the terminal within seconds. **No copy of the session was
  started.** That is the thing this excursion exists to achieve.
- The job sat at `delivered` with `{:status "pending"}` — not a self-issued `done`.
- The roster row read `delivery-mode inbox`, `invoke-route inbox`,
  `"pull-only seat — bells are written to its inbox; invoke is refused"`.
- `/health` reported `unconsumed-count 1`, `oldest-unconsumed-agent-id claude-clink-1`,
  with the age climbing — the alarm correctly describing an unread bell.
- `clink-inbox.py list` and `read` worked against the real file.

### After the restart (2026-08-26, ~11:57Z)

Joe restarted the JVM and the remaining leg closed:

- `POST /api/alpha/invoke/jobs/:id/ack` served. The delivery left pending above acked to
  `done` with receipt `inbox-consumed-ack`, the file moved to
  `consumed/invoke-1787744055336-1458-986e7491.json`, `/health` returned to
  `unconsumed-count 0`, and a second ack was refused `invoke-job-not-delivered`.
- A further bell was sent, delivered, listed, and acked cleanly — a full cycle on the
  restarted server.
- The `delivery-mode` round-trip was verified in both legs: registering pull-only writes
  `:delivery-mode :inbox` into the durable roster, and replaying that payload through
  `POST /api/alpha/agents/restore` (exactly what `restore-agent-via-handler!` sends on
  boot) brings the seat back as `invoke-route inbox`.

Note for anyone reloading rather than restarting: the seat came back `push` across this
restart, which looked like a persistence defect and was not. `roster_store` had not been
among the namespaces reloaded into the running JVM, so the row was persisted by the old
projection with no `delivery-mode` field at all, and restore defaulted it. **Reload every
namespace a change touches, or the durable artifact is written by the old code while the
roster shows the new behaviour.**

**What did not complete at the time of writing: the ack.** `POST …/ack` is a new route, and route topology is
captured in the handler closure at build time. The namespace reload took (all the function
redefinitions above are live), but `!handler-builder` is nil on this JVM — it was started
by a path that never captured a rebuild function — so the handler cannot be rebuilt in
place and the route 404s. `README-drawbridge` already classifies route-topology changes as
restart-only. **A restart of the futon3c JVM, which is Joe's call, finishes this.** The
delivery above is deliberately left unacked so the restart has something real to discharge.
