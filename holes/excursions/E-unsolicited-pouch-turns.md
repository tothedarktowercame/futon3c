# Excursion: E-unsolicited-pouch-turns — the pouch takes turns nobody fed it

**Date:** 2026-08-03
**Status:** INSTANTIATE for fix (1) — built, tested, **flag-gated load-dark**
(`FUTON3C_POUCH_DEMUX`, default OFF; the OFF path is the original synchronous read).
Fixes (2)–(5) still scoped, not built.
**Repo:** futon3c (`agency/agent_pouch.clj`, `transport/http.clj`, `emacs/claude-repl.el`).
**Spawned from:** Joe, 2026-08-03 — "badly sequenced agency replies … M-agency-hardening needs to be
revisited", citing `*claude-repl:claude-11*` lines 345–347.
**Reopens:** `holes/missions/M-agency-hardening.md` (CLOSED 2026-06-12) and its logic model
`holes/missions/turn-delivery-invariants.md`.

## HEAD (the bug, one line)

The invariants D1–D5 are all stated over **turns the queue accepts**; the warm pouch has a second,
unmodelled turn source — the agent's own background-task completions — which emit `result` events that
**no `feed-turn!` is waiting on**, and every such event shifts the operator's REPL one reply behind.

## The incident, reconstructed from artifacts

Buffer `*claude-repl:claude-11*` 345–361 shows Joe's message answered by an unrelated status report,
Joe re-sending the identical message, and only then getting a real answer.

The pouch transcript (`~/.claude/projects/-home-joe-code/4a51ee7e-….jsonl`) shows the agent behaved
**correctly**:

| when | uuid | what |
|---|---|---|
| 06:32:08.247Z | `e2ef00ee` | assistant text "Still computing — 20 null draws…", parent `16a73122` = **a tool_result** |
| 06:34:47.176Z | `8482fc4d` | user — Joe's "I think damage band across pheno / geno / exo…" |
| 06:35:21.110Z | `e355c06d` | assistant "Good — that settles the criterion." (parent chain → `8482fc4d`) |

Joe's message entered the pouch **exactly once** and was answered **correctly, in 34 s**. The text the
REPL displayed under his prompt was produced **2 m 39 s before his message existed**, as the tail of a
turn whose parent is a tool_result — i.e. a turn the agent took on its own.

`/tmp/invoke-trace.log` shows **two** operator feeds — `06:34:45.667Z` and `06:35:00.749Z` — against
that **one** transcript user message. One accepted operator turn produced no pouch turn of its own.

So the agent was never confused. **The surface mis-paired prompts to replies.** No amount of agent-side
care could have prevented it, and the operator's only signal was that the answer looked wrong.

## The mechanism (file:line)

1. **The pouch is a persistent stream with a second writer: itself.**
   `spawn-pouch!` (`agent_pouch.clj:345-352`) runs
   `claude --print --input-format stream-json --output-format stream-json --verbose`.
   In that mode a background task completing **re-invokes the agent**, producing a full turn and its own
   `result` event with no corresponding stdin write. The transcript records these as
   `<task-notification>` user messages injected by the harness (e.g. `0cda4b5a`, 06:30:50Z).

2. **`read-turn*` returns on the FIRST `result` it sees** (`agent_pouch.clj:429-441`). It cannot tell
   its own turn's result from an unsolicited one.

3. **`drain-pending!`'s correctness premise is false.** Its docstring (`agent_pouch.clj:449-456`) reads:
   *"Under feed-turn!'s per-pouch lock the process is idle between turns, so anything readable here is
   orphaned output."* Measured on claude-11 for 2026-08-03:

   | turn source | count |
   |---|---|
   | fed via `feed-turn!` (`--- CURRENT TURN ---`) | **11** |
   | self-initiated by task-notification | **23** |
   | inputs queued mid-turn (`queue-operation`) | 64 |

   The pouch is busy between fed turns **2:1**. Idleness is the exception, not the rule. `drain-pending!`
   is also *blind*: it discards the autonomous turn's text outright, so work the agent genuinely did is
   never shown to the operator — and it cannot drop bytes not yet flushed, so it races a turn still in
   flight.

4. **Nothing carries a turn identity to the surface.** The `done` event is
   `{:type "done" :ok :result :session-id}` (`http.clj:4128-4133`) — no turn-id, msg-id, or job-id.
   `claude-repl.el` correlates only by process identity, a single slot per buffer
   (`agent-chat--pending-process`, `claude-repl.el:1021, 1052-1053`). The buffer therefore **renders
   whatever text arrives next under whatever prompt was typed last**. It has no means to detect a
   mis-pairing even in principle.

5. **The D1 gate is unrunnable as instrumented.** `turn-delivery-invariants.md:122-126` specifies the
   gate as *"each `(recipient, msg-id)` appears once."* In `/tmp/invoke-trace.log`, **`msg-id` is empty
   on all 3057 lines.** The 2026-06-12 measurement that disproved the two-dispatcher hypothesis and
   **dropped E1** counted *thread names*, not msg-ids. D1 may well hold — but it has not been measured
   the way the gate says, and cannot be until `msg-id` is populated.

6. **The one diagnostic is unobservable.** The desync repair prints
   `[pouch] <id> drained N stale line(s)` (`agent_pouch.clj:513-516`). The serving JVM's stdout is
   `/dev/pts/3` — a terminal, not journald, not a file (`futon1b-server` is the only futon systemd unit).
   Nobody can audit whether the 2026-06-11 desync fix fires, or how often. M-agency-hardening W5 required
   an *"append-only runtime log … routed to ignored runtime storage, preferably `data/`"*; that
   deliverable is not in place for the pouch.

## Why the June work did not catch this

`E-per-turn-isolation.md` (2026-06-05) scoped four fixes. E2 in `turn-delivery-invariants.md`
(2026-06-12) built (1) **unify admission** — the REPL now goes through the queue
(`FUTON3C_REPL_THROUGH_QUEUE=true`, verified live in the running JVM's environment).

It achieved (2) **per-invocation sink** *by serialization rather than by keying* — the sink is set inside
the drainer's `process-fn`, "turn-exclusive ⇒ no cross-talk". That reasoning is sound **only if the
drainer is the sole source of events on that pouch.** Unsolicited turns break exactly that premise.

Fix (4) — *"Emacs-side defence-in-depth: correlate the streamed reply to its own request id"* — was
**never built**, and is precisely the defect Joe saw. The excursion called it "defence-in-depth"; it is
in fact the only layer that can detect this class at all.

## Corroboration — this is not a one-off (buffer sweep, 2026-08-03)

Swept five REPL buffers for the signature (an operator turn answered by an unrelated status report,
and/or the operator re-sending verbatim). Read-only; no buffers modified.

| buffer | occurrences | lines | operator re-sent verbatim |
|---|---|---|---|
| `*claude-repl:claude-11*` | 1 | 345–361 | **yes** |
| `*claude-repl:claude-12*` | 2 | 5–41, 121–155 | no / **yes** |
| `*claude-repl:claude-4*` | 1 (empty-reply variant) | 2995–3003 | **yes** |
| `*claude-repl:claude-9*` | 0 | — | — |
| `*claude-repl:claude-10*` | 0 | — | — |
| `*claude-repl:claude-7*` | 0 | — | — |

Four occurrences across three buffers; **three cost Joe a verbatim re-send.** The claude-12 line-121 case
is the cleanest replica of the claude-11 incident: Joe asked the agent to follow
`E-memory-resourcing-and-strategy.md` and read the claude-7 buffer; the reply was a bwrap/codex-sandbox
debugging report engaging none of it; Joe re-sent byte-identically at line 153 and the second reply
answered properly, with an explicit `## Zai and live proofs` section. The claude-4 case is the
**degenerate form** — `[no text or tool calls in this turn]` after `Cooked for 18m 14s`, then a verbatim
re-send that worked: a turn whose result went somewhere else entirely.

**Better detector than `Cooked for` duration: the MISSING `Cooked for` trailer.** In claude-12 the
mis-paired reply at line 123 is the only closed reply in that buffer with no duration line, and it is
exactly the one Joe had to re-send. A missing trailer means the SSE `done` event never arrived for that
stream — the direct surface signature of a `result` consumed by the wrong reader. Zero false positives
on the swept set. Duration alone is unreliable: short `Cooked for 0s`/`1s` reports in claude-9 are
**bell-return continuations** with no preceding operator turn, not defects.

Caveat on `*claude-repl:claude-12*` line 5: that mis-paired reply also terminates in
`API Error: Opus 4.8 can't help with this.` — a separate confound (cf. the isolation-vocabulary finding),
not evidence for this mechanism. The mis-pairing there stands on the content mismatch alone.

## Correction: why `drain-pending!` could not have worked

The first draft of this excursion said the drain's premise ("the process is idle
between turns") was false, which is true but not the whole reason. The deeper
one: **peeking at a stream you are not continuously consuming cannot distinguish
an orphaned COMPLETE turn from an in-flight turn's PARTIAL output.** Draining the
latter destroys the agent's work *and* leaves the next read starting mid-turn —
strictly worse than doing nothing. That is why the repair has to be a reader that
never stops reading, not a smarter peek.

## Measured protocol (2026-08-03, live pouch — not assumed)

An earlier design correlated turns by counting how many had been seen before the
write. It was wrong, and the tests caught it: the count reflects what the reader
has **processed**, not what the process has **emitted**, so a turn already sitting
in the pipe was invisible and got handed to the next caller — the very bug. A
probe against a real pouch supplied the right primitive:

```
 6.6  result  success             <- the fed turn ends
12.9  system  task_notification   <- background task completed
13.0  system  init                <- a NEW turn opens, solicited by nobody
19.3  result  success
```

- Every turn opens with `system`/`init`, so boundaries are explicit.
- An agent-initiated turn is **announced** by `system`/`task_notification`
  between turns. Ownership is decided by the protocol, not by timing.
- There is **no `user` echo on stdout**, so a turn cannot be matched to its
  prompt by id — the notification marker is what makes this tractable at all.
- `system`/`task_started` is NOT a trigger: it fires *inside* a turn, when the
  agent launches the job.

## The missing invariant

D3 says *exactly one thread ever calls `feed-turn!`* — a **single-writer** rule. There is no dual:

> **D6 — Single-reader / result accounting.** Every `result` event a pouch emits is consumed by the turn
> that solicited it. A `result` with no soliciting turn is **routed and surfaced as agent-initiated
> activity**, never silently discarded and never returned to the next caller.

D1–D5 are invariants of the *queue*. D6 is the invariant of the *pouch*, and it is the one that failed.

## The fix (scoped, not built)

1. **Demultiplex, don't drain. ✅ BUILT 2026-08-03, flag-gated `FUTON3C_POUCH_DEMUX` (default OFF).**
   One dedicated always-reading thread per pouch (`demux-loop!`), owning stdout for the process's whole
   life. `feed-turn!` enqueues a waiter and is handed its own turn's result; a turn opening after a
   `task_notification` belongs to no waiter and goes to `set-unsolicited-sink!` (default: logged), so
   agent-initiated work is surfaced and attributed instead of discarded. Ownership binds at turn START,
   so a waiter's `on-event` streams live and only ever sees its own turn. `drain-pending!` is skipped
   entirely on the ON path. Stream close fails every outstanding waiter rather than parking them until
   timeout. Tests: `test/futon3c/agency/agent_pouch_demux_test.clj` — a scripted fake pouch that emits
   an autonomous turn before any input *and* after every reply (the one-behind generator); the fed turns
   stay aligned and all three autonomous turns land in the sink. clj-kondo 0/0; check-parens clean;
   `agent-pouch`, `agent-pouch-joey`, `turn-queue`, `invariants` suites green (18 tests / 56 assertions).
   **Not activated:** the flag is unset in the running JVM, so this is inert until someone opts in.
2. **Carry a turn-id end to end.** Stamp each accepted turn, echo it in every SSE event including `done`,
   and have `claude-repl.el` assert it matches the prompt it is rendering under — dropping to a visible
   "reply for an earlier turn" marker rather than mis-attributing. This is E-per-turn-isolation fix (4),
   still owed.
3. **Populate `msg-id` in `[invoke-trace]`** so the D1 gate can actually be run, and re-run it.
4. **Give the JVM a real runtime log** (`data/`, append-only) so `[pouch] … drained` and the startup
   recovery report are auditable — the W5 deliverable, applied to the pouch.

5. **Ship the missing-trailer check as a tripwire** (cheap, today). A reply with no `Cooked for`
   trailer means its `done` event never arrived; on the swept buffers that predicted every operator
   re-send with no false positives. Useful as a standing detector *before* (1)+(2) land, and as the
   regression test afterwards.

(1)+(2) are the load-bearing pair; (3)+(4) are what make the result checkable rather than asserted;
(5) is what tells us whether it is still happening in the meantime.

## Relations

- `holes/missions/M-agency-hardening.md` — closed on D1–D5; this is the turn source those invariants
  do not quantify over.
- `holes/missions/turn-delivery-invariants.md` — D1 gate unrunnable (§5 above); E2's turn-exclusivity
  argument assumes a single event source.
- `holes/excursions/E-per-turn-isolation.md` — fix (4) unbuilt; this excursion is its recurrence with
  the root cause identified.
- `holes/excursions/E-crossed-bells.md` — the agent-facing sibling: there, agents mis-*thread* messages;
  here, the *surface* mis-pairs them. Same "which turn is this?" question at two layers.
- `holes/missions/M-kangaroo.md` — warm-pouch lifecycle owner; background work in pouches is already a
  known hazard there (reset kills session-held processes). This is the read-side counterpart.
