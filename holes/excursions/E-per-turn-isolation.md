# Excursion: E-per-turn-isolation — REPL replies dropped when bells interleave

**Date:** 2026-06-05
**Status:** IDENTIFY (failure mode PINNED in code + evidence; fix scoped, not built).
**Repo:** futon3c (the bug is in `transport/http.clj` invoke-stream + the registry event-sink; the
downstream symptom is in `emacs/claude-repl.el`).
**Spawned from:** Joe observed a REPL reply that never reached him (`*claude-repl:claude-6*`), then similar
routing issues in `*claude-repl:claude-1*`. The phatic / push-to-talk idea and `E-turn-dsl` (`OT:`) are the
operator-facing complements; THIS excursion is the load-bearing server-side fix.

## HEAD (the bug, one line)

A REPL turn's streamed reply can be cross-routed or dropped when a **bell** invocation overlaps it on the
same agent — because the REPL stream and bells are **two un-unified admission paths** sharing one per-agent
event sink and one session, with the REPL path off the job queue entirely.

## Diagnosis journey (kept, because the corrections are the lesson)

1. **First hypothesis (Emacs-side):** the chat buffer's single `agent-chat--pending-process` + the sentinel
   discard at `claude-repl.el:997-1005` (a superseded stream's response is silently dropped). Real, but a
   *downstream* symptom, not the root.
2. **Over-read ("merge"):** from an ambiguous evidence timeline I inferred Joe's REPL turn and a bell *merged*
   into one conflated turn. **Wrong** — Joe pushed back (coding agents are FIFO; merge is implausible), and
   the code confirms him: `create-invoke-job!` dedups **only by job-id** (`http.clj:427-432`), so two turns
   are two separate jobs — no coalesce. The reply I cited was fully explained by Joe's own "go" turn.
3. **Pinned (code, this excursion):** the real mechanism is shared-mutable-state clobber across two
   admission paths (below). Execution *is* serialized by the subprocess lock (Joe's FIFO intuition is
   correct at the execution layer); the bug is that strict queuing was **never enforced across the REPL and
   bell paths**, and the per-agent sink + session are not protected by that serialization.

## The evidence (claude-1, session `f813a7b8`, 2026-06-05 ~13:11)

```
13:11:03.957  invoke-start    emacs-repl   Joe: "go" (option 1)            ← REPL turn starts
13:11:12.448  context-retrieval bell       Bell from claude-7 — de-risk RESULT  ← bell overlaps, same session
13:11:47.182  invoke-complete  (surf -)    "Joe greenlit. Relaying the go to claude-7…"
13:11:50.250  chat-turn        (surf -)    (same reply)
```
Pulled via `GET /api/alpha/evidence?agent-id=claude-1`. A bell's processing overlaps the in-flight REPL
turn's window on one session. (`surf -` is just untagged `invoke-complete`/`chat-turn` event types, NOT
"surface collapse" — an earlier over-claim of mine.)

## The pinned mechanism (file:line)

- **REPL path bypasses the job queue.** `handle-invoke-stream` (`http.clj:2582`) **never calls
  `create-invoke-job!`** — in-flight REPL turns are invisible to the per-agent job bookkeeping that bells
  flow through. So the serialization/queue that *would* hold a bell behind a REPL turn does not apply.
- **Single per-agent event sink.** invoke-stream installs ONE sink keyed by agent-id:
  `reg/set-invoke-event-sink! aid sink-fn` (`http.clj:2626`). The streamed reply reaches the buffer *only*
  through that sink — `make-claude-invoke-fn`'s NDJSON loop "emits events as they arrive" to whatever sink is
  currently installed (`http.clj:2588`).
- **Unconditional teardown of the shared sink.** Cleared in `on-close` (`http.clj:2628-2630`) and `finally`
  (`http.clj:2694-2695`). An overlapping invocation's teardown clears the sink the still-pending turn needs;
  a second stream's `set-invoke-event-sink!` overwrites the first.
- **Shared session.** Both REPL and bells call `reg/invoke-agent!` (`http.clj:2647`) resuming the agent's one
  session; serialized by the subprocess lock, but the sink lifecycle is not serialized with it.
- **Downstream Emacs symptom.** Even when a reply is produced, the chat buffer's single
  `agent-chat--pending-process` + sentinel discard (`claude-repl.el:997-1005`) drops a superseded stream's
  response → buffer shows nothing → the turn looks unanswered → redelivered (the duplicate Joe saw).

**Net:** not a merge, and not clean FIFO either. Execution serializes; the **sink + session are shared
mutable state** two un-unified paths stomp on when they overlap.

## The fix (scoped, not built)

1. **Unify admission.** invoke-stream registers a job and respects the same **per-agent serialization** bells
   use, so a bell queues strictly behind an in-flight REPL turn (the strict-queuing Joe assumed already held).
2. **Per-invocation event sink** keyed by job-id (or trace-id), not one slot per agent — overlapping turns
   can't clear or overwrite each other's sink.
3. **Per-turn session handling** so sequential resumes don't cross.
4. **Emacs-side defence-in-depth:** correlate the streamed reply to its own request id; *queue* a superseded
   process instead of discarding it (`claude-repl.el:997-1005`).

Localized: one handler (`handle-invoke-stream`), the sink registry, and the Emacs sentinel. (1)+(2) are the
load-bearing pair.

## Relations

- `E-turn-dsl` (`futon3c/holes/excursions/E-turn-dsl.md`) — `OT:` / phatic is the operator-facing reliable
  side-channel; this is the server-side isolation it complements. Per-turn *attribution* (DSL) and per-turn
  *isolation* (this) are the same unit seen from two sides.
- `M-autoclock-in` — shares the agent-chat/REPL surface and the turn-evidence path.
- `E-repl-evidence-turns` (futon0) — the `{session-id, agent-id, surface, turn-index, timestamp}` evidence
  schema this was traced through.
- The phatic / push-to-talk idea — a reliable lane that doesn't contend with the main invoke path; once
  per-turn isolation lands, `OT:` is its trigger.
