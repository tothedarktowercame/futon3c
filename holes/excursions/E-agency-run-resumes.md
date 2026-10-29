# E-agency-run-resumes — design intake

Scope: read-only tracing on futon3c `master` at `31349ee85` (2026-10-29).
This note distinguishes source facts from design inferences.  It does not
authorize a change to either server or Emacs.

## Current boundary

**[read]** `parked-resume!` splits on an `emacs*` surface.  The buffer branch
pushes a durable ready-inbox item and sends only a `park-ready` WS poke;
the headless branch creates an invoke job and submits it to the recipient's
`turn-queue` lane (`src/futon3c/transport/http.clj:1940-1985`).  Therefore the
buffer branch has no Agency job, no drainer entry, and cannot be rendered by
the existing job follower.  The ready-inbox is currently documented as the
only authoritative buffer-delivery path, with the WS frame explicitly
non-authoritative (`http.clj:1946-1954`).

**[inferred]** The smallest server experiment is not “make the poller faster”:
add an opt-in, per-agent/surface decision that uses the existing non-buffer
job + `turn-queue/accept-async!` construction for a buffer resume, retaining
the ready-inbox path as the flag-off fallback.  It must give the job a surface
and caller that let the buffer identify it as a resume, rather than silently
using the current headless `parked-resume-caller` identity.

## 1. Buffer-owned turn work that a drainer resume would bypass

| Concern | Current fact | Consequence of Agency execution |
|---|---|---|
| Finalize-once / within-turn defer | **[read]** `agent-repl-park--turn-parked-p` asks `/api/alpha/parked` for `:more-pending`; `agent-chat-turn-continued-fn` installs it (`emacs/agent-repl-park.el:505-521`). A parked segment banks elapsed and defers flair/clock; the final continuation emits the single total flair (`emacs/agent-chat.el:1337-1374`). | **[inferred]** A server job cannot be that continuation without either a server-side equivalent of this state machine or an explicit decision to end the old buffer turn before the server job. Leaving `:more-pending` true while no client consumes a ready item strands the old UI turn. |
| Attribution / ordering in transcript | **[read]** a ready item is launched as `agent-chat-send-unsolicited-input` with speaker `"continuation"`; it does not use the operator input area (`emacs/agent-repl-park.el:91-124`). Operator input during it is queued behind it by the local chat queue (`agent-repl-park.el:99-102`; `agent-chat.el:3191-3208`). | **[inferred]** Follow-mode currently labels an outside job `agent⇐caller`, not `continuation:` (`emacs/agent-follow-mode.el:267-312`). A display adapter needs explicit resume metadata and a continuation rendering branch; merely enabling follow-mode changes attribution. |
| Turn evidence | **[read]** the Claude stream completion calls segment evidence even when flair/clock finalization is deferred (`emacs/claude-repl.el:1252-1267`); normal REPL input hooks emit user/assistant/commit evidence (`emacs/claude-repl.el:1374-1388`). Separately, a server invoke job emits caller and successful agent evidence (`src/futon3c/transport/http.clj:5846-5863`). | **[inferred]** If Emacs merely displays a job, it should not run its normal send hooks, otherwise server and buffer evidence duplicate. But server job evidence is not proven equivalent to the unified per-segment buffer evidence, so a cut must first name the desired evidence authority and test exactly one result. |
| Flair/cost | **[read]** `agent-chat-finish-turn!` owns total flair and `agent-chat-refresh-cost-basis!` (`emacs/agent-chat.el:1360-1374`). Follow-mode only avoids clobbering an own-stream progress line (`emacs/agent-follow-mode.el:243-261`); it does not call buffer finalization. | **[inferred]** live display alone will not produce the former unified flair/cost line. Either retain client finalization driven by a terminal server event, or define a separate server-run flair representation. |
| Pouch/session | **[read]** server jobs obtain the registry session id and call `invoke-agent-with-session-recovery!` (`src/futon3c/transport/http.clj:5806-5835`); the comment records composition with an existing event sink. Normal buffer completion updates its local session and session file from the server `done` event (`emacs/claude-repl.el:1240-1251`). | **[inferred]** the server job uses the agent registry/pouch identity, so it is plausibly the same warm session, but a follower currently has no terminal path that updates the buffer's `agent-chat--session-id` or Claude session file. This needs an explicit equality test, not an assumption. |

The current contract is also stated explicitly in
`holes/excursions/E-repl-continuations.md:157-164` and `README-park.md:110-120,
193-206`: polling reruns the agent through normal buffer machinery, with a
unified flair/evidence outcome.

## 2. Displaying a drainer-run resume

**[read]** There is already a server-to-buffer *observation* path, not a live
stream transport: `record-job-stream-event!` adds trimmed prompt/text/tool
events to the in-memory job ledger and deliberately persists only on finalize
(`src/futon3c/transport/http.clj:1384-1410`). `agent-follow-mode` polls
`GET /api/alpha/invoke/jobs` every 10 seconds as fallback and uses a WS
`invoke_event` doorbell only to trigger another bounded poll
(`emacs/agent-follow-mode.el:36-50,67-95,419-467`). It replays new ledger
events and marks progress, but intentionally calls this replay of outside
turns (`agent-follow-mode.el:1-19,267-312`).

**[read]** The actual REPL live path is NDJSON from `/api/alpha/invoke-stream`:
the server installs an event sink and, when enabled, puts the REPL request into
the same per-agent drainer (`src/futon3c/transport/http.clj:6937-6984,
7037-7078`). Claude/Codex render that stream through
`agent-chat-begin-streaming-message`, `agent-chat-stream-text`, and
`agent-chat-end-streaming-message` (`emacs/agent-chat.el:2323-2408`; e.g.
Claude consumption at `emacs/claude-repl.el:1252-1273`).

**[inferred]** A live Agency-run resume needs a job-id-to-buffer binding plus
an event transport that drives these streaming functions.  Reusing the
existing `invoke_event` WS as a doorbell still leaves up to its poll/min-gap
latency and only bounded ledger events; it is not itself streamed text/tool
events.  The least new display behavior is therefore: on a resume job
announcement bind its job id to the originating buffer, fetch/render its
existing event ledger immediately, and on terminal update session/finalization
once.  A truly push-per-event design is a second, larger transport behavior
and is not justified by the traced code alone.

**[read]** While a local turn streams, `agent-chat-send-input` queues later
operator input rather than starting another local process (`emacs/agent-chat.el:3191-3208`).
Follow-mode avoids changing progress while that local stream is active
(`emacs/agent-follow-mode.el:243-261`).  **[inferred]** A server-run job is
not assigned to `agent-chat--pending-process`, so without a display binding it
does not put the input area into the local “still responding” state; the
current follower supplies only a progress line.

## 3. Ordering with operator input

**[read]** `FUTON3C_REPL_THROUGH_QUEUE` defaults true and routes
`/api/alpha/invoke-stream` turns through `turn-queue/accept-async!`
(`src/futon3c/transport/http.clj:6860-6867,7037-7078`). The queue entry carries
the agent, caller, surface and prompt; it reports queue position/activity
(`http.clj:6875-6911`). Thus a REPL/operator turn already is a drainer entry
when the flag and drainer-v2 are enabled; FIFO order against an Agency-run
resume is defined by queue admission order.

**[inferred]** If an operator posts while a resume is queued, the operator
request will also enqueue, so it cannot overtake the resume. The remaining
UI problem is that its HTTP stream is open and can display queue activity,
whereas the resume needs the job-to-buffer binding above. The flag-off legacy
path is not FIFO with a server resume and must remain outside any claim of
this invariant.

## 4. Followups

**[read]** Followups use the same client-owned model: polling leases one item,
then `agent-chat-send-unsolicited-input` runs it as `"followup"` and ACKs it
(`emacs/agent-repl-park.el:253-318`). The batch poller still calls this
delivery after each returned seat (`agent-repl-park.el:419-489`). The server
followup ready handler is a separate queue/lease protocol
(`src/futon3c/transport/http.clj:6216-6250`).

**[inferred]** They can move to a drainer only by adding a server-side
followup-to-job admission step, preserving its queue identity/lease/ACK or
replacing them with an equally durable consumed state. That is not “the same
one-line change” as a park resume: a followup has its own cancellation,
validity and terminal semantics. Keep it client-polled until its independent
admission/ack contract is specified.

## 5. Cut list

1. **Server opt-in admission.** Add a per-agent/surface flag that changes only
   buffer *park resumes* from ready-inbox push to a named resume invoke job on
   `turn-queue`; retain ready-inbox + WS poke when false.  Acceptance: an idle
   flagged seat receives exactly one drainer entry/job; an active turn delays
   it; flag-off still creates a ready item; operator request/resume order is
   FIFO by queue id.
2. **Emacs job binding/display.** Bind the opt-in resume job id and origin
   buffer, render ledger events through the existing chat streaming primitives,
   and handle exactly one terminal event.  Acceptance: text/tool events appear
   in the right buffer while typed operator input stays queued; duplicate WS
   doorbells and polling do not duplicate text; terminal session id reaches
   the same buffer.
3. **Finalize/evidence equivalence decision.** Before retiring the poller,
   choose and implement one evidence/flair authority for server-run resumes.
   Acceptance: a one-park run has exactly the selected evidence records and
   one final flair/cost outcome, while a client-run fallback retains today's
   unified behavior.
4. **Retire polling only after the above.** Disable park polling for opted-in
   seats, but retain the ready-inbox endpoint and 404 fallback
   (`emacs/agent-repl-park.el:405-489`) for unreloaded/flag-off Agency and for
   rollback.  Followup polling remains unchanged.

No cut should claim that a job-ledger event survives a restart as a live
stream: the producer explicitly says those per-event records are
best-effort/in-memory (`http.clj:1384-1388`).
