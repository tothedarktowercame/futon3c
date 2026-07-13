# E-park-delivery-losses — three ways a parked resume dies between the join and the buffer

**Date:** 2026-07-13. **Found by:** claude-6 + Joe, live, during the first
routine use of README-park.md. **Trigger observation (Joe):** "It may be that
you *did* autowake for the smoke test but nothing raised in the
*claude-repl:claude-6* buffer." He was right.

## The incident

claude-6 armed three parks (a 120s-deadline smoke test with an unfulfillable
dep; two 45-min parks on Codex jobs) and loaded `claude-repl-park.el` into the
live Emacs mid-session (it had **never been loaded** — the poller half of the
protocol was simply not installed; that is finding 0). Within one poll tick the
buffer's inbox was drained; only one `[park] resuming` line appeared in
*Messages*; **no continuation ever rendered in the buffer**; the inbox ended
empty. Two resumes were destroyed without a trace, one was delivered into a
mid-turn buffer and swallowed.

## The three bugs (plus finding 0)

**0. The poller was never installed.** Backend flag on, engine sound, parks
firing — and resumes rotting in the ready-inbox because no Emacs ever polled
`GET /api/alpha/parked/ready`. The load is also non-persistent: an Emacs
restart silently reverts it. → Action: make `claude-repl-park.el` load part of
the standard agent-chat/claude-repl bootstrap, not a manual step.

**1. Pop-all / deliver-one contract mismatch (FIXED 2026-07-13).**
`parked-ready-pop!` (`transport/http.clj:673`) popped the ENTIRE queue per GET
(`dissoc` of the key) while the elisp handles only `(aref ready 0)` with the
comment "extras next tick." With queue depth N, N−1 resumes were silently
destroyed per poll. It was also read-then-clear racy. **Fix applied:** pop-one
FIFO via `swap-vals!`, file + live JVM patched via Drawbridge (single-defn
redefinition, no serving-ns reload), self-tested in-process (pk-1/pk-2/empty).
"Extras next tick" is now actually true.

**2. Mid-turn injection race (FIXED 2026-07-13).** `claude-repl-park--poll-once` gated on
`agent-chat--streaming-started` as the buffer-idle signal, but in pouch-driven
buffers that flag is not a reliable turn-activity indicator
(`--resume-in-buffer`'s own comment says it "never resets here" — and we
observed a resume injected+sent seconds into an active turn, where it was
swallowed). **Fix applied:** `handle-parked-ready` uses the registry
`:invoking` status as the single source of truth and returns `:withheld true`
without leasing while the agent is busy; the Elisp poller no longer attempts to
infer activity from streaming flags.

**3. At-most-once delivery, no ack (FIXED 2026-07-13).** The pop was destructive
before delivery was confirmed; an injection that failed (bug 2, dead buffer,
Emacs crash) lost the payload permanently. **Fix applied:** ready delivery is
lease/ack based (`POST /api/alpha/parked/ready/ack`), expired leases are
redelivered, and the Emacs side dedups by park-id.

## What was lost in the incident

- The smoke-test resume (moot — its only content was "report that the backstop
  fired").
- The codex-2 slice-1 review resume (moot — the auto-bellback path delivered
  the same news and the review ran off that).
- codex-3's live `--park` test resume (delivered into the mid-turn buffer and
  swallowed; also note codex-3 ran its test **in claude-6's name**, which is
  how its resume landed in claude-6's inbox at all — test dispatches should be
  sent as oneself, cf. the no-nick-claimjumping discipline).

## Post-fix observation (2026-07-13, later the same day)

First successful end-to-end park resume delivered into `*claude-repl:claude-6*`
after the pop-one fix (the codex-2 slice-2 park) — the delivery path works.
It also demonstrated a redundancy that later proved operator-visible as three
wakes for one `--park`ed bell completion: the recipient agent's explicit
completion bell, the harness auto-bellback, and the park resume. The server
duplicate is fixed: `finalize-invoke-job!` now runs the parked-on completion
hook before deciding the auto-bellback, and suppresses the auto-bellback only
when `note-completion!` successfully returns a released park for the same
caller. The job ledger records the suppression under `:auto-bellback` so a
repeat finalization cannot later turn the suppressed event into a bellback.
The recipient agent's own explicit completion bell is intentionally unchanged;
that is agent behavior, not server-generated delivery.

Also observed on that first delivery: the resume's surface header read
`From: joe / Origin: operator` instead of the designed `continuation:`
attribution (`claude-repl-park.el` binds `agent-chat-user-speaker`
"continuation", but the surface-contract header apparently fills From/Caller
from the buffer default). That is a PROVENANCE bug, not cosmetics: a machine
resume that presents as operator input can be mistaken for a fresh operator
instruction (cf. transcript-operator-provenance discipline). Add to the
hardening review scope.

## Bug 4 — within-turn deferral vs background parks (FIXED 2026-07-13)

The finalize-once model installs `agent-chat-turn-continued-fn` =
`turn-parked-p`, which defers a turn's finalization (no flair, **no prompt
restore**) whenever the agent has ANY outstanding park or inbox item. That was
right for the original design (short within-turn joins) but wrong under the
new bell-and-park routine, where a 45-minute background park is outstanding
almost always — Joe's buffer stopped returning its input prompt, injected
resume text stranded in the input area, and his next RET sent the stale
resume + his typed message as one operator turn.

Fix applied: **two park modes.** `POST /park` takes `:mode :within-turn`
(default, current semantics) vs `:mode :background` (what `agency_send.py
--park` sends). Background parks are EXCLUDED from `more-pending` /
`turn-parked-p`, and their resumes arrive as fresh turns attributed
`continuation:` — no deferral, no prompt-stranding, no unified-flair machinery.

## Bug 5 — WS + poll double delivery (FIXED 2026-07-13)

`parked-resume!` pushes every buffer-surface resume BOTH over the agency WS
(`park-ready` frame) AND into the poll inbox; `claude-repl-park.el` subscribes
to both. When README-park.md was written the WS connector was down, so the
poll path was the only live one — but the connector is now up (5,682 frames
observed), so every resume delivered TWICE: the WS copy instantly, the inbox
copy on the next 3s poll. The second injection always lands while the turn
started by the first is streaming → "Claude is still responding" error →
stranded text in the operator's input area, every time. This (not zai-15's
lease work, which was NOT yet live-patched — verified pop-one still current,
no lease vars in the JVM) caused the 2026-07-13 double delivery of the slice-2
resume and the operator-visible stall.

Mitigation applied live: WS `park-ready` handler unsubscribed in the running
Emacs (poll path is sufficient); stale 614s accum-elapsed cleared in
`*claude-repl:claude-6*`. Fix applied (revised at review, 2026-07-13): the
durable ready-inbox is the ONLY delivery path — every resume flows through
lease → deliver → ACK, so the busy gate, redelivery, and dedup always apply —
and the targeted WS `park-ready` frame is demoted to a wake-up POKE that makes
the buffer poll immediately instead of waiting out the interval. The frame
carries no payload; losing it costs one poll tick of latency, never the resume.
(The first cut of this fix made an accepted WS send REPLACE the inbox push —
fire-and-forget delivery with no lease behind it, so a frame consumed by a dead
buffer, a crashed Emacs, or the busy-collision below was a permanent loss. The
poke pattern keeps the WS speed without giving it custody of anything.)

## One-slot shift — unsolicited turns corrupt operator reply pairing (FIXED 2026-07-13)

A background-task completion woke claude-5 in an unsolicited turn; Joe sent a
fresh operator message while it was in flight. The buffer paired Joe's message
with the wake turn's output, and the genuine reply to Joe's message disappeared.
Fix applied: unsolicited resumes are launched through a distinct
`agent-chat-send-unsolicited-input` path, rendered as their own `continuation:`
turn, and never inserted into the operator input area. If the operator sends
while an unsolicited turn is in flight, the operator text is queued and launched
as its own `joe:` turn after the continuation finishes.

Review finding (claude-5, 2026-07-13): the MIRROR race was a loss path. A
resume arriving while an OPERATOR turn was in flight hit a `user-error` in
`agent-chat-send-unsolicited-input` — but `resume-in-buffer` had already added
the park-id to the dedup ring, so the lease-expiry redelivery was skipped AND
ACKed: the resume was silently destroyed (bug 3's failure mode, reintroduced
through the side door). Fix: unsolicited turns now QUEUE behind an in-flight
turn exactly as operator turns queue behind unsolicited ones (the queue entries
carry their own speaker/origin), and the send path never signals. Remaining
accepted blast radius: an ACKed resume queued in a buffer that dies before
draining is lost with the Emacs — same custody semantics as one that dies
mid-send.

## Finding 6 — the deadline backstop never woke anyone (FIXED 2026-07-13)

`sweep-deadlines!` treated a past-deadline park as a RETRACTION: drop the
record, print "deadline expired, retracted" to JVM stdout, deliver nothing.
Only no-dep timer parks got `resume!`. The original VERIFY case 5 explicitly
specified this ("expires WITHOUT resuming") — so the implementation was
faithful to a spec that contradicted the README's own "deadline backstop"
framing and every operational wake payload written against it. Both smoke
tests proved it live: their deadline "wakes" were silent drops (smoke 1's
silence was previously mis-attributed wholly to bugs 0/1).

**Semantics changed deliberately:** expiry now calls `resume!` with
`:deadline-expired? true`, and `assemble-resume-prompt` renders
"DEADLINE EXPIRED with N of M dependencies complete — the awaited work did
NOT finish". Case 5 rewritten to specify the wake. Commit `ec53990`,
live-patched, parked-on tests 11/36 green. Smoke test 3 armed as the
acceptance run of the repaired path.

## Ownership

Fix 1 done (claude-6, review-fix carve-out). Bugs 2 and 3 + finding-0
bootstrap wiring: charter as a follow-up slice once codex-3's
Agency-ergonomics slice (in flight, `invoke-1783934754106-308-69ad7fe1`) lands
— they touch the same surfaces and should be reviewed together.
