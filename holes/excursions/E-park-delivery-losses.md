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

**2. Mid-turn injection race (OPEN).** `claude-repl-park--poll-once` gates on
`agent-chat--streaming-started` as the buffer-idle signal, but in pouch-driven
buffers that flag is not a reliable turn-activity indicator
(`--resume-in-buffer`'s own comment says it "never resets here" — and we
observed a resume injected+sent seconds into an active turn, where it was
swallowed). → Candidate fixes: (a) a real busy predicate exposed by
agent-chat/claude-repl; (b) server-side gating — the kangaroo layer KNOWS
whether a session has an in-flight turn, so `handle-parked-ready` could simply
withhold items while the session is busy (most robust: one source of truth).

**3. At-most-once delivery, no ack (OPEN, narrowed by fix 1).** The pop is
destructive before delivery is confirmed; an injection that fails (bug 2, dead
buffer, Emacs crash) loses the payload permanently. Pop-one caps the blast
radius at a single resume, but a real fix is an ack step
(`POST /parked/ready/ack?park-id=`) or redelivery-with-dedup (re-push on
missing ack within T, `:park-id` idempotency on the elisp side).

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
It also demonstrated a benign redundancy: a `--park`ed bell produces TWO wakes
for one completion (auto-bellback + park resume). Refinement candidate:
`finalize-invoke-job!` could skip the auto-bellback when a park is awaiting
that job-id, making the park resume the single wake. Low priority — the
duplicate is idempotent if agents treat resumes as "check state, then act,"
which the wake-payload-as-checklist convention already encourages.

Also observed on that first delivery: the resume's surface header read
`From: joe / Origin: operator` instead of the designed `continuation:`
attribution (`claude-repl-park.el` binds `agent-chat-user-speaker`
"continuation", but the surface-contract header apparently fills From/Caller
from the buffer default). That is a PROVENANCE bug, not cosmetics: a machine
resume that presents as operator input can be mistaken for a fresh operator
instruction (cf. transcript-operator-provenance discipline). Add to the
hardening review scope.

## Bug 4 — within-turn deferral vs background parks (OPEN; operator-visible)

The finalize-once model installs `agent-chat-turn-continued-fn` =
`turn-parked-p`, which defers a turn's finalization (no flair, **no prompt
restore**) whenever the agent has ANY outstanding park or inbox item. That was
right for the original design (short within-turn joins) but wrong under the
new bell-and-park routine, where a 45-minute background park is outstanding
almost always — Joe's buffer stopped returning its input prompt, injected
resume text stranded in the input area, and his next RET sent the stale
resume + his typed message as one operator turn.

Fix direction: **two park modes.** `POST /park` takes `:mode :within-turn`
(default, current semantics) vs `:mode :background` (what `agency_send.py
--park` should send). Background parks are EXCLUDED from `more-pending` /
`turn-parked-p`, and their resumes arrive as fresh turns attributed
`continuation:` — no deferral, no unified-flair machinery. Interim mitigation
applied live (2026-07-13): `agent-chat-turn-continued-fn` set to nil
buffer-locally in `*claude-repl:claude-6*`; genuine within-turn unification is
disabled there until the mode split exists. Add to the hardening review scope
alongside bugs 2-3 and the attribution finding.

## Bug 5 — WS + poll double delivery (OPEN; mitigated live)

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
`*claude-repl:claude-6*`. Real fix for the hardening round: the server should
choose ONE delivery path per resume (WS when connected, else inbox — with the
inbox as fallback only after a WS delivery timeout), and the elisp should
dedup by park-id regardless (zai-15's ack/dedup work covers the client half).

## Ownership

Fix 1 done (claude-6, review-fix carve-out). Bugs 2 and 3 + finding-0
bootstrap wiring: charter as a follow-up slice once codex-3's
Agency-ergonomics slice (in flight, `invoke-1783934754106-308-69ad7fe1`) lands
— they touch the same surfaces and should be reviewed together.
