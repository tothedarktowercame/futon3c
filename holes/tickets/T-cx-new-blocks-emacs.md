# T-cx-new-blocks-emacs — `cx new` blocks Emacs for a while (DEFERRED)

Observed by Joe, 2026-06-10. Parking for "another time" — do NOT pick up without
Joe's go-ahead.

## Symptom
Running `cx new` (mint a fresh codex agent) blocks/freezes Joe's Emacs for a
noticeable stretch before returning.

## Not the cause
NOT cold-resume: `cx new` is a *fresh* codex session (no resume), and E4
measurement showed codex resume ≈ fresh anyway (~5.5–6.2s, model-latency-bound,
no claude-style multi-MB replay). See M-kangaroo E4 finding.

## Hypotheses to check (later)
1. `cx` (futon0/scripts/cx) runs a synchronous first codex turn (`codex exec`,
   ~5.5s model latency) *in the Emacs process* during agent setup → Emacs blocks
   for the whole turn. Check whether `cx new` does a blocking init/hello turn and
   whether it can be made async (spawn + return, let the first real turn happen
   on demand).
2. Synchronous registration round-trip to Agency (HTTP POST /agents/...) on a
   loaded JVM — cf. [[blackboard-backpressure]] (futon3c hot paths synchronously
   block on emacsclient; the inverse — Emacs blocking on a slow Agency call —
   would present the same way).
3. Codex CLI auth/login or MCP-server init on first spawn adding startup latency.

## Repro
`cx new` from Joe's Emacs; time how long Emacs is unresponsive vs the codex
first-turn wall time.

## Disposition
Deferred. Bounded UX annoyance, not a correctness bug. Revisit after the drainer
fix lands.
