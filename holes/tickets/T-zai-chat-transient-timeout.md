# T-zai-chat-transient-timeout — a transient model-call failure must not kill a turn that can continue

Surfaced by the P0 acceptance run, 2026-07-22: job
`invoke-1784754078682-19-6f993ec9` (zai-3, a94A06 proof) died at 21:17,
~16 minutes and ~30 tool rounds in, with `invoke-exception: request timed
out`. Work products survived on disk (uncommitted); the turn and its
in-flight reasoning did not. Filed by claude-3; **M-zaif-harness track**
(harness hardening belongs with Joe + claude-2's upgrade work).

## Mechanism (verified in code, zai_api.clj)

- `chat!` (~L593) posts to z.ai via java.net.http with
  `.timeout (Duration/ofMillis (or timeout-ms 300000))` — 5 minutes per
  model call.
- **Asymmetric failure handling**: an HTTP-status failure returns DATA
  (`{:error {:message "HTTP 500" ...}}`), but a transport-level timeout
  THROWS (`HttpTimeoutException` from `.send`) — propagating up to the
  invoke wrapper, which fails the whole job (`invoke-exception`).
- So: one slow model response (likely: long context late in a proof turn +
  provider latency) exceeds 300s → the entire multi-round turn dies. No
  retry, no downgrade to data, no resume.

## Why the precedent applies

`57dd416` — "Zai transcript persistence must never kill a turn" — is the
same principle one layer down. Persistence failure was made non-fatal;
model-call transport failure should be too, when the turn has completed
rounds behind it (transcript persisted per round via U1; session history
intact; files on disk).

## Hardening ladder (smallest first)

1. **H1 — symmetry fix**: catch transport exceptions in `chat!` and return
   the same `{:error ...}` data shape HTTP failures already use. Turn
   death becomes a policy decision upstream instead of an accident of
   exception propagation. (Smallest diff, prerequisite for the rest.)
2. **H2 — one bounded retry**: on timeout/5xx/transport-error, retry the
   chat call once with a short jittered backoff (5–15s). Chat-completion
   requests are idempotent; a second 300s window usually clears provider
   hiccups. Cap: 1 retry, then surface the error.
3. **H3 — per-profile timeout**: proof-run profiles may warrant 600s (long
   contexts late in a session legitimately run slow). Prefer H2 over large
   global raises — retry beats long silent hangs.
4. **H4 — transient-failure auto-continue**: the auto-continue machinery
   (`FUTON3C_ZAI_AUTO_CONTINUE_MAX`, budget-exhaustion path) already
   re-enters the round loop with history. Treat a chat failure after ≥1
   successful round as continue-able (bounded by the same counter) rather
   than job-fatal. Bigger change; needs care that the failed call's
   partial state isn't double-applied.
5. **H5 — failure taxonomy on job events**: mark `failed` events transient
   vs terminal (parallel to futon1b's `:store-timeout`/`:store-unreachable`
   fix, TN-futon1b-memory-incident). Parked callers can then auto-decide
   re-dispatch on transient without a human postmortem.

## Acceptance sketch (when picked up)

A simulated chat timeout mid-turn (stub client) results in: (H1) error as
data, (H2) one retry visible in the round log, and — if H4 lands — the
turn completing on the retried/continued path; job events distinguish the
transient case; existing zai_api tests still pass; no change to the
happy-path round loop.

## References

- Job postmortem: `GET /api/alpha/invoke/jobs/invoke-1784754078682-19-6f993ec9`
- Code: `src/futon3c/agents/zai_api.clj` `chat!` (~L593-627), invoke wrap
  in `make-invoke-fn` (~L1045+)
- Precedents: `57dd416` (persistence must not kill turns);
  `futon1b/TN-futon1b-memory-incident.md` (failure-taxonomy pattern)
- Context: M-typed-memories P0 acceptance run notes
  (`holes/technotes/TN-zai2-session-notes-2026-07-22.md`, mission §Prototype ladder)
