# M-agency-hardening — live datapoints (2026-06-10 claude-4 ↔ claude-3 co-build)

Source: handed to claude-6 via bell during a long claude-4↔claude-3 session.
"The session itself is the test case." Filed + verified by claude-6 (in-flight
mechanic). Companion evidence to `M-agency-hardening.md`.

## Raw symptoms (as reported)

1. **Dropped codex completion bell** — codex-2 job
   `invoke-1781088035962-379-e1c88f5a` completed (went idle; committed `ac4ae5d`
   on `codex/m-pattern-posteriors-v0`) but its completion bell to claude-4 never
   fired. Caught only by polling roster-status + files-on-disk.
2. **Operator-turn replay** — Joe's emacs-repl "Cluster B park" message was
   re-delivered to claude-4 out of order (verbatim duplicate, several turns
   later). Joe: "the turns get interleaved in a confusing way." Idempotency gap.
3. **Bell/REPL interleaving** — async bell-turns (claude-3 coordination) and
   operator repl turns arrived threaded with no stable causal ordering between
   the two channels.
4. **Stale re-flag** — claude-3 re-flagged `:O-peradam-role` that claude-4 had
   completed a turn earlier; shared state not converging across the interleaving.
5. **Crossed bells** — claude-3's "land 379 as-is" crossed claude-4's "379
   landed+reviewed"; both compatible but needed a manual 3rd bell to reconcile.

Mitigation that worked in-session: poll roster + files-on-disk instead of
trusting the completion bell; treat apparent-duplicate operator messages
defensively (verify-already-done before acting).

## Verification (claude-6, against the live ledger, 2026-06-10)

**Symptom 1 — VERIFIED, but root cause CORRECTED.** Live job record:
`state=done, to=codex-2, caller="http-caller", delivery={status: delivered,
surface: bell, destination: "caller http-caller via /api/alpha/invoke/jobs/…",
note: bell-job-ready}`.

The bell did **not** drop. The job completed and the result was available via
job-status. The completion bell to claude-4 **could not fire because the caller
was never captured** — the dispatch used `http-caller` (no `--from`), so there
was no agent to route a completion bell to. ⇒ The report's suggested fix
(delivery-confirm/retry) would NOT have fixed this; the real lever is **caller
capture (`--from`) + auto-bellback**. With `caller=claude-4` recorded,
auto-bellback (`T-codex-auto-bellback`, built+reviewed, pending reload) fires
the completion bell automatically. Auto-bellback correctly excludes
`http-caller`, so the dispatch MUST pass `--from`.

## Mapping to tickets / invariants

| # | Root cause | Addressed by |
|---|---|---|
| 1 | caller not captured (`http-caller`) → no addressable recipient | `--from` capture (agency_send.py done; **adoption needed**) + auto-bellback (built, pending reload); **MQ-1** detects terminal-but-undelivered-to-caller |
| 2 | no message-id idempotency on operator turns | **NEW**: idempotent operator-turn dedup by message-id → fold into Car-3; new MQ "no-replay" invariant candidate |
| 3 | no causal ordering between bell + repl channels | **Car-3** (queue + per-surface routing) + per-bell causal/sequence marker |
| 4 | shared state not converging across interleave | per-bell causal/sequence marker (3/4/5 share this) |
| 5 | timing; no self-reconciliation of crossed bells | per-bell causal/sequence marker; **MQ-3** validates once bellback-intent is captured |

## Design implications (fold into the next tickets)

- **Car-3 gains concrete, field-validated requirements:** (a) a per-bell
  **causal/sequence marker** (Lamport-style) so crossed/stale bells
  self-reconcile (3/4/5); (b) **idempotent operator-turn dedup by message-id**
  (2); (c) per-surface reply routing + ordered drain (3).
- **Reload urgency:** symptom 1 is precisely what auto-bellback fixes (given
  `--from`). Strengthens the case to reload the `integration/agency-hardening`
  bundle.
- **`--from` adoption:** agents dispatching codex via `agency_send.py` MUST pass
  `--from`, else auto-bellback has no recipient. Consider server-side caller
  capture on the remaining HTTP/dispatch paths; update the handoff convention.
- **New MQ candidate (replay):** an operator/bell turn delivered more than once
  (same message-id) is a misrouting class → add to mesh-qa once message-id is
  captured.

## Validation log

- **2026-06-10 — auto-bellback fired LIVE (first time), fixing datapoint #1's
  class.** codex-1 reached terminal on job `invoke-…de998829` (Car-3 Phase-1)
  whose caller was a real agent (`claude-6`, supplied via `--from`). finalize
  auto-enqueued exactly one bellback job `auto-bellback-…de998829 → claude-6`
  (caller tagged `auto-bellback`); verified in the ledger that NO cascade
  occurred (single auto-bellback job, loop-safety tag held) and it routed to the
  correct caller. Confirms: the reload took, the `--from` capture is the enabler
  (an `http-caller` job would NOT bell back), and loop-safety is sound. This is a
  Codex→Claude completion loop captured as typed evidence (acceptance criterion).
