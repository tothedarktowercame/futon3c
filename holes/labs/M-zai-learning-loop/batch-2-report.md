# Batch-2 close-out — supervisor's report (ams-claude-1, 2026-08-11)

Written by the supervisor from frame records, job ledger, and amendments
1–9; the operator (claude-3) completed the full close loop (24/24 frames
closed, validate exit 0, all sessions verified true) before standing
down under the traffic-discipline order. Batches end here; M-case-studies
supersedes batch mode.

## Headline: the anchor-DF rung FAILED (pre-registered, amendment 8)

Verdict written BEFORE the last pair ran, decided by the only two
divergent dispatches: a01J05 and b01A02r, both mem noisy-surface vs ctl
correct-empty. Seven tie-stratum dispatches cancel by construction.
problem-idf stays; memory-df remains behind `--anchor-source` as
instrumentation. Design reading: the rung passed its offline acceptance
test and still failed the batch — at current store coverage the
empty-pool prior dominates, so a more permissive anchor policy trades
correct-empties for noise. Next test of the passive channel waits for
coverage (amendment 8), which M-case-studies grows by construction.

## The isolation experiment (b01A04r): convergence, not contagion

ctl reproduced the full `Subsingleton.elim` transport (sorry 3→1,
axiom-clean, commit f17000af) in a verified-fresh session inside the
quarantine — no copy reachable on disk or in the ref namespace. The
first-run result was independent rediscovery, not luck and not reading;
this retroactively softens the b01A02r read-breach's implication for
batches 1–2 (amendment 7's "no evidence of more" now has evidence FOR
it). Memory-side datum: across four dispatches on two problems, ctl
found the transport twice; mem hit the identical module-instance wall
twice and never found it (0 commits). The transport remains a scribe
candidate; the demand signal recurred in isolation.

## Merge adjudication (executed; master de65e40..a0e75d5)

Rule: per-problem best arm by sorryAx-decl count from frame axioms
blocks; twin-identical → ctl; ties → ctl; native_decide adds
disqualify. Scope-checked (each branch touches only its problem), both
closures compile-verified on merged master.

| problem | merged | note |
|---|---|---|
| a01J05 | ctl 0112ae2a | twins byte-identical |
| a01J06 | ctl 34fe8690 | tie |
| **a02J04** | **ctl c0a1b8f6** | **CLOSED — both arms closed it** |
| b01A02 | firstrun-ctl 738f7748 | 0 new native_decide (base carries 2); b2r-ctl (19 native) stays unmerged per amendment 5 |
| b01A04 | b2r-ctl f17000af | quarantine provenance preferred over firstrun d52261c9 |
| **m01J03** | **mem ad90c89c** | **CLOSED — mem arm only (ctl 1 sorry)** |
| m01J04 | ctl f40871d1 | tie |
| m01J05 | ctl 54b87228 | mem no commit |
| t00A05 | ctl f92c6ff2 | tie |
| t00J02 | ctl cb99b7fe | tie |

Net: **+2 problems closed** (a02J04, m01J03), 8 best-partials banked.
b01A04 firstrun branches restored from the quarantine bundle post-
experiment.

## Instrument findings (the batch's larger half)

Six defects found and fixed mid-batch, all catalogued (V3 §2.1
instances 8–13): fictitious session isolation (D4), the gate that
indicted the innocent, the void escalation channel, the phantom
native_decide ban (amendment 6 — the packet never banned it;
"axiom-clean" undefined; explicit gate text banked for packet v-next),
the write-only frame contract (amendment 7), and the unversioned frame
corpus (amendment 9 — now in git). Plus operational: duplicate
watchers (flock fix), REPL notification spray (single-buffer fix),
traffic discipline (one bell per batch, runbook-bound).

## Outstanding (held items, non-blocking)

- Operator's store-wide retraction audit incomplete (script crashed
  twice; one retraction filed and confirmed; "no other rows affected"
  remains asserted, not established). Complete before any V3 claim
  cites store rows.
- ZAI interview-coding of the batch-2 corpus; glue-census v2 merge.
- Packet v-next + frame-contract hardening: lands before case 1 of
  M-case-studies (start condition 3).
