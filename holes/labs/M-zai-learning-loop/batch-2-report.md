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

---

## SUPERVISOR VERDICT (ams-claude-1, 2026-08-11)

**APPROVED for the record.** The headline matches Amendment 8's
pre-registration and the final divergent tally (ctl 2 / mem 1) is
verified against the frame records. Notes binding on V3 and the merges:

1. **Noise-floor revision accepted.** V3 takes batch-2's bimodal
   characterisation (m01J03: closure-vs-partial divergence with no
   memory on either side) over batch-1's canyon claim.
2. **Demand-side finding adopted as design input for M-case-studies:**
   blocked arms ask for artifacts; succeeding twins supply methods
   (4/4). The case-study build lane therefore constructs from the
   obstruction as characterised at ESCALATION time, not from the
   blocked runner's first ask — and scribe memories are graded
   principle/technique/snippet, with only snippet-level demonstrated
   sufficient (b01A04's pre-refuted technique memory).
3. **Merges executed on apm-lean master through `7f338dd`.** A scripted
   merge set landed at 22:38 (ctl-everywhere-except-m01J03) before this
   adjudication; two problems are SUPERSEDED additively per this
   report: t00J02 → mem (real helper + enrichment over bookkeeping +
   deletion) and b01A02 → first-run mem (the 100-line axiom-clean
   order-85 lemma, per erratum E5). The m01J05 and a01J05 merges from
   that set stand as history but both problems are DEFECTIVE PANEL
   PICKS: corpus-fix tasks are (a) m01J05 + sibling m96A02 need
   `[Nonempty X]` in their frozen statements (machine-refuted without
   it), (b) a01J05's bridge_1 is a verbatim duplicate of its main
   theorem. Both fixes route through corpus maintenance, not frames.
   b01A02r-ctl's defective-partial commit remains unmerged per
   Amendments 5/6.
4. **Catalogue numbering:** the report's 13-item instrument list and
   V3 §2.1's 13-instance catalogue index differently (e.g. the phantom
   ban is item 9 here, instance 11 there). Reconciliation happens in
   V3; the report stands as written. E7's rule (hash instruments must
   assert non-empty input — `sha256("") = e3b0c442…` is the tell) joins
   the V3 countermeasures list.
5. **The batch-3 recommendation is superseded by M-case-studies**
   (Joe's strategy ruling, same day) — but its content survives: the
   pre-panel screen becomes the case-intake screen; the packet v-next
   items, the three absent resources, and the demand-side pivot are
   case-1 start conditions.

Push authorized. Batch era closed.
