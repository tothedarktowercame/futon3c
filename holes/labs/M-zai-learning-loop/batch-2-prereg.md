# Batch-2 preregistration — wave 2: the anchor-DF rung

Registered 2026-08-11 by ams-claude-1, instantiating the programme's ladder
(B1 nulled → retrieval-quality contrast next; batch-1-report.md rec. 1).
Operator: claude-3 per E-batch-operator-runbook.md. Awaiting Joe's blessing
before first dispatch.

## Contrast

BOTH arms `--memory-channel :push`. Arm labels reuse the frames vocabulary:
- **ctl** = `--anchor-source problem-idf` (status quo; seat ams-codex-1)
- **mem** = `--anchor-source memory-df` (the rung; seat ams-codex-2)

Seat note: ams-codex-1 receives the memory channel for the first time;
dispatch-time recall is packet injection and seat-agnostic (the f56780aa
provisioning difference concerns memory WRITING, unaffected here).

Mechanism under test (commit 6521fd3a): anchors from the pre-cap term pool,
memory-corpus df band [3,150], ranked [problem-IDF, memory-df]. Acceptance
already passed offline: a01A12 surfaces its slit-wedge memory (anchor
"slit"); a01A01 clean; default path byte-identical.

## Panel (fixed rule: first-k lexicographic open per class; excluding
## batch-1's panel, solved problems, and the 18 topology-blocked)

a01J05 a01J06 a02J04 · m01J03 m01J04 m01J05 · t00A05 t00J02 · b01A02 b01A04

## Predictions

1. **Surfacing relevance rises in the mem arm**: fewer wrong-terrain
   surfaces, more terrain-matching ones (scored per memory against problem
   terrain at harvest). Batch-1 baseline: 2 relevant / 6 surfacing.
2. Outcome metrics (closure, sorry delta): NO predicted difference at n=10
   — retrieval quality is upstream of use; the priors' small-effect stance
   stands. Stating this so a null on outcomes cannot be spun either way.
3. Any USED event in the mem arm whose memory would NOT have surfaced
   under problem-idf (checkable by replaying anchor selection offline) is
   the rung's win condition — P6-style, reportable at n=1.

## Falsifier

If mem-arm surfacing relevance does not exceed ctl-arm relevance (same
scoring), the rung failed and problem-idf stays. Outcome-metric equality
is EXPECTED and is not the falsifier.

## Choreography

Identical to batch-1 (bellback-only, brief interviews, per-job receipts,
sentinel-refusing closes, twin subcommand at pair completion) plus:
harvest records each arm's anchor + surfaced-set delta; the offline
anchor-replay (both sources against the same terms) is cut per problem at
pair completion — it is the cheap counterfactual this rung uniquely allows.

## Budget

20 codex dispatches + interviews (codex fresh at 100%); Claude spend
limited to claude-3 operator turns + one Fable liaison review.
