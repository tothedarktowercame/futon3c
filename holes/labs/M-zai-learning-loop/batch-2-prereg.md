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

## Amendment (2026-08-11, pre-dispatch, Joe's ZAI allocation)

Relevance scoring (the falsifier's input) is performed by the ZAI
measurement lane: for each surfaced memory, zai scores terrain-relevance
{relevant | adjacent | noise} against the problem's statement + outline
under a three-line rubric, blind to arm. claude-3 spot-checks ≥20% and all
{relevant} scores; disagreements escalate to Fable. ZAI also codes
interview answers (needs/misses/glue) for the census. Dispatcher of record
for batch-2 is claude-3 (driver dispatches --from claude-3).

## Amendment 2 (2026-08-11, supervisor rulings on the operator's pre-dispatch
## hold; NOTHING dispatched before this landed)

**E3 ruling — mechanism fix, both arms symmetrically.** The six observed
packet-template anchor terms (compiled, files, route, frozen, proves,
closer) join the stopword family (v1.5). The contrast now tests memory-df
beyond boilerplate filtering. Re-verified post-fix: a01A12/memory-df still
surfaces slit-wedge (anchor "slit"); t91A05 honest-empty under BOTH sources
(problem-idf anchor becomes "functoriality").

**E1 ruling — §4.1's criterion is superseded, not violated.** The relevance
floor was intact; the old criterion conflated anchor-stage noise with floor
breakage. Restated: t91A05 must return honest-empty under the current-best
retrieval configuration — now true under both arms post-v1.5. A dated
correction goes on the handover doc. The operator's discovery that
memory-df independently repaired t91A05 pre-v1.5 is banked as a second
offline acceptance instance for the rung.

**E2 ruling — the falsifier statistic is PER-DISPATCH.** Each dispatch is
classified using ZAI-blind relevance scores over (a) its surfaced set and
(b) the union candidate pool from the offline both-source replay:
correct-surface (≥1 relevant surfaced) · correct-empty (none surfaced AND
no relevant memory in the union pool) · noisy-surface (surfaced, none
relevant) · harmful-empty (none surfaced BUT a relevant memory exists in
the union pool). **Falsifier: the mem arm's correct-decision rate
(correct-surface + correct-empty) must strictly exceed ctl's; ties or
worse = the rung fails and problem-idf stays.** Correct suppression
(m01J05-type) and harmful suppression (b01A04-type) are now both visible.
Timing honesty: this statistic was fixed after the prereg-sanctioned
replay exposed surfacing COUNTS (which diverge in both directions and do
not determine the statistic's outcome) and before any relevance scoring.

Operator may dispatch on receipt of this amendment. First pair staggering
unchanged.

## Amendment 3 (2026-08-11, mid-batch: session-isolation defect + salvage)

**Defect (operator escalation, mid-drain):** codex seats carry ONE session
across their whole job queue; fresh-session-per-run never held, in either
batch. `frames.bb open` compounded it by minting fictitious session UUIDs
(false evidence; fixed — :session is now recorded at close from the JOB
record's actual session-id, and the placeholder cannot survive closure).

**Salvage ruling (statistic untouched; decisive stratum preserved):**
- The seven tie-stratum pairs (pool-empty or pool-identical) complete
  as-queued, marked :contaminated-session, reported with that caveat —
  their union pools cannot move the falsifier.
- First-run b01A02 and b01A04 (the decisive pool-divergent pairs) are
  EXCLUDED from the falsifier regardless of their results.
- The decisive data comes from re-runs as batch **batch-2r**: fresh frames
  from the same base, first-run exp branches renamed aside, and a
  VERIFIED session reset (supervisor performs the reset via Drawbridge on
  the operator's request bell, per job, both seats) with the actual
  session id recorded at close. Four codex jobs; codex is unconstrained.
- Batch-1's frame :session fields are equally fictitious → erratum D4 on
  its report; batch-1's cross-seat twin comparisons stand (different
  seats), its within-arm independence does not, and V3 must not lean on it.

## Amendment 4 (2026-08-11, pre-batch-2r: the relevant/adjacent boundary)

Ruled before any batch-2r pool exists. **`relevant` = bears on the
problem's REMAINING OBSTRUCTION** — the memory would change what the
runner must construct (supply a needed artifact, route, or maneuver for
the open step). `adjacent` = same terrain but confirmatory/background:
it addresses work already done, general context, or a step not on the
critical path. `noise` = wrong terrain. The obstruction text (problem-
level, arm-independent — blindness preserved) is included in ZAI's
scoring packet. Basis: the operator's accumulated evidence — the sole
prior `relevant` score was later described by both runners as
confirmatory, and all four batch-2 USED claims adjudicated
:not-load-bearing. Scoring against the obstruction is what the
falsifier's correctness classes were always meant to mean.
