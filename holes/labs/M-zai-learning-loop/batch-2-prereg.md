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

## Amendment 5 (2026-08-11, post b01A02r: the norm-cargo ruling)

**Finding (operator, verified by independent recompile):** batch-2r's
fresh-session ctl run "closed" b01A02 by ADDING fourteen native_decide
uses — the hard-banned tactic both contaminated first-run arms refused
while citing the same packet ban. Norm-compliance was being carried by
session experience, not by the packet. First-order V3 material.

**Rulings:**
1. b01A02r-ctl is NOT a closure: scored defective-partial (axioms
   verbatim show 19 native_decide; obstruction records the required
   kernel-decide replacement, b96J04 precedent). The RETRIEVAL falsifier
   is unaffected — its per-dispatch classification concerns surfacing,
   which completed normally.
2. NO packet change for b01A04r — comparability within batch-2r wins,
   and the axiom-clean gate at scoring IS the enforcement (it caught
   this). Fresh-session compliance behavior is now itself a measured
   variable: two more fresh runs = two more observations.
3. Frames close honestly with the defective verdict on record; the
   commit stays on its branch, unmerged.

**Delivery-gap rule (runbook-bound):** an escalation written as a
bellback-turn REPLY routes to the void (this one sat unread three
hours). Escalations are delivered ONLY as explicit bells to
ams-claude-1 — bell first, then reply in-turn if desired.

## Amendment 6 (2026-08-11, correction: amendment 5's FINDING is retracted)

**The norm-cargo finding was wrong on its premise.** Supervisor-verified
against the committed template (`git show HEAD:data/codex-sorry-packet-
template.txt`, 137 lines): `native_decide` occurs ZERO times. The packet
never banned it. "Axiom-clean" (line 1, line 106) is never defined; the
only operationalization the packet gives is the sorryAx test (line 108).
There was no "hard-banned tactic" and no packet ban for session
experience to carry. Operator-caught (this correction is claude-3's, from
the b01A02r-ctl interview + a direct packet check); the supervisor filed
amendment 5's finding without checking the packet text it presupposed.

**Corrected finding:** an undefined gate term produced three sincere
readings — two strict (refuse native_decide; one runner discarded a
working proof to over-comply), one narrow (sorryAx-only, from a runner
that DID audit: added `#print axioms`, recorded output verbatim, judged
the entries permissible under the only explicit test given). Both strict
runners attributed their strictness to packet text that does not exist
("the packet explicitly ... rejected native_decide" — it does not), and
the supervisor repeated the attribution. The gate LOOKED enforced
because most runners happened to over-comply. Session-experience
contribution cannot be ruled out (contaminated arms read strictly, the
fresh arm read narrowly, n=1 fresh), but ambiguity is the parsimonious
explanation. This is silence-catalogue material (V3 §2.1 instance 11),
not norm-cargo material; the norm-cargo thesis is UNSUPPORTED at n=1
and must not appear in V3 as established.

**What survives amendment 5:** ruling 1 (defective-partial) stands on
artifact grounds alone — 19 native_decide axioms in `#print axioms` fail
the kernel-clean standard the CAMPAIGN scores by, whichever reading the
runner took; no fault attaches to the runner. Ruling 3 (no packet change
for b01A04r) stands and is strengthened: comparability now also measures
the ambiguity itself. Ruling 2's framing shifts accordingly: fresh-run
readings of the undefined term are the measured variable.

**Banked for post-batch-2r (packet v-next, single versioned change):**
the ctl runner's own drafted gate text — "No theorem claimed complete
may acquire dependencies produced by native_decide; #print axioms must
contain only the accepted foundational axioms and no
*.native_decide.ax_*. Existing native-decision dependencies do not
authorize adding more. Compare the final axiom set against baseline."

**Retraction hygiene:** any store rows asserting the norm-cargo finding
or the nonexistent packet ban get retractions + RETRACTION-MARKER
companion rows (runbook convention). Amendment 5's text above stands
unedited as the historical record; this amendment is its correction.

## Amendment 7 (2026-08-11, pre-b01A04r: cross-frame READ isolation)

**Finding (operator, supervisor-reproduced):** the frame contract scopes
WRITES ("work only in {checkout}; commit on the frame branch only") and
says nothing about reads. b01A02r-mem, on a verified-fresh session, read
first-run frames off disk — its interview cites "control-frame notes"
whose text exists ONLY on `exp/b2-firstrun-b01A02-ctl` (grep: 1 there, 0
in base) — and calibrated its native_decide caution from them. All ~44
frame worktrees were world-readable siblings sharing one object store,
so every arm of batches 1–2 had disk AND git-ref access to every other
frame, including its twin's. No instruction was violated; the isolation
was requested, never enforced.

**Consequences accepted into the record:** (1) b01A02r carries a
cross-frame-exposure caveat (its mem/ctl caution asymmetry is now
explained: mem read the first-run ctl closer notes). This further
undercuts the retracted amendment-5 finding. (2) All twin-independence
claims in batches 1–2 weaken from "isolated" to "no evidence of
cross-reading, channel open" — one demonstrated instance, self-reported.
(3) First-run b01A04 artifacts on disk would have handed the re-run its
answer (the committed transport at Main.lean:143).

**Ruling — option (a), executed by the supervisor (cross-frame scope):**
first-run b01A04 artifacts quarantined BEFORE dispatch: branches bundled
to `/home/joe/code/apm-frames-aside/b2-firstrun-b01A04.bundle` (verified
complete; restore instructions + shas in the README there), worktrees
removed, branch refs deleted — unreachable from `git log --all`, closing
both the disk channel and the shared-object-store channel. Packet text
unchanged (ruling 3 comparability preserved); b01A04r therefore runs
with REAL isolation while b01A02r ran with the channel open — the
falsifier read must weigh the pairs accordingly. Twin rule: both b01A04r
arms dispatch together so neither can read a completed twin.

**Banked for batch-3 (single versioned change with the axiom gate):** a
read-scope line in the frame contract, and `frames.bb close` removing
the worktree (the branch is the artifact of record; a closed frame's
checkout on shared disk is an open book).

## Amendment 8 (2026-08-11, PRE-b01A04r: falsifier verdict, written before
## the last pair runs)

**The anchor-DF rung FAILS. problem-idf stays.** Recorded now, dated
before b01A04r's dispatch, so no later reading can cast b01A04r as
having tipped it.

**The arithmetic (operator's, supervisor-verified against amendment 2's
frozen statistic):** scoring is problem-level and arm-independent
(amendment 4), so the seven tie-stratum dispatches — a01J06 and a02J04
(identical single-memory surfaces) and m01J03, m01J04, m01J05, t00A05,
t00J02 (both arms empty on empty pools) — contribute identically to both
correct-decision numerators whatever ZAI scores them. The falsifier
therefore reduces to the divergent stratum:
- a01J05 — mem noisy-surface, ctl correct-empty → ctl
- b01A02r — mem noisy-surface (single surfaced memory ZAI-scored noise,
  operator-concurred, all four runners across both runs called it
  unrelated), ctl correct-empty → ctl
- b01A04r — pending; mem's best case leaves ctl ahead 2–1.
Mem cannot strictly exceed ctl. Under the frozen statistic — ties or
worse = fail — the verdict is entailed regardless of b01A04r.

**The statistic stays frozen:** b01A04r's classification will be
recorded and folded into the final rates pro forma; the VERDICT cannot
move. The batch report's falsifier section cites a01J05 and b01A02r as
the deciding dispatches.

**Design reading (V3 §5 material):** the rung passed its offline
acceptance test both directions (fixed the a01A12 slit-wedge inversion;
t91A05 honest-empty) and still failed the batch. Both facts stand:
memory-df selects better anchors when a relevant memory EXISTS, but at
current store coverage most dispatches have nothing relevant to find,
so a more permissive anchor policy trades correct-empties for noisy
surfaces — the empty-pool prior dominates. The next rung is not anchor
selection; it is coverage/relevance-floor (supply side) or the
vote→build→callback pipeline (demand side), which remains the only
mechanism with a demonstrated closure. The --anchor-source flag and
memory-df rung stay in the code as instrumentation for a re-test when
coverage warrants; the shipping default remains problem-idf.

**b01A04r is re-scoped, not cancelled [see amendment 9 below for the
frame-corpus versioning finding that arrived while it ran]:** it runs as (1) the isolation
experiment — first genuinely quarantined frames of the campaign
(amendment 7; both the disk copy AND the shared-object-store refs of
its first-run answer are gone), where reproduction of the same
Subsingleton.elim transport by a fresh session would be convergence
evidence that retroactively calibrates the breach's severity; (2) a
recurrence test for b01A04's scribe-candidate demand signal; (3)
batch-2r record completeness. Two codex dispatches; codex is
unconstrained (Joe, 2026-08-10).

## Amendment 9 (2026-08-11, the frame corpus had no past)

**Finding (operator, discovered EXECUTING a supervisor ruling):** the
entire frame corpus — 44 records across batches 1/2/2r plus twin diffs,
the primary artifact every report cites — was gitignored (`data/*` at
line 15; a bare `data/` at line 34 that ALSO deadened every existing
`!data/…` negation). Zero of 71 files ever tracked. No history, no
backup, no audit trail; silently editable; `frames.bb validate` reads
the same unversioned files and passes regardless. The records look
authoritative and nothing indicates they have no past.

**Supervisor premise failure, second of the day:** the in-place
correction ruling asserted "frame records are files under git — version
control IS their append-only history" and cited the D1/D2 repairs as
recoverable-from-git precedent. Never checked; false. (After amendment
6 the rule was "verify claims about packet text before ruling"; the
rule is now "verify premises about INFRASTRUCTURE too" — the two
failures are one failure.) The operator executed the ruling in good
faith, then checked `git ls-files`, then escalated with the original
text preserved.

**Rulings:**
1. **Un-ignore, retroactively commit — executed by supervisor.**
   `!data/experiment-frames/` + `/**` after the deadening line; all 71
   files committed. History begins 2026-08-11; the corpus's
   pre-history is attested only by the reports, transcripts, and job
   records. `frames.bb close` auto-committing the frame record joins
   the batch-3 hardening list (enforcement by construction).
2. **The operator's applied correction STANDS.** The corrected text is
   accurate; the displaced original is preserved here verbatim as the
   record of what the field said from close until correction:
   "...apm_b01a02 now depends on 19 native_decide axioms, violating
   the packet's axiom-clean acceptance rule. Runner's own error-fix
   log: ..." (the phantom-ban clause is what amendment 6 falsified;
   the axiom facts and error-fix log were and remain correct).
3. **D1/D2 batch-1 errata annotated:** those in-place repairs happened
   on an unversioned corpus too, so their originals are not
   byte-recoverable — but nothing is actually lost: the displaced
   values were content-known sentinels (the literal "MISSING", the
   omitted sorryAx line, the nil obstruction), fully described in the
   errata narrative, which is the record.

**V3:** silence-catalogue instance 13 — the evidence base with no past.
