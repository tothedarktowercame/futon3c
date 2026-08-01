# E-memory-v3 — the experimental programme, derived

**Opened 2026-08-01 by claude-2.** Strategy for settling the V3 model in
**≤ 10 experiments, probably fewer** (Joe's bound), registered through the
`DarkTower` `ExperimentalDesign` / `ExperimentPreregistration` facility.

Inputs: `retrieval-whitepaper-v2.md` (complete draft), `E-memory-v3-staging.md`
(the bank), `E-memory-whitepaper-v2-programme.md` (what V2 froze).

---

## 1. Why DarkTower is the right instrument, and not merely available

**Three of V2's hardest-won methodological findings are things this facility
makes type errors.** That is the argument for adopting it; the CA provenance is
incidental.

| V2 found, empirically and painfully | DarkTower encodes it as |
|---|---|
| **Discoverability scored 0** because the receipt channel cannot distinguish *"no memory was needed"* from *"I could not ask"* (§4.1, §5.1) | `Observable.check_sound` — its docstring names exactly this: *"the field that rules out the check which cannot distinguish 'the thing is absent' from 'I was unable to ask'"*. Such a test **cannot inhabit the type.** |
| **Our adjudication rubric was not total** — 5 of 49 rows fell through all three categories, because the rubric assumed the substitutive model (§4.4) | `DecisionRule.classify : Trace → Outcome` is a **total function**. A category for trajectory-changing use would have had to exist *before* adjudication, not been discovered during it. |
| **A flag that never takes effect** — we specified an exclusion flag "default off" and had no way to state what turning it on must do (staging A2, P2b) | `Flag` has no constructor without an `Observable`. *"Declaring a flag without declaring what it does is a type error rather than an oversight."* |

Two more that bite directly on our failures:

- **`Registration.estimatedCost` must come from a *measured* rate including
  sampling multipliers** — the discipline codex-7 applied when it stopped at
  the blast-radius gate rather than guessing.
- **`ReplicationPlan` requires disjoint pilot and confirmation seeds fixed
  before the pilot is inspected** — the noise-floor-first requirement, typed.
  Our P2b design had this as prose; here it is a proof obligation.

**Consequence for how V3 should be written:** register each experiment as a
`ProspectiveRegistration` *before* dispatch. The preregistrations then become
the paper's Appendix B by construction rather than by transcription, and the
three failures above cannot recur silently.

## 2. The claims V3 must settle

Derived from V2's open questions, not inherited from a list. Each names what
would settle it.

| # | claim | current status | decisive test |
|---|---|---|---|
| **C1** | **The bottleneck is the attachment layer, not the lexical stage** | conjecture; three converging measurements, no direct test (V2 §5.2) | E1 |
| **C2** | Load-bearing verdicts are measurement, not judgement | 38% from an unrunnable counterfactual (§4.4) | E2 |
| **C3** | `used-ids` is *biased* toward dropping consequential uses, not merely lossy | n = 6, blind subset n = 2 (§4.4) | E2 (shared arm) |
| **C4** | Reachability is a distinct axis with a measurable rate | 5 instances noticed in passing; structurally invisible to receipts (§5.1) | E3 |
| **C5** | Retrieval granularity should be lane-scoped (regulative vs substitutive) | 45–54% vs 15% use rates, one corpus | E4 |
| **C6** | Connectivity is route-relative | n = 1, post-hoc contaminated (staging §G6) | E5 |
| **C7** | Findings generalise beyond one runner model | untested; confounded by lane instrumentation | E6 |
| **C8** | **Retrieval failure has a write-side, and precision-tuned retrieval manufactures duplication** | one measured witness (17 byte-identical proofs across two files, no import); prediction untested | **E7** |

**Eight claims, seven experiments.** C2 and C3 share arms in E2.

## 3. The experiments, ordered by decisiveness ÷ cost

### E1 — RUN 2026-08-01. Result in; **C1 is not settled by it.**

**Registered** as `DarkTower/MemoryArmPreregistration.lean`, run by codex-1 on
the frozen corpus. The feasibility check found something better than the
attachment-count proxy this section originally proposed:
`:memory-use/surfacing-via` records, **per surfaced memory, which arm delivered
it** — `:content-match` or `:pattern`.

**Classification `patternArmSubstantial`**: 67 pattern surfacings against 82
content-match, a **44.97% pattern share** against a 25% threshold fixed in
advance. So the pattern arm is *not* marginal, and the "well-curated list, not
a graph" reading is wrong for the period measured.

**But both registered observables return FALSE, and that is the finding:**

| observable | verdict | why |
|---|---|---|
| `attributionComplete` | **false** | 17 non-empty dispatches carry no arm attribution |
| `coverageNotTail` | **false** | the attributed window is **6.9 h of a 105.3 h corpus — 6.57%** |

Attribution begins at 2026-07-30T21:53 and covers only the final hours. So
**C1 remains open**: we cannot distinguish *"the pattern arm always contributed
~45%"* from *"it became substantial as attachments accumulated"*, because the
instrument starts where the interesting comparison would start.

**Three instrument defects were found across three review passes**, none in the
data: observables that were vacuous tautologies; a coverage check measuring
*ordinal position* where the defect was *temporal* (it passed on the very trace
it was written to reject); and independent second-truncation able to flip a
verdict at the boundary. All fixed; the registration now reports its own
non-generalisability.

**What would settle C1:** an instrument covering the whole corpus, i.e.
back-filling arm attribution, or a fresh measurement window long enough for
`coverageNotTail` to pass. Neither is expensive; both are prerequisites.

### E1 (original plan, retained for the record)

**Why first: it is the largest open claim and probably the cheapest.** V2
tested two *lexical* mechanisms and both failed; the surviving conjecture is
that "empty" means no *memories* surfaced rather than no *text* matched, and
that generic queries reach patterns with few reviewed attachments.

**This should be runnable on frozen data.** For each of the 126 analysed
dispatches, compute the reviewed-attachment count of the patterns its query
reaches, and test whether that predicts emptiness where DF (p = 0.618) and
co-occurrence (right magnitude, wrong sign) did not.

*If it is frozen-data-runnable it costs almost nothing and settles the paper's
central conjecture — which makes it strictly the first thing to do.* Feasibility
check needed: are dispatch-time pattern endpoints recoverable from the receipts,
or only the resulting memory ids?

### E2 — DESIGNED AND VALIDATED 2026-08-01; **do not run yet**

**Status:** `DarkTower/MemoryAblationPreregistration.lean` typechecks and builds
clean (763 jobs). All four outcomes reachable; **seven guards each
independently reaching `indeterminate`**; two adversarial regressions proven
(one run withholding the wrong memory id, and two runs sharing a session, each
now yields `indeterminate` rather than a verdict).

**Infrastructure is standing and costs nothing to hold:** the `apmablate`
account exists, four problem trees are staged at their own pre-solution
revisions (`a95J08`→`61ddc05`, `a02J05`→`fddc86c`, `a01A07`→`81dccb3`,
`a93J07`→`c8e6f11`), toolchain and Mathlib cache copied, and five isolation
probes pass strictly — each denial verified as an actual `Permission denied`
rather than any non-zero exit. Setup is one command:
`scripts/setup-ablation-account.sh`.

**Nine serial defects were found across eleven review passes, all in the
registration.** The decisive pattern: **three fields whose semantics existed
only in docstrings** — `withheldMemory`, `sessionId`, `baseRevision` — each
permitting a trace that ablated nothing, shared one session, or ran at
post-solution revisions to reach `rubricValidated`. The worst was the first: an
ablation study validating a rubric **without performing an ablation**.

**Design corrections worth carrying forward regardless of E2's fate:**

- the DV is **attempts, not closure** — on a 100%-solved panel closure has no
  variance, and it is blind to the five of seven use-modes that are regulative;
- the unit of analysis is the **problem, not the (problem, seed) pair** —
  treating seeds as independent was **pseudoreplication** that would have
  trebled `n` and made a null look significant;
- ties must be a third state, not folded into a Boolean — equal attempts were
  being scored as an incidental *win*;
- an exact one-sided sign test replaces an arbitrary `+3`, with severe honest
  critical values (n=6→k=6, n=7→k=7, n=8→k=7).

**Blocking requirement before any tokens are spent:** refactor to

```
raw Trace → validate once (all errors) → ValidatedTrace → classify → Outcome
```

so `classify` cannot omit a documented invariant, because an unvalidated trace
does not typecheck into it. The panel binds problem, revision and both memory
ids together. The adversarial traces built during review become a permanent
regression suite. **Full detail in staging §H6.**

*One open item the setup script cannot close: how a runner is invoked **as**
`apmablate`. Codex agents run as joe, and a dispatch that quietly ran as joe
would satisfy every isolation probe while being completely unisolated.*

### E2 — original design notes (staging §H)

Settles **C2** and, by including the prose-only memories as an arm, **C3**.
Design already specced and costed: closed problems re-run from pre-solution
state; DV is **did it close** — binary against a known-achievable target;
arms are LB-judged vs IN-judged ablations against a same-corpus noise floor.

**Isolation is a validity requirement, not hygiene**: a git worktree at an old
revision still reaches the solution through the shared object database, and
three futon3c lab artifacts describe the routes. Separate user, history
truncated at the target revision, no read access to `/home/joe`, fresh session
per run. ~30–50 dispatches, ~0.8–1.4M runner tokens.

### E3 — Reachability rate

Build the declaration → module → reachable-from-problem index (staging §B1) and
count how often a blocked target names a result proved elsewhere in the corpus.
Converts C4's five passing observations into a rate. **Note this cannot be done
with receipts at any level of analysis** — the mode is closed over the offered
set — which is precisely why it needs its own instrument.

### E4 — Lane-scoped granularity

Requires `:memory-use/kind` (staging §B4) to exist first. Then an A/B: does
mid-DF term selection help substitutive retrieval while broad surfacing is
retained for regulative memories? **Do not ship any term-selection rule before
E1** — if the bottleneck is the attachment layer, this experiment is aimed at
the wrong stage and would produce a repair that looks principled and does
nothing.

### E5 — Route-relative connectivity, blind

Re-run the a95J08 route-vocabulary comparison with vocabularies selected
**without knowledge of the target lemma** — the contamination its own author
declared. Cheap; settles C6 or kills it.

### E7 — Duplicate detection over the Lean corpus  ⟵ strong candidate for next

*Added 2026-08-01 from claude-9's LusinN finding (staging §I). It is the
write-side of the reachability axis and, on the argument below, the cheapest
remaining experiment with a hard ground truth.*

**The claim (C8).** We have studied only *"I need a fact — does it exist?"*,
whose failure is loud, bounded and self-correcting: you re-derive, at the cost
of one dispatch. The unstudied direction is *"I have just proved a fact — does
it already exist?"*, whose failure is **silent, permanent and compounding**,
because every later fix must be applied N times or the copies diverge. Nobody
runs that query because nothing prompts it.

**Why it is testable and why it is cheap.** Unlike every other experiment in
this programme, duplicate detection has **hard ground truth**: a Lean statement
is a syntactic object with a normal form, and `LusinN`/`a95A02` is a known
positive with **17 byte-identical proofs**. So a detector can be calibrated
against labelled instances rather than adjudicated. No dispatches, no runner
lanes, no token budget worth discussing.

**Preregisterable shape:**

| | |
|---|---|
| **H1** | near-duplicate declarations exist beyond the known positive, at a rate above zero |
| **H2** | duplication rate **rises with retrieval threshold** — precision-tuned retrieval manufactures duplication |
| falsifier | H1: no further duplicates found under a detector that recovers the known positive. H2: rate flat or falling across thresholds |
| ground truth | the 17 `LusinN`/`a95A02` declarations, plus any further pairs confirmed by proof-body diff |

**H2 is the load-bearing one** and it needs threshold variation, so it is
gated on the retrieval configuration being adjustable — check that before
committing.

**What it would settle beyond C8.** It is the natural calibration instrument
for embedding the Lean code, which was originally invoked for the retrieval
direction. Duplicate detection is an easier target than relevance and has a
checkable answer, so it earns the embedding work its keep either way.

**And it exposes what is actually missing: identity, not recall.** Episodic
memory answers *"have I seen something like this?"* and not *"is this THE one,
and where does it canonically live?"* — no canonical location per fact, no
notion that two episodes concern the same object. That gap is not addressed by
any other experiment here.

### E6 — Zai generality arm

Blocked on staging §A3 regardless: comparing a structured-receipt lane against
a prose lane confounds model with instrumentation. Sequence last.

## 4. Sequencing — state as of 2026-08-01, end of session

**This section is the resumption point. Everything above is context.**

| | status | next action | blocked on |
|---|---|---|---|
| **E1** | **RUN.** `patternArmSubstantial` 44.97%, but both observables false — covers 6.57% of the corpus by elapsed time | back-fill arm attribution, or open a window long enough for `coverageNotTail` to pass | nothing — cheap, and it is what would settle C1 |
| **E2** | **designed, validated, builds clean; NOT run** | the `ValidatedTrace` refactor (staging §H6) | that refactor; then the runner-as-`apmablate` dispatch path |
| **E5** | not started | re-run the a95J08 route-vocabulary comparison with vocabularies chosen *blind* | nothing — cheap, and it discharges a contamination its own author declared |
| **E7** | not started | duplicate detection over the Lean corpus, calibrated against the 17 `LusinN`/`a95A02` byte-identical proofs | nothing for H1; H2 needs the retrieval threshold to be adjustable |
| **E3** | not started | build the declaration→module→reachable index | index construction |
| **E4** | not started | — | `:memory-use/kind` (staging §B4), **and gated on C1** |
| **E6** | not started | — | staging §A3; sequence last |

**Cheapest next moves, in order:** E1's coverage back-fill (settles C1, the
paper's largest open conjecture), then **E7** (hard ground truth, a known
positive to calibrate against, and it earns the code-embedding work its keep),
then E5 (one measurement, discharges a declared contamination). None needs a
runner lane or a token budget worth discussing.

**E7 is the newest and arguably the best-posed**, because it is the only
experiment here whose answer can be *checked* rather than adjudicated.

**E2 is the expensive one and is correctly parked.** Its infrastructure is
standing and free to hold; its blocking item is a bounded refactor, not more
review.

**Lane hygiene remains a real constraint:** of ten codex agents, most have now
touched this programme, and E2's arms need runners with no prior exposure.
Budget lanes before tokens.

**Bound check: seven experiments against Joe's ceiling of ten.** C1 is still
open, so E4's shape remains undetermined; the realistic count is **five or
six**, still inside the bound.

## 5. What would make this "research-based, not vibes"

Three commitments, all enforceable by the facility rather than by intention:

1. **Every experiment registered as a `ProspectiveRegistration` before
   dispatch** — replication seeds, stop rule and total decision rule fixed in
   advance. V2 had three of four preregistered expectations turn out wrong, and
   that was its most valuable property; typing the registration makes it
   unskippable.
2. **Every observable inhabits `check_sound`.** If a proposed measurement
   cannot distinguish absence from inability-to-ask, it does not get to be an
   experiment. This single constraint would have caught V2's discoverability
   zero before we spent the coding pass discovering it.
3. **Costs from measured rates, with a stated budget cap and a teardown
   deadline scheduled independently of success.**
