# M-apm-demonstration

**Status:** HEAD complete; IDENTIFY draft pending operator acceptance (2026-08-14)
**Gate:** operator-acceptance — HEAD must be recognised as faithful to the
mission's live shape before IDENTIFY hardens it. Per the lifecycle, the
operator clears this by editing the file.
**Gate:** operator-decision — IDENTIFY's completion criteria (§1.5) name
thresholds that are proposals, not settled numbers.

**Note on phase discipline (Joe, 2026-08-14).** An earlier draft of IDENTIFY
carried design conclusions from `capability-proof-apm-v2-plan.md` — "N7 is the
keystone", the measurement vector, `L(i)`'s form, a dispatch order — as though
they were settled. They are not: they were written before any survey of what
the loop already emits, which is MAP's job. They now live in the **N-register
(§1.4)** as open directions, each with the question MAP must answer. The
lifecycle is explicit that MAP "produces facts, not decisions"; importing
DERIVE-level design into IDENTIFY is the failure HEAD's exit criterion warns
about — pretending the unknowns have already been settled.

Supersedes `E-apm-halftime-pre-go-live.md`, **closed 2026-08-14**: its 24
items are discharged, corrected in place, or explicitly parked. What remains
is not repair but construction, so it is a mission rather than an excursion.

`E-apm-halftime-pre-go-live-EF.md` remains live **as an empirical source**,
not as the plan. It is the informal description of the system; this mission is
the formal one.

---

## HEAD

*Per `futon4/holes/mission-lifecycle.md`: HEAD preserves the operator's voice
and carries tensions forward before IDENTIFY hardens them. It is not design.*

### Operator-voice anchor

Joe, 2026-08-14, in his own words across the session that produced this file:

> "Our actual plan to develop a meaningful *training* signal using the
> multiagent system, to look at how it improves things per problem, and to
> demonstrate improved capability on BPM or Arxiv proofs isn't actually in the
> document as far as I know — or if it is, it isn't backed by a
> description/demonstration of a viable system — because we only have prepared
> the precursor to that viable system now."

> "The previous capability-proof just amounts to 'if Codex is good at math it
> can solve these problems' which isn't very interesting."

> "Since the current state of evidence seems to be 'pilot not measurement', I
> want each round of the agentic loop to lead to measurables that can serve as
> something like a loss function as we move from problem to problem."

> "This is a much better state of affairs for experimental work b/c we can now
> pin hashes to experiment stages."

### The guiding light, reversed

Joe offered this as the mission's orienting principle, to be read **inverted**:

> "It was on display in the bottom of a locked filing cabinet stuck in a
> disused lavatory with a sign on the door saying 'Beware of the Leopard.'"

Inverted, it is an acceptance criterion: **an artifact counts as available only
when the person or agent who needs it can find it, by the vocabulary they
actually use, at the moment they need it.** Technically-present is not
available.

This is not decoration. Every failure repaired on 2026-08-14 was this failure,
and so is the retrieval defect the V3 whitepaper diagnoses (§4.1): a01A12's
memory was in the store, indexed by a vocabulary its own runner would never
search by, and the runner re-derived the content instead.

### What's already felt to be true

- **The relay works.** Boundary artifacts transport work between agents; the
  repository itself (boundary comments, neighbouring solved problems) has been
  the memory channel that demonstrably functions.
- **Framing dominates architecture.** One sentence of task framing moved memory
  consultation from 0 to 21 calls, same agent and same store (E9/E10,
  preregistered).
- **The demand loop has fired once, end to end.** Votes → build → callback →
  one-pass closure of a theorem that had survived three prior closer hops
  (V3 §6; `10eac91b` → `a266157d`, merged `087924c`). That is learning at the
  ability level, witnessed, n=1.
- **Failure is legible when instrumented.** The day's substrate work produced
  named errors and measured rates rather than mysteries.

### Anti-glibness discipline

What would make this mission superficial:

- **Reporting a solve rate as if it were a capability claim.** The corpus
  percentage is an odometer, and its current gauge is known to read low.
- **Letting a falling `L` stand in for learning.** Without pre-declared
  difficulty stratification, a falling loss is indistinguishable from an easier
  tail of the corpus.
- **Measuring across a retrieval-regime change.** The anchor-selection repair
  (`6521fd3a`) is under live test; a slope spanning it confounds "the process
  learns" with "we fixed the index."
- **Counting a round that never elicited consultation.** If the treatment arm
  does not look, the round measures nothing while appearing valid.
- **Upgrading a warrant on the existence of an artifact** rather than on a
  demonstration that its consumer can reach it.

The discipline that prevents it: the warrant machinery the capability proof
already has — *a warrant upgrades only by certificate, never by narrative* —
plus the reversed guiding light as a new certificate class.

### Working-economy position

**What this underwrites:** the claim that the futon stack constitutes a
capability, not a collection of agents that sometimes succeed. The
demonstration target is improved capability on **BPM or arXiv proofs** — held
out from APM, which makes it formally a transport claim.

**What underwrites it:** as of 2026-08-14, and not before —

- three repos at inbox zero and even with origin, so an experiment stage pins
  to a hash (futon3c `33a148c2`, futon6 `01d7802`, futon1b `3e1b0d2`);
- the 154-file evidence corpus reachable and sha-verified (154/154) with
  `backup_evidence.sh` as the sync basis;
- a substrate whose census is a measurement, not archaeology (`pattern/library`
  1288 rows == 1288 distinct names; `pattern/clause` 9668 == 9668; zero
  non-qualified ids or relation endpoints);
- a written identity convention, making conformance a one-line audit.

### Clarity-gap / carried-forward tensions

Named here so IDENTIFY does not pretend they are settled:

1. **The odometer is miscalibrated.** v1 says ~27% closed; `status.json` gives
   186/475 = 39.2%, and E5 showed that file *understates* closure. No
   percentage should be quoted until a comment-aware sorry detector exists.
2. **`L(i)` needs a difficulty model.** Stratification or matching, declared in
   advance. Not yet designed.
3. **N6's transport derivation is an unpaid obligation.** v1 states it
   correctly as a selection diagram and never discharges it.
4. **The retrieval regime is mid-repair.** batch-2 is under live test; the
   regime boundary is a stratum boundary for every measurement.
5. **The scribe role is staffed but not performed.** `ams-scribe-1` was
   repurposed as a bridge-lane seat; the hunger audit has never run at campaign
   scale. Two of the most informative measurables are empty for that reason.
6. **The sync is manual.** `backup_evidence.sh` is invoked by hand; until the
   rsync pattern exists, the corpus's reachability rests on someone
   remembering — the same failure shape, relocated from git to attention.
7. **The one effect-size datum is one edge wide.** RAW-CTL corrected:
   enriched 3/25 = 12.0%, raw 4/26 = 15.4% over 51 inference edges. **Not a
   directional prior** — a sizing lesson.

### Provenance

Generated 2026-08-14 from a single operator session (surface `emacs-repl`,
seat `claude-2`), following the closure of `E-apm-halftime-pre-go-live` and
the repair of the futon1b substrate. Direct antecedents:
`capability-proof-apm-v2-plan.md` (written earlier the same day),
`E-apm-halftime-pre-go-live-EF.md`, `docs/retrieval-whitepaper-v3.md`,
`holes/labs/M-diagramprover/capability-proof-apm.tex`. Operator quotations are
verbatim from the session, not paraphrase.

---

## 1. IDENTIFY *(draft — gated)*

### 1.1 Motivation — the gap, in the operator's words

Joe, 2026-08-14, naming what two days of work have been circling:

> "At the level of IDENTIFY, I'd say the key gap is what we've now spent 2 days
> trying to even get our heads around —
> **(1)** how much evidence of success was there from the initial APM work?
> **(1a)** How much evidence was there from the initial APM work, full stop?
> E.g. we had 'one' success across — how many runs?
> and **(2)** can we demonstrably build a system that gathers evidence from the
> first cycle, and that **(2b)** plausibly will show improvement at each
> iteration."

**These four questions are the gap.** They are not answered here; answering
them is MAP's job (§MAP). What IDENTIFY can state is *why they are hard*, and
that diagnosis is the mission's actual motivation:

#### The structural problem: numerators without denominators

Every positive result on record is a **numerator**. The denominator has never
been stated.

| the numerator we cite | the denominator nobody has stated |
|---|---|
| *one* end-to-end demand→build→closure revolution (V3 §6) | out of how many demand cycles? how many votes reached threshold and produced nothing? |
| *one* rewrite found at 0.77 cosine (glue census) | out of how many rewrites looked for? |
| 17 of 45 adjudicated load-bearing | 45 out of 130 jobs surfaced memory IDs at all; 129 offered halves → 114 joins → **20 metric-bearing rows (15.5%)** |
| Steinhaus closed end-to-end under automation | out of how many chains attempted? |
| 0 → 21 store lookups on task framing (E9/E10) | the one contrast that *does* carry its own denominator |

So (1) cannot be answered as a rate, and (1a) — *how much evidence exists at
all* — turns out to be the prior question, because the observation channel is
admitted selective: only 15.5% of offered rows became metric-bearing.

**This is the same failure as the guiding light, one level up.** The evidence
was not missing; it was uncountable — recorded in a form where the total was
never fixed, so no result could be expressed as a fraction of anything.

#### Why the warrant table looks the way it does

The certified/uncertified split is a *symptom* of the above, not the disease.
Sorting v1's own table:

| certified (scaffolding) | uncertified (the thesis) |
|---|---|
| N1 extra resources fill Mathlib holes — n=3 | **N5 retrieval serves the need — weak** |
| N2 work transports between agents — n=4 | **N6 transports to held-out — designed** |
| N3 the store records learning — n=4 | **N7 outcomes mechanically scoreable — designed** |
| N4 agents consult when instructed — n=1 (ctrl) | **N8 learns at ability level — designed** |
| N9 the pipeline runs — n=2 | |

Everything certified is an *existence* claim — a thing happened at least once —
and existence claims need no denominator. Everything uncertified is a *rate* or
*trend* claim, which cannot be stated without one. **The table is not evidence
of dishonesty or of a weak pipeline; it is evidence that the programme has been
able to certify exactly the class of claim that does not require counting.**

*(Correction carried from `capability-proof-apm-v2-plan.md` §8.1: N8 is
mis-graded — V3 §6 witnesses it at n=1. But n=1 of an unstated denominator is
still not a rate, which is exactly Joe's point.)*

#### What (2) and (2b) then ask for

Not "instrument the loop" in the abstract. Specifically:

- **(2)** a system where **cycle one already yields evidence** — the
  denominator fixed in advance, so the first round is countable rather than
  retrospectively reconstructible;
- **(2b)** and where **iteration plausibly improves** — which requires the
  round-to-round quantity to be comparable, i.e. the difficulty and
  retrieval-regime controls named in the carried-forward tensions.

(2b) says *plausibly*, not *provably*. That is the right bar for IDENTIFY: the
mission must show the design could exhibit improvement, not that it has.

### 1.2 Theoretical anchoring

Retained from v1 and not in question: the **constructive (BHK) reading** of the
capability claim; **graded warrants with typed refusal**; **transportability
via selection diagrams** for N6.

Candidate, *not* adopted: a **loss-function framing** in which each round emits
measurables and the claim becomes a slope rather than a property; and the
**reversed guiding light** as a warrant condition (reachability by the
consumer's own vocabulary). Both are the operator's, both are plausible, and
neither has been tested against what the system already emits. See §1.4.

### 1.3 Scope in / out

**In.** Determining, for each uncertified node, whether the work is already
done, partly done, or greenfield — and only then designing. A demonstration on
held-out BPM or arXiv proofs. Revising `capability-proof-apm.tex` to v2.

**Out (explicitly deferred).** Rewriting v1's warrant machinery — it is the
contribution and survives intact. Substrate-2 reingest. The `#uuid` gate.
B/C/D packet residue.

### 1.4 OPEN IN IDENTIFY — the N-register

Joe, 2026-08-14: *"your plan according to N-numbers from previous rounds could
serve as a checklist for IDENTIFY — for example, 'N7 is the keystone' may be
true but IDENTIFY would help us see what work (if any) has already been done,
or whether this is a greenfield design issue … as they currently comprise a
speculative design."*

**Everything in `capability-proof-apm-v2-plan.md` §2 is speculative design.**
It was written before any survey of what the loop already emits. It is retained
as a *source of hypotheses*, not as a plan. Each node below is therefore left
**open**, with the question MAP must answer stated as: *already done / partly
done / greenfield?*

| node | v1 grade | what is FACT | what is SPECULATION | MAP must determine |
|---|---|---|---|---|
| **N5** retrieval serves the need | weak | The defect is diagnosed (V3 §4.1, inverted relevance on a01A12). Users search by **engine names** and **structural similarity**, never concept vocabulary (V3 §4.2, twelve exit interviews). A repair shipped as a switchable contrast (`6521fd3a`), under live test as batch-2. | That the hunger audit is "the instrument", and that running it is "a measurement, not a build". | What does the hunger audit actually record today? Has it ever run? What would campaign scale cost? Is batch-2 concluded? |
| **N6** transports to held-out | designed | v1 states it as a transport claim with selection variable `S`; the causal engine exists (backdoor/front-door/transportability). | That the derivation is discharge-able at all, and that BPM is the right held-out set. | Has any transport derivation been attempted? Does the engine refuse it? Which of BPM / arXiv has the better-formed selection diagram? |
| **N7** outcomes scoreable | designed→mechanical | v1 names "delta-form endpoint, executed-witness-only scoring" but never defines the endpoint. 57 `receipt.json` files exist with a rich schema. Ledgers exist (`bridge-pilot-jobs.jsonl`, `escalation-queue.jsonl`, `axiom-audit.jsonl`). | **That N7 is "the keystone"**; the six-row measurement vector; `L(i) = cost + residual + rework`. None of this has been checked against what the loop emits. | Which vector quantities are **already emitted**? Which need new bookkeeping? Is there a scoring implementation already, in the driver or the gate? |
| **N8** learns at ability level | designed | **Mis-graded.** V3 §6 records a complete demand→build→closure revolution (`10eac91b` → `a266157d`, merged `087924c`) closing a theorem that had survived three prior closer hops. The vote ledger exists: 14 concepts, three past threshold. | That N8 should be restated as `dL/d(problems seen) < 0`; that difficulty stratification is the right control. | Is the vote ledger machine-readable as a series? Is there a difficulty signal already in the corpus (`closer_hop`? sorries at formalization? family?) |

**Consequence for ordering.** The v2 plan asserted an order (N7 first, then N8
and N5, N6 last). That order rests on the keystone claim, which is itself
speculative. **No order is adopted here.** MAP's findings determine it.

**Consequence for the three preconditions.** Headroom, arm independence and
elicitation-verified are *observed invalidators*, not design — they are facts
about assays that failed, so they stay. What is speculative is promoting them
to formal pass/fail conditions; that is a DERIVE decision.

### 1.5 Completion criteria *(testable; thresholds are proposals — see Gate)*

Stated so as not to presume the register's outcome:

1. **Every N-register row resolved** to already-done / partly-done /
   greenfield, with evidence — this is MAP's exit condition and this mission's
   first real deliverable.
2. **N7 carries a mechanical warrant**, by whatever endpoint DERIVE settles on.
3. **N5 graded against the users' contract** (engine-name and
   structural-similarity queries), not a generic relevance notion.
4. **N8 stated in a checkable form** and evaluated within a fixed retrieval
   regime named by hash.
5. **N6 discharged or refused**, with the refusal recorded if so.
6. **A held-out demonstration** on BPM or arXiv satisfying the three
   preconditions.
7. **The capability proof revised** to rank nodes by load-bearing status and to
   drop the miscalibrated odometer.

### 1.5 Relationship to other missions

- **Supersedes:** `E-apm-halftime-pre-go-live` (closed 2026-08-14).
- **Depends on:** `E-apm-halftime-pre-go-live-EF` as empirical source;
  `docs/retrieval-whitepaper-v3.md` for the retrieval regime; the batch-2
  live test (`6521fd3a`) for regime stability.
- **Enables:** `capability-proof-apm.tex` v2.

### 1.6 Source material

`holes/labs/M-diagramprover/capability-proof-apm.tex` (v1, 311 lines) ·
`holes/labs/M-diagramprover/capability-proof-apm-v2-plan.md` ·
`holes/excursions/E-apm-halftime-pre-go-live-EF.md` ·
`docs/retrieval-whitepaper-v3.md` (§4 retrieval, §6 demand loop, §7 terms) ·
`futon6/README-apm-lean-ground-control.md` §1 (the regime) ·
`data/evidence/` (154 files, sha-verified) ·
`futon6/holes/TN-raw-ctl-reanalysis.md` (the one effect-size datum) ·
`scripts/backup_evidence.sh`, `scripts/batch2r_pair.sh`, `scripts/rawctl2.py`.

### 1.7 Owner and dependencies

Owner: `ams-claude-2` (Zone), operator Joe.
Repos: **futon3c** (mission, loop, evidence), **futon6** (ground control,
RAW-CTL, playbooks), **futon1b** (substrate), **apm-lean** (corpus).
External: Codex lanes for implementation under the handoff protocol;
`oxf-claude-3` holds Dionysus.

---

## MAP *(not started)*

**Exit condition:** Joe's (1) and (1a) answered with denominators, and every
row of the N-register (§1.4) resolved to *already-done / partly-done /
greenfield*, with evidence. Until then no ordering, no keystone, and no
endpoint definition is adopted.

MAP is research: it produces facts, not decisions.

### Track A — answer (1a) first: how much evidence exists, full stop?

The prior question, because (1) is a fraction of it.

- **A1** What is the **complete inventory of runs** the APM programme has
  executed — across the 130 frozen job results, the conductor ledgers
  (`apm-conductor-log.edn` 5,564 lines / v2 1,854 / v3), the bridge-pilot
  jobs, the escalation queue, and the campaign ledger? Deduplicated, with a
  stated definition of what counts as one run.
- **A2** For each run, is the outcome **recoverable**? The V2 ledger reports
  91 completion events over 73 unique problems, 15 proved / 76 partial. Does
  that reconcile with the 475 `status.json` files and the 186 at zero sorries?
- **A3** Why did **only 15.5%** of offered rows become metric-bearing
  (129 offered → 114 joins → 20 metric-bearing)? Is the loss at emission,
  join, or scoring? This determines whether the historical corpus can ever
  yield rates, or only existence claims.
- **A4** What is the true corpus closure under a **comment-aware sorry
  detector**? (Current gauge is known to read low: E5, `a94A09`.)

### Track B — then (1): how much of it is evidence of success?

- **B1** For each numerator in §1.1, what is its denominator? Specifically:
  how many demand cycles produced no artifact; how many rewrites were sought
  vs found; how many chains were attempted vs closed.
- **B2** Is there a **run-level record** that survived, or only
  problem-level? A rate over problems is not a rate over attempts.
- **B3** Which historical results carry their own denominator already?
  (E9/E10 does — 0 vs 21 in a fixed design. Are there others?)

### MAP Track B findings

**B1 — denominators.** The audit distinguishes a countable source population
from the denominator of the claim. A large corpus is not automatically the
number of opportunities on which a cited event could have occurred.

| cited numerator | surviving denominator |
|---|---|
| One demand→build→callback→closure revolution | **Unrecoverable as a rate.** V3 §6 preserves one dated revolution and a one-day ledger *snapshot* of 14 concepts, three past threshold. It does not preserve a series of demand cycles, a count of threshold crossings, or an artifact/no-artifact outcome for every crossing. Therefore neither “cycles attempted” nor “threshold concepts producing no artifact” can be counted from this record. The snapshot's 14 and three are populations of concepts at one time, not a denominator for revolutions. Track A's total-run inventory may add runs, but cannot be substituted here unless it identifies demand-cycle boundaries and outcomes. |
| One rewrite found verbatim at cosine 0.77 | **Unrecoverable as a rewrite hit rate.** `have-corpus.jsonl` has 6,114 statement rows and `clusters.tsv` has 3,805 cluster rows; these are the indexed search space and clustering output. V3 §7 says one evening step matched two prior occurrences at 0.77, but no artifact defines or counts the population “rewrites looked for.” Thus 6,114 and 3,805 are not denominators for the one cited rewrite. |
| 17 adjudicated load-bearing | **17/45 only within the non-unclear reported-use subset.** The complete selection chain is: 154 frozen files → 130 raw job records → 121 jobs with a memory report → 45 jobs whose reports surfaced memory IDs (76 reports without IDs) → 49 candidate memory-use instances → 45 adjudicable instances after four `UN` exclusions → 17 `LB` (with the remaining 21 `CO`, five `TRAJ`, two `IN`). A separate observation-channel funnel is 129 offered halves → 115 outcome halves → 114 joins → 20 metric-bearing rows (15.5%). The 17/45 is therefore not a rate over 130 jobs, 129 offers, all APM attempts, or all memories offered. Sources: the frozen worksheet, candidate JSONL, verdict JSON, observation-channel audit, and `evidence-summary-20260814.md`. |
| Steinhaus closed end-to-end under automation | **Unrecoverable as a chain-closure rate.** The surviving material names a successful theorem/problem episode, but the surveyed records contain no frozen chain ledger defining all Steinhaus-chain attempts, including retries and abandoned chains. A problem-level success cannot supply its own attempt-level denominator. This remains dependent on Track A if its deduplication finds an explicit attempt series; absent that, it is an existence witness only. |
| 0 → 21 store lookups under task framing (E9/E10) | **Denominator present: one observed run in each of two fixed conditions.** E9 records zero memory-tool calls under the invitation framing; E10 records 21 under the two-part frame, with the same agent and store. This differs from the other rows because the contrast and scoring unit were fixed prospectively, the baseline and treatment were both observed, and transcripts count calls in both conditions. It is an `n=1` paired mechanism contrast, not a proof-success-rate estimate. Sources: `E9-pull-probe-prereg.md` and the E9/E10 synthesis in `M-diagramprover.md`. |

**B2 — record granularity: mixed.** The frozen July corpus contains genuine
run-level records for 130 invokes (126 terminal `done`, four `failed`), and the
E9/E10 probe preserves condition-level transcripts. Those records can support
rates whose unit is explicitly one of those invokes or conditions. Other cited
results survive only at problem or episode level: the demand ledger is a
concept snapshot, the glue result does not enumerate rewrite-seeking attempts,
and the Steinhaus success has no complete chain-attempt register. Consequently
there is no defensible historical “successes per APM attempt” rate yet. Track
A owns the cross-ledger run total; this finding does not duplicate or guess it.

**B3 — historical results already carrying denominators.** These may be quoted
as rates or fixed contrasts now, provided their narrow units and caveats travel
with them:

- E9/E10: 0 versus 21 memory-tool calls, one observed run per framing
  condition; a mechanism contrast, not outcome lift.
- Observation-channel completeness: 115/129 outcome halves, 114/129 joins,
  and 20/129 metric-bearing rows. These measure evidence capture, not proof
  efficacy.
- Load-bearing adjudication: 17/45 non-unclear reported-use instances (49
  candidates before four `UN` exclusions), explicitly a filtered-subset rate.
- Frozen raw jobs: 126/130 terminal `done` and 4/130 `failed`; operational job
  completion only, across heterogeneous phases and targets.
- Retrieval damage-state sweeps: 5/55 single-edge perturbations and 1/55
  pattern-role removals changed the top-five set, on two frozen cases. This is
  retrieval sensitivity, not proof success.
- RAW-CTL corrected contrast: enriched 3/25 versus raw 4/26 over 51 inference
  edges. The denominator is explicit, but the mission already records that it
  is one edge wide and supplies no directional prior.
- The warrant audit's negative rate is also countable: 52/53 attachments marked
  independently witnessed lacked a witness record. It invalidates a success
  measure rather than supporting one.

No denominator was promoted from corpus size, problem count, cluster count, or
ledger snapshot by inference.

### Track C — the N-register (§1.4): already-done / partly-done / greenfield?

- **C1** Which measurement-vector quantities are **already emitted** by the
  loop, and which need new bookkeeping? Anything needing new bookkeeping will
  not survive contact with a campaign.
- **C2** What does the hunger audit currently record, has it ever run, and what
  would campaign scale cost?
- **C3** Is there a difficulty signal already in the corpus usable for
  stratification (`closer_hop`? sorries at formalization? problem family?).
- **C4** Has any transport derivation been attempted, and does the causal
  engine refuse it? Which held-out set — BPM or arXiv — has the better-formed
  selection diagram?
- **C5** What is the batch-2 regime boundary in commit terms, and how many
  problems fall each side of it?
- **C6** Is the vote ledger machine-readable as a *series*, or only as a
  snapshot? (14 concepts, three past threshold.)

### Track D — (2): can cycle one be countable?

- **D1** What would have to be fixed **in advance** for a first cycle to yield
  a countable result — the denominator, the arms, the stopping rule?
- **D2** Does `batch2r_pair.sh` plus the three preconditions already constitute
  a countable cycle, or is something missing?

### MAP Track D findings

**Surveyed 2026-08-14 by codex-4. MAP facts only; no assay run and no design
adopted.** Sources checked: the four batch/cohort preregistrations in
`holes/labs/M-zai-learning-loop/`, `futon6/README-apm-lean-ground-control.md`
§1, both `data/ct-commissioning/` problem files, `scripts/batch2r_pair.sh`,
the receipt/session code named below, and
`~/apm-evidence/case-1-corpus/stage2-synthesis.md`.

#### D1. What must be fixed in advance

The repository contains worked precedents for all four ingredients, but it
does not yet contain one preregistration that fixes them together for the next
CT/APM cycle. Therefore “exists” below means an existing mechanism or worked
precedent, not that cycle one has already been registered.

| ingredient | state | what exists, and what remains unfixed for cycle one |
|---|---|---|
| **Denominator** | **partly exists** | `cohort-1-prereg.md` fixes five fresh attempts by a deterministic first-five rule, separates the revisit row, and says no retries/every outcome counts. `bpm-batch-0-prereg.md` fixes ten ordered problems with the same rule. `batch-1-prereg.md` and `batch-2-prereg.md` each name ten paired problems. The next cycle has no frozen panel/attempt count yet; the two commissioning problem files are probes, not a declared attempt population. |
| **Arms** | **partly exists** | `batch-1-prereg.md` defines push-memory versus none; `batch-2-prereg.md` defines problem-IDF versus memory-DF under push; `batch2r_pair.sh:23-54` materializes the latter as `mem`/`ctl`. The CT commissioning files define positive and negative retrieval probes, not control/treatment execution arms for a learning-effect cycle. No next-cycle arm contract is frozen. |
| **Stopping rule** | **partly exists** | `bpm-batch-0-prereg.md` makes one session's terminal state the attempt outcome and forbids retries; cohort 1 adds the approximately 20-minute honest-partial pacing instruction; batch 1 closes/scored the batch after its fixed panel. Ground-control §1 defines terminal prover reports (Tier A, Tier B, defective statement). These are precedents, but no next-cycle rule currently fixes time/turn cap, terminal states, retry treatment, and cycle close in one place. |
| **Outcome** | **exists** | The strongest complete definition is `bpm-batch-0-prereg.md`: solved/partial/failed from compile exit, sorry/error counts and commit SHA, with ground-control recompilation. Batch 1 adds sorry delta, twin route divergence, memory citation corroborated by committed code, and tokens. `dispatch_with_recall.clj:1226-1234` requires one `USED`/`IGNORED` verdict per surfaced id; `memory_outcome_sweeper.py` writes the outcome half. The machinery exists, although the next cycle still has to select and freeze which existing outcomes are its endpoints. |

The distinction exposed by the preregs is that a named panel is not enough:
cohort 2 added `S1b` after a stub misdispatch and later found `S6` already
closed. Both were honestly labeled, but they show why the denominator, retry/
continuation treatment, and dispatch-time eligibility must be frozen and
checked rather than reconstructed at close.

#### D2. `batch2r_pair.sh` and the three preconditions

`batch2r_pair.sh` is a two-arm dispatcher, not by itself a countable cycle. It
does not choose a fixed multi-problem denominator, declare the cycle stopping
rule, check headroom, or enforce elicitation. Its fresh-session claim is also
stronger than its code.

| precondition | present status | checked fact |
|---|---|---|
| **Headroom** | **checkable, not mechanically checked** | `problem-ct-primary.md:7-8` supplies an explicit cold-control rule: if Run 0 solves competently, re-author rather than reinterpret. It also supplies ground truth. No code in `batch2r_pair.sh` runs or grades that assay, records a headroom verdict, or refuses dispatch when it fails. |
| **Arm independence** | **checkable, not mechanically checked** | The script uses separate seats (`ams-codex-2` and `ams-codex-1`) and separate frames, but lines 23-57 contain no session lookup, reset, comparison, or refusal. `frames.bb` deliberately records the actual runner session only at close (`:recorded-at-close`; lines 128-131, 171-182). Batch-2 amendment 3 required a supervisor Drawbridge reset per job; that action is outside this script. Amendment 7 further shows distinct sessions alone did not close shared worktree/git-ref read channels. Thus the comment “verified-fresh sessions” is not verified by the script. |
| **Elicitation verified** | **checkable, not enforced for this runner** | Contrary to a clean “no mechanism” premise, `src/futon3c/peripheral/pull_receipts.clj` records every successful or empty `memory_search`, `pattern_memory`, `library_search`, `evidence_graph`, or `psr_search` result with dispatch, session, tool and round. This can mechanically establish that a pull lookup occurred. Separately, the dispatch packet and runner gate require per-surfaced-id `USED`/`IGNORED` attribution, which establishes disposition of pushed material, not that the runner initiated retrieval. `batch2r_pair.sh` uses operator-side push and contains no requirement or gate for a pull receipt, so E9/E10-style elicitation is not checked in this path. |

The runner can therefore produce countable rows only if an external prereg and
operator procedure supply the missing cycle boundary and precondition verdicts.
The shell script itself does not constitute that procedure.

#### D3. Case 1 as the negative worked example

**Fixed in advance:** the stage-2 synthesis named one control-versus-treatment
pair on the b01A04 bridge, fixed the candidate deposits (`c1+c2`), used a known
ground-truth route, and named observable success as trajectory difference,
time-to-route, or an honest characterization of the wall. Its preceding mining
also fixed the evidence corpus: nine reads, 41 marks, and five staged
candidates.

**Discovered too late:** the control already found the full route, so the
treatment had no headroom and the deposits were not load-bearing at that
target. The treatment then explicitly revealed session contamination—“Route:
unchanged from the first assay”—because both arms used one seat/session. These
were not adverse outcomes inside a valid denominator; they invalidated the
comparison itself. The useful residual result, correct non-application of the
memories, is an existence/pilot observation rather than the preregistered
treatment-effect measurement.

Case 1 had fixed the proposed contrast and observable but had not assayed
headroom or enforced independent sessions before exposing the treatment. It
therefore discovered its two validity conditions from the outcomes instead of
recording them as passed preconditions.

#### Honest Track D verdict

**Cycle one is not countable with the currently assembled path.** Nearly all
raw components exist, so this is not greenfield instrumentation, but no current
artifact binds them into one pre-outcome contract. The shortest factual list
of what is missing is:

1. a frozen cycle-one panel/attempt count with exclusions, retries and
   continuations accounted for;
2. a frozen arm assignment and stopping rule for that panel;
3. a recorded headroom pass before treatment exposure;
4. a pre-dispatch independence check that covers actual session identity and
   the known cross-frame read channel; and
5. an elicitation requirement wired to the already-available pull receipt (or
   an explicit declaration that pushed-material disposition, rather than
   agent-initiated lookup, is the measured exposure).

This list names absent bindings/checks; choosing their exact form belongs to
DERIVE.
