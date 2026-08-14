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

1. **The odometer is miscalibrated.** ~~v1 says ~27% closed; `status.json` gives
   186/475 = 39.2%, and E5 showed that file *understates* closure. No
   percentage should be quoted until a comment-aware sorry detector exists.~~
   **RESOLVED 2026-08-14 (MAP A4/A4a).** A comment-aware detector now exists and
   has been written twice independently, by claude-2 and ams-codex-1, agreeing
   at **214/475 = 45.1%** over 448 source-bearing bundles. E5 was right that
   `status.json` understates: the gauge reads 186. The tension is discharged —
   but see A4a, because the first attempt at the replacement gauge was itself
   miscalibrated, in the mission's own signature way.
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

### MAP Track A findings

*Surveyed 2026-08-14. These are inventory facts, not a choice of experimental
unit or a proposed repair.*

#### A1. Run inventory

For this inventory, **one run means one problem offered to one agent in one
top-level attempt**. An internal phase redispatch or continuation inside that
attempt is not another run; two separately timestamped attempts at the same
problem are. A batch job containing several named problems contributes one run
per offered problem, because its per-problem outcomes are the quantities later
compared. Where an Agency job ID exists it is the deduplication key; older
ledgers have no job ID, so their dated lifecycle/dispatch records are the only
available identities.

| source | counted problem-runs | basis |
|---|---:|---|
| conductor v1 | 28 | `:problem-start`; the 1,776 `:phase-dispatch` records are internal phase calls, not 1,776 top-level attempts |
| conductor v2 | 118 | `:problem-start`; the 324 `:dispatch` records include kicks/retries inside those attempts |
| conductor v3 | 954 | 11 ordinary `:started`, 8 initial batch problems + 901 subsequent `:batch-dispatch`, and 7 initial cleanup problems + 27 subsequent `:cleanup-dispatch` |
| frozen 2026-07-31 job results | 130 | 130 files, 130 distinct Agency job IDs, each prompt names one APM problem |
| bridge pilot | 169 | 169 rows and distinct job IDs, covering 167 distinct problems |
| campaign ledger | **1,913 confirmed; at most 1,918** | 347 problem slots in 86 accepted batch jobs; 493 pass-1 jobs; 157 of 162 repair offers with a later gate; 916 accepted closer jobs |
| escalation queue | 0 additional | 55 construction-target summaries, no dispatch/job identity; these are derived queue records, not runs |

This yields **3,312 confirmed problem-runs and an upper bound of 3,317**. The
five-run interval is irreducible from these files: `repair-dispatch` was logged
before `agency.dispatch_fn`; five of 162 repair offers have neither a job ID nor
a later `repair-gate`, so the ledger cannot say whether invocation failed or a
run occurred without a gate. The 55 `closer-hop-annulled` rows are not added a
second time: each exactly annotates a preceding accepted closer hop whose job
failed quickly. If “run” is instead restricted to substantive non-annulled
attempts, subtract those 55; the corpus itself uses both notions (offer versus
attempt), so MAP does not collapse them.

Across the three sources that carry IDs, the 130 frozen, 169 bridge, and 579
campaign job IDs form a union of **878 with zero cross-source collisions**.
That proves deduplication there. No comparable cross-source identity exists for
the March/April conductor ledgers. Their date ranges do not overlap the
July/August ID-bearing sources, but their individual records cannot be joined
to Agency history by ID. The checked copies of the three conductor logs are in
the frozen evidence tree (`/home/joe/apm-evidence/code/futon3c/data/`); they are
absent from the current futon3c worktree. The v3 file also contains malformed
EDN (a bare timestamp), so its event inventory required event-level scanning
rather than whole-file EDN parsing.

#### A2. Outcome recoverability and the status snapshot

Outcome recovery is incomplete and source-dependent:

| source | top-level runs | terminal/outcome records recoverable in that source |
|---|---:|---:|
| v1 | 28 | 15: 11 complete (4 proved, 7 partial) + 4 abandoned; 13 lack a top-level terminal record |
| v2 | 118 | 115: 91 complete + 21 abandoned + 3 timed out; 3 lack a top-level terminal record |
| v3 | 954 | 949: 7 ordinary `done` + 908 batch returns + 34 cleanup returns; five lack the corresponding top-level return/done |
| frozen jobs | 130 | 130 terminal states: 126 done, 4 failed |
| bridge pilot | 169 | 0; this file records dispatch identity only |
| campaign | 1,913–1,918 | not one-to-one recoverable: 86 accepted initial jobs have only 76 `poll-done` rows and batch result tails; pass-1/repair gates are problem/artifact observations rather than job outcomes; closer rows omit job IDs |

The reported v2 numbers reproduce exactly: **91 completion events over 73
unique problems, 15 `proved` events and 76 `partial` events**. They do **not**
reconcile as the same measure as the current **186/475** top-level
`status.json` zero-sorry count:

- v2 covers 73 problems and attempt-time classifications; status covers 475
  current problem bundles and is a later mutable snapshot;
- repeated v2 attempts mean the latest classification over those 73 is only
  10 proved / 63 partial, rather than 15 / 76;
- 37 of those 73 currently report status sorry-count zero; across all 475 the
  count is 186, i.e. 171 more than the 15 v2 proved events, but over 402 more
  problems and months of subsequent work;
- only 5 of the 15 problems ever classified proved by v2 currently report
  status zero. This is direct evidence that the attempt label and regenerated
  bundle status are not a durable shared outcome key, not evidence of 10 proof
  regressions.

Therefore the corpus can count final/current bundle state and can count some
historical attempt outcomes, but **cannot recover an outcome for every run or
reconstruct a complete run-level success rate**.

#### A3. Observation-channel loss

The frozen audit reproduces **129 offered halves → 115 outcome halves → 114
joins → 20 rows with non-empty `used-ids`**. The loss is not primarily a join
failure:

- **Emission:** 15/129 offered rows have no outcome half. Twelve surfaced
  nothing and three surfaced IDs. Their later memory disposition was never
  recorded.
- **Join:** all 114 emitted outcomes belonging to an offer join successfully.
  There is one orphan outcome without an offered half, but it removes no row
  from the 129-offer denominator.
- **Retrieval eligibility:** 70 of the 114 joined rows surfaced no IDs, so they
  cannot carry a memory-use score. Across all offers, 82/129 surfaced nothing;
  46 record timeout and 12 store-unavailable, so much of “nothing” is an
  infrastructure observation rather than a negative retrieval judgment.
- **Scoring/report emission:** 44 joined rows did surface IDs. Of these, 18
  report used only, 2 report both used and unused, 5 report unused only, and
  **19 report neither used nor unused**. The audit's published
  `metric-bearing` definition counts only the 20 with non-empty `used-ids`; it
  excludes the five explicit unused-only negatives as well as the 19 with no
  disposition.

Thus the answer is **mixed, dominated after joining by eligibility/scoring,
not linkage**: 15 outcomes were never emitted; no additional offered outcome
was lost at join; 70 joined rows had nothing to score; and 19 memory-bearing
offers reached an outcome without emitting a use/non-use disposition. The
historical corpus can support existence claims and rates over the explicitly
observed subset. It **cannot ever yield a complete success/use rate over all
129 offers**, because the missing 15 outcomes and 19 missing dispositions are
not recoverable from the frozen receipts.

#### A4. Comment-aware closure

> ⚠ **RECONCILED 2026-08-14.** Track A first reported 403 source-bearing /
> 185 zero-sorry / **38.9%**. Three independent counts disagreed; all three are
> now accounted for exactly, and the figure is **214/475 = 45.1%**. The
> reconciliation is recorded below because *how* the numbers were wrong is the
> more useful result.

There are 475 top-level `status.json` files. **448 have `lean/Main.lean` on
disk**; **27 have no Lean source of any name**. A lexer that removes nested
`/- ... -/` comments, `--` comments, and strings before matching the `sorry`
token finds:

- among the 448 bundles with Lean source, raw token scanning says 204 have no
  `sorry`; comment-aware scanning says **214/448** have no code-level `sorry`;
  the ten corrected comment-only cases are `a01A06`, `a01J01`, `a93A04`,
  `a94A09`, `a94J02`, `a94J06`, `a95J06`, `a96A02`, `b00J04`, `m95A05`;
- on the full 475-problem denominator, the evidence-backed closure gauge is
  therefore **214/475 = 45.1%**, requiring a Lean file as well as zero
  code-level sorries;
- a purely vacuous “zero sorries” detector would call all 27 missing-source
  bundles zero and report **241/475 = 50.7%**. That is not closure evidence and
  is reported only to expose the denominator trap.

**What 214 is not** *(checked by claude-2, 2026-08-14)*. It is a **source-level**
count, not a compile certificate. Of the 214, **zero** contain a code-level
`admit`, `axiom`, or `native_decide` — so the figure is not being propped up by
a cheaper hole than `sorry`. But **nothing in this corpus was compiled**, so
214 must never be quoted as "214 verified proofs." The gap between
source-level and machine-checked closure is unmeasured and remains open.

**Why the predicate is `lean/Main.lean`** *(checked by claude-2)*. On Zone,
`lean/Main.lean` and any `lean/*.lean` both yield **448**. "Any `*.lean`
anywhere under the bundle" yields **450**, because `t94A04` and `t96J03` carry
`.lean` files under `candidates/*/lean/` — **abandoned attempt directories**.
A broader predicate would score rejected candidate scratch as solution source,
inflating closure. 448 is correct, and this is the one place where a *looser*
predicate would have been the wrong repair.

##### A4a. How three counts of one corpus differed — and why it reconciles

Three agents counted the same corpus and got 185, 203, and 214. Every step of
the spread is now identified, with nothing left over:

| count | who | predicate | value |
|---|---|---|---|
| 185/403 | ams-codex-1 | `status.json`'s `lean.files` / `lean.main` metadata | 38.9% |
| 203/448 | oxf-claude-3 (Dionysus) | disk, raw `\bsorry\b`, at `origin/master` | 42.7% |
| 204/448 | claude-2 (Zone) | disk, raw `\bsorry\b`, at `a92ffb6` | 42.9% |
| **214/448** | claude-2 + ams-codex-1 (corrected) | disk, comment-aware | **45.1%** |

Two independent bridges close the gap exactly:

1. **185 → 214 is a predicate defect, not a corpus fact.** codex-1's predicate
   read `status.json` metadata and never probed `problem_root/lean/Main.lean`
   directly. Exactly **45 bundles have `lean/Main.lean` on disk but no
   `lean.files` and no `lean.main` entry**, so they were silently excluded.
   Of those 45, 29 are zero-sorry and 16 carry a code-level `sorry`:
   403 + 45 = 448 and 185 + 29 = 214. codex-1 confirmed this itself and
   retracted 185: *"my predicate accidentally measured metadata completeness."*
   Running its own (stricter, depth-tracking) lexer over all 448 disk files
   also yields 214 — so **two independently written comment-strippers agree**.
2. **203 → 204 is one real byte-level divergence between hosts.** Zone is
   `origin/master + 3`; those three commits touch exactly one `Main.lean` —
   `problems/b01A04/lean/Main.lean`, which has **1 `sorry` at `origin/master`
   and 0 at Zone's HEAD** (the case-1 b01A04 cyclotomic closure). Dionysus,
   sitting clean at origin, cannot see it. 203 + 1 = 204, and 204 + 10
   comment-only cases = 214.

**Bundle inventory is identical across hosts** (475 / 448 / 27 on both), and
the 27 source-less bundles are source-less on *both* — so no Lean source is
missing from Zone. **Zone's** 8 "dirty" files are all **untracked** (`.bak`
scratch files and staging dirs); zero tracked files are modified and zero
`Main.lean` is dirty. Dionysus is clean at `origin/master` (`7f338dd0`) with
9 untracked files and 0 tracked-dirty. *(Corrected: an earlier draft of this
paragraph attributed Zone's 8 untracked files to Dionysus.)*

**Method lesson, entered as evidence for the guiding light.** The error that
produced 185 is the mission's own thesis in miniature: the source *was there*,
on disk, readable — and the count missed it because it asked the index rather
than the shelf. A file present but absent from the metadata that describes it
is exactly "technically-present, not available." That the miscount landed in a
gauge intended to *replace* a miscalibrated gauge is the sharpest available
argument that the guiding light needs to be a mechanical check, not a slogan.

**Staging lesson (oxf-claude-3).** Identical counts did **not** imply identical
content: all three of 475/448/27 matched across hosts while the corpora still
differed by a byte. Only a content digest caught it —

```bash
cd ~/code/apm-lean && ls -d problems/*/ | sort | while read d; do
  f="$d/lean/Main.lean"; [ -f "$f" ] && printf "%s  %s\n" \
    "$(sha256sum "$f" | cut -d' ' -f1)" "$(basename $d)"
done | sha256sum
# Zone a92ffb6 : b012d0bda61bca9e6a3db8a9d96a03e09ed627cdd9f4f778cd1dee9abda37f1e
# Dionysus     : d4fe1417333725f181bb7da77c42d76cfb57fd646a5bfc65b1281c04dc548249
```

Since Zone carries three unpushed commits, **the commit sha and the bytes
currently disagree about what "the corpus" is.** Joe's "we can now pin hashes
to experiment stages" therefore needs either a push before staging, or a
content digest as the pinned object. **Open decision for the operator.**

The existing 186/475 status gauge is neither of these. It counts 25
missing-source bundles as zero, six status-zero bundles now contain a real
code-level `sorry`, and many nonzero status counts are stale relative to live
source. Comment awareness corrects ten live-source false positives, but a
sorry detector alone does not establish compilation or axiom cleanliness;
those outcomes were not counted here because the requested inventory did not
provide build receipts for all 475 bundles.

#### What this corpus cannot count

- An exact all-program run count: five repair offers have no execution
  identity, and March/April runs lack Agency IDs.
- A terminal outcome for every run: the source-level gaps above are real
  missing records, not partial joins that can be repaired retrospectively.
- A complete historical memory-success rate: 15 outcomes and 19 surfaced-ID
  dispositions were never emitted.
- “Closed and verified” for all 475: 72 bundles have no Lean source to inspect,
  and zero textual sorries is not a compile or axiom certificate.

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

#### B-recheck. The "unrecoverable" verdicts, rechecked as a class

*(claude-2, 2026-08-14.)* Track C overturned B's first verdict — the vote
ledger **is** a timestamped series (`e-concept-vote-*` rows in the evidence
table; 27 ids / 11 supplements / 10 concepts in a 200-row window), not the
one-day snapshot B took it for. One of four wrong makes the class suspect, and
the shared wording is the tell: B repeatedly writes *"the surveyed records
contain no…"*. That is the same move `ams-codex-1` made in A4 — **asking the
index rather than the shelf.** So the remaining verdicts were rechecked
against disk and store rather than against V3's prose.

**A per-attempt register does exist, and B did not survey it.** 21 of 475
bundles carry `candidates/apm-v2-<problem>-<epoch-ms>/` directories — **55
timestamped attempt frames**, 31 Mar–1 Apr 2026, 51 of them with a
`proof-frame-receipt.v1` receipt naming frame id, workdir, artifacts and
graph-refs. B2's "no run-level record survived for these" is therefore **too
strong as stated.**

**But B2's conclusion survives, for a different reason than B gave.** The
register cannot yield a success rate, on three independent grounds:

1. **The receipts carry no outcome.** `state` is `{"readonly": [], "writable":
   []}` in all 51. There is no verdict, acceptance, or disposition field —
   provenance only.
2. **`proof/cycle-id` is `null` in all 51,** so attempts cannot be grouped
   into demand cycles even in principle.
3. **The root file is not the accepted candidate.** Every root `Main.lean`
   hashes differently from all of its candidates. `b94J01` is decisive: six
   candidates from 31 Mar averaging ~12 lines, versus a 154-line root from
   9 Aug carrying two `sorry`s. The root is *later independent work*, not a
   selected winner — so root-vs-candidate cannot be read as accept-vs-reject.

**A tempting number, and why it must not be quoted.** Treating candidates as
attempts gives 57/74 = 77.0% zero-sorry. It is an artifact: only **31 of 55**
candidates state a `theorem` or `lemma` at all, and of the 42 zero-sorry
candidates only **18** are non-vacuous. This is the A4 denominator trap
recurring one level down — clean because empty, not clean because proved. The
figure is recorded here **solely so that no later reader recomputes it and
believes it.**

**Revised verdicts:**

| # | B's verdict | after recheck |
|---|---|---|
| 1 | revolution rate unrecoverable | **overturned** (C) — series exists |
| 2 | rewrite hit rate unrecoverable | **stands** — no artifact defines "rewrites sought" |
| 3 | 17/45 within subset | **stands** — recovered, correctly scoped |
| 4 | chain-closure rate unrecoverable | **conclusion stands, reason corrected** — a register exists but emits no outcome |

**Why this matters more than the verdicts.** Two independent audits now find
the *same* failure shape. A3: 15 outcomes never emitted, **zero lost at
join**, 19 of 44 with no disposition. B-recheck: 51 receipts with full
provenance and **no outcome field at all**. The system has never had a linkage
problem — it records *where work happened* meticulously and *how it turned
out* not at all.

That converts Joe's question (2) from open to nearly answered: **cycle-one
instrumentation does not need a new evidence channel, it needs an outcome
field on the channel that already exists.** Entered as a MAP fact; the design
is DERIVE's.

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

### MAP Track C findings

Surveyed 2026-08-14. Verdicts below describe the artifacts that exist; they
do not adopt the v2 plan's proposed vector, loss, ordering, or held-out set.

| question / node | verdict | existing evidence | absent or incomplete evidence |
|---|---|---|---|
| **C1 / N7 — which proposed measurement-vector fields are emitted?** | **partly-done** | Formalizer/reviewer findings are append-only records: `statement-review-verdicts-20260806.tsv` has 117 verdict rows, and `statements-manifest.jsonl` has 2,730 events with statuses, hashes, reviewer notes and gates. Prover gates emit `outcome`, build result, executable-sorry count, boundary conformance/sites, theorem name and axiom probe; the canonical 12-chain ledger contains 31 gate events and 16 `closer-hop` events. `axiom-audit.jsonl` has 214 records. Five early chains emit `scribe` and `promotion-queued` events. | No artifact computes the six-row vector or `L(i)`. Reviewer **escape rate** is not a field (later prover-discovered defects can be reconstructed only by joining review and gate histories). Neither frozen theorem hashes nor `frozen_declarations` cover `def` bodies, so freeze **contract leaks** are not observable. The gate records hops and residual sorries, but not a uniform attempt/cost count across the later campaign. Scribe/hunger fields disappear from `campaign-ledger.jsonl` (zero `scribe` transitions), and there is no campaign-wide promoted→later-surfaced→used join. The 57 `receipt.json` files are proof-frame provenance receipts—all 57 have the same frame/inputs/state schema—not solver-outcome receipts. Thus N7 is not mechanically complete. |
| **C2 / N5 — hunger audit** | **partly-done** | `scribe.md` and `templates/scribe.md` are byte-identical and require collection of every non-degraded empty/noisy memory query, classification as later grounded or not, literal demand vocabulary on grounded memories, and an `open-hunger` memory otherwise. It **has run**: the canonical ledger has 5 `scribe` and 5 `promotion-queued` events over five distinct chains (`a96J02`, `a97J01`, `a97J02`, `a97J03`, `a97J07`); their reports explicitly enumerate hunger findings. | It did not run in the Aug 8–9 campaign: `campaign-ledger.jsonl` has zero scribe transitions, consistent with the scribe seat's repurposing. The only defensible scale statement from existing artifacts is one additional session-reading, drafting, store-write/read-back pass per completed chain (early implementation: 5 passes for 5 chains). No ledger records elapsed time or token/call cost for those passes, so campaign-scale person/time cost cannot be counted from this corpus. |
| **C3 / N8 — extant difficulty signal** | **partly-done** | Problem family is recoverable for all 475 bundles from the ID prefix (`a=151`, `b=76`, `m=102`, `t=146`). Current executable-sorry counts are present under `lean` for 468/475 status files. `closer_hop` is present for 220/475 (1:16, 2:14, 3:126, 4:64). Current statement length is reconstructible from each Lean source. | None is a complete pre-treatment difficulty measure. Current sorry count and statement length can be altered by work; status carries explicit initial `lean_sorry_count` for only 3/475, and no status history. `closer_hop` is missing for 255/475 and measures expended effort as well as difficulty. Family is complete but coarse. No existing artifact combines or validates these signals for stratification. |
| **C4 / N6 — transport derivation and held-out selection diagram** | **greenfield** | The causal engine is real and tested for backdoor/front-door identification and typed refusal; e.g. its R1 receipt refuses an uncontrolled four-closure causal reading with `:open-selection-backdoors`. `capability-proof-apm.tex` contains one explicit N6 selection diagram: `S → problem distribution → solved`, labelled APM versus held-out **BPM**. BPM also exists as a concrete ten-problem panel in the frame corpus. | No APM→held-out transport derivation or engine receipt was found, for BPM or arXiv, so the engine has not yet accepted or refused N6 itself. No arXiv selection diagram was found. On artifact formation alone, BPM is better formed: it has the only explicit selection diagram and a fixed problem panel; arXiv has substantial proof corpora/pipelines but no N6 selection-variable encoding. This is an inventory result, not adoption of BPM. |
| **C5 / N5 — batch-2 regime boundary** | **already-done** | The boundary is commit `6521fd3aa158da16a6d520a6c2eef19195ac54cd` (2026-08-11T16:11:26Z), which introduced the switchable memory-DF anchor. Before it, batch-1 contains 10 distinct problems / 20 arm frames. After it, batch-2 contains 10 distinct problems / 20 arm frames; batch-2r adds 4 arm frames rerunning 2 of those same post-boundary problems. `batch-2-report.md` says 24/24 batch-2 plus rerun frames closed and explicitly ends batch mode in favour of M-case-studies. | The reruns are not two additional problems; counting them as such would inflate the post-boundary denominator. `docs/retrieval-whitepaper-v3.md` still says “under live test”/“pending,” so that prose is stale relative to the prereg amendments and close-out report. |
| **C6 / N8 — vote ledger series or snapshot?** | **already-done** | The substrate session `vote-and-callback-pipeline` is an append-only **series**: a bounded read returned 65 timestamped records, including 39 `:concept-vote`-tagged events over 16 machine-readable `:concept` values. Concepts have distinct dated vote/supplement rows (for example Schwarz–Pick has three vote rows); threshold/build/callback and later adjudication records remain separate events. Therefore trend reconstruction is possible; this is not merely a current tally. | The whitepaper's “14 concepts, three past threshold” is a dated snapshot, not the current store census (now 16 concept values). Several of the 39 tagged rows are supplements, demand tags, artifact specs, or reclassifications rather than independent votes, so `39` must not be read as a vote denominator without applying distinct-problem and row-kind semantics. |

**Register resolution.** N5 is partly instrumented and its batch-2 test is
closed; N6's specific transport obligation remains greenfield; N7 is partly
emitted but has no complete mechanical vector; N8 has one witnessed revolution,
a real vote-event series, and only incomplete difficulty covariates.

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

---

## MAP Track E — ConstructionTargets as a known-failing retrieval test

*(Joe's proposal, 2026-08-14, executed by claude-2 the same turn. Joe:
"the ConstructionTargets should give a kind of 'mockup' of a replicable
finding — for instance Rouché was one of the first ones, and this **was**
reused, even if it wasn't reused through the memory system itself. This
'positive' result (for the mathematics) is still a 'negative' finding for the
system as a whole, but it is a negative finding that could be the basis of a
known-failing test.")*

### E1. The reuse is real, measured, and far larger than recorded

`ConstructionTargets/` holds lemmas built to unblock specific APM problems.
Re-derived from disk (not read off the in-repo table):

| | in-repo table (2026-07-30) | re-derived 2026-08-14 |
|---|---|---|
| modules | 15 | **18** |
| consumer edges | 23 | **64** |
| `Rouche` consumers | 2 | **9** |

An `import` is not reuse, so edges were split by whether the consumer actually
references a declaration from the module: **39 of 64 = 60.9% are
declaration-using**, the other 25 import-only. `Rouche`: 9 imports, **7
load-bearing**.

⚠ **The in-repo table is stale by roughly 3×** and should be regenerated, not
patched. Note the file already warns about exactly this failure — a previous
version named `a94A10` as a `Rouche` consumer, "which was never true and cost
a day of misrouted dispatches." The table is derived-by-grep by policy; the
derivation simply has not been re-run since 2026-07-30.

*(Method note: the first count returned 19/64 because the detector excluded
dotted references — `(?<![\w.])name` — which silently drops every **qualified**
use, the normal way these lemmas are called. Same class as the anchored-grep
trap. Corrected before use.)*

### E2. The negative system finding, demonstrated rather than assumed

Joe's claim — reused, but not *through the memory system* — was tested against
the live store (`localhost:7073`), not assumed:

| query | hits | surfaces `Rouche` / `ConstructionTargets`? |
|---|---|---|
| `text=Rouche` | 5 | **yes** |
| `text=ConstructionTargets` | 5 | **yes** |
| `text=YoungConvolution` | 5 | **yes** |
| `text=argument principle` | 8 | **no** |
| `text=counting zeros` | 8 | **no** |
| `text=winding number` | 8 | **no** |

**The store answers to the artifact's own name and not to the need it
serves.** A consumer such as `a97A08` — which genuinely used `Rouche`, and
whose source states the need as *"f has no zeros on |z| = 2 (so the count on
the open disk is well-defined)"* — could only have found it by already knowing
its name. This is the reversed guiding light failing mechanically and
reproducibly: **technically present, not available.**

### E3. Why this is the instrument the mission has been missing

The four questions asked for evidence with denominators. This supplies one:

- **Ground truth is independent of the memory system.** The reuse is provable
  from the repo — the import edge and the declaration reference are on disk.
  Nothing has to be taken on narrative.
- **n = 39, not n = 1.** Each load-bearing edge is one test case: *at the
  moment consumer C needed module M, would retrieval have surfaced M given
  C's own vocabulary?*
- **It is expected to fail now.** That makes it a **known-failing test** whose
  passing is a certificate, exactly the shape the warrant machinery requires
  (*upgrade by certificate, never by narrative*).
- **It runs against today's store, with no new evidence channel** — it is
  evidence from cycle one, which is Joe's question (2).
- **It targets N5 directly** (retrieval serves the need), the weakest node in
  v1's warrant table.

The obvious confound must be pre-declared: many of the 39 edges post-date the
module's creation and some consumers were told about the module by dispatch.
**Whether a given edge was memory-mediated, dispatch-mediated, or
author-recalled is not currently recorded** — the same provenance-without-
outcome gap the B-recheck found. Stratifying the 39 accordingly is DERIVE's
first job, not MAP's.

### E4. The gating question, answered

Joe, on the 24 vacuous candidates: *"all of that kind of stuff is supposed to
be gated and checked but clearly wasn't."*

**The gate held.** Of the 448 promoted `problems/*/lean/Main.lean`, **zero**
lack a `theorem`/`lemma` and **zero** are under 200 bytes. Every vacuous
artifact sits in `candidates/` — unpromoted attempt frames from 31 Mar–1 Apr
2026. Nothing vacuous reached the corpus.

What is missing is not a gate but a **score**: the frames were never
adjudicated, so a later reader who treats `candidates/` as an attempt series
computes a rate (57/74 = 77.0%) that is mostly measuring stub-ness. The defect
is the same one found twice already — **provenance recorded, outcome not.**

### E5. The waste is real, measurable, and cheaply preventable

Joe, 2026-08-14: *"adjudication should be part of our one-problem one-shot
solver then for sure; but creating vacuous candidates is still a waste even if
it's gated!"*

Correct, and the waste has a sharper shape than "low-quality output". All 24
vacuous frames are **the untouched scaffold**, byte-identical across problems:

```
lean/Main.lean     399 bytes   imports, namespace, boilerplate comment, `end`
lean/Scratch.lean  283 bytes   ditto
```

Zero of the 24 have a `Scratch.lean` over 400 bytes, and zero state a
`theorem` or `lemma` anywhere in the frame. **The work did not land in the
wrong file — no work was done at all.** The frame was created, the scaffold
written, and nothing else happened.

**43.6% of all attempt frames (24/55) contain no work.** The nominal 55
attempts are effectively **31**.

The empties concentrate exactly where retries happened:

| problem | empty / frames |
|---|---|
| `b94J01` | 4 / 6 |
| `t97J01` | 4 / 6 |
| `a96J01` | 3 / 6 |
| `a92J02`, `a93A01`, `a93J02` | 2 / 2 — **every frame empty** |

So the high-retry problems were not tried six times; they were **re-framed**
six times and worked on twice. Three problems received a frame apiece and no
work whatsoever. Any "attempts per problem" figure taken from this directory
overstates effort by ~1.8×.

**Two consequences, both DERIVE inputs, recorded here as operator constraints:**

1. **Adjudication belongs inside the one-problem one-shot solver** (Joe's
   requirement). A frame that ends is a frame that must carry a verdict.
2. **A no-op frame should never be emitted.** The check is as cheap as a
   check can be — refuse to close a frame whose `Main.lean` is byte-identical
   to the scaffold. This costs one hash comparison and would have suppressed
   24 of 55 frames. It is the same insight as the mission's headroom
   precondition, applied at the frame level rather than the round level: a
   round that cannot measure anything should not be counted as a round.

**Method-honesty note.** This also means the earlier attempt-level figure was
wrong twice over, not once: 57/74 = 77.0% was inflated both by counting
vacuous frames as clean *and* by counting no-op frames as attempts. Neither
error would have been visible without opening the files. Recorded because the
mission's central risk is a number that survives on its plausibility.

### E6. Invariant F1 — a created frame is a worked frame

**Operator ruling, Joe, 2026-08-14.** *"The b94J01 problem sounds more like a
mechanical plant failure than a mathematical aberration. I don't think we need
a historical deep dive on the 24 frames, just an invariant that says that if a
frame is created it is worked on. That will come out directly from a one-shot
system (which we can later just map over the list of problems)."*

This **reclassifies E5 out of the evidence question.** The 24 empty frames are
a plant failure, not a signal about mathematical capability, and they are not
evidence about the corpus. They therefore have no bearing on the closure gauge
(A4), the reuse count (E1), or the four questions.

> **F1 (frame liveness).** If a frame is created, it is worked on. A frame
> whose `Main.lean` is byte-identical to the scaffold at close is a plant
> failure, not an attempt: it must not be emitted, and must never be counted
> in any denominator.

**Status: CLOSED as a historical question, OPEN as a design requirement.** No
further archaeology on the 24 — the mechanism by which they were created is
explicitly *not* being investigated. F1 is not retrofitted to the March/April
batch; it is a property the **one-problem one-shot solver** must have by
construction, which then holds automatically when that solver is mapped over
the problem list.

That is a strictly stronger position than an enforcement check bolted onto the
existing batch path: an invariant that falls out of the design cannot drift,
whereas a check added afterwards is one more thing that can silently stop
running — the failure mode this mission has now catalogued three times.

**Consequences for what MAP may still assert.** The candidate register is a
source of **31 worked frames**, not 55 attempts. It remains unusable as a
success rate for the reasons in the B-recheck (no outcome field, null
`cycle-id`, root ≠ accepted candidate). Nothing further is owed here.

---

## MAP consolidation — the register is resolved

*(claude-2, 2026-08-14, at Joe's instruction to consolidate C and D.)*

### The N-register (§1.4), resolved

§1.5's first completion criterion — *every N-register row resolved to
already-done / partly-done / greenfield, with evidence* — is **met**. This is
MAP's exit condition and the mission's first real deliverable.

| node | verdict | resting on | the sharpest fact |
|---|---|---|---|
| **N5** retrieval serves the need | **partly-done**, and now *demonstrably failing* | C2, C5, **E2** | The batch-2 boundary is closed and named by hash (`6521fd3a`). The hunger audit **has** run — 5 chains — but not in the Aug 8–9 campaign. E2 then showed the live store answers to an artifact's *name* and not to the *need*: `text=Rouche` hits, `argument principle` / `counting zeros` / `winding number` do not. |
| **N6** transports to held-out | **greenfield** | C4 | No transport derivation has ever been attempted. The causal engine is real and can issue typed refusals; **it has simply never been asked about N6.** BPM is better-formed than arXiv on artifact grounds — the only explicit selection diagram plus a fixed ten-problem panel. |
| **N7** outcomes scoreable | **partly-done** | C1 | Rich append-only records exist (117 verdict rows; 2,730 manifest events; 31 gate + 16 closer-hop events; 214 axiom-audit records). **No artifact computes the vector or `L(i)`**, escape rate is not a field, and freeze contract leaks are unobservable because no hash covers `def` bodies. |
| **N8** learns at ability level | **partly-done** | C3, C6 | The vote ledger **is** a machine-readable series (65 timestamped records, 39 concept-vote-tagged, 16 concept values) — so B's snapshot reading was wrong and trend reconstruction is possible. But no complete *pre-treatment* difficulty covariate exists: `closer_hop` is missing for 255/475 and confounds effort with difficulty. |

**The v2 plan's ordering is not adopted.** "N7 is the keystone" was
speculative; MAP shows N7 is the *best-instrumented* of the four, while N6 is
the only true greenfield. Ordering is DERIVE's, on these facts.

### Two systemic failures, each found independently by three tracks

MAP's most durable result is not any single number but a pair of recurring
mechanisms. Neither was looked for; both fell out.

**1. Provenance is recorded, outcomes are not.**

| track | finding |
|---|---|
| A3 | 15 outcomes never emitted; **0 lost at join**; 19 of 44 offers with no disposition |
| C1 | the 57 `receipt.json` files are *proof-frame provenance* receipts — "not solver-outcome receipts" |
| B-recheck | 51 candidate receipts, full provenance, `state` empty, `cycle-id` null in all |

Three independent surveys, one conclusion: **the system has never had a linkage
problem.** It records *where work happened* meticulously and *how it turned
out* not at all. This is why the past is only partly countable, and it is the
single highest-value target for cycle-one instrumentation — an outcome field on
an existing channel, not a new channel.

**2. Documents assert what the code and data do not.**

| track | drifted artifact |
|---|---|
| E1 | the `ConstructionTargets` consumer table — stale ~3× (15 modules/23 edges recorded; 18/64 actual) |
| C5 | V3 §"under live test / pending" — stale relative to the close-out report |
| D2 | `batch2r_pair.sh`'s "verified-fresh sessions" — a comment with no implementing code |
| A4 | `status.json` as a source of truth about its own bundle's Lean files — wrong for 45 |

Every one is the reversed guiding light: **an index that disagrees with the
shelf.** F1 (E6) is the mission's first structural answer — prefer invariants
that hold by construction over checks that can silently stop running.

### What MAP establishes against the four questions

| question | answer |
|---|---|
| **(1a)** how much evidence, full stop? | **3,312 confirmed problem-runs** (A1) — the denominator that makes the other questions well-formed. |
| **(1)** how much is evidence of *success*? | Source-level closure **214/475 = 45.1%** (A4, not a compile certificate). **39 load-bearing reuse edges** (E1). E9/E10's **0 vs 21** in a fixed design. One witnessed demand→closure revolution. **No defensible success-per-attempt rate** (B2, B-recheck). |
| **(2)** can cycle one be countable? | **Not with the currently assembled path** (D). Not greenfield either — the components exist; five bindings are missing, and none is a build. |
| **(2b)** will it improve per iteration? | **Not yet answerable.** N8 is checkable in principle and the vote series supports trend reconstruction, but stratification has no complete pre-treatment covariate (C3). |

### What MAP does not settle, and is not asked to

The measurement vector, `L(i)`'s form, the node ordering, the held-out set,
and whether the three preconditions become formal pass/fail conditions. All
DERIVE. MAP's job was facts, and the facts are now in.

### The one MAP question still open

C4 established that the causal engine exists, issues typed refusals, and has
**never been asked about N6** — the mission's only greenfield node. Whether the
engine accepts or refuses the N6 selection diagram is a *fact about the engine*,
answerable now, and it discharges §1.5 criterion 5 either way. **Belled out
2026-08-14.** Everything else in the register rests on completed survey.

---

## MAP addendum — N6: not discharged, not refused, **unaskable**

*(ams-codex-2, job `invoke-1786719652409`, receipt commit `5de70dbd`; reviewed
and independently verified by claude-2, 2026-08-14.)*

The packet asked for an engine verdict on N6 and pre-declared that *a typed
refusal is a fully successful outcome*. The actual result is **a third
outcome neither the packet nor the wake-checklist anticipated**, and it is
recorded as such rather than forced into the binary.

**Verdict: the engine was not invoked, because N6 cannot be represented by its
callable contract.** There is no engine identification and no typed refusal
keyword.

### Review (what claude-2 actually checked)

| check | result |
|---|---|
| receipt exists at claimed path | yes, 1,575 bytes |
| sha256 matches claim | **yes** — `d54ef283…48b9b66` |
| commit `5de70dbd` exists, scope | yes, 1 file / 33 insertions, no code touched |
| encoded diagram matches the TeX | **yes** — `capability-proof-apm.tex:261`, caption quoted accurately |
| engine contract as described | **yes** — `identify.clj:98`, `[causal-dag treatment outcome]` |
| transport support anywhere in `causal/` | **none** — zero hits for transport, selection-diagram, S-node, source/target-domain |

The receipt is honest in the way that matters: `:invoked? false`,
`:engine-verdict nil`, `:typed-engine-refusal nil`, and an explicit
`:non-equivalent-invocation-rejected` entry recording the tempting wrong move
(running `pipeline → solved`, which asks a single-domain question and not a
transport one). **codex-2 stopped rather than inventing the missing structure**,
which is what the packet required.

### What this changes

C4 said the engine *"has never been asked"* about N6. That was too generous:
**it cannot be asked.** `identify` implements single-domain identification —
backdoor → front-door → general ID → typed refusal `:not-identifiable` with
`:proof-status :proved-impossible`. That refusal machinery is real, but it
answers *"is P(Y|do(X)) identifiable in this one DAG?"*. Transport needs
selection diagrams with S-nodes plus source/target data availability — a
different algorithm.

Precisely missing, per the receipt: transport-query entrypoint; source and
target domain inputs; selection-node semantics; source-versus-target
observational availability; cross-domain mechanism-invariance declarations.

**N6's register verdict is unchanged — greenfield — but its reason is now
sharper and its cost is now known.** It is not blocked on running a tool; it
is blocked on a capability the tool does not have.

### Status of §1.5 criterion 5

**NOT discharged.** The criterion reads *"N6 discharged or refused, with the
refusal recorded if so."* Neither occurred: there is no derivation and no
engine refusal. Recording it as "refused" would be exactly the laundering this
mission exists to prevent — **an audit note is not an engine receipt**, and the
receipt says so itself in its own `:note`.

Criterion 5 remains open. It is a *mission* completion criterion, not MAP's
exit condition, so it does not block MAP's close.

### MAP is closed

§1.5 criterion 1 — every N-register row resolved with evidence — is met:
N5 partly-done (demonstrably failing), N6 greenfield (unaskable), N7
partly-done, N8 partly-done. Every row now rests on completed survey.

**DERIVE inherits a sharper first question than expected.** Not "which node
first?" but: *is a transport query worth building an engine capability for, or
should N6 be restated to something the existing engine can adjudicate?* That is
a design decision with a real cost attached, which is what MAP is supposed to
hand over.

---

## MAP addendum 2 — is "transport" a terminology collision?

*(Joe, 2026-08-14: "v1 states it as a transport claim — that leads to a
question in light of the recent Codex finding, could we validate it if it was
such? Does this transport claim align with a transport in the causal prover,
or is that just a terminology collision.")*

Three distinct senses of "transport" are live in this stack. Separating them
answers the question and turns up a defect in the capability proof itself.

**1. v1's usage is correct Pearl, not a collision.** `capability-proof-apm.tex`
uses "transport" in the exact Bareinboim–Pearl sense — *"'solves APM ⇒ capable
on held-out BPM' is formally a transport claim, not an induction"* (line 278),
with `S` switching the problem distribution across domains and identification
requiring the `S`-dependence to separate from the pipeline edge (line 261).
That is transportability as the literature defines it. **N6 is a genuine
transport claim, correctly stated.**

**2. But v1 asserts an engine capability that does not exist.** Line 275
describes the engine as *"Pearl-style: backdoor and front-door adjustment,
general identification, **transportability via selection diagrams**"*. The
first three are real — `identify.clj`, `idalg.clj`. **The fourth is not
implemented.** This is a false capability claim in the document that exists to
certify capability claims, and it is the **fifth** instance of the mission's
documentation-drift pattern (E1, C5, D2, A4, and now the capability proof
itself).

**3. The real collision is inside the engine's own vocabulary, and it plausibly
caused the error.** `causal/receipts.clj` contains `r1-selection-variant` and
the typed refusal `:open-selection-backdoors` — the very refusal Track C4 cited
as evidence of the engine's rigour. But that "selection" is **selection bias
within a single domain**, a backdoor problem (`:selection-regime` edges,
adjusted by `dsep/backdoor-adjustment?`). It is *not* a **selection diagram**,
which indexes differences *between* domains by an S-node.

> **Same word, two concepts.** Selection *bias* (one domain, confounded
> sampling) vs. selection *diagram* (two domains, transportability). The engine
> has the first and not the second. Anyone reading "selection" in the engine and
> "selection diagram" in the paper would conclude — wrongly — that transport was
> supported.

### Could we validate N6 if it is a genuine transport claim? Yes.

The substrate is closer than "greenfield" suggested. Transportability (`sID` /
`sTR`, Bareinboim–Pearl) is built from components the engine already has:

| needed by sID | present |
|---|---|
| latent projection to ADMG | `admg.clj` — `latent-project` |
| general identification | `idalg.clj` — `identify-effect`, `formula` |
| d-separation | `dsep.clj` |
| graph surgery | `surgery.clj` |
| typed refusal + receipts | `identify.clj`, `receipts.clj` |

What is absent is the **transport layer of the algorithm**, not its
foundations: S-node semantics, source/target domain inputs, per-domain
observational availability, and mechanism-invariance declarations (the receipt's
own `:missing` list). So N6 is **validatable in principle by a bounded
extension** — an added entrypoint over existing machinery, not a rewrite.

**Recorded as a MAP fact; whether to build it is DERIVE's call.** The honest
framing for that decision: N6 is not blocked on evidence, it is blocked on an
unimplemented algorithm that the capability proof already claims to have.

### Correction owed to v1

`capability-proof-apm.tex:275` must drop "transportability via selection
diagrams" from the engine description, or the engine must acquire it. Until one
of those happens the paper overstates its own instrument — which is precisely
the failure mode the paper was written to prevent.

*(Note: a **third**, unrelated sense of "transport" exists in futon3c — the
message transport layer of I-2, "Transport Routes, It Does Not Create". No
overlap with either causal sense; flagged only so the three are never conflated
in future prose.)*

---

## Carried into DERIVE — component **T**: transport identification

*(Joe, 2026-08-14: "this creates an interesting dependency to validate some of
the features — we don't need to decide implementation details or ownership now
but I think we should retain the dependency (e.g. making it a modular component
within DERIVE)".)*

Recorded as a **named modular component with explicit edges**, deliberately
without design or owner. The point of naming it now is that an unnamed
dependency is one that disappears between phases; this mission has catalogued
five artifacts that drifted precisely because nothing held them accountable.

> **T — transport identification.** The capability to adjudicate a
> Bareinboim–Pearl transport query: selection diagram with S-node, source and
> target domains, per-domain observational availability, mechanism-invariance
> declarations. Absent from the engine as of `d4867bd3`.

### What T gates, and what it does not

**Modular is the operative word.** T is on nobody's critical path. Naming its
edges is what keeps that true rather than assumed.

| depends on T | how |
|---|---|
| **N6** (capability transports to held-out) | entirely — N6 is a transport claim and cannot be adjudicated without T |
| **§1.5 criterion 5** ("N6 discharged or refused") | entirely — currently neither, because the query is unaskable |
| **warrant class of §1.5 criterion 6** (held-out demonstration) | the demonstration can be **run** without T; what T changes is whether its result is *identified by design* or an *extrapolation* |
| **capability-proof v2, line 275** | the fork is delete-the-phrase vs implement-T |

| does **not** depend on T |
|---|
| running the held-out BPM/arXiv demonstration at all |
| N5 (retrieval), N7 (scoreability), N8 (learning slope) |
| cycle-one instrumentation — the outcome-field work, which remains the highest-value target |
| F1 and the one-shot solver |

### The substitution, stated now so it is a choice later

If T is never built, N6 does not become false — it becomes **inductive rather
than designed**. v1 already says the right thing: transport is *"a derivation
obligation, not an extrapolation."* Declining T means accepting the
extrapolation and **saying so in the warrant**, which is a legitimate outcome
under this mission's rules and must not be reached by silence.

So T has exactly three admissible dispositions, all acceptable, none default:

1. **build** — bounded extension over `admg/latent-project`,
   `idalg/identify-effect`, `dsep`, `surgery`;
2. **restate** N6 into a claim the existing single-domain engine can adjudicate;
3. **refuse** — record N6 as extrapolative, downgrade the warrant, and strike
   "transportability via selection diagrams" from the capability proof.

**Not decided here. Not owned here.** DERIVE picks one and records why.

### Why this is worth carrying rather than closing

T is the only place in the register where the *instrument*, not the evidence,
is the binding constraint. Every other open item is answered by measuring
something that exists. That makes T structurally different, and it is the
reason it earns a component of its own rather than a line in a node's notes.

---

# DERIVE

*Opened 2026-08-14 on Joe's instruction ("for DERIVE let's follow the standard
checklist"). Follows `futon4/holes/mission-lifecycle.md` §3. Design work is the
Claude owner's per the handoff protocol carve-out (d).*

**Exit criterion (lifecycle):** someone could implement this from the DERIVE
section alone. Where that is not yet true, it is marked **[OPEN]** rather than
papered over.

**The design is driven by one MAP fact above all others.** Three independent
tracks found that the system records *where work happened* and not *how it
turned out*. So this is **not** an evidence-collection build. It is the
addition of a **disposition** to channels that already carry provenance, plus
the joins that make dispositions computable. Most of what follows is smaller
than it looks for that reason.

## D.1 Entity types

Identity pattern throughout: **qualified, human-readable, deterministic** —
per the identity convention adopted 2026-08-14 (`pattern/library` census). No
fresh UUIDs where a natural key exists; that decision already paid for itself
in the A3 join.

| entity | identity | source | notes |
|---|---|---|---|
| **Problem** | `apm/<bundle-id>` e.g. `apm/a97A08` | ingested | 475 in corpus; attributes frozen at panel time |
| **Cycle** | `cycle/<bundle-id>/<epoch-ms>` | authored | **the unit of measurement**; one problem, one shot |
| **Frame** | `frame/<cycle-id>` | derived | workspace; subject to **F1** |
| **Disposition** | `disp/<cycle-id>` | authored | **THE new entity.** Exactly one per closed cycle |
| **RoleEvent** | `ev/<cycle-id>/<role>/<seq>` | emitted | formalizer, reviewer, freeze, prover, scribe |
| **MemoryOffer** | `offer/<cycle-id>/<memory-id>` | emitted | what retrieval surfaced |
| **MemoryUse** | `use/<offer-id>` | authored | consulted? load-bearing? — the A3 gap |
| **RetrievalProbe** | `probe/<consumer>/<module>` | authored | the 39 known-failing cases |
| **ReuseEdge** | `reuse/<consumer>/<module>` | derived | 64 import / 39 declaration-using |
| **Stratum** | `stratum/<scheme>/<level>` | derived | frozen pre-assignment |
| **Regime** | `regime/<commit-sha>` | derived | retrieval regime, named by hash |
| **Measurement** | `meas/<cycle-id>` | derived | the per-cycle vector |

## D.2 Relation types

Binary where the connection is binary:

`cycle --of--> problem` · `frame --realises--> cycle` · `disp --closes--> cycle`
· `ev --within--> cycle` · `offer --surfaced-in--> cycle`
· `use --disposes--> offer` · `reuse --consumer--> problem`
· `reuse --module--> construction-target` · `cycle --under--> regime`
· `problem --assigned--> stratum` · `meas --scores--> cycle`

**One genuine hyperedge.** Adjudication is irreducibly n-ary:

```
adjudication(cycle, offer, outcome, verdict, adjudicator, at)
```

**IF** we modelled this as binary `offer --verdict--> value`, **HOWEVER** a
verdict is only meaningful relative to the outcome it is being credited
against *and* the adjudicator who made the call, **THEN** it is a hyperedge,
**BECAUSE** the A3 audit failed precisely by losing the (offer, outcome) pair —
19 of 44 memory-bearing offers reached an outcome with no disposition, and a
binary model cannot even express that as a missing row.

## D.3 Invariant rules

Checkable propositions. **F1 is already ruled (E6);** the rest follow its shape
— prefer invariants that hold by construction over checks that can stop running.

| id | invariant | check |
|---|---|---|
| **F1** | a created frame is a worked frame | `Main.lean` hash ≠ scaffold hash at close |
| **F2** | every closed cycle has exactly one disposition | `count(disp where closes=cycle) == 1` |
| **F3** | every memory offer carries a use-disposition | `count(offer without use) == 0` |
| **F4** | difficulty stratum frozen before arm assignment | `stratum.at < assignment.at` |
| **F5** | no measurement spans a regime boundary unstratified | `distinct(cycle.regime) == 1` per comparison |
| **F6** | no denominator promoted from corpus/cluster size | denominators declared, not inferred |
| **F7** | **availability**: an artifact counts as available only if a need-vocabulary probe retrieves it | `probe(need) ∋ artifact` |

**F3 and F7 are the two that would have changed history.** F3 is the A3 defect
stated as an invariant; F7 is the reversed guiding light made mechanical — the
first time it becomes a check rather than a principle.

## D.4 Data flow

```
   apm-lean problems/*/          futon1b (7073)
        │  ingest                     ▲
        ▼                             │ evidence rows
   [ one-shot solver ] ──cycle──▶ [ disposition writer ]
        │  F1 by construction         │
        ├── RoleEvents ───────────────┤
        ├── MemoryOffers ─────────────┤
        └── close ──▶ Disposition ────┘
                                      │
                            [ measurement join ] ──▶ meas/<cycle>
                                      │
                       ┌──────────────┼──────────────┐
                       ▼              ▼              ▼
                 L(i) series   retrieval board   warrant table
                 (N8)          (N5, 39 probes)   (capability proof)
```

Producers: the solver (cycles, frames, role events, offers); the adjudicator
(dispositions, uses); the derivation job (reuse edges, strata, measurements).
Storage: futon1b evidence rows, qualified ids. Query: `memory/search` with
`text=` (**note the parameter is `text`, not `q`; unknown params are silently
ignored — a live trap, see D.7**).

## D.5 The measurement vector and L(i) — grounded, not proposed

MAP forbade adopting the v2 plan's speculative vector. This one is built from
**what C1 found is actually emitted**, with each field marked by cost.

| field | role | status | source |
|---|---|---|---|
| statement defects at review | formalizer | **free** | `statement-review-verdicts` (117 rows) |
| outcome ∈ {closed, TierA, TierB, defective} | prover | **free** | gate events |
| residual executable sorries | prover | **free** | gate emits |
| attempts / closer hops | prover | **free** | 16 closer-hop events |
| axiom cleanliness | prover | **free** | `axiom-audit.jsonl` (214) |
| memories promoted | scribe | **free** | `promotion-queued` |
| **escape rate** (defects review missed) | reviewer | **join** | reconstruct from review × gate histories |
| **promoted → later surfaced → used** | scribe | **join** | needs F3 dispositions |
| **contract leaks** (post-freeze `def` changes) | freeze | **NEW** | no hash covers `def` bodies today |

```
L(i) = w₁·attempts(i) + w₂·residual(i) + w₃·rework(i)
```

**IF** we want `L` computable from cycle one, **HOWEVER** `rework` needs
def-body hashing that does not exist, **THEN** ship `L` with `w₃ = 0` and the
rework term declared-but-unpopulated, **BECAUSE** two of three terms are free
today and a loss function that waits for its third term is a loss function that
never ships — while silently dropping the term would misrepresent `L` as
complete. `w₃` becomes non-zero the day def-body hashing lands, and **that is a
regime change requiring F5 stratification.**

**Weights are [OPEN]** — an operator decision. They are not derivable from the
corpus and must not be fitted to it.

## D.6 Difficulty stratification (N8's confound)

C3: no complete pre-treatment covariate exists. `closer_hop` is missing for
255/475 and confounds effort with difficulty; sorry counts and statement length
are altered by work; family is complete but coarse.

**IF** stratification needs a pre-treatment difficulty measure, **HOWEVER** no
complete one exists and the two most informative candidates are contaminated by
effort, **THEN** stratify on **family (complete, coarse, uncontaminated) ×
initial statement length at formalization-freeze (complete going forward,
pre-treatment by construction)**, and record `closer_hop` as a covariate
without stratifying on it, **BECAUSE** a coarse-but-clean stratifier supports a
defensible comparison whereas a fine-but-contaminated one produces a falling
`L` indistinguishable from an easier tail — the exact anti-glibness failure
HEAD names.

Retrospective stratification of the existing 475 is **[OPEN]** and may be
refused; going forward it is free, since freeze-time length is captured at
freeze.

## D.7 N5 — the retrieval instrument

The n=39 known-failing suite (Track E). Each probe: *given consumer C's need
vocabulary, does retrieval surface module M?*

- **Ground truth**: independent of the memory system — the import edge and
  declaration reference are on disk.
- **Need vocabulary**: extracted from the consumer's own `problem.md` /
  `proof-outline.md`, **not** from the module. Extraction rule is **[OPEN]**;
  it must be frozen before scoring, or the test is unfalsifiable.
- **Pass bar**: **[OPEN]** — operator decision. Current baseline is a
  demonstrated **0** on the three probes run by hand.
- **Confound to pre-declare**: mediation class (memory / dispatch / author /
  unknown) is unrecorded for all 39. Stratify or declare unknown.

**Also fix the silent-parameter trap**: `memory/search` ignores unrecognised
query params, so a typo returns plausible unfiltered results. That is F7's
failure mode inside the instrument meant to test F7. Reject unknown params.

## D.8 Views

| view | shows |
|---|---|
| **cycle inspector** | one cycle: frame, role events, offers, disposition, measurement |
| **retrieval scoreboard** | 39 probes, pass/fail, by regime hash |
| **L trend** | `L(i)` over problems seen, banded by regime, stratified by family |
| **warrant table** | N1–N9 ranked by load-bearing status, certificates linked |

## D.9 Wiring diagram

**Applicable and recommended.** This mission defines a loop (solver → outcome →
measurement → retrieval) crossing three repos (futon3c, futon6, apm-lean) and
two timescales. Per the lifecycle, sketching the futon5 AIF+ exotype now settles
ports, exogeneity and closure before code hardens. **[OPEN]** — not yet drawn.

## D.10 Fidelity contract (GF)

**Required** — this extends existing behaviour (driver, gates, scribe).

| donor capability | disposition |
|---|---|
| gate outcome/build/sorry/axiom emission | **preserve** — L reads it |
| `proof-frame-receipt.v1` | **adapt** — add disposition; keep provenance fields |
| scribe hunger audit | **preserve** — it works, it has simply not been run |
| `batch2r_pair.sh` two-seat dispatch | **adapt** — keep two seats, add real freshness |
| candidate-frame emission without work | **drop** — F1 forbids it |
| `status.json` as Lean-source oracle | **drop** — wrong for 45 bundles (A4) |

Tripwire tests: F1 on a scaffold-only frame; F3 on an offer with no use; the
A4 predicate on a bundle whose `status.json` omits `lean.files`.

## D.11 Component T — carried, not scheduled

T (transport identification) stays modular per Joe's ruling, with the three
admissible dispositions unchanged. Joe, 2026-08-14: *"that may be a source of
creative ideas later on!"* — recorded as a **generative seam**, not a blocker.
Nothing in D.1–D.10 depends on T.

## D.12 What is [OPEN] at DERIVE close

1. `L(i)` weights `w₁ w₂ w₃` — operator
2. Retrieval pass bar — operator
3. Need-vocabulary extraction rule — must freeze before scoring
4. Retrospective stratification of the 475 — may be refused
5. The futon5 wiring diagram — not yet drawn

**The lifecycle exit criterion is therefore not yet met**, and this section says
so rather than claiming completeness. Items 3 and 5 are the two that block
implementation; 1, 2 and 4 are decisions, not unknowns.

## D.13 Exit criterion, revised — measurables-per-problem, built live

**Operator ruling, Joe, 2026-08-14.** *"If the old data had been gathered with
enough enrichment to make it work, we could build your five open items against
the historical data and thread it through. I don't think that's possible. So,
we have to use the historical data as 'inspiring but useless' for our next round
of data gathering. … your 'five open items' reduce to measurables-per-problem so
we'd be able to step through a single problem solve and see them all become
green. On that basis, they could be built as we step through 'live' on one
problem, and then checked as we step through another."*

This replaces D.12's framing and is a better resolution than the one it
replaces. The five items were treated as *decisions to make in advance*; they
are in fact **measurables that either light up on a single problem or do not.**

> **DERIVE exit criterion (revised).** Step through **one** problem solve live,
> building each measurable as it is needed, until every field of the D.5 vector
> is populated for that problem. Then step through a **second** problem,
> building nothing, and confirm the same fields populate unaided. **Build on
> problem 1, verify on problem 2.**

**IF** the five open items were genuine unknowns, **HOWEVER** each is in fact a
per-problem quantity that is either emitted or not, **THEN** settle them by
walking one problem rather than by deciding in advance, **BECAUSE** a threshold
argued in the abstract is exactly the "speculative design" IDENTIFY was
restructured to prevent — whereas a measurable that fails to go green on a real
problem is self-refuting and needs no argument.

**Consequences for the five:**

| # | item | disposition under the revised criterion |
|---|---|---|
| 1 | `L(i)` weights | **settled live** — observe the three terms on problem 1 before weighting |
| 2 | retrieval pass bar | **settled live** — the probe either surfaces the module or it does not |
| 3 | need-vocabulary extraction | **built live** on problem 1, frozen before problem 2 |
| 4 | retrospective stratification of the 475 | **CLOSED — refused.** Historical data is inspiring but useless |
| 5 | futon5 wiring diagram | still open; not blocking the walkthrough |

**Item 4 is now a decision, not a gap.** The historical corpus is retired as an
evidence base for this design: it lacked the enrichment, and MAP's whole
achievement was establishing *precisely how* it lacked it (no disposition, null
`cycle-id`, root ≠ accepted candidate, 45 metadata-invisible bundles). That
knowledge is the "inspiring" part. Nothing further is owed to it.

## D.14 Duplicate proof detection — a measurable, validated today

Joe: *"one thing that has been noticed in that regard is duplicate lemmas being
proved and we did discuss this and tried a quick experiment with a cheap
embedding approach that might help with that."*

**Exact duplicate detection needs no embedding, and it works.** Scan: extract
every `theorem`/`lemma`/`def` block from all 448 problem `Main.lean` files plus
all 18 `ConstructionTargets` modules, whitespace-normalise, hash, group.

**Validated against ground truth.** `ConstructionTargets.lean` documents that
`LusinN` shares "**17 of its 18 declarations**" byte-identically with `a95A02`,
which does not import it. The detector independently found **17**. Method sound.

**It then found an undocumented case that is worse:**

| pair | shared declarations |
|---|---|
| `CT:LemniscateComponents` ↔ `prob:a00J04` | **21** |
| `CT:LusinN` ↔ `prob:a95A02` | 17 *(documented)* |
| `CT:LemniscateComponents` ↔ `prob:a01A08` | 15 |
| `prob:a00J04` ↔ `prob:a01A08` | 15 |

⚠ **`LemniscateComponents` is one of the two ConstructionTargets modules with
ZERO consumers** (E1). So it is a 21-declaration copy of `a00J04` that nothing
imports — the same defect as `LusinN`, undocumented, and strictly worse, since
`LusinN` at least has three consumers. It is dead weight that also fragments
maintenance: a fix to `a00J04` does not reach it.

**Corpus-wide, exact duplication is rare and concentrated**: 38 duplicated
bodies over 466 sources, essentially all in the four pairs above. That is a
real result — the fear of pervasive duplicate proving is **not** borne out at
the exact-match level.

**Which sharpens what embeddings are for.** Exact matching cannot see the case
Joe is actually worried about: the *same mathematics proved independently* with
different names and phrasings. The scan above bounds the exact case at four
pairs, so **any embedding work is aimed squarely at semantic near-duplicates,
with exact duplicates already covered for free.** Cheap embeddings remain the
right tool; they now have a known baseline to beat rather than an unbounded
target.

**As a per-problem measurable** (D.5 vector, new row):

| field | role | status |
|---|---|---|
| **duplicate declarations** — count of this problem's declarations already proved elsewhere in the corpus | prover / scribe | **free** — computable today, validated |

**IF** duplicate-proving is a retrieval failure, **HOWEVER** it is also a
plain measurement, **THEN** add it to the per-problem vector immediately rather
than waiting for the retrieval work, **BECAUSE** it is the one N5-adjacent
measurable whose ground truth is on disk and needs no model, no probe design,
and no pass-bar decision — it goes green or it does not.

## D.15 The >1000 near-duplicate report, reconciled

Joe: *"there was a previous report of inexact duplicates that was noticing #'s
of lemmas over 1000."*

That report exists and its number is real — **but it does not contradict D.14's
38, because the two count different objects.** Source:
`data/glue-census/clusters.tsv` (3,805 clusters) over `have-corpus.jsonl`
(6,114 rows).

| filter | clusters | occurrences |
|---|---|---|
| spanning >1 problem | 383 | **2,322** |
| …text > 10 chars | 381 | 1,445 |
| …text > 20 chars | 288 | 775 |
| …text > 30 chars | 167 | **390** |
| …text > 50 chars | 67 | 161 |

So ">1000" recovers as **1,445–2,322** depending on the filter. Two facts
determine how to read it:

**1. These are `have` steps, not lemmas.** The corpus indexes intermediate proof
steps. A repeated `have` is ordinary proof craft; a repeated *lemma* is
duplicated work. Different objects, different significance.

**2. The corpus is variable-anonymised** — `V` for variables, `N` for numerals.
So `have H : N < V` matching across 45 problems means 45 problems contain a step
of the form *"some numeral < some variable"*. That is not duplication, it is
shape. **The anonymisation makes this corpus structurally unable to distinguish
"same lemma" from "same shape."**

The degenerate head dominates: `have H` alone is **753** occurrences across 258
problems, and the top eight forms account for **1,167 of 2,322 — 50%**. Filter
to substantive statements (>30 chars) and the whole tail is 390 occurrences.

⚠ **This is the denominator trap again, and it is the fourth sighting today.**
A large number over an unfiltered population, where half the population is
degenerate. Recorded so the ">1000 duplicate lemmas" reading does not persist.

**The two methods converge where it matters.** The largest substantive glue
cluster — `have H : (V : ConnectedComponents ↥((lemniscate V)ᶜ)) = V`, 8
occurrences over 2 problems — is `a00J04`/`a01A08`, **exactly the pair D.14's
exact-declaration scan flagged.** Independent methods, same real case. That is
evidence both are working, and that the real duplication is concentrated rather
than pervasive.

## D.16 Unconsumed promotions — the "yikes", quantified

Joe: *"LemniscateComponents and other ConstructionTargets that no one imports —
yikes."*

Justified, and it concentrates in one module.

| module | lines | decls | consumers | overlap with a problem |
|---|---|---|---|---|
| **`LemniscateComponents`** | **602** | 24 | **0** | **21 of 24 duplicate `a00J04`** (15 also in `a01A08`) |
| `SetIntegralPrimitive` | 30 | 1 | **0** | none — original, merely unused |

**`LemniscateComponents` is the whole problem in one artifact**: 602 lines,
~87% of its declarations copied from a problem, and **nothing imports it**. It
is simultaneously a duplicate *and* dead weight, and a fix to `a00J04` does not
reach it. `SetIntegralPrimitive` is a 30-line original that simply found no
consumer — minor.

**Beyond the two zero-consumer modules, 25 of 64 import edges are import-only**
(E1): the consumer imports the module and references no declaration from it.
That is a milder form of the same waste and should be measured, not assumed
benign — an unused import is a retrieval signal that fired and led nowhere.

**New measurable — promotion consumption.** Add to the D.5 vector:

| field | role | status |
|---|---|---|
| **unconsumed promotions** — modules promoted with zero declaration-using consumers | scribe | **free** — computable today |
| **import-only edges** — consumer imports module, uses nothing from it | scribe | **free** |

**IF** promotion to a shared library is the loop's mechanism for compounding
work, **HOWEVER** two modules have no consumers at all and 25 of 64 edges use
nothing from what they import, **THEN** promotion consumption is a first-class
measurable rather than a maintenance chore, **BECAUSE** an unconsumed promotion
is precisely N3/N5's failure mode made visible — the store recorded the lemma,
and no one who needed it reached it. **This is F7 at the library level**, and
unlike the retrieval probes it needs no need-vocabulary design: the import graph
is ground truth on disk.

**Cleanup is NOT proposed here.** `ConstructionTargets.lean` already specifies
the `LusinN` cleanup and holds it pending a statement-defect review. The
`LemniscateComponents` case should be recorded as known debt on the same
footing; whether and when to act is the operator's, and doing it now would
disturb a corpus we are about to measure against.

## D.17 The real duplication figure — 1,943 locked lemmas

*(Found 2026-08-14 by following Joe's pointer to the `*claude-repl:claude-1*`
buffer, which led to `apm-lean/LEMMA-INDEX.md`. This supersedes D.15's reading
of what the ">1000" report was about.)*

**`LEMMA-INDEX.md` states it in its own header**, and the number is exact:

```
2139 lemmas already proved in this repo, outside the problems'
own statements. GREP THIS BEFORE RE-DERIVING ANYTHING.
```

| | count | share |
|---|---|---|
| helper lemmas proved, sorry-free | **2,139** | |
| **`LIB:` — importable today** (18 CT modules) | **196** | 9.2% |
| **locked inside a single problem file** | **1,943** | **90.8%** |

Spread over **361 problems**, median **5** locked lemmas each.

**1,943 is the ">1000" figure**, and it is a far better one than D.15's
have-clusters: these are *lemmas*, individually proved and sorry-free, not
anonymised proof-step shapes.

### This is F7 at full scale, and the index is its confession

A lemma locked in one problem file cannot be imported. It can only be reused by
reading someone else's proof and re-typing the argument — **re-derivation with a
hint, not reuse.** The index exists precisely because these are unreachable, and
its remedy is an instruction to *grep before re-deriving*: a **documentation
workaround for a code-level availability failure.** "Technically present, not
available" is not an analogy here; it is what the file says it is for.

**So the duplication exposure is not 38 — it is 1,943.** D.14's 38 exact
duplicate bodies are the *realised* fraction of that exposure; 196 promotions are
the *mitigated* fraction (9.2%). The remaining ~90% is standing risk.

### The correlation that makes this predictive

Top holders of locked lemmas, against the duplications D.14/D.16 found:

| problem | locked lemmas | rank of 361 | implicated in a duplication? |
|---|---|---|---|
| `a96A04` | 34 | 1 | |
| `a96J01` | 30 | 2 | |
| `a01A10` | 20 | 3 | |
| **`a00J04`** | **19** | 4 | **yes — 21 decls copied into `LemniscateComponents`** |
| **`a01A08`** | **19** | 5 | **yes — 15 decls shared** |
| **`a95A02`** | **18** | 6 | **yes — 17 decls copied into `LusinN`** |

**Three of the top six holders are exactly the three problems involved in every
duplication found today**, against a median of 5. Duplication is not occurring
randomly: **it occurs where lemma density is high and importability is absent.**

**IF** duplication were a discipline failure, **HOWEVER** it lands precisely on
the problems holding the most unreachable lemmas, **THEN** treat it as a
*structural* consequence of non-importability rather than as carelessness,
**BECAUSE** that makes it predictable and preventable — the exposure is
computable per problem today, and the fix (promote, or make importable) is
mechanical, whereas "be more careful" is neither.

### New measurable — locked-lemma exposure

| field | role | status |
|---|---|---|
| **locked-lemma exposure** — helper lemmas this problem holds that no other problem can import | scribe | **free** — `lemma_index.py` computes it |
| **promotion coverage** — importable ÷ total proved helpers | scribe | **free** — currently **196/2,139 = 9.2%** |

Promotion coverage is a **single corpus-level number that moves as the loop
works**, needs no arm assignment, no stratification and no probe design, and is
already generated. It is the cheapest N3/N5 gauge available and it should be on
the board from cycle one.

*(Generator: `holes/labs/M-diagramprover/apm-driver/lemma_index.py`; last
regenerated at `ae23c95`. Predicted-duplication risk should be validated against
the next duplicate found, not asserted from these six rows.)*

## D.18 The 1,943 as a failing test suite — and the layer they fail at

**Operator framing, Joe, 2026-08-14:** *"from a memory system point of view,
each of those is, effectively, a failing test."*

Correct. And checking *how* they fail changes what the remedy is.

### They are not merely un-imported. They are un-importable.

`apm-lean/lakefile.toml` declares exactly three `lean_lib` targets:
**`ApmCanaries`, `ConstructionTargets`, `YoungL2`.** There is **no `lean_lib`
for `problems`**. So no problem file is on the module path, and
`import <problem>` fails with *"unknown module prefix"* — the same error the
lakefile's own comments record happening twice before:

> *"Until 2026-07-30 there was no lean_lib for [ConstructionTargets], so the
> files were NOT on the module path … The proved lemmas were therefore
> unreachable from the problems they were built for."*
>
> *"[YoungL2] had the SAME defect … a94J04's runner hit this on 2026-07-31 and
> reported the proved lemma as unreachable."*

**This failure mode has already been hit and fixed twice, at module
granularity. The 1,943 are the same failure at problem granularity, unfixed.**

### Why this reframes the remedy

A test can fail at either of two layers:

| layer | question | status for the 1,943 |
|---|---|---|
| **retrieval** | would the memory system surface this lemma given the need? | untested — and **moot** |
| **importability** | could the consumer *use* it if surfaced? | **no — fails structurally** |

**Retrieval improvements cannot move these.** If the store surfaced the exact
lemma with perfect relevance and the consumer's own vocabulary, the consumer
still could not `import` it. **The only available reuse mechanism is to copy the
text.**

**IF** duplication looked like a discipline failure, **HOWEVER** the sole
mechanism by which a locked lemma can be reused is textual copying, **THEN**
copying was the *rational* response and not carelessness, **BECAUSE** the agents
that produced `LusinN` and `LemniscateComponents` had no other way to reuse
`a95A02` and `a00J04` — the import they would have needed does not resolve.
D.17's structural claim is thereby confirmed at the mechanism level, not merely
correlated.

### Consequence for ordering — importability is upstream of retrieval

**Promotion coverage (9.2%) bounds what retrieval quality can achieve.** No
amount of N5 work reaches the other 90.8%. That reorders the design:

1. **importability** — can it be reached at all? (`lean_lib`, promotion)
2. **findability** — is it surfaced by need vocabulary? (F7, the 39 probes)
3. **use** — is it load-bearing once surfaced? (adjudication, D.3 F3)

The 39-probe suite (Track E) tests layer 2 **on the 196 that have cleared layer
1**. That is the right suite for layer 2 and it should not be enlarged to 1,943:
those would fail for a reason the probe does not measure, which would make the
retrieval gauge read low for a non-retrieval cause — the A4 miscalibration
mistake, repeated.

> **Two suites, not one.**
> **Suite I (importability):** 1,943 cases. Pass = the lemma is reachable by
> `import`. Currently **0/1,943**; corpus gauge = promotion coverage 9.2%.
> **Suite II (findability):** 39 cases with demonstrated demand. Pass = a
> need-vocabulary probe surfaces the module. Baseline 0 of 3 probed by hand.

**Honest limit on Suite I.** The 1,943 are a *contract* failure, not 1,943
demonstrated unmet needs: most may never be wanted again. Suite I therefore
measures **exposure**, not damage. Its pass criterion is availability, and it
must never be reported as "1,943 failed retrievals" — that would be a numerator
without a denominator, which is the diagnosis this mission opened with.

**Cheapest possible first experiment.** Adding a `lean_lib` for `problems` (or a
generated re-export module) is a lakefile-level change that would move Suite I
from 0/1,943 toward complete **without promoting anything**. Whether that is
sound Lean practice at this scale is **[OPEN]** and is a question for the next
one-shot walkthrough — it is exactly the kind of thing D.13 says to settle live
on one problem rather than argue in advance.

## D.19 The Zai rewrite-rule inspiration — designed, demonstrated, never instrumented

**Operator framing, Joe, 2026-08-14:** *"the point that motivated this whole
project is that Zai uses very stereotyped language to talk about its mistakes.
My observation is that each of those could become a rewrite rule. The earlier
claim that we had 1 success across however many N trials might invalidate that
suggestion, or it might show that it was never instrumented properly."*

**It is the second: never instrumented properly.** The evidence is a count, not
an opinion.

### The mechanism was fully specified

`algorithms/zai-learning-loop.md:81` names it as one of four scribe lanes:

> **arc-lane**: error→fix spans → scoped tactic rewrite rules
> (scope / before / after / level / confidence / evidence-ids).

So the idea is not a sketch — it has a schema, a source (error→fix spans), and a
place in the loop.

### It was demonstrated, and the demonstration is good

`s1-pilot/memory-drafts-cohort1.edn` contains rules of exactly this shape. One
in full (abridged):

```edn
{:problem-class "Prove a complex equality by real and imaginary parts when the
                 expression contains integers cast directly into ℂ…"
 :symptom "After `Complex.ext` and expansion with `Complex.add_re`, …, goals
           retain terms such as `(↑⌊x⌋ : ℂ).re`…; `Complex.ofReal_re` and
           `Complex.ofReal_im` do not match those direct integer casts."
 :lemma "`Complex.ofReal_intCast`"
 :before "`simp only [Complex.ofReal_re, Complex.ofReal_im, …]` leaves
          integer-cast projections opaque, so `linarith`, `ring`, `omega`…" }
```

**`:symptom` is the stereotyped register, already captured as a field.** It is
a mechanically matchable description of a recurring failure, paired with the
lemma that resolves it. This is precisely Joe's observation, realised.

### But it ran essentially once

| source | drafts | with `:before`/`:after` |
|---|---|---|
| `s1-pilot/memory-drafts*.edn` | 86 | **4** |
| the nine per-problem `*-scribe/` passes | 9 EDN | **0** |

**Four rewrite rules exist in total, all from the s1 pilot.** The nine later
per-problem scribe passes — `a94A09`, `a96J02`, `a97J01`, `a97J02`, `a97J03`,
`a97J07`, `e9`, `e10`, `j07` — produced **no arc-lane output at all**. No lane
tags appear in any of them.

### Consequence for the "1 success across N trials" claim

**It does not invalidate the rewrite-rule idea, because the idea was never
given a trial.** With four rules produced, none in the per-problem passes and
none wired into recall as a rule set, no run could have benefited from a rewrite
rule it was never offered. A mechanism that fires four times cannot be refuted
by an outcome measured over N runs.

This is the same shape as C2's finding about the hunger audit — *staffed but not
performed* — and the same shape as the mission's recurring diagnosis. **The
arc-lane is a designed instrument that was demonstrated and then not run.**

**IF** the low success rate were evidence against error→fix rewrite rules,
**HOWEVER** only four such rules were ever produced and none reached the runs
being counted, **THEN** the success rate is silent on the question, **BECAUSE**
you cannot measure the effect of a treatment that was not administered — and
recording it as evidence against would be exactly the "numerator without a
denominator" failure this mission opened with, inverted.

### What this adds to the design

| field | role | status |
|---|---|---|
| **arc-lane yield** — rewrite rules extracted per session | scribe | **new bookkeeping** — the lane must actually run |
| **rule-offered / rule-used** | scribe | **join** — same shape as F3's offer/use disposition |

The rewrite rule is structurally the *same object* as a memory: it is offered,
may be used, and needs a disposition. So **it needs no new evidence channel** —
it needs F3 and the arc-lane switched on. That is consistent with the whole
design: the channel exists, the disposition does not.

### Scope limit — this is a Zone-only answer

Searched: `data/repl-caches/2026-07-13/` (12 Zai REPL transcripts, 894 KB — but
these are coordination buffers, not mathematical self-report),
`holes/labs/M-zai-learning-loop/`, and `~/apm-evidence/`. **Joe notes some Zai
material may exist only in `apm-evidence` or only on Dionysus and has not left
it.** The count above is therefore a *floor* on arc-lane output, not a census.
**[OPEN]** — a Dionysus-side check would firm it up, and the conclusion
("demonstrated, not instrumented") would only strengthen if more unrun lanes
turned up.

## D.20 What the Scribe was actually doing — one lane of four, done well

Joe: *"So what was the Scribe role doing, b/c scribe was supposed to be run
after each Zai, or, later, Codex run, to extract patterns and rewrites similar
to the ones we found…?"*

**It ran. It performed one of its four lanes. That lane works.**

### The lane inventory across all nine per-problem scribe passes

`a94A09`, `a96J02`, `a97J01`, `a97J02`, `a97J03`, `a97J07`, `e9`, `e10`, `j07`:

| lane (per `algorithms/zai-learning-loop.md`) | shape | count |
|---|---|---|
| **solve-lane** — lemma-location + proof-shape | `:lemma`, `:strategy` | **8 + 7** |
| **arc-lane** — error→fix → *rewrite rules* | `:symptom` `:before` `:after` | **0** |
| **trajectory-lane** — cost/process memories | `:cost` | **0** |
| **challenge-lane** — corrections of prior claims | `:challenge` | **0** |

The algorithm doc describes solve-lane as *"Near-mechanical, highest
precision"* and trajectory-lane as *"Highest novelty — mines what error→fix
schemes can't see."* **The scribe ran the near-mechanical lane and none of the
novel ones.** So C2's "staffed but not performed" needs amending: it was
staffed and *partially* performed — the cheapest quarter of it.

### What one pass produced

`a97J01-scribe/promotion-report.edn` (2.8 KB, asserter `ams-codex-1`,
2026-08-04): **three memories**, each with a memory-id, a hyperedge-id, one
pattern (`math/measure-integration-api`), a 14–20 keyword tag bag, and
`:verified-present? true` with a tag-query verification block confirming each
memory is retrievable by its own tags.

That is careful work. It is simply *one lane's* work.

### The lane it ran passes F7 — and that is the most useful thing here

`:attachment-status :proposed` with `:attachment-reviewer-pending` looked
alarming, since the algorithm doc warns *"pattern-mediated recall only surfaces
memories reachable through a reviewed edge … Content without wiring is
invisible (we proved this the hard way)."* **Tested against the live store, they
surface anyway:**

| need-vocabulary query | hits | surfaces the a97J01 memory? |
|---|---|---|
| `bounded finite measure integral` | 6 | **yes** |
| `monotone convergence truncation` | 4 | **yes** |

**Contrast with E2**, where `ConstructionTargets` modules surfaced by *module
name* and not by `argument principle` / `counting zeros` / `winding number`:

> **Scribe memories are findable by need. ConstructionTargets modules are
> findable only by name.** The difference is the tag bag — the scribe's
> 14–20 keyword vocabulary is exactly the need-vocabulary work that makes F7
> pass, and promotion to `ConstructionTargets` carries no such vocabulary.

**IF** retrieval were uniformly broken, **HOWEVER** scribe-authored memories
surface on need vocabulary while name-only promotions do not, **THEN** the
defect is not retrieval but **what promotion records**, **BECAUSE** the two
populations sit in the same store behind the same query path and differ only in
whether a need-vocabulary tag bag was written at authoring time. **This is the
first mechanism-level explanation of the N5 defect in the mission**, and it is
cheap to act on: give promotions the scribe's tagging.

*(Caveat: two queries, both hitting. A larger probe set is Suite II's job —
this is a positive existence result, not a rate.)*

### Answering the question directly

The scribe was **not** idle and **not** broken. Per session it produced ~3
well-tagged, verifiably retrievable solve-lane memories. What it did **not** do
— on any of the nine sessions — is the arc-lane extraction that turns Zai's
stereotyped mistake language into rewrite rules. Those four rules (D.19) came
from the s1 pilot and were never made routine.

**So the rewrite-rule pipeline was never wired into the per-session scribe
pass**, which is why nine sessions of scribe work yielded zero rewrites while
the mechanism itself demonstrably works when run.

### Design consequence

| field | role | status |
|---|---|---|
| **lane coverage** — which of the four lanes ran this session | scribe | **new bookkeeping**, trivial |
| **promotion tag bag** — need-vocabulary tags attached at promotion | scribe | **new**, mechanically derivable from the module's own source |

Lane coverage is the F1-shaped fix: **a scribe pass that reports which lanes it
ran cannot silently run one of four.**

## D.21 Registration as the structural fix for "written but not wired up"

**Operator ruling, Joe, 2026-08-14:** *"'written but not wired up' is a common
defect in the APM project so far, it seems! The demonstration should have much
better behaviour than that. NB that in `~/code/mathlib4/DarkTower/` we have
material for creating an ExperimentalDesign and ExperimentPreregistration and
checking them formally. I'd argue that each problem is (henceforth) potentially
an experiment, and the experiment should be registered. The DarkTower formalism
corresponds to a Clojure semi-formalism that ensures that the actual
implementation matches the specification."*

### The defect, catalogued

Joe's diagnosis is supported by ten instances found in this mission alone:

| # | written | not wired |
|---|---|---|
| 1 | `ConstructionTargets` lemmas | no `lean_lib` → unreachable *(fixed 07-30)* |
| 2 | `YoungL2` lemma | same defect → a94J04's runner hit it *(fixed 07-31)* |
| 3 | **1,943 problem-held lemmas** | no `lean_lib` for `problems` → **unfixed** (D.18) |
| 4 | `batch2r_pair.sh` "verified-fresh sessions" | a comment; no implementing code (D2) |
| 5 | arc-lane rewrite-rule spec | 4 rules, never wired into per-session scribe (D.19) |
| 6 | capability proof: "transportability via selection diagrams" | not implemented (addendum 2) |
| 7 | `ConstructionTargets` consumer table | stale ~3× (E1) |
| 8 | V3 "under live test / pending" | stale vs the close-out report (C5) |
| 9 | `status.json` as Lean-source oracle | wrong for 45 bundles (A4) |
| 10 | mission section content | wrong header → *"the hole will NOT discharge"* (`README-missions.md`) |

**These are one defect, not ten.** In every case an artifact asserts a
capability that nothing checks. F1 and F7 were the first two local answers;
registration is the general one.

### What DarkTower already supplies

`ExperimentPreregistration.lean` (426 lines) and `ExperimentalDesign.lean` (244
lines) are not a sketch. They define `Observable`, `Flag`, `Axis`, `Arm`,
`ArmRole`, `ClaimForm`, `Registration`, `StopRule`, `DecisionRule`,
`ProspectiveRegistration`, `ReplicationPlan` (pilot vs confirmation), and
`Obligation` — plus `Evidence`, whose four fields read as a direct rebuke of
this project's failures:

- tools **resolved *and executed*** in the run's own environment;
- the remote **asserted** at the expected commit, not assumed;
- the teardown path **itself exercised**;
- the arms **observed to produce distinct output**.

**And `Launch` is gated.** `Launch` requires `ReadyToRun`, which requires
`Discharged`. **An experiment that has not discharged its obligations cannot
be launched** — "wired up" enforced by the type checker rather than by
diligence.

### The no-witness theorems are this mission's preconditions, already proved

| DarkTower theorem | what it refuses | our name for it |
|---|---|---|
| `no_witness_of_inert_flag` | a flag not observed to act on the smoke trace | **exactly defect #4** — the batch2r comment |
| `no_witness_of_missing_control` | "rarer than chance" with no no-selection arm | **headroom** (precondition 1) |
| `no_witness_of_constant_axis` | an axis whose score never varies | a measure that cannot move |
| `no_witness_of_dead_axis` | a profile zero at every sampled level | a treatment with no effect surface |
| `no_witness_of_undischarged` | obligations not discharged | **F1/F3 generalised** |
| `no_witness_of_over_budget` | over-budget registration | cost discipline |

The three preconditions promoted in the v2 plan (headroom, arm independence,
elicitation verified) are **already theorems here**, or one field away from it.
We were re-deriving, informally, what exists formally two directories over —
which is D.17's locked-lemma pathology at the level of our own methodology.

### The correspondence is live, not aspirational

The Clojure side exists: `M-typed-holes` carries the `(typed-hole, fill)`
datatype under DarkTower's `Fill`/`Comb`/`Discharge` laws, and
`futon3c.logic.capability-star-map-extractor/structural-hole-report` is the
structural counter the outer-loop tracker reads. `README-missions.md` states the
enforcement in its own terms: *"If you advance a phase but put the content under
the wrong header, the tracker will NOT see it and the hole will NOT discharge."*

**That is defect #10 and its own cure in one sentence** — the tracker refuses to
discharge what it cannot see, which is the behaviour every other item in the
table lacked.

### Design consequence — every problem is a registered experiment

> **D.21 ruling.** Henceforth each problem attempt is an **experiment with a
> `Registration`**. A cycle may not launch without a discharged registration;
> an unregistered run is not a failed experiment but **not an experiment**, and
> contributes to no denominator.

This subsumes several earlier DERIVE items rather than adding to them:

| earlier item | subsumed as |
|---|---|
| **F1** frame liveness | an obligation on the frame observable |
| **F3** offer carries a disposition | an `Observable` with a discharge condition |
| **F5** no measurement spans a regime | `Registration` names the regime; stage-typed |
| **D.13** build-on-1, verify-on-2 | `ReplicationPlan` — **pilot vs confirmation, already formalised** |
| three preconditions | the no-witness theorems above |

**D.13's two-problem protocol is `ReplicationPlan` exactly** — `pilotUnits` and
`confirmationUnits`, with `confirmation_not_pilot` proved. The walkthrough Joe
specified is already the formalism's shape.

### Starvation becomes a theorem

`AIF-COMPLIANCE.md`: *"every external dependency (observation/feed) is a
satiety-graded `TypedHole`, so **starvation is a theorem**."*

The hunger audit measures queries returning empty. Under this formalism an
unserved memory need is not a logged metric but a **provable starvation of a
typed hole**. That is a strictly stronger instrument than the one N5 has been
waiting on, and it is the same move as F7: turn the principle into a check.

### [OPEN]

1. Which DarkTower `Registration` fields a *problem-level* experiment needs — the
   existing preregistrations are cohort-scale. **Settle on problem 1** (D.13).
2. Whether the Clojure semi-formalism validates a `Registration` end-to-end today
   or only counts structural holes.
3. Whether `MemoryAblationPreregistration.lean` (1,040 lines) already covers the
   memory arm this mission needs — **read before writing** (I-4).

**Item 3 first.** On today's evidence the likeliest error is building something
that already exists two directories away.

## D.22 Reading the defunct ablation preregistration — what survives 1-shot

Joe: *"The Ablation experiment is defunct b/c we are moving to a 1-shot
modality, but you can read it to get the feel for what's there."*

Read (`MemoryAblationPreregistration.lean`, 1,040 lines). Two thirds of it dies
with the modality; **the third that survives is the third this mission needs.**

### What dies with 1-shot

Everything whose inferential power comes from running *the same problem more
than once*: `ArmKind` (full / ablated / control), seeds as within-problem
repeats, `cell` / `cellAt` / `harderAt`, `signVsControl`, `problemSign`,
`binomTail` / `signTestPasses`. The ablation's evidence is a **paired sign test
over problems**, and 1-shot has nothing to pair.

### What survives, and is directly reusable

The **Observable layer**, which is modality-independent:

| observable | what it checks | our name |
|---|---|---|
| `sessionsDistinct` | the runs really were separate sessions | **precondition 2** — the batch2r defect (D2), *checked* |
| `homeUnreadable`, `historyTruncated` | isolation actually held | isolation **verified**, not asserted |
| `noOmissions`, `noDuplicates`, `noExtras` | runs agree exactly with the panel | F6 denominator discipline |
| `expectationWellFormed` | the prediction was stated before outcomes | preregistration proper |
| `withholdingAsRegistered`, `revisionsAsRegistered` | the protocol was followed as written | anti-drift |

**The discipline that makes them work is stated in the file itself** (line 410):

> *"Every observable here is checked against **recorded probe evidence** rather
> than against an assertion."*

**That single sentence is the cure for all ten instances in D.21's table.** Our
defects are uniformly assertions with no probe: a comment claiming freshness, a
table claiming consumers, a paper claiming transportability. DarkTower's answer
is not "assert more carefully" but "an observable is a *probe*, and its
soundness is definitional."

Two further lessons transfer verbatim:

- **Unit of analysis** (line 344): *"Seeds are repeats within a problem, not
  independent observations… Running a sign test over 18 seed-level pairs would
  treat three repeats of one problem as three independent facts."* This is the
  mission's denominator discipline, already written down and already applied.
- **Completeness as three checks, not one** (line 534): `noOmissions` +
  `noDuplicates` + `noExtras` are jointly equivalent to exact agreement but
  **separately actionable** — a failing check tells you *which* way the panel
  is wrong. A single multiset equality would only say "no".

### The consequence 1-shot forces, stated plainly

**1-shot removes the within-problem contrast.** The ablation could hold the
problem fixed and vary the treatment; a one-shot series cannot. So:

**IF** each problem is one experiment, **HOWEVER** a single shot admits no
within-problem control arm, **THEN** the per-problem registration is a
**measurement registration** — fixing what will be observed and what counts as
discharge — and **not** a contrast, **BECAUSE** the learning claim (N8) lives in
the *slope across the series*, not inside any one problem.

Two things follow, and both sharpen earlier sections:

1. **Difficulty stratification (D.6) becomes more central, not less.** It was
   one control among several when contrasts were within-problem; under 1-shot
   it is *the* identification strategy for N8. Family × freeze-time statement
   length now carries weight it did not before.
2. **`sessionsDistinct` still matters — and for a new reason.** Within-problem
   contamination is moot, but a 1-shot *series* runs problems in sequence, so
   contamination flows from problem *i* to problem *i+1*. **In a learning
   experiment that leakage is indistinguishable from learning**, which is the
   single most dangerous confound the new modality introduces. It needs a probe,
   not an assurance.

### Recorded

`MemoryAblationPreregistration.lean` is **defunct as a design and live as a
reference**. Its observables and its probe-evidence discipline should be lifted
into the problem-level registration (D.21); its arm/seed/sign-test machinery
should not. Point 2 above is **new** — it is not in D.6 or the three
preconditions, and it is a direct consequence of the modality change.

## D.23 Did frame containment actually work? Partly — and the split is instructive

Joe: *"Containment in frames was maybe the one thing that actually worked. But,
I don't know that it did actually work. Anyway, yes, we do need it here."*

**Both halves are right.** Containment worked exactly where a type system
enforced it, and is unknown exactly where a receipt field was left empty.

### Structurally real, and mechanically verified

Each of the 51 frames carries a distinct workspace record:

| field | isolation |
|---|---|
| `workspace/root` | `futon6/.state/proof-frames/<problem>/<frame-id>/` — distinct per frame |
| `workspace/lean-root` | `apm-lean/ApmCanaries/Frames/<PROBLEM>/<Frame_id>/` — distinct |
| `workspace/module-root` | `ApmCanaries.Frames.<Problem>.<Frame_id>` — **51 distinct namespaces** |

On disk: **77 frame directories, 220 Lean files.** And the check that matters:

> **Zero namespace collisions** across every frame under `ApmCanaries/Frames`.

That is not an assertion — it is a property Lean's module system enforces and
that I re-verified today. **Containment held where it was mechanically
enforced.** This is the strongest positive result about the old apparatus in the
whole mission, and it justifies Joe's "maybe the one thing that actually worked."

### Unknown where it was only declared

The receipts contain the field that would witness filesystem containment, and it
is empty in **all 51**:

```edn
:state {:readonly [] :writable []}
```

Zero readonly paths, zero writable paths — the capability record was never
filled. And `boundary/workdir` is `/home/joe/code/futon3c` for **all 51**: the
frames had distinct *workspaces* but ran from a **common working directory**.

So whether a frame read or wrote outside its workspace is **not recorded and not
recoverable**. Joe's "I don't know that it did actually work" is the correct
epistemic position, and the reason is now exact: **the evidence slot exists and
is empty.** Same shape as the missing outcome disposition (A3, B-recheck, C1) —
the schema anticipated the evidence; nothing populated it.

### The designed escape hatch was never used at all

Every frame's workspace names one shared writable surface:

```
workspace/shared-extension-root : /home/joe/code/apm-lean/ApmCanaries/Local   (all 51)
```

and the frame scaffold instructs: *"Promote reusable lemmas explicitly into
`ApmCanaries.Local`."*

**`ApmCanaries/Local` contains zero files.** Created 31 March, never written to.

⚠ **Eleventh instance of D.21's defect** — and a consequential one. The
designed promotion channel was advertised in the scaffold, wired into every
workspace record, and **never once used.** Promotion instead happened ad hoc
into `ConstructionTargets` **by copying** — which is exactly how `LusinN` and
`LemniscateComponents` came to be (D.16), and part of why 1,943 lemmas stayed
locked (D.18).

**IF** containment is judged by leakage, **HOWEVER** the one sanctioned leak was
never exercised while unsanctioned copying flourished, **THEN** the apparatus
was *over*-contained rather than under-contained, **BECAUSE** it sealed frames
from each other and left the sole legitimate exit unused — so the only way to
share work was to copy it. **Containment without a working promotion path
manufactures duplication.**

### What this means for the design

1. **Keep frame containment. It is the one component with a passing mechanical
   check** — namespace isolation, verifiable at any time by the collision scan
   above. Lift it into the 1-shot modality unchanged.
2. **Fill `:state`.** The readonly/writable record becomes an `Observable`
   checked against recorded probe evidence (D.22) rather than a declared shape.
   Then "did containment hold" is answerable instead of arguable.
3. **The promotion path must be exercised, not merely available.** An unused
   sanctioned exit is worse than no exit, because it licenses the belief that
   sharing is possible while the only working mechanism is copying. Suggested
   obligation: *a cycle that produces a reusable lemma and promotes nothing
   must say why.*

**Recorded as a positive result with a named limit** — containment is the first
component this mission can report as *working*, and the report says precisely
which half.

---

## DERIVE CANDIDATE — the system, consolidated

*Written 2026-08-14 at Joe's request, after D.1–D.23 established the shape.
**D.1–D.23 are the derivation log; this is the design.** Where they disagree,
this section governs. Kept under the `DERIVE` header deliberately — per
`README-missions.md`, content under the wrong header never discharges the hole.*

### 0. What we are building, in one paragraph

**A one-problem one-shot solver whose every run is a registered experiment.**
The run cannot launch without a discharged registration; it cannot close without
a disposition; and every capability it claims is checked by a probe rather than
asserted in prose. Its outputs are a solved-or-not problem, a complete
measurement vector, and a promotion that is *reachable* by the next problem that
needs it. The learning claim lives in the slope across a series of such runs,
not inside any one of them.

### 1. Why this shape — the two facts it is built around

1. **The system records where work happened, not how it turned out.** Three
   independent audits (A3, C1, B-recheck) plus the empty containment record
   (D.23). Every channel has provenance; none has disposition.
2. **"Written but not wired up" is the project's characteristic defect** —
   eleven instances catalogued (D.21, D.23). In every case an artifact asserts a
   capability nothing checks.

**So the design is not new machinery. It is dispositions on existing channels,
plus registration to make claims checkable.** That is why it is smaller than it
looks.

### 2. Modules

Eight, with `T` optional. Each names what it *guarantees* — not what it does.

| id | module | guarantees |
|---|---|---|
| **R** | **Registration** | a run without a discharged registration cannot launch, and is not an experiment |
| **F** | **Frame** | namespace-isolated workspace, with reads/writes *witnessed* |
| **S** | **Solver** | one problem, one shot; a created frame is a worked frame |
| **A** | **Adjudicator** | every closed cycle carries exactly one disposition |
| **M** | **Memory** | every offer carries a use-disposition; retrieval answers to need |
| **P** | **Promotion** | what is promoted is *importable* and *need-taggable* |
| **X** | **Measurement** | the per-cycle vector, computed from emitted fields |
| **T** | **Transport** *(optional)* | N6 adjudicable — or explicitly refused |

**Dependency structure.** `R` gates `S`. `F` contains `S`. `S` emits to `A` and
`M`. `P` consumes `A`/`M` output. `X` reads all of them. `T` hangs off nothing.

```
        R ──gates──▶ S ──within──▶ F
                     │
              ┌──────┴──────┐
              ▼             ▼
              A             M ──▶ P
              └──────┬──────┘
                     ▼
                     X ──▶ L(i) · retrieval board · warrant table

        T ── independent, no edges into the above
```

### 3. Module specifications

#### R — Registration *(new; DarkTower-derived)*
Per-problem `Registration` (D.21): observables, flags, axes, stop rule, decision
rule, obligations. `Launch` requires `ReadyToRun` requires `Discharged`.
**Under 1-shot this is a *measurement* registration, not a contrast** (D.22) —
it fixes what will be observed, not two arms to compare.
*Reuse:* `ExperimentPreregistration.lean`, `ExperimentalDesign.lean`.
*Do not reuse:* the ablation's arm/seed/sign-test machinery.

#### F — Frame *(exists; works; needs witnessing)*
Distinct `workspace/root`, `lean-root`, `module-root`. **Namespace isolation is
mechanically enforced and verified passing** — keep unchanged (D.23).
*Change:* populate `:state {:readonly … :writable …}` as an observable checked
against probe evidence.

#### S — Solver *(new, but assembled from existing gates)*
One problem, one shot. **F1 holds by construction, not by check** (E6): a frame
that would close scaffold-identical is not emitted. Adjudication is *inside* the
solver, not a later pass (Joe, D.13).

#### A — Adjudicator *(the central gap)*
Emits the `Disposition` — the entity the whole system lacks. Also carries the
n-ary `adjudication(cycle, offer, outcome, verdict, adjudicator, at)` hyperedge
(D.2), because A3 failed precisely by losing the (offer, outcome) pair.

#### M — Memory *(exists; partly working)*
Offers and uses. **The scribe's need-vocabulary tag bags demonstrably pass F7**
(D.20) — that is the working model. Four lanes, of which only solve-lane has
ever run: **lane coverage is now reported per pass**, so one-of-four cannot
recur silently. Arc-lane (rewrite rules) is the highest-value unrun lane (D.19).

#### P — Promotion *(exists; two defects)*
1. **Importability.** 1,943 lemmas are unimportable because `problems` has no
   `lean_lib` (D.18). Retrieval cannot reach them at any quality.
2. **Need-taggability.** Promotions carry no tag bag, so they surface by name
   only (E2) — unlike scribe memories (D.20).
**Obligation:** a cycle producing a reusable lemma and promoting nothing must
say why. *Containment without a working promotion path manufactures
duplication* (D.23).

#### X — Measurement *(joins over emitted fields)*
The vector (D.5) plus the free additions found later: duplicate declarations
(D.14), locked-lemma exposure and promotion coverage (D.17), unconsumed
promotions and import-only edges (D.16), lane coverage (D.20).
`L(i) = w₁·attempts + w₂·residual + w₃·rework`, shipped with **`w₃ = 0` and the
rework term declared-but-unpopulated** until def-body hashing exists — and
populating it is a regime change requiring stratification.

#### T — Transport *(optional; three dispositions)*
Build / restate / refuse (component T). Nothing above depends on it.

### 4. Invariants

| id | invariant | enforced by |
|---|---|---|
| **F1** | a created frame is a worked frame | S, by construction |
| **F2** | exactly one disposition per closed cycle | A |
| **F3** | every memory offer carries a use-disposition | A |
| **F4** | difficulty stratum frozen before assignment | R |
| **F5** | no measurement spans a regime boundary unstratified | R (regime named in registration) |
| **F6** | no denominator promoted from corpus size | X |
| **F7** | an artifact is available only if a need-vocabulary probe retrieves it | M, P |
| **F8** | *(new)* containment is witnessed, not declared | F |
| **F9** | *(new)* every claimed capability has a probe | R — this is D.21's general fix |

**F9 subsumes the other eight.** They are the instances we know; F9 is the rule.

### 5. The two suites

| | Suite I — importability | Suite II — findability |
|---|---|---|
| population | 1,943 locked lemmas | 39 demonstrated-demand reuse edges |
| pass | reachable by `import` | need-vocabulary probe surfaces it |
| baseline | **0 / 1,943** | **0 of 3 hand-probed** |
| gauge | promotion coverage **9.2%** | pass rate |

**Do not merge them.** Suite II on locked lemmas would fail for a non-retrieval
reason and miscalibrate the retrieval gauge — the A4 mistake repeated (D.18).

### 6. Build order

Per D.13, and this *is* DarkTower's `ReplicationPlan` (`pilotUnits` /
`confirmationUnits`, `confirmation_not_pilot` proved):

1. **Problem 1 — pilot.** Walk one solve live. Build each measurable as it is
   needed until every field of §3's X populates. Settle the open weights and
   bars *by observation*, then freeze them.
2. **Problem 2 — confirmation.** Build nothing. Confirm the same fields
   populate unaided.

**Exit criterion:** problem 2 populates unaided.

### 7. Sequencing constraint

**Importability precedes findability precedes use** (D.18). A `lean_lib` for
`problems` is the cheapest experiment available and could move Suite I from
0/1,943 without promoting anything — its soundness at scale is a question for
problem 1, not for argument now.

### 8. What this design deliberately does NOT do

- **No retrospective instrumentation.** Historical data is *inspiring but
  useless* (Joe, D.13). Nothing is threaded back through the 475.
- **No cleanup.** `LusinN`, `LemniscateComponents` and the import-only edges
  stay as they are; measuring against a moving corpus is worse than measuring
  against a flawed one.
- **No within-problem contrast.** 1-shot cannot pair; the learning claim is the
  series slope, stratified.
- **No embeddings yet.** Exact duplicate detection is validated (17/17 against
  ground truth) and bounds the target; embeddings are for semantic
  near-duplicates only, with a known baseline to beat.

### 9. Open, and who closes it

| # | open | closed by |
|---|---|---|
| 1 | `L(i)` weights | problem 1, by observation |
| 2 | retrieval pass bar | problem 1 |
| 3 | need-vocabulary extraction rule | problem 1, frozen before problem 2 |
| 4 | which `Registration` fields a *problem-level* experiment needs | problem 1 |
| 5 | is the Clojure semi-formalism a full validator or a hole counter? | **read before building** (I-4) |
| 6 | futon5 wiring diagram | not blocking |
| 7 | `lean_lib` for `problems` — sound at scale? | problem 1 |

**Item 5 first.** On this mission's evidence the likeliest error is building
something that already exists two directories away.

### 10. Honest status

**This is a candidate, not a ratified design.** It satisfies the lifecycle exit
criterion in structure — the modules, guarantees, invariants, suites and build
order are stated implementably — but seven items in §9 are open, four of which
close only by walking problem 1. **That is the design's own claim about itself:
it expects to be corrected by contact with one problem, and says so in advance
rather than after.**

---

## Gate: DERIVE → ARGUE

**Operator ruling, Joe, 2026-08-14:** *"even if it comes up with a model, that's
only DERIVE — we haven't yet run ARGUE. So I'd say the gate for DERIVE to ARGUE
is to have a formalisation that seems good enough to argue for. When we run
ARGUE we may well change the design after we kick it around informally."*

**Recorded to prevent a specific misreading:** codex-4's returning a compiling
`ExperimentalDesign` + `ExperimentPreregistration` would **not** close DERIVE and
would **not** authorise implementation. A formalisation is a *thing to argue
about*, not an argument.

> **Gate condition.** DERIVE → ARGUE opens when there is **a formalisation that
> seems good enough to argue for.** Not correct, not complete — *arguable*.

**The design is expected to change in ARGUE.** Per the lifecycle, ARGUE's
pattern cross-reference is *"a structured survey, not a post-hoc decoration —
patterns you discover here may revise the DERIVE design."* The DERIVE candidate
already declares itself provisional (§10); this gate is where that gets
exercised rather than merely stated.

### ARGUE's checklist, and what this mission already has for it

| lifecycle item | status going in |
|---|---|
| **pattern cross-reference** (`futon3/library/`) | **the resource Joe flagged — see below** |
| theoretical coherence vs IDENTIFY | the four questions and the N-register are stated and resolved |
| trade-off summary | partly written — candidate §8 ("what this design deliberately does NOT do") |
| generalization notes | not started |
| **plain-language argument** (3–5 sentences, no jargon) | **not started — and the hardest item** |

**Exit criterion (lifecycle):** the design feels *inevitable* given the
constraints, not merely possible; and an outsider can understand it from the
plain-language argument alone.

### The historical output that ARGUE actually needs

Joe: *"that's another place where the historical work had some useful output,
namely the math-formal, math-informal and math-informal-\* patterns."*

Located and counted — `futon3/library/`:

| family | patterns |
|---|---|
| `math-informal` | 38 |
| `math-formalization` | 18 |
| `math-strategy` | 13 |
| `math-informal-CT` | 7 |
| **total** | **76** |

**These are already in the exact argument form ARGUE requires.** A flexiarg is
a structured `IF / HOWEVER / THEN / BECAUSE` argument with context, keywords,
audience and tone — for example
`math-informal/construct-an-explicit-witness`:

> **IF** the existence statement is concrete enough to describe the object…
> **HOWEVER** explicit constructions can obscure *why* the object exists…
> **THEN** build the object step by step… **BECAUSE** an explicit witness is the
> gold standard of existence proofs.

That is the same form used throughout this DERIVE section. **So the pattern
cross-reference is not a translation exercise — the library speaks the
methodology's own argument language.** This is the second time historical work
has proved "inspiring" in Joe's sense: unusable as *data*, directly usable as
*form*.

### A reflexive opportunity worth taking

These 76 patterns were ingested into futon1b by this session's own flexiarg work
(`pattern/library` 1,288 rows; `pattern/clause` 9,668). **So ARGUE's pattern
cross-reference is a genuine retrieval task with a genuine need** — and can
therefore double as an **F7 probe on non-mathematical vocabulary**: does the
store surface the right argument pattern when someone describes a *design*
problem rather than a *proof* problem?

If it does not, that is a finding about retrieval breadth obtained for free,
while doing work we owe anyway. **[OPEN]** — to be decided when ARGUE opens, not
now.

### Status

**DERIVE remains open.** Awaiting codex-4 (`invoke-1786723026643-4478-31c19ce7`,
parked `park-251041e1`). The gate is Joe's to call.

---

## DERIVE candidate §9 — updated by the round-1 formalisation

*(codex-4, job `invoke-1786723026643`, commits `37c502ba` + `c89af757` on
`darktower`; **reviewed and independently verified by claude-2**, 2026-08-14.)*

Artifact: `~/code/mathlib4/DarkTower/APMDemonstrationPreregistration.lean`, 269
lines.

### Review — what claude-2 actually checked

| check | result |
|---|---|
| the build claim (`761/761 jobs`) | **real** — `.olean` present, 946 KB, timestamped the same minute as the source |
| "no `sorry`, `admit`, or added axioms" | **true** — the single regex hit is the *string literal* `"axiom cleanliness"` at line 122 |
| no ablation pairing machinery | **clean** — zero hits for `ArmKind`/`cell`/`harderAt`/`signVsControl`/`binomTail`/`seedResult` |
| `ReplicationPlan` used, not reinvented | **yes** — `.pilot [problem] (by simp) variation` |
| structure not invented to force a typecheck | **confirmed** — see below |
| commits exist on `darktower` | yes |

**The fence held.** Underspecified quantities are **exposed as arguments**, with
the docstrings saying so: *"mandatory but currently unspecified operational
quantities exposed as inputs rather than invented constants"* and *"that choice
remains an argument."* This is the behaviour the packet asked for and the
opposite of the fabricated-premise failure mode.

### The refusal theorem is the design's whole point, discharged

```lean
theorem no_round1_witness_of_failed_invariant … (hfail : ¬ (invariantObservable i).holds smoke) :
    IsEmpty (ExperimentalDesign.ReadyToRun (round1Base …) e smoke)
```

**If any of F1–F9 fails on the smoke trace, round 1 cannot launch** — and it is
proved via `no_witness_of_inert_flag`, *the very theorem D.21 identified as
corresponding to the `batch2r` defect*. "Written but not wired up" is now
structurally impossible for this experiment: an invariant that does not act on
the trace is an inert flag, and an inert flag admits no witness.

### codex-4's own self-review catch, which is the mission's signature bug

Commit `c89af757` *"closes an empty-expectation loophole found during
self-review: F9 and X now check **registration-fixed** lists rather than
trace-supplied lists that could be empty."*

**That is the vacuity trap, caught unprompted, in new work.** This mission hit
it three times in old work — vacuous candidates scoring clean because empty
(D.14), containment `:state` empty (D.23), `ApmCanaries/Local` empty (D.23).
The docstring is exactly right: *"A trace may not shrink this denominator."*
**Denominator discipline applied reflexively to the formalism itself.**

### §9 revised — what the formalism actually demanded

| # | open item | source | closed by |
|---|---|---|---|
| 1 | round-1 problem + its stratum, regime, locked-lemma exposure | pre-flagged; now a typed `ProblemUnit` | **operator choice** |
| 2 | **`VariationPlan`: the facility requires a reproducibility *or* identity-floor endpoint — the one-shot candidate selects neither** | **NEW — surfaced by formalising** | **ARGUE** |
| 3 | stopping predicates and executable checks | new | problem 1 |
| 4 | outcome type and total decision rule | new | problem 1 |
| 5 | estimated cost, budget cap, teardown deadline | new | operator |
| 6 | field-level validators/types for the vector (membership + completeness only, so far) | new | problem 1 |
| 7 | `L(i)` weights, retrieval pass bar, need-vocabulary rule | candidate §9 | pilot observation |
| 8 | Clojure semi-formalism: full validator or hole counter? | candidate §9 | **read before building** |
| 9 | futon5 wiring diagram | candidate §9 | not blocking |

**Item 2 is the sharpest result of the whole exercise.** The candidate declared
a one-shot round to be *"a measurement registration, not a contrast"* (D.22) —
but DarkTower still requires a variation endpoint. **The formalism will not let
a round decline to say what would count as the same result twice.** That is a
real tension between the 1-shot modality and the facility, it was invisible in
prose, and it is exactly what formalising is for.

### Two observations for ARGUE, not defects

1. **`measurementArm.axes := []`.** With no axes, `no_witness_of_dead_axis` and
   `Axis.Navigable` have nothing to bite on. Defensible — a descriptive round
   has no treatment to vary — but it means **round 1 is formally incapable of
   supporting a learning claim.** That is correct and should be stated out loud
   rather than discovered later.
2. It is structurally the same *shape* as the empty-list loophole codex-4 just
   closed. Worth one deliberate look in ARGUE.

### Gate status

**DERIVE remains open.** Per Joe's ruling, a formalisation is *a thing to argue
about*, not an argument. On the evidence above the artifact does appear **good
enough to argue for** — it typechecks, it refuses launch on invariant failure,
it invented nothing, and it surfaced a real design tension. **The gate is Joe's
to call.**

---

## DERIVE candidate amendment — module C, the pattern cascade

**Operator observation, Joe, 2026-08-14:** *"I don't think we mentioned patterns
at all. But a suitably constructed pattern cascade should provide efficiency
boosts to the proving environment. E.g. the simplest example is 'is this a
topology problem? If so, look at topology patterns.' But that should then cascade
in a way that makes the search based on paths-in-the-grass rather than
random-access shots-in-the-dark."*

**The gap is real** — neither the candidate nor codex-4's formalisation mentions
patterns. But the mechanism Joe describes is **already specified**, in two
halves, and finding that changes what needs building.

### Half 1 — the cascade is pattern-mediated recall, which already exists

`algorithms/zai-learning-loop.md:103`:

> *"**Recall requires this**: pattern-mediated recall only surfaces memories
> reachable through a reviewed edge to a **pattern endpoint in the domain**.
> Content without wiring is invisible (we proved this the hard way)."*

**"Is this a topology problem? then look at topology patterns" *is* that
mechanism** — the domain-scoped pattern endpoint is the routing step. It is not
a new idea to design; it is an existing mechanism the candidate failed to name.
Substrate: `pattern/library` 1,288 rows, `pattern/clause` 9,668, plus the 76
`math-*` flexiargs (ARGUE gate section).

### Half 2 — "paths in the grass" is Ψ closure, also already specified

`algorithms/zai-learning-loop.md:144`:

> *"**Close Ψ — receipts feed retrieval.** Per-memory use/offer statistics as a
> bounded multiplicative ranking boost `(1 + α·used/offered)`, α≈0.5,
> **cold-start neutral**, never-blocking, audited in the receipt. **Every use
> event now bends future recall.** Reasoned non-use mildly demotes — that is
> correct, not a bug."*

**That is paths-in-the-grass, formally.** A path is worn by recorded use; the
boost is the wear.

### The consequence — the cascade is blocked on F3, the mission's central gap

`used/offered` is exactly the **disposition** this mission has spent the day
establishing does not exist: 19 of 44 offers with no disposition (A3), `state`
empty in all 51 receipts (B-recheck), provenance without outcome (C1).

> **Without F3 there are no paths, only grass.** The ranking boost has no
> numerator *and* no denominator, so search stays random-access by construction
> — which is precisely the "shots-in-the-dark" behaviour Joe wants to leave
> behind.

**IF** the pattern cascade is the efficiency mechanism, **HOWEVER** its wear
term is `used/offered`, **THEN** F3 is not bookkeeping but the **substrate of
path formation**, **BECAUSE** every use event is a footfall and a path is
nothing but recorded footfalls — so the disposition record and the cascade are
one piece of work, not two.

### And this is plausibly N8's missing mechanism

N8 asks for `dL/d(problems seen) < 0` *conditioned on memory availability*. The
candidate never said **by what mechanism `L` would fall.** This is it:

> **`L` falls because paths form.** Search stops being random access and becomes
> path-following. Path formation is measurable directly — the distribution of
> `used/offered` over patterns — and it is *upstream* of `L`, so it can be
> observed before any slope is claimable.

**"Cold-start neutral" fits the pilot exactly**: round 1 has no paths, so it
measures the shots-in-the-dark baseline honestly, and path formation becomes
observable from round 2 — the same pilot/confirmation split the design already
uses.

### Module C — added to the candidate

| id | module | guarantees |
|---|---|---|
| **C** | **Cascade** | search is **directed** — classified by domain to a pattern endpoint, refined along reviewed edges, ranked by recorded wear |

Edges: `M` supplies offers/uses → `C` computes wear → `C` directs `M`'s next
retrieval. **`C` closes the loop `M` currently leaves open**, which is why the
candidate's diagram had `M` feeding only `P` and `X`.

**New invariant:**

> **F10 (earned paths).** A ranking boost is warranted **only** by recorded
> use/offer statistics. No boost from assumed relevance, hand-tuned priors, or
> similarity alone.

F10 is F9's shape applied to ranking: *every claimed capability has a probe*
becomes *every claimed relevance has a footfall*. Without it the cascade would
be a place to smuggle in unearned confidence — and a fabricated path is worse
than no path, because it is indistinguishable from a learned one.

### Deferred to ARGUE, per Joe

- **Reproducibility / the `VariationPlan` endpoint** (§9 item 2). Joe:
  *"Reproducibility is an interesting question, but yes, let's come back to that
  in ARGUE, possibly we have patterns that can help us with that design aspect."*
  **Noted as promising**: the 76 `math-*` flexiargs are IF/HOWEVER/THEN/BECAUSE
  arguments, and the pattern library may well contain the argument form that
  settles what "the same result twice" means for a one-shot round.
- **Cascade depth and classification granularity** — how many refinement steps,
  and what counts as "the domain". Not decidable from the desk.

**Recorded as a candidate amendment, not a redesign.** `C` names an existing
mechanism, `F10` guards it, and both are ARGUE's to revise.

---

## DERIVE amendment 2 — the student, and why S was wrong to be generic

**Operator finding, Joe, 2026-08-14:** *"this is another medium-is-the-message
finding, because I think Codex's design misses the 'student' concept that we'd
sketched before whereby Zai is specifically valuable b/c it is successful on
about 8.33% of formalised research mathematics problems, whereas Codex is much
better. Zai can become the repro lane that Codex was asking for. But that was
missing from the original design."*

**Confirmed, and the concept is already written down** — `M-case-studies.md`
§Stage 1–3 (Joe, 2026-08-11). The candidate missed it because **module S treats
the solver as capability-agnostic.** It is not. Capability tier is a design
variable, and the *weakness* of the student is the resource.

### What the existing sketch says

> **Stage 1, detection (zai-1):** *"an expert's compression drops exactly the
> steps that are automatic for the expert, which are often the teachable ones;
> **a weak solver's surprise is a direct measurement of where teachable content
> sits**, where the expert reader reports 'nothing new here.'"*
>
> **Stage 3, teachability assay (zai-1 as test-solver):** *"the direct transfer
> test **with real headroom** — re-run a SOLVED problem (ground truth known,
> cheap to score) on zai-1 with and without the deposit and compare
> trajectories. **A deposit that lifts an 8.33% solver demonstrably teaches**;
> whether it also lifts codex AT ITS FRONTIER is the standing hypothesis (every
> solver is a novice at its own frontier)."*

### This retro-explains batch-1's null, and that matters

`M-case-studies.md` states it parenthetically and it is the most consequential
sentence in the file:

> *"(It is also why the channel experiments nulled: expert consumers have no
> headroom — **measuring memory effects on codex is measuring at ceiling**.)"*

`batch-1-report.md` recorded the falsifier firing: *"Per-arm explicit sorry
delta: mem −6, ctl −6 — identical … the channel did nothing detectable this
batch."*

**Read together: the null may be a ceiling artefact, not an absence.** That
changes how the historical result should be quoted — **not** as evidence the
memory channel does nothing, but as evidence it is **not measurable on a solver
with no headroom.** Recorded as a *supported hypothesis*, not an established
fact: `M-case-studies` asserts the ceiling reading, `batch-1` says only that the
channel did nothing *detectable*, and no experiment has yet contrasted the two
tiers directly. **That contrast is now the obvious experiment.**

### It resolves §9 item 2 — the VariationPlan endpoint

codex-4's sharpest complaint was that DarkTower requires a **reproducibility or
identity-floor** endpoint and the one-shot candidate selects neither. Joe's
answer supplies one:

> **The identity floor is the student.** "The same result twice" means: *a
> solver at ~8.33% reaches it, given the deposit.* Re-running a SOLVED problem
> on zai-1 has known ground truth and is cheap to score — precisely the shape an
> identity-floor endpoint needs.

**IF** a one-shot round cannot pair a problem with itself, **HOWEVER** the
facility still demands to know what would count as the same result twice,
**THEN** pair across **capability tiers** rather than across repetitions,
**BECAUSE** a result that a weak solver can reach with the deposit and not
without it is reproducible *in the sense that matters* — it survives transfer to
a consumer who had no other way to get there. **The variation axis is the
solver, not the run.**

### Amendment to module S

| id | module | guarantees |
|---|---|---|
| **S** | Solver *(now tiered)* | one problem, one shot, **at a declared capability tier** |
| **S-frontier** | Codex | closes at its own frontier; **no headroom for channel measurement** |
| **S-student** | Zai (~8.33%) | **the headroom carrier**: repro lane, teachability assay, surprisal detector |

The three roles the student plays are one property used three ways:
**surprisal detector** (Stage 1 — where teachable content sits),
**identity floor** (the repro endpoint), and **the only tier on which channel
effects are measurable at all**.

**New measurable — teachability delta.** Trajectory of `S-student` on a solved
problem, with and without the deposit. Ground truth known, cheap to score,
**and it has the headroom the frontier tier structurally lacks.**

### Two honest caveats

1. **8.33% needs its denominator recorded.** It appears in `M-case-studies:136`
   as a bare rate; 8.33% is 1/12, and this mission's own F6 forbids quoting a
   rate whose denominator is not declared. **[OPEN]** — recover it before the
   figure is used in a registration.
2. **"Every solver is a novice at its own frontier" is a standing hypothesis,
   not a result.** If it holds, the student tier generalises to Codex at its
   frontier; if it does not, the student is a measuring instrument only. Do not
   assume it.

### Why the design missed this

The candidate abstracted the solver to "one problem, one shot" and treated *who
solves* as implementation. **That is the medium-is-the-message error**: the
agent is not a substitutable component but the variable that determines whether
anything is measurable at all. Recorded as the second thing formalising
surfaced — the first was the VariationPlan gap it now answers.

## DERIVE amendment 2a — the student figure has an external source

Joe, 2026-08-14: *"The figures are from here (Zai is GLM 5.2)
https://matharena.ai/?comp=arxivlean--june&view=problem"*

**This upgrades the student concept from a project-internal belief to an
externally measured one**, and supplies more than a denominator.

### The benchmark

**ArXivLean** (MathArena, ETH Zürich / INSAIT) — formalised statements
automatically extracted from recent arXiv paper abstracts, each checked by a
team member, evaluated by producing formal Lean proofs. Updated quarterly.

| release | problems | closed models (GPT-5.4, Claude-Opus-4.7, Gemini-3.1-Pro) | open models |
|---|---|---|---|
| **March 2026** (first) | **41** | **6–7 solved** (≈15–17%) | Step 3.5 Flash, **GLM 5.1: 1 solved** (Q27) ≈2.4% |
| **June 2026** | *(not recovered)* | — | **Zai = GLM 5.2, ~8.33%** per Joe |

> *"a substantial capability gap between the open and closed model ecosystems
> for Lean"* — MathArena's own summary.

### What this settles, and what it does not

**Settled — the tier gap is real and externally measured.** The student/frontier
split (amendment 2) is not a project belief about our own agents; it is the
headline finding of an independent benchmark. That materially strengthens the
headroom argument: *measuring memory effects on the frontier tier is measuring
at ceiling* now rests on external evidence, not only on `batch-1`'s null.

**Not settled — the June denominator.** The site is JS-rendered and the tables
did not resolve to a fetch; the March figure (41) is confirmed, June is not.
8.33% is consistent with denominators of 12, 24, 36 or 48 (1/12, 2/24, 3/36,
4/48). **F6 still applies: do not put 8.33% in a registration until the
denominator is read off the live page.** [OPEN] — a one-minute check in a
browser, which is where it should be done rather than guessed here.

### Two consequences nobody asked for

**1. The benchmark already includes a memory channel.** Models are run *"with
extensive tool access, including Lean verification, semantic search through Lean
and Mathlib, and **a persistent file of proven Lean lemmas**."*

That persistent lemma file is structurally **our `ConstructionTargets` plus
`LEMMA-INDEX`** — the same construct, in the evaluation harness of an external
benchmark. So the thing this mission is building is not exotic; it is the
augmentation the field already assumes. It also means **any comparison must
declare whether our channel is additional to that one or a replacement**, or the
contrast is confounded from the start.

**2. ArXivLean is a candidate held-out set for N6.** HEAD names the target as
*"improved capability on BPM or **arXiv proofs** — held out from APM, which makes
it formally a transport claim."* ArXivLean is exactly that: arXiv-derived,
formalised, **externally scored, uncontaminated, and refreshed quarterly** so
contamination has a clock on it.

**IF** N6 needs a held-out set, **HOWEVER** our own BPM panel is scored by us,
**THEN** ArXivLean is the stronger candidate, **BECAUSE** an external scorer
removes the grader from the party with an interest in the result — which is the
same separation-of-powers principle the coding-handoff protocol already applies
to author-vs-reviewer. Recorded as a **candidate**, not adopted: C4 found BPM
better-formed *on artifact grounds* (it has the only explicit selection
diagram), and choosing between them is ARGUE's, not MAP's or mine.

*Sources:* [MathArena ArXivLean](https://matharena.ai/arxivlean/) ·
[MathArena competitions](https://matharena.ai/competitions) ·
[Beyond Benchmarks: MathArena as an Evaluation Platform (arXiv:2605.00674)](https://arxiv.org/abs/2605.00674)

---

# ARGUE

*Staged per Joe's instruction as PRO / CONTRA **backed purely by patterns**, and
belled to `zai-1` — uninvolved, and **blinded**: it received a standalone extract
of the DERIVE candidate (sha `6cc741fe…`) and codex-4's Lean file, never the
mission file, our derivation log, or our critique. zai-1 confirms: "Blinding held
throughout — the mission file was never opened." PAR `par-5ca3cf5e`.*

## A.1 The blind test — scoring overlap

We withheld five known findings. Result:

| our withheld finding | zai-1, independently |
|---|---|
| `measurementArm.axes = []` ⇒ round 1 cannot support a learning claim | **partial hit** — "missing counterfactual series for the slope claim"; "single-axis regime string vs multi-axis stationarity" |
| importability precedes findability | **hit, as a PRO** — praised suite separation via `baldwin/ablation-axes-must-not-disable-the-instrument` |
| `VariationPlan` endpoint unselected | **partial** — reached via the counterfactual-series point, not named as the endpoint gap |
| **patterns / cascade absent from the design** | **missed** |
| **F3 offer-disposition is the load-bearing gap** | **missed** |

**Two of five hit, one partial, two missed — and that is the good result.** Per
the packet's own terms, *divergence is more valuable than overlap*, and zai-1
returned **five findings we did not have**, two of which are sharper than
anything on our list.

## A.2 The two CONTRA findings that verify, and bite

**Verified by claude-2 before acceptance** — not taken on report.

**1. `modules` and `enforcedBy` are decorative.** Each appears exactly **once**,
at its own definition site. Nothing consumes them.

> **The artifact whose entire purpose is to prevent "written but not wired up"
> contains two definitions that are written and not wired.** Twelfth instance of
> the defect, inside its own cure. Found by a blinded reviewer who had never seen
> our eleven-item catalogue.

**2. F1 is encoded as a watchdog, not as a construction guarantee.** The design
says F1 *"holds by construction, not by check — a frame that would close
scaffold-identical is not emitted."* The Lean says:

```lean
observable "F1: every emitted frame differs from its scaffold" fun t =>
  t.frameCreated = true → t.closingHash ≠ t.scaffoldHash
```

That is **detection after the fact on a trace** — the exact substitution E6
rejected, and it silently converts the design's strongest guarantee into its
weakest. zai-1 caught it against `invariant-coherence/reachable-from-boot`:
*"a watchdog asks 'did someone follow the discipline?'; this invariant makes the
discipline impossible to violate."* **The reviewer used the same pattern to
praise the design (PRO 1) and to convict its formalisation (CONTRA 7).**

**3. String-identity probes as acceptance proxies** — zai-1 calls this "the
deepest one". F9's check is membership in a registered string list, so it
verifies *names*, not capabilities. **That is the vacuity trap one level up**:
`c89af757` closed the empty-list loophole, but a non-empty list of strings still
proves nothing about whether the capability was probed. Not yet independently
verified in detail; recorded as zai-1's, and the most important item to resolve.

Remaining CONTRA: half-checked F2 locus; `L(i)` outside the registered
denominator; single-axis regime string vs multi-axis stationarity.

## A.3 Pattern cross-reference — and a disconfirmed expectation

**PRO (9)** drew on `pattern-discipline/peripheral-as-sokoban`,
`invariant-coherence/reachable-from-boot`, `invariant-coherence/shape-first-identify`
(F9-as-named-shape is *verbatim* that pattern's move),
`baldwin/ablation-axes-must-not-disable-the-instrument`, the baldwin divergence
clause, and `small-n-is-a-design-feature` for the honest n=1 scoping.

⚠ **The expectation at the DERIVE→ARGUE gate was wrong.** We recorded the 76
`math-*` flexiargs as "the historical output ARGUE actually needs." zai-1's
UNSUPPORTED section reports **"the math-\* families' inability to carry a design
review"**, and the patterns it actually used came from `pattern-discipline/`,
`invariant-coherence/`, `baldwin/` and `metaca/`.

**The math-\* families are proof patterns; this was a design review.** Right
library, wrong shelf. Recorded as a **correction to our own gate note**, and as
a genuine result about the library's coverage.

## A.4 The F7 / cascade probe result

The ARGUE run doubled as a retrieval probe on non-mathematical vocabulary
(gate section). Findings:

- **The methodology families carried the review**; the mathematics families did
  not. Coverage is real but domain-shaped.
- zai-1 reports the substrate endpoint **"returning bodyless rows"** — search
  yields ids without content, so a reviewer cannot judge relevance without a
  second fetch. **An independent second sighting of an F7-class defect in the
  retrieval surface itself** (claude-2 hit the silent-unknown-parameter trap in
  the same endpoint).
- zai-1 read patterns **from files**, not from the store. The store did not
  serve the need; the filesystem did.

**This is the cascade's problem statement, evidenced rather than asserted.**

## A.5 Coherence with IDENTIFY

| question | does the design serve it? |
|---|---|
| (1a) how much evidence, full stop | **yes** — registration makes every run countable or explicitly not-an-experiment |
| (1) how much is evidence of success | **yes** — the disposition is the missing term |
| (2) can cycle one be countable | **yes** — and cheaper than feared: dispositions on existing channels |
| (2b) will it improve per iteration | **mechanism now named** — `L` falls because paths form; path formation is measurable upstream of `L` |

The theory has not shifted. It has **narrowed**: from "build measurement" to
"record outcomes on channels that already carry provenance."

## A.6 Trade-offs

**Given up:** within-problem contrast (1-shot cannot pair); retrospective use of
the 475; corpus cleanup; embeddings for now; and — newly — the assumption that
the solver is a substitutable component.

**Bought:** launch gated on discharge; measurability from cycle one; an external
scorer available (ArXivLean) if we want the grader out of the party with an
interest; and a variation endpoint that costs no repetition (the student tier).

**Still owed:** the three verified CONTRA items above, before any registration
is frozen.

## A.7 Generalization

**F9 generalises past this project.** *Every claimed capability has a probe* is a
general antidote to documentation drift, and the twelve instances catalogued here
are one organisation's sample of a universal failure. So does the headroom
result: **an augmentation cannot be measured on a consumer that already
succeeds** — which is why the channel experiments nulled, and which applies to
any tool-augmentation evaluation, not just this one.

## A.8 Plain-language argument

> Several AI systems work together here to prove mathematical theorems, and we
> want to know whether the group actually gets better with practice or merely
> gets lucky sometimes. The trouble is that it has always recorded what it *did*
> and never recorded how things *turned out* — so we could count the attempts but
> not the successes. We are rebuilding it around one rule: nothing may claim to
> work unless something checks that it does, and every run must say in advance
> what will be measured. We test this by walking through a single problem and
> watching each measurement come to life, then walking through a second one
> without building anything and seeing whether they still work. If the system is
> genuinely learning, we will see it in the paths — it should stop searching at
> random and start following the routes that earlier work wore into the grass.

## A.9 Exit criterion — not yet met

The lifecycle asks that the design feel **inevitable**. It does not yet, for one
honest reason: **three verified defects sit between the design and its
formalisation** — decorative definitions, F1 demoted to a watchdog, and
string-identity probes standing in for capability probes. Each is the design's
own named defect reappearing inside its cure, which is either an indictment of
the design or the strongest possible evidence that its central claim is right.

**ARGUE's verdict: the design is arguable and the argument mostly holds; the
formalisation is not yet faithful to it.** That is a repair, not a redesign.

## A.10 Stage-1 repair — verified, and ARGUE's exit test re-run

*(codex-4, commit `d0623df8`, 269 → 329 lines. **Verified by claude-2 at the
semantic level**, not by naming: the right structure names can exist without the
right constraints.)*

### The three defects, repaired and checked

| # | defect | repair | verified |
|---|---|---|---|
| 1.1 | `modules`/`enforcedBy` decorative (1 use each) | now inhabit `SystemDesign`; **4 uses each** | `theorem every_enforcer_is_installed (i m) (h : m ∈ systemDesign.enforcedBy i) : m ∈ systemDesign.modules` — the enforcer map is *proved* coherent with the module list |
| 1.2 | F1 demoted to a trace watchdog | **structural** | `structure WorkedFrame where … changed : closingHash ≠ scaffoldHash` — the inequality is a **field**, so no `WorkedFrame` exists without a proof of it, and `Trace` requires one. A scaffold-identical closing trace is **unconstructible** |
| 1.3 | string-identity probes verify names | **typed evidence** | F9 now requires `capability.holds t ∧ ∃ probe … recorded = true ∧ evidenceId ≠ ""` — the concrete predicate **first**, the receipt as corroboration. `registeredCapabilities : List Capability`, no longer `List String` |

Fences all held: build real (`.olean` 1.29 MB, 16:27, same minute as source),
761/761, zero `sorry`/`admit`/axioms, **8.33% not hardcoded**, no ablation
machinery.

**codex-4 agreed with zai-1's 1.3** rather than defending its own work:
*"String membership proved only naming agreement."*

### codex-4's boundary caveat is the most valuable line in the return

> *"This makes invalid frames unconstructible inside the Lean model. The external
> runtime still needs a validator that only constructs `Trace` after obtaining
> the hash inequality; **Lean cannot prevent unrelated Clojure from emitting its
> own untyped record**."*

**That is precisely the seam Joe named** — *"the DarkTower formalism corresponds
to a Clojure semi-formalism that ensures the actual implementation matches the
specification."* F1 is now structural *inside* the model, and the obligation
moves to the boundary. This is not a weakness; it is an exact statement of where
the guarantee ends, and it hands the Clojure side a precise job:

> **The semi-formalism must be the sole constructor of `Trace`.** Any untyped
> record entering by another path defeats F1 no matter how strong the Lean is.
> **F9 applied to the boundary.**

### Stage 2 correctly not attempted

codex-4 took the checkpoint: *"fixing 1.3 changes the evidence shape C and F10
should use… proceeding under the old shape would violate the checkpoint."*
**Correct, and the reason is the right one** — C and F10 are about *earned* paths,
and F10's whole content is that a boost is warranted only by recorded evidence.
Encoding that against string-identity probes would have built F10 on the very
defect 1.3 removed.

### ARGUE exit test, re-run

ARGUE failed its exit criterion for exactly one reason: three verified defects
sat between the design and its formalisation. **All three are now repaired and
independently verified.** Re-running:

| criterion | status |
|---|---|
| the design feels **inevitable**, not merely possible | **argued** — see below |
| an outsider understands it from §A.8 alone | **untested** — needs an actual outsider |

**The inevitability argument.** The design's central claim is F9: *every claimed
capability has a probe.* Over one day that claim was tested twelve times against
this project's own artifacts — and then a thirteenth and fourteenth time against
**the formalisation written to enforce it**, which contained decorative
definitions and a watchdog masquerading as a guarantee. A design principle that
keeps catching its own implementation is not merely workable; **the alternative
designs are the ones that already failed here, repeatedly, in recorded fact.**

**Remaining gap, and it is not a fidelity defect.** The formalisation is one
amendment round behind the design: module C, F10, tiered S and the student
identity floor are designed but not encoded — deliberately, by a checkpoint we
asked for. Plus three lower-priority zai-1 items (F2 locus, `L(i)` outside the
registered denominator, multi-axis regime stationarity) parked pending design
decisions.

> **Assessment: ARGUE's substantive work is done and its exit criterion is met on
> the first clause.** The second clause — outsider comprehension — cannot be
> self-certified and is the one honest reason not to declare ARGUE closed.
> **Both the DERIVE→ARGUE gate and ARGUE→VERIFY are Joe's to call.**

### First closed loop in this mission

Worth recording plainly: a defect was **found by a blinded independent reviewer,
verified by the owner, repaired by the author, and re-verified — within one
session**, with the fix confirmed at the semantic level rather than accepted on
report. Author ≠ reviewer ≠ verifier held throughout. **That is the coding-handoff
protocol working end to end**, on the very artifact whose subject is whether
claims get checked.

## A.11 The student figure — denominator recovered, and two caveats it brings

**[OPEN] item closed.** `matharena.ai/competitions` gives **ArXivLean June 2026 =
48 problems**. So Joe's 8.33% is exactly:

> **Zai (GLM 5.2): 4 / 48 on ArXivLean June 2026 = 8.33%**

F6 is satisfied — the rate now carries its denominator and may enter a
registration.

### Caveat 1: the point estimate is not sharp

`4/48` has a **Wilson 95% interval of [3.3%, 19.6%]**. The figure looks precise
because it is a repeating decimal; it is not. **The upper end of that interval
overlaps the frontier tier's March performance (6–7/41 = 15–17%).**

That does **not** collapse the tier distinction — the point estimates differ by
2×, MathArena reports the open/closed gap as its own headline, and the March
open-model result (GLM 5.1: **1/41 = 2.4%**) is independent corroboration. But
**"an 8.33% solver" must not be written as though it were a measured constant.**
Where the design needs a headroom argument, the argument is *"far from ceiling,
externally corroborated"* — not *"8.33%"*.

### Caveat 2: the student is a moving instrument — this one bites

| release | model | score |
|---|---|---|
| March 2026 | GLM 5.1 | **1 / 41 = 2.4%** |
| June 2026 | GLM 5.2 | **4 / 48 = 8.3%** |

**The student tier improved 3.4× between two releases, and neither the benchmark
nor its difficulty is constant across them.**

**IF** the identity floor is "a solver at ~8.33% reaches it, given the deposit",
**HOWEVER** that rate is a property of a model version that is revised
quarterly, **THEN** the registration must name **the model version and benchmark
release**, never the rate, **BECAUSE** a floor defined by a percentage silently
rises whenever the vendor ships — and a result that "reproduces at the identity
floor" would then mean something different each quarter, with nothing in the
record showing that the goalposts moved.

**Consequence for the design:** `S-student` is a **versioned** component.
`Registration` must pin it the way it pins a retrieval regime by commit hash —
this is F5 (no measurement spans a regime boundary unstratified) applied to the
solver rather than to the store. **A model upgrade is a regime change.**

### Recorded

- `S-student` = **GLM 5.2**, floor evidence **4/48 on ArXivLean June 2026**.
- Registration pins **version + benchmark release**, not a rate. **[NEW
  OBLIGATION]**
- The headroom argument stands on the tier *gap*, corroborated by two releases
  and MathArena's own summary — not on the point estimate.

*Source:* [MathArena competitions](https://matharena.ai/competitions)

## A.12 Correction — the Clojure is not unrelated, and the boundary validator exists

**Operator correction, Joe, 2026-08-14:** *"whether that's true or not in the
abstract, the ExperimentalDesign is set up to work in concert with Malli + CLean
to provide behavioural certificates for the Clojure code, so associated Clojure
is not 'unrelated'."*

**Correct, and A.10 oversold codex-4's caveat.** I recorded it as "the most
valuable line in the return" and assigned the Clojure side a new job — *"be the
sole constructor of `Trace`"* — as though that job were open. **It is not open.
It is `futon6/scripts/clean_to_lean.py`, 1,028 lines, and it already exists.**

### The mechanism, in its own words

`clean_to_lean.py` docstring:

> *"CLean → DarkTower Lean emitter (deterministic). … **The render IS the
> correctness gate: a CLean is well-formed iff it produces type-correct, 0-sorry
> DarkTower Lean.**"*

**The Lean is *generated from* the Clojure-side record, not written alongside
it.** So the failure codex-4 imagined — an untyped Clojure record bypassing the
Lean — is not a gap to police: **a record that does not render to valid Lean is
by definition not well-formed.** The correspondence is definitional, not
aspirational. That is the behavioural certificate.

### `validate_experiment` already enforces things this mission re-derived

| check (line) | what it prevents | our name for it |
|---|---|---|
| *"must be `:registered-not-run` before rendering"* (180) | rendering a registration for an experiment that already ran | **preregistration, mechanically enforced** — you cannot retrofit a registration to results |
| *"axis requires at least two reachable levels"* (193) | a dead or constant axis | `no_witness_of_dead_axis` / `Axis.Navigable`, enforced **before** Lean |
| non-empty unique axis ids; numeric levels (190, 196) | ill-formed axes | shape discipline |

And the v3 programme records the pipeline as **attack-verified**: *"claude-4's
CLean pipeline gained `ArmRole` + `axisPredictedNonNavigable` (mathlib4
`084930e`, futon6 `9a25e8c`, **attack-verified — a live axis cannot be laundered
as a positive control**)."* Someone tried to defeat it and could not.

Malli is the Clojure-side schema layer for the runtime records
(`futon3c/deps.edn` → `metosin/malli 0.16.3`; used in `reflection/envelope.clj`,
`peripheral/proof_shapes.clj`, `social/shapes.clj`, `peripheral/mission_shapes.clj`).

### What is actually open — much smaller than A.10 implied

Not "build a boundary validator". The accurate statement:

> **F1's hash-inequality obligation is new and is not yet among
> `validate_experiment`'s checks.** The job is to add one obligation to an
> existing, attack-verified validator — not to construct the correspondence.

**A.10's paragraph beginning "The semi-formalism must be the sole constructor of
`Trace`" is superseded by this section.** The principle stands; the implication
that it needed building does not.

### The lesson is the mission's own, applied to me

I read codex-4's caveat, found it well-reasoned, and recorded it as a finding
without checking whether the capability it presumed missing already existed.
**That is I-4 (read before you write) violated by the agent maintaining a
document about capabilities that go unchecked** — and it is why the operator
correction was needed. Recorded rather than quietly fixed, because a silent
correction here would be the same defect once more.

### Incidental: D.14 was E7's H1, already specified

`E-memory-v3-programme.md` lists **E7** as *"duplicate detection over the Lean
corpus, calibrated against the 17 `LusinN`/`a95A02` byte-identical proofs"*,
blocked on nothing for H1. **That is exactly what D.14 did today** — including
validating against the same 17. The experiment was designed, unrun, and
independently re-derived. It reinforces D.17's finding at the level of the
project's own plans: **material that is written but not reachable at the moment
of need gets rebuilt, not reused.**

## A.13 The actual CLean point — bridges before proofs

**Operator correction, Joe, 2026-08-14:** *"we use CLean in different ways — the
Lean-from-CLean isn't the example I had in mind."* Source:
`futon5/TN-baldwin-reboot.md` §20.

**A.12 was right that a boundary mechanism exists and wrong about which one
matters.** The render-gate is a *well-formedness* check. The concept Joe means
is deeper and is stated there in full.

> Joe, quoted in §20: *"this is what we were trying to go towards with the
> Lean+CLean formalism… the problem is we would have been formalising the wrong
> things. What I think we need isn't just a formal model, it is, ultimately, a
> **proof that what we have works**. And that can't be created if we don't have a
> working system. But at least we could be **honest about the holes**."*

### The distinction that governs everything we built today

| kind | what it proves | who does it |
|---|---|---|
| **model-internal proof** | properties of an idealisation | Lean, well |
| **model–artifact bridge** | **whether the running thing satisfies what the model says about it** | almost nothing, so far |

> *"Every failure in this register is the second kind."* … *"**Lean sits entirely
> on the model side.** The §18 invariants are the first bridges built here — not
> proofs, but **machine-checked statements that the artifact respects a bound the
> model derives**. That is why they caught what review did not."*

**This reclassifies our own work.** F1–F10, `WorkedFrame`, the typed
`CapabilityProbe` — all **model-internal**. `WorkedFrame` makes a
scaffold-identical trace unconstructible *in the model*; it says nothing about
the running solver. codex-4's caveat was gesturing at this and I mistook it for
a plumbing gap.

> **Ordering, now explicit: bridges before proofs.** *"A proof that the system
> works is only meaningful once the system's claims about itself are true, and
> that is a property of the artifact, not of the model."*

### The failure mode that should frighten us most

§20.1, on `DarkTower/Patterns/Propagator.lean`:

> It is **sorry-free**. Its theorems are stated over `Equiv.Perm` — bijections.
> Nothing in it is wrong. The false claim was *"that 2015 paper found a single
> member of this family."* **In Lean that claim is not false — it is
> inexpressible.** You cannot construct `k ↦ max(k-1,0)` as an `Equiv.Perm`,
> because it has no inverse. **The formalism *contained* the refutation, as a
> typing obstruction, and was never asked. The claim was made in LaTeX.**

**That is this mission's guiding light, at the level of formalism.** A
sorry-free artifact, holding the refutation as a typing obstruction, never
queried — technically present, not available. It is the third distinct form of
the same failure today, after the 1,943 locked lemmas and the retrieval index.

> *"A proof assistant checks theorems, not the **relevance** of theorems."*

And the repair is stated: **generalise until the load-bearing question becomes a
typing question.** State `T1′` over general endomaps rather than permutations and
*"is the bug in this family?"* stops being prose. *"It moves the load-bearing
claim inside the formalism, which is the only place a formalism can help."*

### Candidate invariant F11

> **F11 (ask the formalism).** A formalism earns its keep only if the
> load-bearing claim is **expressible inside it**. If the claim that would
> falsify the design cannot be stated in the model, generalise the model until
> it can — or record that the claim lives outside and is unchecked.

F9 says every claimed capability has a probe. **F11 says the probe must be able
to fail.** A model in which the refutation is inexpressible cannot refute, and
its sorry-freeness is then a fact about its types, not about the world.

### And the cheapest thing in the whole mission

§20.3:

> *"A `sorry` is an honest hole: the obligation is named, typed, and visible to
> the checker."* … *"`declared-channels`' docstring — 'HAND-DECLARED, not
> derived… do NOT assume symmetry with `:rule-change`' — **is already a `sorry`
> written in Clojure.** It is a named, located, honest obligation. **It needs no
> new machinery, only doing it everywhere it applies.**"*

**A Clojure docstring that names its own unverified assumption is a bridge-side
`sorry`.** Zero cost, no tooling, and it is precisely what the eleven
"written but not wired up" instances lacked: not a check, just an honest hole
where a check should be.

**Design consequence.** Every model-side invariant F1–F11 gets one of two
things, and must carry which:
1. **a bridge** — a machine-checked statement that the artifact respects it; or
2. **a declared hole** — a located Clojure `sorry` saying it is unchecked.

**Silence is the only forbidden option.** That is "honest about the holes" made
operational, and it is what this mission's twelve defects were each an instance
of failing to do.

### Correction to A.12

A.12's claim — *the correspondence is definitional, so the boundary is closed* —
**overreached.** The render-gate closes *well-formedness*: a CLean that does not
render is not well-formed. It does **not** establish that the running system does
what the rendered model says. **That gap is exactly the model–artifact bridge,
and it is open.** A.12 stands on the facts and is corrected on the conclusion.

## A.14 The bridge exemplar, traced end to end

Joe: *"if you look in the DarkTower repo you will see some experiments that we
registered in connection with the futon5 work, and we should be able to trace
through and find the associated CLean + Malli specs that validate the actual
implementation."*

**Traced. The pattern is complete, live, and mutation-tested — and it is the
method this mission has been missing.**

### The chain

| layer | artifact | role |
|---|---|---|
| **registration** | `DarkTower/ExotypeLiftVariantPreregistration.lean` | **names the Clojure artifact**: `futon5/src/futon5/hexagram/lift.clj`, `futon5.exotype.efe/predict` |
| **bridge** | `futon5/test/futon5/exotype/invariants_test.clj` — 37 KB, **25 tests** | machine-checked statements that the *artifact* respects bounds the *model* derives |
| **falsifiability** | mutation testing (§18.4) | every mechanism verified to **kill a defect**, not merely to pass |
| **declared holes** | four `hole-*` tests | what remains unchecked, named and executable |
| **runtime shapes** | Malli 0.16.3 (futon3c: `envelope`, `proof_shapes`, `social/shapes`, `mission_shapes`) | validates records on the coordination side |

*(Correction: futon5 itself uses no Malli — its bridges are `deftest` invariants.
Malli is the futon3c-side shape layer. Both are artifact-side; they are not the
same mechanism.)*

### The test that settles A.13's story

`permutation-writes-are-uniform-but-the-2015-bug-is-not` constructs the very map
Lean could not express:

```clojure
(let [bug (mapv #(max (dec %) 0) (range 8))
      counts (frequencies bug)]
  (is (= 2 (get counts 0)) "position 0 is written twice")
  (is (nil? (get counts 7)) "position 7 is never written")
  (is (not= (into {} (map #(vector % 1) (range 8))) counts)
      "the bug is therefore not a permutation, and not in the 8! family"))
```

Its docstring: *"This is `holes/F-what-the-propagator-actually-does.md` 5, **made
executable**."*

> **The claim that was *inexpressible* in Lean is asserted, and checked, in
> Clojure.** The model held the refutation as a typing obstruction nobody could
> query; the bridge asks it directly. That is "bridges before proofs" with a
> worked instance, not a slogan.

### What a `hole-*` test actually does — better than a docstring `sorry`

`hole-the-objective-is-degenerate-over-its-entire-domain` does not assert
correctness. It asserts the **current unsatisfactory state**, with a dated
history of movement (`full 2 → 3`, `3 → 2`, `2 → 4`) and the reason each number
changed — so any drift is caught and any improvement is visible.

> **A declared hole is not a comment saying "unverified". It is a test pinning
> exactly how unverified, so the number cannot move in silence.** That is
> strictly stronger than the docstring-`sorry` of A.13, and §18.3 records the
> upgrade path: *"The bound previously lived in a docstring, which is precisely
> where §17 #3 walked past it."*

### Anti-vacuity is built in — independently of us

§18.1: the RNG scanner *"also asserts the scanner still sees sites of each
class, so it cannot silently degrade into a vacuous pass if the file layout
changes."*

**That is the vacuity trap this mission hit four times, pre-empted in someone
else's test file.** And §18.4 states the principle we reached as F11 from the
other direction: *"a test that cannot fail is worth nothing."* **Mutation
testing is F11's method** — it demonstrates the probe can fail, rather than
asserting it.

### The method this mission now adopts

Each model-side invariant F1–F11 gets a bridge of this exact shape:

1. **The registration names the artifact** — namespace, file, function.
2. **An invariants test asserts the model's bound over the artifact**, not over
   an idealisation.
3. **Anti-vacuity clause** — the scan must still see instances of each class.
4. **Mutation-tested** — reintroduce the real defect; confirm it is killed;
   record which assertions failed.
5. **`hole-*` test** for whatever remains unchecked, pinning the current numbers
   with dated history.
6. **Bounds live in tests, not docstrings.** A docstring is where the last one
   was walked past.

**This is the VERIFY phase's method, found rather than invented** — and finding
it cost one operator correction and two wrong guesses on my part (A.10's "sole
constructor", A.12's "definitional, therefore closed"). Recorded plainly: the
method existed, was reachable, and I proposed building it twice before reading
it. **Thirteenth instance, mine.**

---

# VERIFY — readiness assessment and spike design

*Joe, 2026-08-14: "we have iterated twice on DERIVE and once on ARGUE — are we
ready to go to VERIFY? In this case, that could possibly be done by making fake
data for a frame-0 and feeding it through the Lean code to produce relevant
witnesses; if that works we could implement in the spec-locked Clojure and be
confident tests & procedures would work in reality."*

## V.0 Are we ready? Yes — and the gaps are VERIFY's work, not blockers

Per the lifecycle, VERIFY is *"check the architecture against constraints
**before** committing to full implementation… targeted risk reduction."* Its
exit criterion is that risks which cannot be verified statically **have been
spiked**. That is precisely the state we are in.

**What is ready:** a consolidated DERIVE candidate plus two amendment rounds; an
independent blinded ARGUE; three defects found, repaired and re-verified at the
semantic level; a Lean file that typechecks and refuses launch on invariant
failure.

**What is not, recorded honestly:**

| gap | disposition |
|---|---|
| ARGUE exit clause 2 — outsider comprehension of §A.8 | **untested**; needs a person, not a phase |
| formalisation is **two amendments behind** the design — stage 2 (C, F10, tiered S, student floor) *and* the A.13/A.14 bridge + F11 material | VERIFY work item |
| no futon5 wiring diagram | VERIFY item 1 — draw it, or record why skipped |
| GF fidelity check (D.10's preserve/adapt matrix) | VERIFY item 4 — tripwires not yet confirmed to exist |

None blocks entry. All four are things VERIFY exists to surface.

## V.1 Joe's spike is the right one — with a scope limit

**The method is correct**: synthetic frame-0 → Lean → witness. It is exactly the
lifecycle's *"minimal spike to validate the riskiest DERIVE commitments."*

**But A.13 constrains what it can establish.** Lean sits entirely on the model
side. A witness produced from fabricated data shows that **the registration
machinery is coherent and satisfiable** — it says nothing about whether a
running system satisfies it.

> **IF** the spike produces a witness, **HOWEVER** the data was fabricated to be
> well-formed, **THEN** the confidence gained is about the **specification**, not
> about reality, **BECAUSE** the model–artifact bridge is exactly what a
> model-internal exercise cannot test — and mistaking one for the other is the
> failure §20 of `TN-baldwin-reboot` documents.

That does not weaken the spike; it names its yield precisely. **"Be confident
tests & procedures would work in reality" should read: be confident the spec is
implementable and internally consistent.** Reality gets checked by the bridges
(A.14), once Clojure exists.

## V.2 The spike must include negative cases — or it proves nothing

A frame-0 constructed to satisfy the observables demonstrates only that
satisfiable observables are satisfiable. We adopted the rule that forbids this
one phase ago:

> *"A test that cannot fail is worth nothing."* (§18.4, adopted at A.14)

So the spike is **not one trace, it is a matrix**:

| case | construction | expected |
|---|---|---|
| **positive** | a well-formed frame-0 satisfying every observable | `ReadyToRun` **inhabited** — a witness is produced |
| **negative × F2…F9** | one trace per invariant, violating exactly that one | `IsEmpty (ReadyToRun …)` — launch refused |
| **negative × F1** | a scaffold-identical closing frame | **must not compile** |

The negatives are nearly free: `no_round1_witness_of_failed_invariant` is
already proved, so each case is an instantiation rather than a new proof.

**The F1 row is the interesting one.** After the repair, `WorkedFrame` carries
`changed : closingHash ≠ scaffoldHash` as a field, so a scaffold-identical frame
is **unconstructible**. Its negative test therefore is not "the observable
reports false" but **"this code does not typecheck"** — the strongest available
outcome, and worth demonstrating explicitly rather than assuming.

## V.3 What the spike would settle, and what it would not

**Settles:** that the registration is satisfiable at all; that every invariant
can be independently violated and independently refuses launch; that F1's
guarantee is structural rather than nominal; that the observable set is not
vacuous.

**Does not settle:** anything about a running solver; the `L(i)` weights, the
retrieval pass bar or the need-vocabulary rule (all deferred to pilot
observation by D.13); the student-tier floor; or whether the Clojure implements
what the Lean says — that is the bridge, and it is VERIFY's *other* half.

## V.4 Recommended sequence

1. **Spike the matrix above** — positive, F1-doesn't-compile, and one negative
   per remaining invariant.
2. **Completion-criteria pre-check** (lifecycle item 3) against §1.5's seven
   criteria — cheap, and it may expose a criterion the design silently drops.
3. **GF fidelity check** (item 4) against D.10's preserve/adapt matrix.
4. **Wiring diagram** — draw or record why skipped.
5. Only then: spec-locked Clojure, with a bridge per invariant (A.14's
   six-part method).

**Gate is Joe's.** Nothing spiked yet.

## V.5 The full VERIFY chain — matrix, then generated spec

**Operator ruling, Joe, 2026-08-14:** *"VERIFY can point at your matrix and if
that passes point also at the Malli+CLean discipline that follows on from the
Lean work as a generated spec."* And on why the phase exists at all: *"naturally
we need to verify before building (post-build verification comes 'for free'…
sometimes in the form of bugs)."*

### The economy argument, in one line

**Verification is not optional; only its timing is.** Post-build verification
arrives free, in the form of bugs — and this project has the receipts for what
that costs: eleven "written but not wired up" defects, an odometer that read low
for four months, and 1,943 lemmas nobody could import. Joe's contrast is exact:
*"a huge upgrade from the first half of APM where we bashed through and didn't
validate anything."*

### Four layers, and they are not the same thing

| # | layer | establishes | can it drift? |
|---|---|---|---|
| 1 | **Lean model** (F1–F11) | the invariants are stated and typecheck | — |
| 2 | **Matrix spike** (V.2) | the model is *satisfiable*, and every invariant can *refuse* | — |
| 3 | **Generated Malli/CLean spec** | the runtime **contract** matches the model | **no — by construction** |
| 4 | **Bridge tests + mutation** (A.14) | the running **artifact** honours the contract | yes; this is what tests catch |

**Layers 3 and 4 are easy to conflate and must not be.** A generated Malli
schema makes *spec drift* impossible — the contract cannot disagree with the
model because it is derived from it. It does **not** establish that the solver
behaves accordingly: a schema validates the *shape* of a record, not that the
runtime refused to emit a scaffold-identical frame. **Shape conformance is
generated; behavioural conformance is tested.** That distinction is A.13's
model/artifact split, reappearing one level down.

### Direction matters — these are two different arrows

There is an existing generator, and it runs the **other way**:

- **`futon6/scripts/clean_to_lean.py`** — *record* → Lean *term*. A CLean EDN
  instance renders to DarkTower Lean; *"the render IS the correctness gate: a
  CLean is well-formed iff it produces type-correct, 0-sorry DarkTower Lean."*
- **Proposed (does not exist)** — Lean *type* → Malli *schema*. The invariant
  and observable declarations generate the runtime contract.

**Types generate the schema; records render to terms.** Different levels, no
circularity — but stating it prevents exactly the muddle that would produce one.
Checked before proposing: **no Lean→Malli generator exists** anywhere in the
tree; the only direction implemented today is CLean→Lean.

### The pattern to copy, rather than invent

`clean_to_lean.py` is 1,028 lines with a `validate_experiment` that fails hard,
**and it has a test** — `futon6/tests/test_clean_to_lean_experiment.py`. A
Lean→Malli generator should follow that shape: deterministic, hard-failing on
ill-formed input, and tested. **It should also carry its own anti-vacuity
clause** (§18.1's move): assert that it still emits schemas for each declared
invariant, so a refactor cannot silently reduce it to generating nothing.

### Revised VERIFY sequence

1. **Matrix spike** — positive, F1-must-not-compile, one negative per invariant.
2. **If it passes: generate the Malli/CLean spec from the Lean.** Spec drift
   becomes structurally impossible.
3. **Bridge tests per invariant**, mutation-tested (A.14's six-part method) —
   this is where behavioural conformance is actually established.
4. Completion-criteria pre-check; GF fidelity check; wiring diagram or a
   recorded reason to skip.

**Steps 1–2 are verify-before-build. Step 3 is the part that only exists once
there is something to build against** — and naming that ordering is what stops
step 2's structural guarantee being mistaken for step 3's empirical one.
