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
