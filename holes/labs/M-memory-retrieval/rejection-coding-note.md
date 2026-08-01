# V2-3 — coding the declined memories (2026-07-31)

Source: 45 dispatches whose receipts record at least one surfaced memory,
extracted from `job-results-20260731/` (the receipts' own
`:rejection-reasons` field carries only 7 non-empty vectors corpus-wide, so
the reasoning was recovered from runner prose). Harness:
`code_rejections.py`; artifacts `coding-sections-20260731.json`,
`rejection-coding-20260731.json`. Deterministic.

## Headline

**211 memories surfaced across 45 dispatches. Receipts record 43 used — but
`used-ids` demonstrably undercounts, so 20% is a FLOOR, not a measurement.**

### `used-ids` disagrees with the runner's own prose — and it is a live regression

Found 2026-07-31 when claude-9 challenged a claim about a96A04. **In 5 of 45
rows the receipt records `used-ids` as empty while the runner's report
explicitly says "Used …"**, accounting for at least 6 uncounted uses:

| job seq | problem | receipt `used-ids` | prose reports |
|---|---|---:|---:|
| 455 | a95A04-symmetric-mean-value | 0 | 1 |
| 467 | a96A01 | 0 | 1 |
| 468 | a96A04 | 0 | **2** |
| 471 | a97A08 | 0 | 1 |
| 474 | a94J04 | 0 | 1 |

Within these 45 rows the disagreements cluster late — 0 of the 34 before job
seq 455, 5 of the 11 from 455 onward — which I first read as a regression at a
session boundary. **Corpus-wide measurement does not support that reading, and
the truth is worse.**

**Cause, traced by claude-9 and verified here: the outcome half has no code
writer at all.** `dispatch_with_recall.clj:906` writes the *offered* half with
`:used-memory-ids []` hardcoded — correct by design, since nothing has been
used at dispatch time. `record-offered!` exists at `:978` and is called at
`:1052`. **There is no `record-outcome!` anywhere in the file.** So the outcome
half is hand-authored into a receipt template, and `used-ids` inherits whoever
is holding the pen.

**But it was never reliable, so there is no good period to compare against.**
Counting claude-9's **outcome-phase** receipts only (offered halves are
supposed to be empty, so counting them would repeat the denominator error this
note is otherwise about):

| day | outcome receipts | with `used-ids` | with `:metric-3-memories-used` |
|---|---:|---:|---:|
| 2026-07-29 | 8 | **0** (0%) | 0 |
| 2026-07-30 | 48 | 9 (19%) | 15 |
| 2026-07-31 | 49 | 8 (16%) | 28 |

Flat at roughly one in six, not a collapse at a session boundary. What *did*
change is the count field: `:metric-3-memories-used` appears 0 → 15 → 28, so a
count was **added alongside** the ids rather than replacing them.

So both first readings were wrong: it is not a code regression (my reading) and
not a template change between sessions (claude-9's). It is a field that has
never had a writer of its own and has been populated about one time in six
throughout.

**The ids are recoverable.** They were never lost — they are in the runner
prose, and all 130 raw job results are already on disk (§3.2.1). The floor of
≥ 23% can be turned into a measurement by back-filling from data in hand,
without re-running anything.

Consequences:

- The true use count is **≥ 49 of 211 (≥ 23%)**, not 43/211 (20%). Every use
  figure in this note is a floor.
- **Any V3 analysis keyed on `used-ids` will silently undercount**, and worse
  for recent data than for old. This is a live defect, not a historical one.
- It compounds the `:rejection-reasons` finding (7 non-empty corpus-wide): the
  structured use/decline fields are *both* unreliable, and the runner prose is
  the more faithful record — which is the opposite of the intended design.

This belongs in V2 alongside the observation-channel audit, and the repair
belongs in V3's instrumentation list.

## Category distribution (94 coded decline-mentions)

| # | category | n | share |
|---|---|---:|---:|
| 1 | **topical mismatch** — different subject entirely | **61** | **65%** |
| 5 | precondition absent — the trigger never fired | 16 | 17% |
| 2 | scope mismatch — right area, wrong sub-object | 6 | 6% |
| 4 | stage mismatch — relevant to a *later* target | 4 | 4% |
| 6 | relevance without applicability — right shape, unusable form | 4 | 4% |
| 3 | subsumption — relevant but already handled | 3 | 3% |
| 7 | **discoverability** — needed something unobtainable | **0** | **0%** |

## Four findings

**1. The modal decline is a retrieval failure, not a judgement.** Topical
mismatch is 65% of coded mentions, and its true share is *higher* than that
(see coverage, below). The dominant reason a reviewed memory is declined is
that it was about something else entirely. That is a statement about
retrieval quality, not about runner discrimination.

**2. The discriminating categories are real but a small tail.** Subsumption,
stage mismatch and relevance-without-applicability together account for
**11 of 94 (12%)**. These are the modes that evidence a runner reasoning
carefully about a genuinely nearby memory — claude-9's *considered-but-
declined*. They exist, they are well-articulated where they occur, and they
are rare.

**3. Precondition-absent (17%) is the corpus working as designed.** "No
liminf obligation arose", "no instance diamond arose" — a conditional memory
was surfaced, its trigger did not fire, and the runner said so. This is
honest non-use of a correctly-surfaced memory and should not be counted
against retrieval.

**4. Discoverability scored exactly zero — as predicted, and the prediction
is the finding.** §3.2.1c of the V2 plan argued structurally that this mode
is invisible to the observation channel, because every receipt field is
closed over the *offered* set: a needed artifact that was never offered
appears nowhere. Coding 94 declines returned 0 instances, confirming it
empirically.

**It surfaces instead as a silence.** **18 of 45 rows (40%) explicitly report
that error-time recall returned no memory IDs** — "Error recall was run for
each compile-error class and returned no error-time memory IDs" — and those
same rows then carry an *Error → fix* log of problems solved from scratch.
That is the only visible form the mode takes, and it is not a decline.

## Why 65% — it is the designed cost of the breadth fallback

Joe's hypothesis (2026-07-31) was that the decline profile traces to imperfect
text recall, with combinatorial aspects noticed previously. **Confirmed, and
the mechanism is documented in the recall path's own docstring.**

`dispatch_with_recall.clj:288–298`:

> MEASURED 2026-07-30 across five live rows: the 3-term query returned **ZERO**
> memories for ALL FIVE, while falling back to 2-term pairs and then singles
> surfaced a memory for THREE of them. The 3-term cap was itself a fix earlier
> the same day (from 36 terms) and it was not enough, because **term SELECTION
> is by statement order rather than by signal**: a01A04's third term is
> `recursion`, a rare word that floors any conjunction containing it.
>
> Ordered strictest-first … **Singles are last and deliberately included:
> offering a marginally relevant memory costs little, because the runner
> reports whether it USED one, and that report is the measurement we actually
> want.**

So the ladder is 3-term conjunction → 2-term pairs → **singles**, and singles
are low-precision by construction. **The topical-mismatch rate is the price of
the breadth rung, knowingly paid.** The combinatorial tension is that the same
knob produces both headline failures in opposite directions:

| term selection | failure |
|---|---|
| too rare / over-conjoined | conjunction floors → **recall-empty** (V1: 64%) |
| too common / single-term fallback | low precision → **topical mismatch** (65% of declines) |

**But the trade's justification no longer holds.** The design pays precision to
buy a measurement — "the runner reports whether it USED one, and that report is
the measurement we actually want". That report is `used-ids`, and it is
populated in **~16% of outcome receipts** (above). *The system is paying the
full cost of breadth and recording roughly one sixth of the benefit it was
purchased for.* That is the sharpest single statement this coding pass
produced, and it is a claim about the loop's economics rather than its ranking.

**The rarity repair exists but has not reached this path.** `b8b1863f` "Rank
sorry recall subjects by rarity" (2026-07-31) touches only
`scripts/codex_sorry_cron.py`, which queries a `df` endpoint and keeps the
rarest terms. `dispatch_with_recall.clj` still selects by statement order; its
`:48–49` comment names ranking by rarity as the structural repair and records
it as out of scope at the time. So dispatch-side subject selection is now
rarity-ranked while the recall ladder is not.

### When is fine-grained retrieval wanted? — granularity is a property of the MEMORY

*Joe asked (2026-07-31) whether df ranking fits the plan, and when fine-grained
retrieval is wanted, noting he was unsure of the specific use cases. Measured:*

Splitting the corpus by claude-9's substitutive/regulative distinction —
**substitutive** memories name a specific mathematical object or technique;
**regulative** ones state a policy about how to proceed (stopping rule,
risk-ordering, route override, scope guard):

| kind | memories | surfacings | used | use rate |
|---|---:|---:|---:|---:|
| **regulative** | 8 | 31 | 14 | **45%** |
| substitutive | 46 | 129 | 19 | **15%** |
| uuid-only (unnamed) | 23 | 51 | 10 | 20% |

**Regulative memories earn a 3× higher use rate despite being surfaced
broadly.** Individually: `override-a-documented-proof-route` 2/2, `order-proof-
search-by-known-route` 1/1, `separate-compact-bump-convergence` 1/1,
`diagnose-recall-empty` 2/3, `bound-the-interface-adapter` 5/8,
`bound-automatic-frontier-descent` 3/9.

*Classifier caveat, stated rather than fixed: my name-based rule captured
`bound-polynomial-sum-degree-by-a-common-summand-bound` (5 surfaced, 0 used) as
regulative because it begins "bound-", but it is a mathematical fact and is
substitutive. Excluding it, regulative use rate rises to 14/26 = **54%**. The
classifier has at least this one known false positive; the direction of the
finding is unaffected.*

**So the answer to "when do we want fine-grained retrieval" is: for
substitutive memories, and only those.** They are useful only on exact topical
match, and they are where the waste is — 46 memories consuming 129 surfacing
slots at 15% use. Regulative memories are useful *across* problem classes, so
broad surfacing is correct for them and a precision filter would be actively
harmful.

**This is the compatibility answer for df ranking, and it is a real
constraint.** A regulative memory's terms are generic *by nature* — "bound",
"descent", "frontier", "recurses", "order", "diagnose". **Rarity ranking
applied globally would push exactly the highest-value memories in the corpus
down the ladder.** df ranking is compatible if applied to the substitutive
lane; applied globally it suppresses the 45–54% band to fix the 15% band.

**Second constraint: do not ship df ranking without the `used-ids` repair.**
The breadth rung is justified as buying a measurement. Precision ranking
reduces the marginal offers that produce use/decline signal — which is the
loop's learning input. Fixing precision while the benefit remains unrecorded
(~16%) would optimise away the exploration *and* still not observe it. The two
repairs are a pair, and the measurement one should land first.

### Two hypotheses tested and REFUTED — recorded so they are not retried

I first supposed a "sticky memory" mechanism: a few memories matching
everything and crowding out good ones. **Both tests refute it.**

- *Frequency does not predict usefulness.* Pearson r between a memory's
  surfacing count and its per-memory use rate: **+0.002** (n = 77). My initial
  read of an inverse relationship came from eyeballing the head of the
  distribution and was wrong.
- *Topical dispersion does not either.* Never-used memories span **2.4**
  distinct problems on average; ever-used memories span **2.8** — slightly the
  wrong way for the hypothesis.

What *is* stark: **52 of 77 memories (68%) were never used in any of the 45
dispatches, and they consume 131 of 211 surfacing slots (62%).** That is not a
ranking failure inside a good candidate set; most of what is offered has no
consumer in this workload. *Caveat: "never used in these 45" is not "never
useful" — with ≤5 slots per dispatch a memory needs luck to be used at all.*

**Not testable with what we hold:** whether surfacing beats random selection.
Usefulness is a property of the (memory, problem) pair and is not
counterfactually observable, so no permutation null is available — unlike the
λ₂ study, where degree-preserving rewiring gave one.

## Coverage and honest limits

**Coverage is 66%, and the shortfall is not random.**

| | n |
|---|---:|
| true declines (surfaced − used) | **168** |
| coded decline-mentions | 94 (56%) |
| + residue | 111 (66%) |

The gap is **collective declines**: "The five supplied memories concerned
weak convergence, Hurwitz, convergence in measure, or Schwarz rigidity; none
applied" is one line declining five memories. Per-line parsing counts it
once. **The bias has a known direction** — bulk declines are almost always
"all unrelated", so the undercount falls disproportionately on topical
mismatch, and its true share exceeds 65%.

**The coding is lexical, not semantic.** The runners write to a near-template,
which makes stereotyped grounds reliably codable and the result reproducible —
but a classifier keying on phrases is not a reader. Stated as a limitation
rather than defended.

**Residue: 17 mentions, not forced into categories.** Four types:

| type | n | example |
|---|---:|---|
| bare decline, no ground stated | 4 | "not directly applicable." ×3 |
| topic named without contrast | 5 | "concerns Fatou's lemma and `L²`" |
| genuine pattern miss | 6 | "the failure is an ill-typed measure/domain pairing, not an instance diamond" |
| parser artifact | 2 | a section header; a collective decline |

The first two are findings about **report quality**, not coding failure. A
decline with no stated ground carries no information; a decline that names the
memory's topic without saying why it does not fit leaves the inference to the
reader. Together that is 9 of 111 accounted mentions where the runner declined
without articulating a ground.

Patterns were **not** tuned to shrink the residue, per the pre-registered
protocol: a category emerging because the first pass did not fit the argument
would be fitting.

## What this does to V2's headline

V2-3 was to be "a taxonomy of why a working solver declines reviewed
memories". It remains that, but the shape of the answer is not the
interesting-discrimination story the five pre-registered categories
anticipated. **Two-thirds of declines say the retrieval was simply
off-topic.** The paper should lead with that, and treat the discriminating
tail — where a runner reasons carefully about a nearby memory — as the
smaller, more interesting finding it is.

## Scope

One corpus, one runner model (all codex), one day. 45 dispatches. Categories,
not validated frequencies: the counts describe this corpus and are not
offered as a rate for memory systems generally.
