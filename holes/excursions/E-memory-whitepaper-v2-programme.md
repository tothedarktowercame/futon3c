# E-memory-whitepaper-v2 — the derived programme

**Opened 2026-07-31 by claude-2, at Joe's challenge** that the day's work had
drifted into hot-fixing rather than deriving. **The challenge is correct.** This
document supersedes §4 of `E-memory-whitepaper-v2-plan.md` as the executable
programme; that document remains the findings record.

## 1. Honest audit — what actually happened today

The plan committed to eight experiments, all against data in hand.

| # | experiment | status |
|---|---|---|
| V2-1 | channel re-audit | **done** |
| V2-2 | timeout mechanism + bias | **not started** |
| V2-3 | rejection taxonomy (the headline) | **done** |
| V2-4 | Ψ-v2 replay at n=20 | **not started** |
| V2-5 | D_state sweep at 73 problems | **not started** |
| V2-6 | load-bearing subsample | **not started** |
| V2-7 | floor sensitivity, multi-seed | **not started** |
| V2-8 | witness-status repair + re-audit | **not started** |

**Two of eight.** Meanwhile the day produced five Codex packets (three WM, one
projection-guard, one topology study), a new excursion, three inter-agent
exchanges, and roughly eight new sections accreted onto the plan document —
now 1,295 lines. **None of the five packets is in the programme above.**

The individual pieces were rigorous: S1 was preregistered with a null model,
positive controls and a disconfirmed expectation recorded as such; the coding
pass refused to tune its patterns and reported 34% uncovered. **The portfolio
was not.** Local rigour, global drift.

## 2. The drift mechanism, named so it can be resisted

Every finding today triggered the same reflex: *finding → propose a packet*.
`used-ids` is unreliable → propose a back-fill. Topical mismatch is 65% → port
df ranking. The projection is dark → add a flag (which was wrong anyway).

The reflex feels responsive and is the opposite of the method. It also has a
specific cost here, which is the substantive reason to stop:

> **V1 was explicitly a pre-repair baseline. V2's job is the post-diagnosis
> measurement of the same system. Every repair that lands before V2's
> measurements are frozen destroys the thing V2 measures.**

If `used-ids` is back-filled and df ranking ported now, the 16% population rate
and the 65% mismatch rate become unreproducible — and they are among V2's most
interesting numbers. **The hot-fix reflex would erase the paper's own
evidence.** That is not a stylistic worry.

## 3. Derivation — from claims to necessary experiments

*What V2 claims, and what each claim actually requires. This is the step that
was skipped: the eight experiments were inherited as a list, never derived.*

| claim | status of evidence | needs |
|---|---|---|
| **C1** The observation channel is unreliable in specific measured ways — `:rejection-reasons` 7 non-empty corpus-wide, `used-ids` ~16% of outcome receipts, error-recall silent in 40% of dispatches | held (V2-1 + coding) | **V2-2** for the recall-empty half |
| **C2** Declines are dominated by topical mismatch (65%), with a 12% discriminating tail | held (coding) | — |
| **C3** The breadth/precision trade is *designed*, and its justification is falsified by C1 — the system pays for a measurement it records one time in six | held (docstring + C1) | — |
| **C4** Use is regulative more often than substitutive, and `used-ids` cannot express it | held (claude-9's 15 rows + the 45) | — |
| **C5** Retrieval granularity should be lane-scoped: regulative 45–54% use under broad surfacing, substitutive 15% | held (coding) | — |
| **C6** Trust boundaries need a fifth (load-bearing) and an orthogonal axis (reachability) | architecture + `e9d008be` + 5 instances | **V2-6** to move load-bearing from anecdote to subsample |
| **C7** λ₂'s threshold is inverted across a 55× richness range; the statistic is informative | held (S1) | — |
| **C8** Structural sensitivity on the *real* memory graph | not held | **V2-5** — S1 measured a git graph, not this corpus |

**Result: five of the eight experiments are not load-bearing for any claim V2
makes.**

- **V2-2** — keep. Only evidence for C1's recall-empty half.
- **V2-5** — keep. The only structural result on the *actual* memory corpus;
  S1 does not substitute.
- **V2-6** — keep. Turns the fifth boundary from one receipt into a subsample.
- **V2-4** (Ψ-v2 at n=20) — **drop from V2.** No claim depends on it; it was
  inherited from the WS-series, and n=20 below calibration cannot support one.
- **V2-7** (floor multi-seed) — **defer to V3.** C5 changes what the floor
  question even means: a floor over a lane-scoped corpus is a different
  experiment, and running the old one now measures a design we intend to
  change.
- **V2-8** (witness-status repair) — **not an experiment.** It is a repair.
  See §4.

## 4. Repairs are sequenced AFTER measurement, not interleaved

Three repairs are known-good and none should land before V2's measurements are
frozen:

| repair | why it waits |
|---|---|
| `used-ids` back-fill + writer | freezes C1's 16% figure; the ids are recoverable from the 130 job results on disk and will still be recoverable next week |
| lane-scoped df ranking | freezes C2's 65% and C5's 45/15 split |
| `:witness-status` earned via `review-attachment!` | freezes the warrant-audit baseline |

**Gate: no repair to the retrieval or receipt path until V2-2, V2-5 and V2-6
are complete and their artifacts frozen.** The one exception is
claude-9's going-forward receipt template, which changes *new* rows only and
cannot alter the frozen corpus.

## 5. The programme

Three experiments, in dependency order. All against data in hand. All
preregistered before running, on the S1 pattern — which worked, and is the
standard the rest should meet.

**P1 — V2-2, recall-empty mechanism and bias.**
Input: 129 offered halves, receipt `4f7eeadd`. Fields confirmed present:
`:terms`, `:term-sources`, `:recall-query`, `:recall-status`, `:problem`,
`:memory-use/surfaced-ids`.

### P1 PREREGISTRATION — written 2026-07-31 before any analysis

**Status disclosure, stated up front:** this is a **confirmatory** test of a
mechanism already documented at n = 5, not a blind discovery.
`dispatch_with_recall.clj:288–298` records that the 3-term query returned zero
for all five rows measured on 07-30, and names `recursion` as a rare term that
floors any conjunction containing it. H1 is therefore a *prior*, not a
prediction from ignorance, and the contribution is establishing it at n = 129
with a stated falsifier.

| | hypothesis | test | falsified if |
|---|---|---|---|
| **H1** | Recall-empty probability rises with the **rarity of the rarest term** in the conjunction | bucket the 129 by min document-frequency of their `:terms` (via the `df` endpoint); compare empty rate across buckets | empty rate is flat across df buckets |
| **H2** | Dispatches reaching the **singles** rung surface *more* memories but at a *lower* use rate than those satisfied at the 3-term rung | infer the rung fired from `:recall-query` / `:term-sources`; compare surfaced counts and use rates | no difference, or the relationship reverses |
| **H3** | Recall-empty is **non-uniform across problem family** (the `aNNXNN` prefix) | permutation test over family vs empty, ≥ 1000 shuffles | the observed distribution sits inside the permutation null |

**Preregistered expectations:** H1 confirmed strongly; H2 confirmed; H3
confirmed (non-uniform). **Recording these so a null is a result and not a
disappointment** — S1 disconfirmed its own preregistration and was better for
it.

**Mandatory caveat for any H2 result:** use rates are computed from `used-ids`,
which is populated ~16% of the time (staging A1). H2's use-rate leg therefore
reports a **floor**, and must say so in the caption rather than the
limitations.

### P1 RESULT — 2026-08-01, frozen. H1 FALSIFIED, and it changes a staged repair

codex-1, commit `999d3588` (4 new files, 844 insertions, nothing modified).
Reviewed by claude-2: scope clean; frozen inputs verified unchanged, with
`receipts-export-20260731-all-authors.edn` now hashing to `0cc527e2…`
identically across **three independent agents** (codex-1's blocked run,
codex-4's P3, my own check). Exact DF taken from a copy of
`fts5-evidence.db`; the live writer was never opened.

**H1 — FALSIFIED.** Tertiles of min document-frequency over 126 analysable
dispatches (3 lack `:recall-query`):

| bucket | DF range | n | empty | empty rate |
|---|---|---:|---:|---:|
| rare | ≤ 11 | 42 | 30 | **71.4%** |
| middle | 12–56 | **45** | 20 | **44.4%** |
| common | > 56 | 39 | 29 | **74.4%** |

Decision rule was *confirmed iff rare > middle > common*. Observed is
**non-monotonic — a U-curve**: both extremes fail, the middle succeeds.
rare − common = −2.9 points, one-sided permutation **p = 0.618**: rarity and
commonness are statistically indistinguishable as predictors.

**This is not a small-bucket artifact.** The middle bucket is the *largest*
(45 of 126) and carries the lowest empty rate.

**What it falsifies is our own code's explanation.**
`dispatch_with_recall.clj:288–298` records, from n = 5, that a rare term floors
any conjunction containing it. At n = 126 rarity does not predict emptiness.
The mechanism documented in the source does not generalise, and the U-curve
implies an **optimal specificity band** rather than a monotonic preference.

**And it inverts a repair that was queued to ship.**
`codex_sorry_cron.py:415` states the rule explicitly — *"Keep the rarest terms
instead: a common term discriminates weakly, an absent one not at all"* — and
staging **A2** proposed porting exactly that to the recall ladder. **Keeping
the rarest terms drives queries into the DF ≤ 11 bucket, which is 71.4%
empty — the worst band measured.** Had A2 shipped on the strength of the
docstring, it would have made recall worse while appearing principled.

*This is the clearest vindication of the sequencing gate: the repair was
plausible, documented in our own source, and wrong. Measuring before repairing
caught it.*

#### Writeup note — the SIP analogy, and why we are not doing that

*Joe, 2026-08-01: note the analogy with Amazon's Statistically Improbable
Phrases, but it sounds like we are building something genuinely different.*

**The analogy.** Amazon's SIPs pick phrases occurring far more often in one
book than across the corpus, on the premise that **rarity is discriminative
power**. `codex_sorry_cron.py:415` states the same instinct in as many words:
*"a common term discriminates weakly, an absent one not at all."* The rule
sounds right, and it is right for the problem SIPs solve.

**Why it fails here — three differences, in increasing order of interest.**

1. **Direction.** SIPs characterise a document you already hold; we retrieve
   one we do not. Labelling is not searching.
2. **Density.** SIPs assume a corpus dense enough that a rare term still has
   referents. Ours has 77 memories over ~1,900 indexed items. **In a sparse
   corpus, "rare" and "absent" converge**: the DF ≤ 11 band is not
   *distinctive*, it is *nearly not there* — which is exactly why it is 71.4%
   empty. Rarity stops being a signal of specificity and becomes a signal of
   missing coverage.
3. **Arity — and this is the part that makes it a different problem.** SIPs
   score a *single* phrase. Our failure is **conjunctive**. That reframes both
   arms of the U:

   | band | conjecture for why it floors |
   |---|---|
   | rare (≤ 11) | the term is nearly absent, so any conjunction containing it is empty |
   | common (> 56) | terms are individually plentiful but **rarely co-occur** — three terms in ~500 documents each can still intersect in zero |
   | middle (12–56) | specific enough to be topical, common enough to co-occur |

   *Conjectural, and testable:* the middle band should show materially higher
   pairwise co-occurrence than either extreme. Recorded as a prediction, not a
   result — the U-curve is measured, this account of it is not.

**What we are actually building.** SIPs are a **term-statistics** answer: score
each term, keep the best. Our problem is a **connectivity** one: we need terms
that *connect a query to the corpus*, which is a property of the bipartite
query–corpus structure, not of any term in isolation. That is the same object
the two-axis distinction names — *reachable* versus *retrievable* (§3.2.1c of
the plan) — and the same object the Laplacian work is groping toward. A term
with ideal DF that co-occurs with nothing is useless; a mediocre term on a
well-connected path is not.

So the writeup should cite SIPs as the natural prior art and the intuition
everyone reaches for — including us, in our own source comment — and then say
plainly that **optimising term distinctiveness is the wrong objective for
retrieval over a sparse corpus, and we have the U-curve to show it.**

### P1b — CO-OCCURRENCE, the mechanism H1's falsification left open

*Derived, not reactive: P1 measured a U-curve and supplied no mechanism. V2
would otherwise report "both extremes fail, we do not know why". This tests the
one candidate mechanism on the same frozen corpus, under the same preregistration
discipline. It is P1 continued, not a new experiment.*

**Origin.** claude-9 measured a95J08's two routes (Jensen, failed ×3; Hölder,
closed first attempt) across 7,863 Mathlib files. **Marginal DF did not
separate them** — Jensen 26, ConvexOn 29, probability-measure 45 against
Hölder 21, lintegral_mul 21 — killing claude-2's prediction that the route
moves the DF band. **Co-occurrence did**: files containing both
`lintegral_mul` and `rpow` = **5**, both `Jensen` and `lintegral` = **1**, and
one of the five is `MeanInequalities.lean`, which holds the closing lemma.

So the route does not slide the marginal band; **it changes whether the query
terms are jointly instantiated in the library.**

**claude-9's own caveat is fatal to the interpretation and they said so
first:** the term pairs were chosen *post hoc, knowing the answer* — precisely
the selection procedure that manufactures this result. Also n = 1 problem,
grep-over-raw-text is not the retrieval index, and `rpow` at 177 files sits in
a different band and carries the count.

**The fix, and why this corpus can supply it: P1's 129 recorded queries were
generated blind.** They were written by runners *before* the answer was known —
no post-hoc selection is possible. So:

**P1b preregistration (written before running).**

| | hypothesis | test | falsified if |
|---|---|---|---|
| **J1** | Recall-empty is predicted by **joint instantiation** of the query's term pairs, where marginal DF failed | for each of the 126 analysable dispatches compute min pairwise co-occurrence over `:terms` from the same `fts5-evidence.db` copy; compare empty rates across co-occurrence tertiles | empty rate is flat across co-occurrence bands |
| **J2** | Co-occurrence **beats marginal DF** as a predictor | compare the two directly on the same 126; DF's benchmark is already fixed and public — non-monotonic, rare−common p = 0.618 | co-occurrence explains no more than DF |

**Preregistered expectation:** J1 confirmed, J2 confirmed. **Recording it
because our last two preregistered expectations were both wrong** (S1's null,
P1's H1), and the value came from having written them down.

**What a confirmation would license, and what it would not.** It would supply
the mechanism for P1's U-curve — common terms are plentiful but do not
co-occur — on blind data at n = 126. It would **not** establish claude-9's
route-level claim, which is n = 1 and post-hoc; it would only show that the
variable their instance points at is the right one at term level.

### P1b RESULT — 2026-08-01, lon-codex-1 on lucy-joe. J1 FALSIFIED *backwards*

Input table hash verified on arrival (`97c73d9b…f789ae12`); Python 3.12.3,
seed 20260801, 10,000 shuffles; **three byte-identical reruns**. No inputs or
known defects modified. Results stay on lucy-joe as a report — the checkouts
are divergent by design and nothing was merged.

| min pair co-occurrence | n | empty | rate |
|---|---:|---:|---:|
| low (1–2) | 48 | 26 | 54.2% |
| middle (3–56) | 36 | 20 | 55.6% |
| **high (58–143)** | 42 | 33 | **78.6%** |

**J1 — FALSIFIED, and in the opposite direction.** We predicted *low*
co-occurrence would mean empty. **High co-occurrence means empty**: low−high =
−24.4 points, one-sided p = 0.9924.

**J2 — CONFIRMED as prediction only.** Co-occurrence separates far better than
marginal DF — 24.4 points against DF's fixed 2.9, two-sided p = 0.0172 — but it
predicts with the wrong sign, so it does **not** confirm the mechanism J1
proposed. lon-codex-1's own framing, and it is the correct one.

**claude-2's review — the obvious confound is refuted.** Min-over-pairs is
mechanically lower when a query has more terms, so the bands could have been
proxying for term count. Checked on the same table:

| band | n | empty | mean terms |
|---|---:|---:|---:|
| low | 42 | 52.4% | 8.52 |
| mid | 42 | 57.1% | 8.02 |
| high | 42 | 78.6% | 8.00 |

Term counts are effectively identical across bands, and **119 of 126
dispatches have exactly 8 terms** — the corpus is near-constant in query
length, so no confound is available. My independent tertile split reproduces
the result (the low/mid difference from lon-codex-1's is tie-handling at the
boundary; the **high band is identical at 78.6%, n = 42**).

**What this does to the argument — and it is bigger than either hypothesis.**
We have now tested **two lexical mechanisms and both have failed**: marginal
rarity does not predict emptiness (U-curve, p = 0.618), and co-occurrence
predicts it with the wrong sign. **The evidence points away from the lexical
stage being the binding constraint at all.**

*Conjecture, consistent with three independent measurements but not directly
tested:* recall is query → text match → **pattern endpoint** → attached
memories, and "empty" means no *memories* surfaced, not no text matched.
High-co-occurrence term pairs are generic, so they may reach generic pattern
endpoints that carry few reviewed attachments. That would locate the bottleneck
in the **attachment layer**, and it fits: P3 found 80/90 queries produced no
candidate baseline under the *lexical-proposal + reviewed-pattern-projection*
operator; 62% of surfacing slots go to memories used nowhere; and 19 runner
reports describe a pattern surfacing with **no reviewed memory attachments
behind it** (§3.2.1b).

**Consequence for the staged repairs: A2 may be aimed at the wrong layer
entirely.** If lexical selection is not the constraint, then neither
rare-first (already shown backwards) nor mid-band term selection will move the
empty rate much. See staging A2, revised a second time.

**H2 — UNTESTABLE, correctly refused.** No receipt records which ladder rung
fired, so the singles-vs-3-term comparison cannot be made from the corpus.
codex-1 declined to substitute a current-index replay. **New instrumentation
gap** → staging B5.

**H3 — CONFIRMED.** Recall-empty is non-uniform across 73 matched problem
families; 10,000 permutations, **p = 0.0004**. The loss is structured by
problem family, not spread evenly — so it is a property of terrain coverage,
not of random query luck.

**P2 — V2-6, load-bearing subsample. DONE 2026-08-01, frozen.**
Three-way separation held: **lon-codex-2** assembled the candidate set on
lucy-joe (49 uses, full population, no sampling), **claude-9** adjudicated
against a rubric fixed in advance, **claude-2** touched neither — having
already coded this corpus for the taxonomy and carrying priors. Candidate file
`load-bearing-candidates-20260731.jsonl`, sha256 `1a4e0ee9…c788c8`, audited on
arrival for evaluative field names, evaluative ordering, and assembler
commentary inside values — all clean.

| verdict | n |
|---|---:|
| load-bearing | **17** |
| corroborative | 21 |
| **TRAJ — rubric cannot classify** | **5** |
| incidental | 2 |
| uncertain | 4 |

**Load-bearing: 17/45 callable = 38%** (17/49 = 35% counting uncertains).

**The headline is that the rubric I wrote partly failed.** Five rows change
*what the runner did* without plausibly changing *what was achieved*:
"prompted early probing of the high-risk integral assembly", "kept API
reconnaissance bounded once the gap was confirmed", "searched components first,
then Zulip; arXiv was unnecessary", "tested the assembly edges before further
leaf polishing", "improved the route over covering-space lifting". Not
load-bearing (outcome unchanged), not corroborative (they *caused* a different
action rather than confirming one), not incidental (they moved the trajectory).
claude-9 marked them **TRAJ** rather than forcing a fit.

**The rubric assumes memories act by supplying content — the substitutive
model.** I wrote it that way *after* establishing from claude-9's use-modes
that five of seven modes are regulative. **Our own measuring instrument
inherited the exact bias we were measuring in `used-ids`, one level up.** V2
must report that.

**The prose-only asymmetry.**

| source | load-bearing |
|---|---|
| **prose-only** (used-ids empty, prose reports use) | **4/6 = 67%** |
| used-ids | 13/43 = 30% |

The six uses the structured field *dropped* are twice as likely to be
load-bearing as the 43 it kept — so **A1 may not merely under-count, but be
biased toward dropping the most consequential uses.**

**Blindness check.** claude-9 declared unprompted that 4 of 49 rows come from
jobs they ran today, all prose-only — the group carrying the asymmetry.
claude-2 quantified it, and it runs *against* the finding:

| prose-only subset | load-bearing |
|---|---|
| blind (seqs 455, 467) | **2/2 = 100%** |
| non-blind (seqs 468, 471, 474) | 2/4 = 50% |

Only 2 of the 4 LB verdicts are non-blind and the *blind* subset is the
stronger one, so non-blindness does not explain the asymmetry. **But the blind
subset is n = 2.** Status: survives the obvious threat, rests on n = 6 total —
**a signal to instrument for, not a result.**

**Caveat for the paper's caption, not its limitations: the counterfactual is
unrunnable.** A dispatch cannot be re-executed with a memory withheld, so every
load-bearing verdict is a judgement of plausible causal contribution from the
runner's verbatim. claude-9 raised this unprompted.

**P3 — V2-5, D_state sweep at scale. DONE 2026-08-01, frozen.**
codex-4, commits `8e46b485` (instrument) and `4ca51a48` (frozen fixture,
result, note). Reviewed by claude-2: commits are pure additions; new-artifact
hashes match the report; frozen inputs verified unchanged — and
`receipts-export-20260731-all-authors.edn` hashes to `0cc527e2…` identically
to what codex-1 independently reported from a different packet, which is
stronger evidence than either check alone.

**The n was wrong again, and this is the fourth time this week.** I wrote "73
problems with receipts". The export holds **92** distinct problems with offered
receipts; 2 have only retroactive receipts with no query, so 90 entered
capture; **80 of those 90 produced an empty candidate baseline under V1's own
operator.** The honest sensitivity sample is **n = 10** — not 73, 90 or 92.
codex-4 retained the 80 in the frozen artifact and refused to score them as
zero-damage, which is the correct treatment: an empty candidate list cannot
identify the effect of deleting an edge.

**Table caption, mandatory and present:** *CURRENT-GRAPH STRUCTURAL
SENSITIVITY, NOT HISTORICAL REPLAY — without dispatch-time snapshots this
measures the reviewed memory graph at capture time. Lexical index `as-of`
2026-07-31T04:44:43Z; graph capture 2026-08-01T09:20:02Z.*

| perturbation | usable | forks | changed | fraction | mean Jaccard | max |
|---|---:|---:|---:|---:|---:|---:|
| remove one reviewed memory edge | 10 | 347 | 50 | **14.41%** | 0.0601 | **1.0000** |
| remove one memory→pattern role | 10 | 347 | 7 | 2.02% | 0.0152 | 0.8000 |
| remove content arm | 10 | 10 | 10 | **100%** | — | — |
| remove pattern arm | 10 | 10 | 10 | **100%** | — | — |

**What this supports (C8).** Structural damage is non-null on the real memory
graph within its reachable subset, and **both arms are load-bearing** — removing
either the content arm or the pattern arm changes the top-five for every usable
problem. Neither is redundant. That is the structural result V1 wanted and
could not get at n = 2.

**What it does not support.** n = 10 problems is small and heterogeneous: the
per-problem edge-removal fraction ranges **0.082 to 0.556**, so the 14.41%
aggregate conceals substantial variation and should be reported with the range,
not alone. This is descriptive sensitivity, not an outcome-lift claim.

**The larger finding is the reachability one.** 80/90 recorded queries returned
no candidate baseline at all. *This is not the same measurement as the live
system's 64% recall-empty and must not be reported as a worsening of it* — V1's
sweep operator is lexical-proposal + reviewed-pattern-projection, while the live
path has the 3-term → pairs → singles ladder beneath it. Different operators,
different numbers. It is consistent in shape with P1's H1 (a strict operator
floors), but it is corroboration by analogy, not evidence for it.

**Stop rule.** When P1–P3 are complete and frozen, V2 is written from what is
held. **No further experiments are added to V2 without deriving them from a
stated claim in §3.** A finding that arrives mid-programme is recorded in the
plan document and scheduled, not converted into a packet the same turn.

## 5b. P2b — MOVED TO V3 (staging §H). Kept here only as a record of a stop-rule violation.

**2026-08-01, Joe: this is V3 work; the immediate aim is to finish V2.**
Correct, and the failure is mine in a specific way worth recording: **§5's stop
rule says "a finding arriving mid-programme is recorded and scheduled, not
converted into a packet the same turn." I wrote that rule this morning and
broke it this afternoon** — P2's unrunnable-counterfactual caveat arrived and I
dispatched an experiment within the turn, dressing it as "derived" because it
followed from a stated claim. Derivation is necessary, not sufficient: the stop
rule also requires that V2 be finished first.

codex-7 stood down before spending: no dispatches run, no budget consumed, no
exclusion flag added.

**Joe's second constraint, carried to §H:** re-proving theorems is expensive,
and a clean ablation needs **genuinely fresh codex lanes** — of the ten codex
agents here, most have now touched this programme, so lane hygiene is a real
scheduling constraint for V3 rather than an afterthought.

*The design below is preserved in staging §H. Nothing further runs until V2 is
drafted.*

<details><summary>Original P2b spec (now staging §H)</summary>

*Derived, not reactive: P2 reports 38% load-bearing and states that the
counterfactual cannot be run. This makes it runnable, so it is P2 continued.
Joe approved the local-filter approach 2026-08-01 and specified a **fresh Codex
runner rather than Zai** — correct, because all 45 original dispatches were
codex, so switching model would vary model and memory simultaneously. Zai's
proper role is a later generality arm, not the primary.*

**Entry point exists.** `dispatch_with_recall.clj` takes `--problem --to --from`
plus `--limit`, `--dry-run`, `--no-receipt-ranking`, `--allow-thin`. **There is
no exclusion flag**, so the ablation needs one — default-off, experiment-only,
so it cannot contaminate ordinary dispatches.

### Stage 1 — the noise floor. Run this FIRST; nothing else is interpretable without it.

Re-dispatch the same problems with the **unmodified** corpus, K times each, and
measure how often outcomes agree. **If identical runs already disagree often, a
single ablation difference proves nothing.** This is the step most likely to be
skipped and the one that decides whether Stage 2 means anything.

- **Primary outcome: final sorry count** — a number, more sensitive and less
  noisy than the categorical outcome.
- **Secondary:** closed / blocked / partial, matching the receipts' own vocabulary.
- Report the same-corpus disagreement rate with its spread, not just a mean.

### Stage 2 — the ablation, gated on Stage 1

Ablate memories claude-9 judged **load-bearing** *and* memories judged
**incidental**, by filtering them from the surfaced set at dispatch.

| preregistered prediction | falsified if |
|---|---|
| LB ablations change the outcome **more often than the Stage-1 noise floor** | LB ablation differences sit inside the noise floor |
| IN ablations change it **no more than** the noise floor | IN ablations differ as much as LB ones |

**What confirmation would license:** P2's 38% becomes a *measured rate* rather
than a plausibility judgement, and the adjudication rubric is validated.
**What refutation would license:** the 38% is judgement only, and V2 must say
so. Either way the instrument gets tested, which no other design here can do.

**Preregistered expectation:** LB ablations exceed the floor; IN ablations do
not. Recorded before running — three of our four preregistered expectations
this week were wrong, and that is where the value came from.

**Filtering is equivalent to a different database from the runner's view**,
since the runner only ever sees what surfaced; on a star-forest graph the two
should coincide. Ship-different-databases (staging §H) remains the rigorous
check on a subset, and a disagreement between the two would itself be a
graph-effect finding.

</details>

## 6. Explicitly not in V2

Recorded so they are not silently re-adopted: V2-4; V2-7; the S2 fix→cause
benchmark (`E-futon-memories`, which answered its gating question and needs no
sequel for V2); the three repairs in §4; the reachability instrument
(§6.3 item 4b of the plan — V3); a clean Zai generality A/B (V3, and blocked on
the lane repair regardless).

## 7. What this costs

Dropping V2-4 and deferring V2-7 removes two results V2 could have reported.
Both were weakly attached to any claim, and reporting a below-calibration
n=20 replay would have been the kind of number this programme exists to avoid.
The sequencing gate delays three repairs by the length of P1–P3. That is the
price of V2 having a measurable subject.
