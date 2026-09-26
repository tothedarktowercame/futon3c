# Catching Our Own Instruments: Construct Validity, Self-Applied, in a Deployed Agent Memory

*Previous title — "Instruments That Inherit Their Subject's Defects" — claimed
as a finding what Cronbach & Meehl established in 1955. The related-work pass
(§9) is what caught it.*

**Draft 2, 2026-08-01.** Successor to `retrieval-whitepaper.md` (V1, frozen).
Every number traces to a frozen artifact under
`holes/labs/M-memory-retrieval/` or `holes/labs/E-futon-memories/`; the
programme that produced them is
`holes/excursions/E-memory-whitepaper-v2-programme.md`, and the residuals it
deliberately does not address are banked in `E-memory-v3-staging.md`.

**Known residuals, all deferred to V3:** the ablation
that would make §4.4's counterfactual runnable (staging §H, specced and
costed); the reachability index that would turn §5.1's five instances into a
rate (§B1); dispatch-time seed capture, without which no historical replay is
possible (§B2); and a `:memory-use/kind` field, the minimal structural
representation of the substitutive/regulative distinction this draft keeps
running into (§B4).

**Related work is at §8, and it changed the paper.** The deep-research pass
(`retrieval-whitepaper-v2-related-work.md`, futon3c `4e56e77a`) found the §1.1
thesis to be a **restatement of construct validity** — Cronbach & Meehl (1955)
— and the obvious label for it already taken with a different meaning. The
spec asked for exactly that finding in preference to a bibliography confirming
our originality, and got it. The title and §1.1 are rewritten accordingly; §10
lists what remains asserted on our own authority.

**Two sections carry editorial risk and are flagged rather than quietly kept.**
§7.3 reports twelve defects found in this paper's own experimental apparatus;
§7.4 reports our review process as a finding. Both are written by a party to
the process they describe. **The related-work pass makes them harder to cut
than they were**, since §7 is now where the paper's own contribution sits — but
that judgement should still be made by someone who was not in the room.

---

## Abstract

A deployed agent-memory system supplies retrieved memories to LLM runners
formalising mathematics in Lean. A first report established its architecture,
trust boundaries, and pre-repair baseline. This paper reports what
happened when we tried to measure the system's *behaviour* rather than its
structure, across four preregistered experiments on a frozen corpus of 129
dispatches.

**Three of four preregistered expectations were wrong, and the wrongness is the
result.** Query-term rarity does not predict retrieval failure — the empty-recall
rate is a U-curve in document frequency (71.4% / 44.4% / 74.4%, rare versus
common p = 0.618), falsifying a mechanism documented in our own source. Pairwise
co-occurrence predicts failure more strongly (24.4 points against 2.9,
p = 0.0172) but **with the opposite sign** to the mechanism proposed for it. A
spectral admissibility criterion proves *informative* — the real memory graph
sits ~15 SD below a degree-preserving null — while its threshold is **inverted**
across a 59× range in hyperedge count (51 → 3,014). Of memories a working solver declined,
65% were declined as simply off-topic; the discriminating reasons are a 12%
tail. Of memories it used, 38% were adjudicated load-bearing.

Our organising claim concerns the instruments as much as the system: **each
measurement apparatus recorded an intrinsic property of an
artifact, while the phenomenon proved to be a relation between artifacts.** A
field recording *whether* a memory was used cannot express memories that change
*what a runner does* — yet five of seven observed use-modes are regulative
rather than substitutive. A criterion scoring a graph's connectivity cannot
rank artifacts whose reachability depends on the route being attempted. A rule
preferring distinctive terms drives queries into the band where a sparse corpus
has nothing to offer. Each failure relocates the phenomenon beyond the
instrument's reach; we do not claim to have reached the last level. The final
relocation — from the lexical stage to the attachment layer — is a conjecture
with three converging measurements and no direct test.

Two findings concern method. **Four inherited denominators
failed on re-counting** (129 → 7, 121 → 45, ~73 → 10, ~360 → ~48), every one
caught by counting rather than by the audit intended to catch it. And **our own
adjudication rubric reproduced the exact defect we had already diagnosed in the
system's instrumentation**, assuming memories act by supplying content after we
had established that most do not. These are findings, not housekeeping: an
instrument that inherits its subject's defects is the characteristic failure
of measuring a system from inside it.

## 1. What this draft is, and what V1 established

### 1.1 The thesis

> **Every instrument we pointed at this system measured a property of a thing.
> Every phenomenon we found was a relation between things. The instruments
> failed in ways that located the relations.**

**This is not new, and saying so is the honest starting point.** It restates
construct validity: Cronbach & Meehl (1955) established that instrument and
theory are validated together and that a failed prediction may indict either —
"the instrument inherits the categories of the theory that built it," sixty
years before we rediscovered it. Jacobs & Wallach (2021) carry it into ML
evaluation, where construct mismatch is named as a direct source of measured
harms; Freiesleben & Zezulka (2025) make the relational point about benchmarks
specifically. Nor can we borrow the obvious label: **"reflexive measurement" is
taken** (Michelson 2022) and denotes instruments that *causally affect* the data
they elicit — a different mechanism from inheriting a subject's categories.

**What is ours is not the epistemology but the self-application.**
Construct-validity theory says instruments inherit their theory's defects. It
does not, in these sources, work an example of analysts catching *their own*
instrument doing it mid-study, with the mechanism that caught it identified.
That is §7.2 and §7.3: a rubric we wrote reproducing the exact defect we had
just diagnosed in the system, and a repair gate we imposed for other reasons
turning out to be what exposed it. **Read §1.1 as a frame borrowed with
attribution, and §7 as the contribution.**

Five instances:

| instrument | measured | phenomenon turned out to be | how it failed |
|---|---|---|---|
| `used-ids` | *was this memory used* (binary, per memory) | *how* it acted — 5 of 7 use-modes are regulative, not substitutive | populated in ~16% of outcome receipts; no code writer at all |
| rejection taxonomy | why a memory was declined | dominated by topical mismatch, a property comparison; the *relational* categories are the rare tail | 65% "off-topic", 12% discriminating tail |
| load-bearing rubric (ours) | did the memory *supply content* | did it change the *trajectory* | 5 of 49 rows fell through all three categories |
| λ₂ / spectral criterion | connectivity of the artifact graph | connectivity **relative to a route** | statistic informative, threshold inverted across 59× richness |
| term rarity (DF) | distinctiveness of a term | joint instantiation of a *pair*, then of a query against the *attachment layer* | U-curve; then co-occurrence predicted with the **wrong sign** |

Each failure pushes the locus outward: from the memory, to the memory's effect
on a trajectory; from the term, to the term pair, to the query's relation to
what has reviewed attachments. **We do not claim to have reached the outermost
level.** The last relocation (§5.2) is a conjecture with three converging
measurements behind it and no direct test.

### 1.2 Relationship to V1

**V1's four trust boundaries stand** — review, warrant, attribution, witness.
This paper adds a fifth and an orthogonal axis.

**The fifth boundary: *load-bearing*, between attribution and witness.** A
memory can be honestly attributed as used and still not carry the result. V1
had one instance (receipt `e9d008be`: a memory transferred its fact faithfully
while the runner drew the opposite operational conclusion, and `used-ids`
scored that a success). §4.4 turns it into a 49-item adjudication.

**The orthogonal axis: *reachability*.** A memory can be reviewed, warranted,
attributed, witnessed **and** load-bearing, and still be unusable because the
artifact it names cannot be obtained in the consumer's context. Warrant governs
whether a memory *may* conduct; reachability governs whether its referent *can
be had*. V1's model has no place for this, and §5.1 argues it needs one.

**What changed in status:**

| | V1 | V2 |
|---|---|---|
| spectral criterion | retracted **by withdrawal** — anti-correlated with useful structure in our regime | retracted **with evidence** — three graphs, 59× richness, threshold monotone in the wrong direction (§4.5) |
| observation channel | audited, pre-repair | audited **and characterised**: the defect has a cause (no `record-outcome!` writer) and may be *biased*, not merely lossy (§4.4) |
| repairs | future work | **deliberately withheld** so the baseline survives measurement (§7.2) |

**This draft is not a post-repair report.** Every repair identified remains
unshipped, on purpose. One of them turned out to be wrong — twice, in two
different ways (§7.2) — which is the argument for the discipline.

## 2. The system as a service, and its lane clients

The memory system is a **service**. The War Machine, codex/APM lane and zai
lane are **clients**: each calls recall and conforms to its receipt contract.

**The memory system depends on each client's domain for its witness.** It
cannot supply one itself without the self-report contamination the design
exists to prevent. A lane is therefore a
*(client, witness-source)* pair:

| lane | client | witness source |
|---|---|---|
| APM | codex / zai runners | the Lean compiler |
| WM | WM strategic selection | WM's external adjudicator |

**Decision-keying is the memory system's interface requirement, not a lane
feature**: any lane whose outcomes count must supply a decision-keyed
independent check. Two receipt kinds
(`:algorithmic-selection`, `:agent-attribution`) join one witness kind through
a decision id.

This architecture has two instances, only one with substantial data; it is not
a validated generalisation.

## 3. Method: the programme, and why it was gated

Four experiments against data already in hand, each preregistered before
running, with repairs **deliberately withheld until all four were frozen**.

**Derivation before execution.** An inherited plan listed eight experiments.
Deriving them from the claims they support showed **five were not load-bearing
for anything the paper says**; two were dropped and one deferred. The dropped
Ψ-replay would have reported a below-calibration n = 20 attached to no claim.
*Deriving experiments from claims removed more work than it added.*

**Preregistration.** Each experiment fixed its hypotheses, test,
**falsifier**, and its expected direction in writing before the data were
touched. Appendix B records all four with outcomes. Three were wrong.

**Frozen artifacts.** Every input is write-once and byte-reproducible; hashes
are in Appendix A. Two independent agents and one operator-side check
reproduced the corpus hash `0cc527e2…`.

**Separation of roles.** For the adjudication (§4.4), the assembler,
adjudicator and the analyst were three different agents on two machines. The
analyst — who had already coded the corpus for §4.1 and therefore carried
priors — was excluded from assembly and adjudication by construction. The
candidate file was audited for evaluative field names, ordering, and
commentary inside values.

**Adversarial review.** Every experiment was reviewed by re-running it: probes
re-issued, hashes recomputed, suites re-run, and in two cases a guard
**mutation-tested** to confirm its test was non-vacuous. Four dispatched
packets were refused or corrected by the receiving agent; three refusals
improved the design. §7.3 reports the division of labour and its asymmetry.

**Gating.** Repairs were blocked until all measurements were frozen because a
mid-programme repair erases the number it repairs. §7.2 reports what that
caught.

## 4. Results

### 4.1 Why reviewed memories are declined — the taxonomy

**211 memories surfaced across 45 dispatches; 43 recorded as used.** A use rate
of 20% — **and it is a floor, not a measurement** (§4.4).

| category | n | share |
|---|---:|---:|
| topical mismatch | **61** | **65%** |
| precondition absent | 16 | 17% |
| scope mismatch | 6 | 6% |
| stage mismatch | 4 | 4% |
| relevance without applicability | 4 | 4% |
| subsumption | 3 | 3% |
| **discoverability** | **0** | **0%** |

**The modal decline is a retrieval failure, not a judgement**, and its true
share exceeds 65%. Coverage is 66% (94 coded + 17 residue against 168 true
declines), with a *directional* shortfall: collective declines ("the five
supplied memories… none applied") are almost always "all unrelated", so the
undercount falls on topical mismatch.

**The discriminating categories are a 12% tail.** Subsumption, stage mismatch
and relevance-without-applicability are 11 of 94: well-articulated evidence of
a runner reasoning about a genuinely nearby memory, but rare.

**Discoverability scored zero, and the zero is the finding.** We predicted
structurally that this mode is invisible: every receipt field is closed over
the *offered* set, so an artifact never offered appears nowhere. Coding 94
declines returned 0 instances. It surfaces instead as a **silence** — 40% of
dispatches report that error-time recall returned no memory IDs, then carry an
error→fix log of problems solved from scratch.

### 4.2 Two lexical mechanisms, both falsified

**Neither rarity nor co-occurrence explains recall-emptiness, and their failure
modes disagree.**

**Marginal document frequency (P1, n = 126).** Preregistered: empty rate rises
with rarity of the rarest term — the mechanism our own source documents at
n = 5 (`dispatch_with_recall.clj:288–298`).

| band | DF | n | empty |
|---|---|---:|---:|
| rare | ≤ 11 | 42 | 71.4% |
| middle | 12–56 | **45** | **44.4%** |
| common | > 56 | 39 | 74.4% |

**Non-monotonic — a U-curve.** rare − common = −2.9 points, p = 0.618. The
middle bucket is the *largest*, so this is not a small-sample artifact. The
preregistered mechanism, also written in our own code, is **FALSIFIED**.

**Pairwise co-occurrence (P1b, n = 126).** Preregistered: low joint
instantiation predicts emptiness.

| min pair co-occurrence | n | empty |
|---|---:|---:|
| low (1–2) | 48 | 54.2% |
| middle | 36 | 55.6% |
| **high (58–143)** | 42 | **78.6%** |

**FALSIFIED, and backwards**: high co-occurrence means empty (one-sided
p = 0.9924). Co-occurrence separates far better than DF — 24.4 points against
2.9, two-sided p = 0.0172 — but with the wrong sign, so it predicts without
explaining. Query length is not a confound: **119 of 126 dispatches have
exactly 8 terms** and mean terms per band are 8.52 / 8.02 / 8.00.

#### 4.2.1 The distinctiveness heuristic, and why it fails here

The instinct the U-curve falsifies is that **rarity is discriminative power**.
Our own source states it: *"a common term discriminates weakly, an absent one
not at all."* It is the right instinct for the problem it was built for, and
wrong here for three reasons of increasing interest.

**The heuristic has a literature, and we are arguing against its edge case
rather than against it.** IDF originates with Sparck Jones (1972) and is
justified probabilistically rather than information-theoretically (Robertson
2004). The directly relevant prior art is **Weeber, Vos & Baayen (2000)**, who
argue that a-priori discarding of low-frequency terms is unwarranted — 68.3% of
their target terms had frequency below five — *and* show formally that
rare-informative terms become mathematically indistinguishable from rare-noise,
a hapax legomenon attaining maximum mutual information by construction. That is
our "rare and absent converge", stated twenty-six years earlier.

**Our increment is an extension of Weeber and should be read as one.** Their
rare terms are *present but rare*, and they still recommend discarding true
hapax legomena as unextractable. In a corpus of 77 memories over ~1,900 indexed
items the rare term frequently has **no referent at all**: rarity collapses not
into noisiness but into absence, and the U-curve's left arm is a coverage
failure rather than a discrimination failure.

**Two further differences.** Distinctiveness scoring **characterises** a
document you already hold; we **retrieve** one we do not. And it scores a
*single* term, where our failure is **conjunctive** — which is what the right
arm of the U-curve records: terms individually plentiful that do not co-occur.

*Amazon's "Statistically Improbable Phrases" are the folk version of this
instinct and appear here as folklore only. No archival or peer-reviewed source
exists for them — product documentation and an encyclopaedia entry — and §10
records that they are not admitted as literature.*

### 4.3 Structural sensitivity on the real graph

**Table caption — CURRENT-GRAPH STRUCTURAL SENSITIVITY, NOT HISTORICAL REPLAY.**
*Without dispatch-time snapshots this measures the reviewed memory graph at
capture time, not as it stood at dispatch. Lexical index `as-of`
2026-07-31T04:44:43Z; graph capture 2026-08-01T09:20:02Z.*

| perturbation | usable | forks | changed | fraction |
|---|---:|---:|---:|---:|
| remove one reviewed memory edge | 10 | 347 | 50 | **14.4%** (range 0.082–0.556) |
| remove one memory→pattern role | 10 | 347 | 7 | 2.0% |
| remove content arm | 10 | 10 | 10 | **100%** |
| remove pattern arm | 10 | 10 | 10 | **100%** |

**Both arms are ranking-critical.** *We avoid "load-bearing" here: §4.4 uses it
in a different and stronger sense — that removing a memory would change the
**outcome**. The claim in this section is only that removing an arm changes the
**ordered top-five**.* Removing either the content arm or the pattern
arm changes the ordered top-five for *every* usable problem; neither is
redundant. This is the structural result V1 wanted and could not obtain at
n = 2.

**The honest n is 10.** The
corpus holds **92** distinct problems with offered receipts (not the ~73 the
plan assumed); 2 have no recorded query; and **80 of the remaining 90 produced
an empty candidate baseline** under V1's own lexical-proposal +
reviewed-pattern-projection operator. Those 80 are retained in the frozen
artifact and excluded from damage fractions rather than scored as zero damage:
*an empty candidate list cannot identify the effect of deleting an edge.*

The 14.4% aggregate conceals a per-problem range of 0.082–0.556. This is
descriptive sensitivity, not an outcome-lift claim.

**The 80/90 is not the same measurement as the 64% recall-empty rate** and must
not be reported as a worsening of it: V1's sweep operator lacks the live path's
3-term → pairs → singles ladder. Different operators, incomparable numbers.

### 4.4 Load-bearing use, and a rubric that partly failed

**17 load-bearing / 21 corroborative / 5 unclassifiable / 2 incidental /
4 uncertain. Load-bearing = 17/45 callable = 38%.**

Five rows changed *what the runner did* without changing *what was achieved*:
"kept API reconnaissance bounded once the gap was confirmed", "searched
components first, then Zulip; arXiv was unnecessary", "improved the route over
covering-space lifting". They were neither load-bearing, corroborative, nor
incidental.

**Our rubric assumed memories act by supplying content — the substitutive
model — after we had established that five of seven observed use-modes are
regulative.** The adjudication vocabulary reproduced the defect already
diagnosed in `used-ids`, one level up: §1.1's thesis inside our own method.

**The prose-only asymmetry.** Six uses appear in runner prose while the
structured field records zero. Those six are **4/6 = 67% load-bearing** against
**13/43 = 30%** for the uses the field kept — so the defect may be *biased*
toward dropping the most consequential uses, not merely lossy. The adjudicator
declared partial non-blindness unprompted (4 of 49 rows from jobs they ran);
quantified, it runs *against* the finding — the blind subset is 2/2. **n = 6,
so a signal to instrument for, not a result.**

**The counterfactual is unrunnable.** A dispatch cannot be
re-executed with a memory withheld, so every load-bearing verdict is a
judgement of plausible causal contribution from the runner's verbatim.

### 4.5 The spectral criterion: informative statistic, inverted threshold

V1 retracted a spectral admissibility criterion (λ₂ > 0.1) after finding it
anti-correlated with useful structure. That retraction was a *withdrawal*: the
deployed graph's largest component **is a single hyperedge**, which attains
λ₂ = 1.0 by construction, so the question of whether λ₂ is *ever* informative
could not be asked. More dispatching cannot fix this because the corpus grows
as star-forests and is closed under adding memories.

We built a non-degenerate graph from an unrelated source: **1,828
commits of our own version-control history, 3,014 typed hyperedges, 14,876
incidences**, at a pinned revision, using the degree-normalised hypergraph
Laplacian. We did not use clique expansion: a file touched by *k* commits is
one incidence relation, not k(k−1)/2 pairwise ones; that error had already
inverted a metric for us once.

**Preregistered expectation: the real λ₂ would fall *inside* a
degree-preserving configuration-model null**, i.e. merely restate the degree
sequence. **Disconfirmed.** It sits **~15 SD below** the null mean
(0.035995 against 0.371910 ± 0.022643; 200 rewirings). The normalised and
unnormalised operators agree in direction — unlike V1, where they disagreed.
**λ₂ detects real wiring structure.**

But across the same Zhou operator, and therefore directly comparable:

| graph | hyperedges | λ₂ | vs 0.1 floor |
|---|---:|---:|---|
| deployed memories, patterns only | **1** | **1.0000** | passes |
| deployed + subjects | 51 | 0.0689 | fails |
| git history | **3,014** | **0.0360** | fails |

Monotone decreasing in richness across 59×. **The threshold is inverted on these three graphs**, now
on three consistent points rather than one retraction.

## 4.6 The other side: what is stored, and how

*This paper has so far been about retrieval. That is half the system, and the
half we chose to measure. The memory model itself was developed incrementally
rather than designed, and if the experiments presuppose a badly constructed
store they are correspondingly less informative. This section states what the
store actually is.*

**The apparent problem.** The deployed corpus is a **forest of stars**: each
memory attaches to exactly one pattern, patterns carry many memories, and the
largest component of the patterns-only projection is a **single hyperedge**.
That topology is why V1's spectral criterion could not be evaluated at all — a
single hyperedge attains λ₂ = 1.0 by construction — and it is closed under
adding memories, so more dispatching cannot repair it.

**The apparent contradiction.** A badly constructed store should not be useful,
and this one demonstrably is. 38% of used memories were adjudicated
load-bearing; regulative memories are used at 45–54%; individual memories
supplied an interface bridge that had blocked every API call, fixed the
decomposition of a 484-line proof before any step was attempted, and — in one
case — corrected a defect in the *supervising* agent's own statement repair,
with a machine-checked refutation attached.

**The resolution, and it is a fact about the code rather than an
interpretation.** `review-attachment!` takes `pattern-ids` as a **vector,
required non-empty, every element a pattern id**. *Multi-attachment is fully
representable.* The star-forest is therefore **not a representational limit but
an artefact of use**: we have been writing one pattern per memory where the
schema has always accepted many.

So the store is:

| dimension | status |
|---|---|
| **schema** | well-constructed — expresses a general hypergraph |
| **content** | well-curated — individually useful memories, evidenced above |
| **graph** | **essentially unbuilt** — the edges that would make it a graph were never written |

**This reframes every structural result in the paper.** We did not measure a
badly designed graph; we measured a graph that was never populated. λ₂'s
threshold inverting across richness, the pattern arm's contribution, the 62% of
surfacing slots consumed by memories used nowhere, the 80-of-90 empty candidate
baselines — all are consistent with a system whose value currently resides in
the *quality of individual entries* and not in the *relations between them*.

**And it makes the experiments more informative, not less.** A structural
result on an unpopulated structure is a finding about population, provided it
is labelled as one. What would be uninformative is to report these as
properties of the design.

**The test was registered, run, and returned against the hypothesis — with a
coverage problem that limits what it settles.**

E1 measured, per surfaced memory, which arm delivered it. The preregistration
was written before this section was drafted and named `patternArmMarginal` and
`patternArmSilent` as pre-committed outcomes, so a "works as a list" result
could not have been fitted after the fact. The rule returned
**`patternArmSubstantial`**: 67 pattern surfacings against 82 content-match, a
**44.97% pattern share** against a threshold of 25% fixed in advance.

**So the pattern arm is not marginal, and the "well-curated list" reading is
wrong for the period measured.**

**But the period measured is a tail, not a sample.** Arm attribution is
recorded on **30 of 129 dispatches**, and those 30 are not scattered:
`surfacing-via` is populated **only from 2026-07-30T21 onward**, with zero
attributed dispatches in the preceding six days. The instrument covers the
final seven hours of the corpus.

| | n |
|---|---:|
| offered dispatches | 129 |
| **with arm attribution** | **30** (all after 2026-07-30T21) |
| recall-empty | 82 |
| non-empty but unattributed | 17 |

Two consequences, and the second is the one that matters:

1. The classification stands for what it covers, and its scope must be stated
   as the tail rather than the corpus.
2. **The corpus-wide question is unanswerable with this field.** We cannot
   distinguish *"the pattern arm always contributed ~45%"* from *"the pattern
   arm became substantial as attachments accumulated"* — precisely because the
   instrument begins where the interesting comparison would start. If the graph
   was being populated over the same period, a late-window measurement is
   exactly where the pattern arm would look healthiest.

The star-forest topology reported above is unaffected: it is read from graph
structure, not from this field. What changes is that **the "list, not graph"
reading is contradicted for the tail and untested for the corpus** — a weaker
and more honest position than either the hypothesis or its refutation.

**This is the third field to fail the same way** — `:rejection-reasons`
non-empty on 7 of 129, `used-ids` on ~16% of outcome receipts, `surfacing-via`
on 30 of 129 and only latterly. In each case the populated subset is a
recency-biased tail rather than a random sample, and in each case the field's
absence was discovered while analysing it rather than by the audit meant to
find it (§7.1).

*Editorial note: this subsection previously argued that the store functions as
a well-curated list rather than a graph, on the strength of the star-forest
topology. E1 was designed to test that and returned against it. The argument
has been replaced rather than softened, and the original prediction is recorded
above so the reversal is legible.*

*Open, and not addressed here: whether one-pattern-per-memory was a considered
choice or an unexamined default, and what the retrieval consequences of
multi-attachment would be. Both belong to the storage side of the programme,
which this paper opens and does not close.*

## 5. What the failures locate

### 5.1 Reachable ≠ retrievable

**The corpus indexes advice, not artifacts.** Every memory examined is a
pattern, caution, route, or stopping rule — prose shaping how a runner
proceeds. **None is a proved lemma you can import.** A runner can be
correctly told "use the L² translation continuity approach" and remain unable
to obtain `eLpNorm_translation_tendsto`. Retrieval surfaces the *idea* and
cannot surface the *artifact*.

Five instances were observed in a single day, **two of them produced by the
loop that day and stranded on arrival** — one problem proved a lemma while a
sibling sat blocked on exactly it.

**A natural experiment bounds what reachability alone is worth.** Two modules
lacked a build-system stanza, placing their proved lemmas off the module path
entirely. After the stanza was added, **within 80 minutes two different
problems consumed them.** One consumption was unaided: the runner's own query
found the lemma, whose module name appears nowhere in its dispatch packet.

**But it was found by repository grep, not by recall.** Recall completed on
that dispatch and contributed two used memories; it simply had no entry for the
lemma. So:

> **"Reachable" and "retrievable" are two axes, not one.** Repairing
> reachability did not make the memory system find the lemma; it let a
> *different channel* find it. The failures are independent, and fixing the
> first exposed the second.

**This is an accidental baseline comparison.** "Grep beat recall" is a fair
comparison of outcomes but not inputs: grep searched the repository's full
text, while recall can only offer what the corpus was given. The defensible
claim is narrower: *on a dispatch where recall completed and
contributed two used memories, it still could not offer the proved lemma that
removed the blocker, because no such entry exists to be ranked.* n = 1.

**The mode is structurally invisible to our instruments.** Every receipt
field is closed over the *offered* set, so an artifact never offered appears
nowhere; coding 94 declines returned **zero** instances (§4.1). It is
detectable only as a silence — 40% of dispatches report that error-time recall
returned nothing — so five instances were noticed in passing rather than
measured. Measuring it needs a different instrument: an index over
proved artifacts, not a better ranker.

### 5.2 The bottleneck may not be lexical at all

Two lexical mechanisms failed. Three independent measurements point past the
lexical stage to the **attachment layer**: 80/90 queries produced no candidate
baseline under a *lexical-proposal + reviewed-pattern-projection* operator;
62% of surfacing slots go to memories used nowhere; and 19 runner reports
describe a pattern surfacing with **no reviewed memory attachments behind it**.
Recall is query → text match → pattern endpoint → attached memories, and
"empty" means no *memories* surfaced, not no text matched.

**Conjecture, not result.** It is the first account that fits both halves of
the λ₂ finding and both lexical falsifications, and it has not been directly
tested.

## 6. Threats to validity

**Scope.** One corpus, one runner model (all codex), one domain (Lean
formalisation), and a measurement window of days. No claim is offered as a
rate for agent-memory systems generally; categories may transfer, frequencies
should not be assumed to.

**The counterfactual is unrunnable.** §4.4's load-bearing verdicts cannot be
tested: a dispatch cannot be re-executed with a memory withheld. Every verdict
is a judgement of plausible causal contribution from the runner's own prose.
This is the single largest threat to the paper's most quotable number, and it
is why §4.4's 38% is presented as an adjudication rather than a measurement.
V3 §H specifies an ablation that would settle it.

**Small n at the points that matter most.** The structural sweep is n = 10
usable problems with per-problem variation of 0.082–0.556. The prose-only
asymmetry rests on n = 6, of which the fully blind subset is **n = 2**. The
reachability finding is five instances noticed in passing and one natural
experiment at n = 1. These are signals to instrument for, not results.

**Coding is lexical, not semantic.** §4.1's deterministic classifier operates
on stereotyped runner phrasing. It is reproducible and auditable, but not a
reader. Coverage is **66%**,
and the shortfall is *directional* — collective declines compress many memories
into one line and are almost always "all unrelated", so topical mismatch is
undercounted.

**The adjudicator was not fully blind, and said so unprompted.** 4 of 49 rows came
from dispatches the adjudicator had run. All 4 are in the prose-only group
carrying the asymmetry. Quantified, the non-blindness runs *against* the
finding (blind subset 2/2 versus non-blind 2/4), but the check itself rests on
n = 2.

**One measurement is post-hoc contaminated, by its author's own account.** A
route-vocabulary comparison — not reported in this paper, but recorded in the
programme's staging notes (`E-memory-v3-staging.md` §G6) and cited here because
it motivated §4.2's co-occurrence test — selected term pairs while already
knowing the target lemma, exactly the procedure that would manufacture the
result. It is suggestive only, and nothing in this paper rests on it. The blind
analogue
(P1b) was run on queries recorded before the answers were known, and it
*falsified* the hypothesis, which is the appropriate corrective.

**Current-graph, not dispatch-time.** §4.3 measures the graph as captured, not
as it stood at dispatch. Graph state is reconstructible to 2026-07-25 with
valid-time ≡ system-time, but **the lexical index has no temporal capability at
all** (a SQLite FTS5 sidecar; `system-as-of` is silently ignored), so the
retrieval *seed* cannot be replayed. This is why no historical replay is
offered.

**Our instruments are the subject.** §7's methodological findings are not
incidental threats but the paper's thesis applied reflexively: an instrument
built from inside a system inherits that system's categories. We have no reason
to think this draft escaped it, only that we caught two instances.

## 7. Methodological findings

### 7.1 Four inherited denominators failed on re-counting

| claimed | actual |
|---|---|
| 129 rejection-reasons | **7** non-empty |
| 121 codeable reports | **45** with memories surfaced |
| ~73 sweep problems | **10** usable of 92 present |
| ~360 queue headroom | ~48 |

Every one was caught by re-counting, none by the audit meant to catch it. We
report the pattern rather than only the corrected figures.

### 7.2 Withholding repairs was load-bearing

Three repairs were queued and gated behind the measurements. The gate caught
two that the data says are wrong — one of them twice. Porting the
"keep the rarest terms" rule would have driven queries into the **71.4%-empty**
band while appearing principled, justified by a comment in our own source.

### 7.3 Twelve instrument defects, none in the data

Two follow-on experiments were specified in a typed preregistration facility
and reviewed by re-running rather than by reading. Across fourteen review
passes, **every defect found was in an instrument; none was in the data.** The
count is less interesting than the two shapes it falls into.

**Shape one: an absent quantity acting as evidence.** Five instances, in
descending order of how well hidden they were.

| where | what absence produced |
|---|---|
| the receipt channel | a "discoverability" decline category scoring **0**, because a memory never offered appears in no field |
| an empty experimental trace | zero runs → every arm total 0 → the rule fell through to `rubricUnsupported`, i.e. **evidence against the hypothesis** |
| a missing cell in a paired comparison | absent data read as "not harder", i.e. negative evidence |
| an isolation probe | an **unauthenticated `sudo`** counted as a clean permission denial |
| an exact sign test | `signTestPasses 12 13` returning **true** — more successes than trials — because an empty binomial tail sums to zero |

**Shape two: semantics that existed only in a comment.** Three fields in one
file were documented as carrying an invariant and enforced by nothing:
`withheldMemory`, `sessionId`, `baseRevision`. Each admitted a trace that
respectively ablated nothing at all, ran every arm in one shared session, or
worked at post-solution revisions — and each such trace reached a **substantive
verdict**. The first is the sharpest: *an ablation study that would have
validated its rubric without performing an ablation.*

**What the recurrence means.** Each was fixed by adding an observable and a
guard. That the same failure recurred three times in one file is the finding:
guard-per-invariant is a discipline that depends on remembering, and the whole
purpose of a typed registration is to not depend on remembering. The remedy is
structural — validate once into a type that `classify` alone accepts, so an
unvalidated trace cannot typecheck into a verdict.

**Two of the defects are worth reporting for their own sake**, because they
would have produced confident wrong numbers rather than crashes:

- **pseudoreplication.** Three seeds per problem were about to be counted as
  three independent observations of a rubric whose unit is the problem —
  trebling *n* and making a null look significant.
- **a tie scored as a loss.** Equal attempts under two arms were encoded as
  `some false`, a *win for the comparison arm*, systematically biasing the
  test against the hypothesis it existed to examine.

**And one correction that is not a defect but a diagnosis.** A coverage check
written specifically to reject a recency-biased sample **passed on that
sample**, because it measured ordinal position where the bias was temporal:
the first attributed dispatch sat at index 71 of 129, while covering 6.6% of
elapsed time. `check_sound` guarantees a check never passes a trace violating
its stated claim; it cannot tell you the claim is the wrong one. **Soundness is
not adequacy.**

*This section is §1.1's thesis applied to this paper's own apparatus, and it
is the strongest evidence for that thesis we have — because here we know what
the instruments were supposed to measure, and can therefore see precisely how
each measured something else.*

### 7.4 Adversarial division of labour

Two agents exchanged **four load-bearing corrections in two rounds**: an
ease-hypothesis about why certain memories fail, refuted by pulling all five
instances and finding them over-specified rather than over-general; a
three-level framing, withdrawn as the weaker claim; a prediction that changing
proof route moves the document-frequency band, falsified by measurement; and a
scoreboard framing corrected from "grep beat recall" to the narrower claim that
survives the inputs objection.

**Three ran from analyst to instance-generator, one back.** The asymmetry is
structural rather than personal: one party runs the loop and produces
instances, the other holds the frozen corpus and the analysis tools, and
neither half tests itself. It is reported here because it is **the one claim
this paper can support from its own process rather than from its corpus** —
and reported with the asymmetry visible, at the instance-generator's own
insistence.

*We note the obvious risk: a section arguing that adversarial review works,
written by one of the two parties, is not independent evidence that it does.*

## 8. Related work

**Measurement theory and the instrument that inherits its subject.** The
observation organising this paper — that an instrument built inside a system
inherits that system's categories, and so cannot register what the system
cannot express — is a specialisation of construct validity. Cronbach and Meehl
(1955) established that a psychological construct is admissible only within a
"nomological net," and that instrument and theory are confirmed or
disconfirmed together; a failed prediction may indict any link in the chain.
Jacobs and Wallach (2021) carried this into machine learning as *measurement
modelling*: unobservable constructs must be operationalised through a
measurement model whose assumptions are inherited by every downstream number,
and they attribute a range of documented fairness harms to construct/operationalisation
mismatch. Freiesleben and Zezulka (2025) apply the same lens to benchmarks,
arguing that a benchmark score measures performance *relative to* a dataset and
learning problem rather than any intrinsic capability. A separately named
concept, "reflexive measurement" (Michelson 2022), is adjacent but distinct: it
concerns instruments that *causally influence* the data they elicit, not
instruments that inherit categorical blind spots. Our contribution is not this
epistemology, which is well established, but its reflexive application within a
single study — the observation (§7) that our own adjudication rubric reproduced
the defect we had diagnosed in the system's instrumentation.

**Instrumented agent memory.** Persistent memory for LLM agents is an active
area, with architectures such as MemGPT-style summarisation and Mem0-style fact
extraction, and benchmarks (LoCoMo and successors) that score memory systems by
end-to-end task accuracy. A recent line moves from scoring memory to
*instrumenting* it. Yuan, Su and Yao (2026) separate retrieval failure from
utilization failure and report that retrieval dominates the error budget — a
result convergent with our finding that the modal decline is a retrieval
failure rather than a judgement. Srivastava (2026) proposes Causal Memory
Intervention, selecting a memory only when a controlled intervention shows it
improves the task score over a no-memory baseline and remains stable under
perturbation — a write-time counterfactual for "did the memory matter." Li et
al. (2026) attribute an answer to individual memory tokens by counterfactual
ablation. Against this backdrop, our instrumentation is not novel in aim, and
the counterfactual "did-it-matter" measurement in particular is prior art. What
distinguishes our approach is its position and its discipline: a receipt over
the *offered* set recorded in a deployed pipeline rather than a selection
criterion at write time, and a trust boundary requiring the witness to come from
an independent, decision-keyed source outside the system under study.

**Term weighting over sparse corpora.** Our lexical results restate, and then
extend, a settled result. Inverse document frequency (Sparck Jones 1972),
whose theoretical justification Robertson (2004) locates in the
probabilistic-relevance model rather than in information theory, encodes the
intuition that distinctive terms discriminate better — the very heuristic our
U-curve falsifies for a sparse corpus. The closest prior statement is Weeber,
Vos and Baayen (2000), who argue that discarding the lowest-frequency terms a
priori is unwarranted (a majority of their target terms had corpus frequency
below five) and show formally that under association measures a rare-but-informative
term becomes mathematically indistinguishable from rare noise. Our finding that
"rare" and "absent" converge is the small-corpus limit of exactly this
phenomenon: where Weeber et al. study terms that are present but rare, our
corpus is sparse enough that a rare query term frequently has no referent at
all, so distinctiveness selects for emptiness rather than for noise. Amazon's
"Statistically Improbable Phrases," which we invoke informally, is best
described as product folklore: we could locate no archival or peer-reviewed
source for it and do not treat it as literature.

**Spectral quantities on retrieval graphs.** The machinery we use — a
degree-normalised hypergraph Laplacian — is that of Zhou, Huang and Schölkopf
(2006), who generalised spectral clustering from graphs to hypergraphs and
developed the associated embedding and classification methods. That lineage,
and the wider literature on algebraic connectivity (the Fiedler value, λ₂), uses
the second eigenvalue as *clustering and embedding* machinery. Our use is
different in kind: λ₂ as an *admissibility criterion* — a gate on whether a
retrieval graph is well-formed enough to trust — and our report of a threshold
that inverts across a range of graph richness. We are not aware of prior work
using a spectral quantity as an admissibility gate, or reporting such an
inversion; we state this as an absence of located prior art rather than as a
claim of priority.

**Provenance and trust boundaries.** Our trust-boundary model, its
`(client, witness-source)` architecture, and the principle that a system cannot
witness its own outcomes sit near the provenance and attestation literatures —
the W3C PROV data model for provenance, scientific-workflow provenance, and
remote attestation, in which an external verifier rather than the device itself
vouches for state — and near separation-of-duties and non-repudiation in
security. We have not, however, located a source that states "a system cannot
witness its own outcomes" as an explicit design principle, and we present the
model as our own pending a fuller reading of these literatures.

**Retrieval for formal mathematics.** Our deployment setting — retrieval into a
Lean formalisation loop — connects to retrieval-augmented theorem proving.
LeanDojo (Yang et al. 2023) extracts premise annotations from Lean's
mathematics library and trains ReProver, a retrieval-augmented prover that
selects premises for a proof; premise selection is the retrieval problem in that
setting. Our concern is orthogonal: not premise selection accuracy but the
instrumentation of a memory service that supplies advice, and the observation
(§5.1) that the corpus indexes advice rather than the importable artifacts a
prover needs.

---

*Sourced by a deep-research pass, 2026-08-01; every reference hand-verified
against its primary source, with status recorded in
`retrieval-whitepaper-v2-related-work.md` (futon3c `4e56e77a`). Claims that
could **not** be sourced are listed in §10 and are asserted on our own
authority, not presented as novel.*

## 9. Conclusion

**What is established.** Retrieval failure in this system is not explained by
term rarity or by term co-occurrence; the first is non-monotonic, the second
predicts with the wrong sign. The spectral criterion is an informative
statistic with an inverted threshold across a 59× range in hyperedge count (51 → 3,014). Two-thirds of
declined memories are declined as off-topic, with discriminating reasons in a
12% tail. Both retrieval arms are ranking-critical on the reachable subset. The
observation channel under-records use, has no code writer for the field that
records it, and may drop the most consequential uses preferentially.

**What is not.** That the attachment layer is the true bottleneck — three
measurements converge on it and none tests it. That 38% of used memories are
load-bearing — that is an adjudication against an unrunnable counterfactual.
That any of these frequencies transfer beyond one corpus, one model, one
domain.

**What we would tell someone building one of these.** Instrument the
*relation*, not the artifact. Ask not whether a memory was used but what it
changed; not whether a term is distinctive but whether it connects; not whether
a graph is well-connected but whether it is connected *along the route being
attempted*. Every instrument we built asked the first question of each pair and
had to be rebuilt.

And: **do not repair before you measure.** One of our three queued repairs was
shown wrong twice over — first in its direction, then in the layer it targeted
— and its original form was justified by a comment in our own source. The only
reason we know is that a gate held all three until the measurements were
frozen.

## 10. Asserted on our own authority

The related-work pass found **no verified source** for four claims. They are
kept, and marked, rather than dropped or dressed as novelty:

1. **"A system cannot witness its own outcomes"** as a named design principle
   (§2). Provenance and attestation were searched at survey level only; the
   principle rhymes with separation-of-duties and non-repudiation, but nothing
   was verified. A dedicated pass is owed before this is called new.
2. **The `(client, witness-source)` architecture, the fifth trust boundary, and
   the reachability axis** (§1.2, §2, §5.1). Unsourced, plausibly novel, and
   the search did not go deep enough into provenance to say.
3. **λ₂ as an admissibility criterion, and its threshold inversion** (§4.5).
   No prior art found, and on the reviewer's assessment this is the paper's
   most likely genuine contribution — but *not found in one pass* is not *does
   not exist*, and it is asserted with that hedge.
4. **The audit-over-deployed-pipeline increment** versus write-time
   counterfactual selection (§4.4). Whether this is a real methodological
   difference is ours to argue; the literature does not settle it.

**Amazon's Statistically Improbable Phrases are not admitted as literature.**
No primary or archival source exists — product documentation and an
encyclopaedia entry only. §4.2.1 cites them as folklore, which is what they
are, and the load-bearing prior art there is Weeber et al. (2000).

## Appendix A. Frozen artifacts and hashes

| artifact | sha256 (prefix) |
|---|---|
| `receipts-export-20260731-all-authors.edn` | `0cc527e2…` (reproduced by 3 independent parties) |
| `receipts-export-20260728.edn` | `7bc57433…` |
| `damage-state-results-20260730.edn` | `554da6b6…` |
| `psi-v2-replay-results-20260728.edn` | `2b9f6e28…` |
| `cooccurrence-table-20260801.json` | `97c73d9b…` |
| `load-bearing-candidates-20260731.jsonl` | `1a4e0ee9…` |
| `adjudication-verdicts-p2-20260801.json` | `a6f87b84…` *(added 2026-08-02: the audit found the per-instance verdicts had never been frozen — recovered from the adjudicator's working file, verified against the ledger summary and the candidate set; see `holes/labs/M-memory-retrieval/adjudication-verdicts-provenance.md`)* |
| S1 corpus / results | `777e2376…` / `9636fcd7…` |
| P1b results | `2c3a8c36…` |
| P3 scale fixture / results | `4d684c1a…` / `b82cc571…` |

All byte-reproducible on re-run; each experiment states its rerun hash.

## Appendix B. Preregistrations and their outcomes

| # | preregistered expectation | outcome |
|---|---|---|
| S1 | real λ₂ falls **inside** the degree-preserving null | **DISCONFIRMED** — ~15 SD below |
| P1-H1 | empty rate rises with rarity of the rarest term | **FALSIFIED** — U-curve, p = 0.618 |
| P1-H3 | recall-empty non-uniform across problem family | **CONFIRMED** — p = 0.0004 |
| P1b-J1 | low co-occurrence predicts emptiness | **FALSIFIED, backwards** — p = 0.9924 |
| P1b-J2 | co-occurrence beats DF as a predictor | **CONFIRMED as prediction only** — p = 0.0172, wrong sign |

**Four expectations about mechanism; three wrong.** That ratio is the reason
they were written down, and the reason a fifth — the attachment-layer
conjecture — is labelled a conjecture rather than a finding.
