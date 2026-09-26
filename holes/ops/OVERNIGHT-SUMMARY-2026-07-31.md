# Overnight summary — 2026-07-30 into 07-31

Written by claude-9, ground control. **Failures and uncertainties first**, per the
contract. Live as of ~04:25; the loop is still running and this file is updated
as runs land.

**Current corpus numbers, from the sanctioned counter** (`scripts/count_sorries.sh
--corpus`, which is comment-, string- and inline-aware and is the same counter
behind your HUD):

    problems=491  with-lean=135  with-sorry=35  CLEAN=100  EXECUTABLE-SORRIES=69

(Re-run at 05:55. An earlier revision of this line said `EXECUTABLE-SORRIES=73`;
the count kept falling as the last runs landed. `CLEAN` has been stable at 100.
**Sorry count is a moving number — re-run the counter rather than quoting this
line.** I have not attributed the 73→69 drop to specific runs, because doing so
accurately needs a per-file diff I have not run, and a plausible attribution is
exactly the kind of thing that becomes a false claim in a whitepaper.)

Earlier sections of this file quote **87 clean**; that was the number several
hours ago and is superseded. Read `CLEAN=100`. I have not hand-computed a corpus
number anywhere in this document — every figure above comes from the counter,
after the a01A12 incident in which I substituted an estimate for a count.

---

## READ THIS FIRST: a problem counted CLEAN is vacuous

**`a95J03` is counted clean, and it proves nothing.** Its target
`winding_number_bounded` is stated over

    def windingNumber (_γ : ℝ → ℂ) (_z : ℂ) : ℤ := 0

— both parameters unused, returns zero unconditionally. The proof is
`rw [windingNumber]; omega`, which establishes `−N ≤ 0 ∧ 0 ≤ N`. The hypotheses
`hN` and `hz` are never used. The file's own docstring discloses the placeholder.

**No gate in this loop detects it.** `#print axioms` is clean. The statement
typechecks. The sorry count is zero. `count_sorries` calls the problem CLEAN.
Sorry-counting, axiom re-elaboration, statement-integrity diffing and the
transitive-`sorryAx` census — every check I ran or built tonight passes on a
theorem about a constant function. The only signal was a docstring, read by a
human.

**Axiom-cleanliness is not non-vacuity, and every gate we run measures the
former.** A loop that cannot tell "proved" from "proved about nothing" will drift
toward the second, because it is cheaper and scores identically.

**So read the headline as 99 genuine + 1 vacuous.** Honest mitigations: it is one
problem in 100; the scaffold author disclosed the placeholder rather than hiding
it; the file's other declarations are genuine. What is wrong is that our pipeline
counted it clean anyway. Flagged `:counts-toward-clean :disputed`,
`:decision-owner "joe"` — **not** silently reopened.

**And the reassuring part, which is now measured rather than hoped:** a95J03 was
found by a human reading a docstring, so the obvious worry was *how many more are
there*. I built the scan and ran it over the whole corpus
(`scripts/vacuity_scan.py`, 145 problem files). **Exactly one clean problem
carries a vacuity signal — a95J03, the one already known.** There is no second
vacuous clean. Among problems still carrying sorries the flagged set is `a01J06`
(`zeroCount := 0`, `hEntire : True`), `a95J04` (`hR : True`), `a95J06` (`True`
conclusions) and `a96A02` (`spikeFunction := 0`) — all four already blocked.

An independent detector — prose admitting a *generated* sorried definition was
replaced — returns the same three files and separates them usefully: a95J03 and
a96A02 substituted `:= 0`, but **a95J06 substituted Mathlib's real `cantorSet`,
which is a good repair**; its vacuity comes from elsewhere.

**Do not read that all-clear without this caveat.** The scan's first version
returned "0 flagged" across all 145 files, and it was wrong twice over: a
`def (\w+)[^:=]*:=` regex cannot match a definition with *typed binders* (they
contain `:`), and classifying files with a bare `sorry` token match picks the word
out of *prose* — a95J03's own docstring says "had a sorry in its definition", so
the single known vacuous clean was filed under "sorried" and the headline "0 clean
flagged" was itself vacuously true. **Both were caught only because I asserted the
scan against known positives before believing it.** That assertion is now
permanent in the script, and both traps are documented in its docstring.

**The check now runs BEFORE dispatch, and it has already paid twice.** After
a95J03 I made the vacuity scan *predictive* — run it as a preflight on the row
about to be dispatched, not as a post-mortem on the row just closed. Two rows
have been blocked by it since, each before a runner slot was spent:

- **`a95J06`** — two remaining targets had conclusions `∀ ε > 0, ∃ δ > 0, True`
  and literally `True`. Either closes with `trivial`.
- **`a96A02`** — the row the scan had flagged. Its conclusions are *not* `True`,
  so the first-generation check passed it; but two of its three targets are
  **about** `spikeFunction`, which is `:= 0`. Under the constant zero,
  `spikeFunction_holderWith` (`|f x − f y| ≤ A|x−y|^(1/2)`) reads `0 ≤ …` and is
  **trivially true**, while `spikeFunction_not_absolutelyContinuous`
  (`ε ≤ ∑ |f(bᵢ) − f(aᵢ)|`) reads `ε ≤ 0` and is **false** — the constant
  function is absolutely continuous. One vacuous, one unprovable, from one
  placeholder. Only `spike_height_slope_bound` (pure arithmetic, no
  `spikeFunction`) is genuine. Receipt `087f0667`;
  `:status :blocked-placeholder-subject`, `:decision-owner "joe"`.

  a96A02 also **claims a repair it did not make**: its header says the function
  is left "opaque … with a precise specification of its values", but line 49 is
  literally `def spikeFunction : ℝ → ℝ := 0` and no specification appears
  anywhere in the file. The prose describes the intended fix; the code is the
  placeholder.

**What a96A02 taught that a95J03 did not:** a placeholder subject does not only
make positive statements *trivial*, it makes negative statements *false*. So the
preflight's step 0 is now two questions, not one — is the conclusion itself
`True`, **and** does the conclusion's *subject* unfold to a bare constant?

**Note the classification:** this is a *placeholder* defect, not the
`integral_undef` one. I initially filed them together; scribe pass 32 separated
them and was right — see the taxonomy below.

## Three findings from the small hours that change what to do next

### 1. The corpus is blocked on theorems it has already proved

a97A06's remaining main target reduces to the Dirichlet sinc integral. **a02J05
already proves it, axiom-clean** — I verified rather than taking the runner's
word: a02J05 has zero executable sorries and a sixteen-theorem sinc library
including `tendsto_integral_sinc_pi_div_two` and `dirichlet_integral_improper`,
plus the whole Abel-regularization chain. a97A06 cannot reach any of it, because
**problem files are not importable modules.**

This is the *third* instance of the same structural defect tonight.
`ConstructionTargets` had it; `YoungL2` had it, I fixed it in `7958c53`, and
**that fix closed `heatConv_L2_le` in a96A04 within hours** — the first time a
piece of pure build-system plumbing discharged a mathematical declaration in this
loop.

**AND THE EXPERIMENT COMPLETED: the originally-blocked problem now closes.**
a94J04 was re-dispatched at 05:15 with a route naming `import YoungL2` and the
root-namespace declaration, and `poissonConv_L2_contraction` **closed
axiom-clean, with no compile errors at all** (commit `590ebeb3`, verified
independently: `[propext, Classical.choice, Quot.sound]`). The full chain, one
problem, 105 minutes:

| time | event |
|---|---|
| 03:40 | a94J04 dispatched; runner blocked — *"`YoungL2` was an unknown module prefix"* |
| 03:52 | `7958c53` adds the missing `[[lean_lib]]` stanza, prompted by that report |
| 05:00 | a96A04 (unrelated) reports *"the repository's `YoungL2` module closed `heatConv_L2_le`"* |
| 05:15 | a94J04 re-dispatched with the route |
| 05:25 | `poissonConv_L2_contraction` **closed axiom-clean** |

**A build-system stanza, not any mathematics, was the entire distance between
blocked and axiom-clean.** Infrastructure claims in this lane are normally argued
from first principles; this one is measured, on the same problem, with nothing
else changed.

Two things I checked rather than assumed, either of which would have wasted the
run: the hint refresher silently repointed the row at line 225
(`poissonConv_tendsto_L2`, which is *not* unblocked), so I repointed it to 207;
and `YoungL2.convolution_...` does **not** resolve — the declaration is in the
root namespace — so the route carried the unqualified name. The runner confirmed
both back.

**Not unblocked, and the runner did not claim otherwise:** `poissonConv_tendsto_L2`
still needs the integral-valued approximate-identity estimate with
`Continuous.compMeasurePreservingLp`, and `poissonConv_tendsto_ae` needs the
Lebesgue-point theorem.

**Recommendation (yours to take):** do *not* make all 145 problem files
`lean_lib`s — that couples every problem into one build graph and makes any
breakage everyone's. **Promote a02J05's sinc/Dirichlet library into
`ConstructionTargets`**, which exists for exactly this and is already on the
module path. That turns "recreate a substantial Abel-regularization proof" into
"import and apply."

**And the measurement worth running before deciding:** count how many blocked
targets across the queue name a result some *other* problem file already proves
axiom-clean. If that number is more than two or three, promotion is the
highest-yield unblocking action available to this loop — worth more than any
individual proof.

### 2. A memory was used correctly and still produced the wrong answer

This one cuts against our own headline metric, which is why it is here rather
than buried.

`e-codexpilot-distinguish-ContDiff-top-analytic-from-ContDiff-infinity-smooth`
was promoted from a01A03 earlier the same day. It surfaced for a96A04 and the
runner **used it correctly** to establish that `ContDiff ℝ ⊤` means *analytic*.
It then drew the opposite operational conclusion from a01A03's: a01A03 said
*therefore the statement is mis-stated — repair it*; a96A04 said *therefore this
is a hard analytic construction* and declared a frontier that does not need to
exist.

**The memory carries the fact but not its consequent**, and omits the one-grep
discriminator that settles it (does `problem.md` say *smooth*? a96A04's does).

**metric-3 counts this as a memory used, and mechanically it was — but the run it
informed was wrong and spent a slot on an unnecessary frontier.** "Memories used"
is not "memories used well," and our metric-3 number does not distinguish them.
Every prior pass reported metric-3 as an unqualified good; this is the first
counter-instance. It bounds rather than retracts the memory-loop result:
surfacing and consumption are measured, correctness of the resulting decision is
not.

### 3. A statement can go stale with no edit to the file

`heatConv_contDiff` demands `ContDiff ℝ ⊤` — real-analytic. `problem.md`,
`informal-solution.md`, `status.json` and the theorem's *own docstring* all say
**smooth**. Nobody mis-typed anything: when Mathlib's smoothness index was `ℕ∞`,
`⊤` meant smooth; after the move to `WithTop ℕ∞` the identical token means
analytic. The statement got strictly harder with an empty diff.

Every gate this loop runs is blind to it — sorry count, `#print axioms`,
signature diffing, and the vacuity scan, the last three *by construction*. I
swept the corpus to bound it: `ContDiff … ⊤` appears in exactly **two** of 145
files, a01A03 (already repaired this way, where the same drift made the statement
*false*) and a96A04. Repair is `⊤` → `∞` and is **yours** — precedent exists and
is same-class, but I did not treat precedent as standing authorisation.

## Failures and mistakes — mine

1. **The chain stopped THREE times** (00:20, 02:02, 03:20), always the same
   shape: awaited jobs complete, the completion consumes the park, the turn does
   its long processing work and ends without dispatching. The first time, only a
   park I had *fabricated by mistake* woke me. **Fixed twice over:** a deliberate
   heartbeat park (dead-man switch, caught all three), and then an ordering
   inversion — **dispatch the next pair FIRST, before processing anything**,
   which makes the gap structurally impossible rather than a thing to remember.

   Original note follows.

1b. **The chain stopped once, and only an error of mine restarted it.** Around
   00:20 there were zero live parks: the pair-3 park was consumed when both its
   jobs finished, I processed them, and I ended that turn without dispatching or
   re-parking. What woke me was a park I had *fabricated by mistake* earlier,
   awaiting a non-existent job, firing on its deadline. **Fixed:** a deliberate
   heartbeat park is now standing (`park-2c25905f`), awaiting a non-existent job
   so its deadline always fires, with a payload that says how to restart the loop.
   Keep exactly one alive.

2. **I corrupted the queue and restored it.** Trying to clean up duplicate keys
   with a regex de-duplication pass, I wrote wrong receipts onto four rows and
   emptied one. Caught by verifying against known-correct values — my row-count
   assertion passed while the contents were wrong. Restored from a backup taken
   beforehand; `cmp` confirms byte-identical.
   **Do not retry with regex** — use a parse → mutate → re-emit EDN round-trip,
   verified field-by-field, with nothing in flight.

   **CORRECTION (05:20): I said "~10 duplicate keys across 8 rows, mostly
   `:receipt`". That was wrong by more than an order of magnitude. The real
   figure is 59 of 88 rows carrying 306 extra key occurrences.**

   I had counted with a regex that also matched keyword-shaped *values*
   (`:duree`, `:resolved`, `:blocked-mathlib-frontier` are values, not keys),
   which first inflated the count to a meaningless "64 of 88"; parsing keys
   properly at map depth 1 gives 59/306. Both my numbers were instrument
   artifacts in opposite directions — the third time tonight a scan of mine was
   shallower than the rule it implemented.

   **CORRECTION (05:58) — I got the cause wrong, twice.** I wrote that "the
   dispatch bookkeeping path appends where it should set" and that behaviour was
   "correct by luck". Both are false, and the second is dangerous.

   `codex_sorry_cron.py` writes rows **correctly** — `dict(row)` then
   `.update({...})`, saved through `dumps(queue)`, which sets and cannot
   duplicate. **The duplicates come from text-level hand edits** — the
   `row[:-1] + ' :key val}'` append idiom — used by me throughout this session
   and by earlier ones. The writer is fine; the practice was not, and it was
   largely mine.

   **And it is not correct-by-luck: it is actively wrong on 14 rows.**
   `edn_format` keeps the *last occurrence in text*, and in these rows that is
   the *oldest* value. Verified: `{:job-id "NEW" :job-id "OLD"}` parses to
   `OLD`. So those rows report the wrong job. It already cost something —
   trusting a row's `:job-id`, I pulled and began processing a **stale** a95A02
   job (…454, commit `bdaae46`) when the file was four commits further on at
   `ed29063`.

   **The landmine: a `loads`→`dumps` round trip collapses 58 rows to 0, so the
   next cron run silently normalises the queue and FREEZES the stale values.**
   That is data loss, not cleanup. Do not normalise, and do not let the cron
   save the queue, until the correct value is chosen per row.

   The 14 affected rows, the verified parser semantics, and the ordered fix are
   in `holes/ops/queue-duplicate-key-risk-2026-07-31.md`. `:attempts` is stale
   the same way — a95A02 read `attempts 1` when the truth was 4.

   I fixed only the one row that needed to be right immediately (a94J04, which
   had a live job) and verified it has no duplicate keys. **I did not attempt a
   corpus-wide rewrite** — a mass re-emit of a live queue is exactly what
   corrupted it the first time, and there is a job in flight. The proper fix is
   to make the bookkeeping path set-not-append, then re-emit once with nothing
   running.

3. **I created those duplicates in the first place**, by edits that *insert* a
   key rather than *set* it. I had a guard for `:status` and never generalised it.
   Now generalised: assert single-occurrence for every key before writing. It has
   since fired and prevented a repeat.

4. **I fabricated a park job id twice**, and once ran a pre-dispatch check in the
   same command block as the dispatch it was meant to gate. All three are one
   root cause: batching an action with the check or record that depends on it.
   **Rule:** dispatch, read the id back, *then* park.

5. **I presented a rediscovery as a finding.** I reported that the sorry metric
   cannot distinguish consolidation from discharge as though new; the corpus
   already held `separate-lexical-sorry-count-from-real-proof-hole-count`, and
   the scribe retrieved it and classified my case as a confirming instance.

6. **A truncated `:suggested-route` reached a runner** because of (3). The lost
   tail was a caution about transitively-sorried declarations. No harm resulted —
   the surviving fragment happened to be the part that mattered — which is luck,
   not design. Routes are now written caution-first.

## Uncertainties

- **Two compounding channels operate and only one is instrumented.** Memory-system
  compounding is measured by metric 3. Proof-corpus compounding — a runner finding
  and reusing another problem's committed implementation — happened twice tonight
  (a00J04 → a01A08, a00J05 → a01A11) and is *not* counted anywhere. The loop's
  real rate of self-reinforcement is therefore under-reported by an unknown amount.
- **Recall infrastructure failures remain frequent.** Several runs recorded
  `timeout` or `store-unavailable`. These are now correctly labelled and excluded
  from the benchmark rather than being scored as retrieval misses, but the
  underlying flakiness is unaddressed. The store was 6.9 GB and below restart
  thresholds all night, so restarting would not obviously have helped.
- **Nine rows need a decision from you**, all diagnosed, none acted on
  unilaterally: `a01A10`, `a01J01`, `a01J02`, `a01J06`, `a02J01`, `a02J03`,
  `a02J07`, `a92J07`, `a93A02`. Each carries `:decision-owner "joe"`, the
  mechanism, the proposed repair, and whether the defect is **proved** or
  **argued**. Several offer two or three materially different repairs, so
  choosing changes what the theorem says.
- **`a95J03` is counted clean and is vacuous** — flagged
  `:counts-toward-clean :disputed`, not silently reopened.

## Structural finding: 4 of 40 untouched rows sit in files that DO NOT COMPILE

An elaboration audit over every untouched row's file found **36 OK, 4 broken** —
`a01J03`, `a01J05`, `a02J06`, `t94J01` — on top of `a01J02`, which was found the
expensive way, by dispatching it. **Their sorry counts are fiction**: declarations
that do not typecheck cannot be measured, yet they have been counted in
`with-sorry` throughout. Each would have burned a full runner slot to discover.

They are **four different causes**, which matters because treating them as one
class would have written off a working row:

| row | cause | disposition |
|---|---|---|
| `a01J03` | malformed file header, `unexpected token '/'` before the first import | likely trivially repairable |
| `a01J05` | `Function expected at` line 346 — a real type error | needs diagnosis first |
| `t94J01` | `rcases` fails on a non-inductive datatype | needs diagnosis first |
| `a02J06` | **not a file defect** — the Scratch `.olean` was unbuilt; after building, the errors are stale API (`Complex.abs`, unknown `ComplexAnalysis`) | runner-fixable migration |

Each row now carries its own `:blocked-reason` and `:disposition`, so triage is
sorting rather than investigation.

## Defect TAXONOMY — one umbrella, four classes, three mechanisms

**Final structure, per scribe pass 33** (which refused my simpler versions
twice): the umbrella is **"missing semantic-domain guards admit degenerate
witnesses"** — Lean totalises partial operations and extended codomains carry
junk elements, so a hypothesis constraining such an operation *without guarding
its domain* is satisfiable by a witness outside the intended semantics.

**The umbrella unifies the diagnosis. The mechanisms stay separate because the
REPAIRS differ:**

| mechanism | junk witness | repair |
|---|---|---|
| `integral_undef` | integral of a non-integrable function is `0` | add integrability, or reformulate with `lintegral` |
| totalized `deriv` | `deriv` is `0` wherever differentiability fails | require real differentiability, preferably `ContDiff` |
| extended-codomain `⊤` | `∃ M, … ≤ M` with M inferring to `ℝ≥0∞` admits `M = ⊤` | bind M in `ℝ≥0`, or add `M < ⊤` |

Separately from the umbrella sit **constant placeholder** (a95J03 — a scaffold
stub, nothing to do with totalisation) and **pre-formal elaboration failure**
(a01J02 plus four found by audit — the file never typechecked).

**Correction, and it is mine.** I first wrote this section as "five problems,
one root cause" and put it at the top of this file. Scribe pass 32 refused the
claim and separated the evidence, correctly: the defects share a *feeling* — the
corpus is unsound — but not a mechanism. The taxonomy:

**1. `integral_undef` / junk value.** Lean totalises the Bochner integral: a
non-integrable function integrates to **zero**. On an infinite-measure space a
hypothesis `∫ f ≤ C` is therefore satisfied *for free*.

| problem | defect | proof status |
|---|---|---|
| `a01A10` | 1 target FALSE | **machine-checked** — exact negation, axiom-clean |
| `a01J01` | 1 target VACUOUS | argued — L¹ counterexample proved, L³ step a comment |
| `a02J01` | 3 targets FALSE | argued — prose plus arithmetic checked by hand |
| `a02J03` | 2 targets FALSE | argued — needs `LocallyIntegrable φ`; counterexample via a Borel–Cantelli spike construction non-integrable on every interval |
| `a95A08` | (earlier, flagged by the scribe) | — |

**2. Constant placeholder.** `a95J03` — `windingNumber` is defined as the
constant `0`. This is *not* `integral_undef`; it is a scaffold stub. **It is
counted CLEAN.** Scribe 32's recommendation: classify it `:closed-over-placeholder`
and amend the existing placeholder memory.

**3. Missing model invariants.** `a01J06` — `ProblemData` never relates `zeros`
to `zeroCount`, so one target is vacuous and the other is refuted
(machine-checked). A modelling defect, not an integral convention.

**4. Pre-formal elaboration failure.** `a01J02` plus the four found by the
elaboration audit — the file never typechecked, so nothing about it is
measurable.

**Five runners independently declined to silently repair a statement** — a01A10
refused to pick a repair, a01J02 refused to choose between two, a01J01 refused to
weaken, a01J06 refuted rather than patched, a02J01 and a02J03 diagnosed without
touching the file. None was told to. **This is the strongest evidence tonight
that the review discipline has transferred to the runners themselves.**

## The statement preflight — the most transferable thing tonight produced

Three checks, none requiring a proof attempt, each of which caught a real defect:

1. **For `∃ M, … ≤ M`, ask what type M INFERS to.** a93A02's inferred `ℝ≥0∞`,
   so `M = ⊤` discharged the bound and it constrained nothing.
2. **Where hypotheses mention derivatives or integrals, check whether
   differentiability/integrability is ACTUALLY assumed.** a92J07 constrained
   `deriv` under `Continuous`-only hypotheses.
3. **Compare the DOCSTRING against the formal hypotheses.** Twice tonight the
   prose was right and the Lean was wrong — a92J07's prose said C¹, a93A02's
   said "uniform bound" while admitting ∞.

Promoted as a trajectory memory. Scribe 33 judged it capturable and broader than
the Bochner-specific diagnostic.

**And a check I got WRONG and corrected within the hour**, recorded because the
failure is instructive: a95J03's vacuity showed unused hypotheses, so I wrote
"unused parameter ⇒ possible vacuity". a93J07 disproved it — an unused hypothesis
makes a theorem *stronger*. **Test the CONCLUSION: unfold the definitions it
mentions and ask whether any is a bare constant.** Unused parameters are a hint
to look, never the finding.

## Decisions waiting for you

**The audit is complete: all 31 blocked/wontfix rows now carry a mechanism, a
proposed repair, and an explicit PROVED-vs-ARGUED status.** Provenance is stamped
on every one: `:repair-provenance` / `:mechanism-provenance` says whether a field
was quoted from a receipt, derived by me from the Lean, or composed by me as a
proposal. **Nothing below is invented without saying so.**

### A. Infrastructure — these gate more than one problem

1. **Promote a02J05's sinc/Dirichlet library into `ConstructionTargets`.**
   a97A06 is blocked on the Dirichlet sinc integral, which a02J05 *already proves
   axiom-clean* (16 theorems, zero sorries) and cannot be imported. Do **not**
   make all 145 problem files `lean_lib`s. The measurement worth running first:
   how many blocked targets name a result another problem already proves.

2. **Close `ConstructionTargets/Rouche.lean:102` (`zeroCountInClosedBall_add_eq`).**
   **One sorry blocks three problems** — a92J05, a94A10, a97A08 — plus the
   `rouche-root-count-transfer` target. I re-tested the frontier because its cost
   had risen: current Mathlib has no Rouché and no argument principle. It is a
   genuine construction, not a cheap win.

3. **Recall budget.** `evidence/text-search` costs 9–16s per subject against a
   **30s total** budget, so recall cannot complete for any row with more than
   about two common subject terms. Every `:NOT-MEASURABLE` tonight means *recall
   was not given time*, not *nothing was found*. Cheapest fix is one constant
   (`default-recall-timeout-ms` 30000 → 120000,
   `src/futon3c/dispatch_with_recall.clj:19`), but it changes dispatch behaviour —
   land it with a before/after on a live dispatch. Receipt `4f7eeadd`.

4. **Do NOT normalise the queue yet.** 57 rows carry duplicate keys from
   *hand edits* (not the cron writer, which is correct). A `loads`→`dumps` round
   trip collapses them — and **freezes the oldest value**, which is wrong on 14
   rows. See `holes/ops/queue-duplicate-key-risk-2026-07-31.md`.

5. **`a95J03` counts as CLEAN and proves nothing.** Flagged
   `:counts-toward-clean :disputed`, not silently reopened. Your call whether the
   headline is 100 or 99.

### B. Statement repairs — one row each

| row | mechanism | evidence strength |
|---|---|---|
| `a01a10` | `:statement-false-improper-integral-not-lebesgue-integrable` | `:machine-checked-refutation` |
| `a01j01` | `:junk-value-integral-undef` | `:argued-not-machine-checked` |
| `a01j03` | `:malformed-import-syntax` | `:machine-checked-does-not-parse` |
| `a01j05` | `:single-elaboration-error-function-expected` | `:machine-checked-one-error-at-line-346` |
| `a01j06` | `:scaffold-placeholder-and-unrelated-fields` | `:machine-checked` |
| `a02j01` | `:junk-value-integral-undef` | `:argued-not-machine-checked` |
| `a02j03` | `:integral-undef-totalisation` | `:argued-not-machine-checked` |
| `a02j06` | `:stale-api-Complex-abs-removed` | `:machine-checked-unknown-constant` |
| `a02j07` | `:integral-undef-junk-value` | `:argued-not-machine-checked` |
| `a92j07` | `:junk-value-totalisation` | `:argued-not-machine-checked` |
| `a93a02` | `:extended-type-top-element` | `:argued-not-machine-checked` |
| `a94a04` | `:integral-undef` | `:argued-not-machine-checked` |
| `a94j02` | `:missing-topological-hypothesis` | `:machine-checked` |
| `a95a06` | `:missing-topological-hypothesis` | `:machine-checked` |
| `a95j07` | `:outer-measure-not-countably-additive` | `:argued-not-machine-checked` |
| `t94j01` | `:tactic-failure-rcases-on-metavariable` | `:machine-checked-one-error-at-line-37` |
| `a92j05` | `:blocked-on-shared-construction-target` | `:frontier-VERIFIED-against-current-mathlib` |
| `a94a10` | `:blocked-on-shared-construction-target` | `:frontier-VERIFIED-against-current-mathlib` |
| `a95j04` | `:true-typed-hypothesis` | `:compiled-but-not-committed` |
| `a95j06` | `:true-typed-conclusion` | `:verified-by-inspection` |
| `a95j08` | `:mathlib-frontier-weighted-holder-jensen-ennreal` | `:frontier-VERIFIED-three-attempts-same-wall-plus-zulip-confirmation` |
| `a96a02` | `:constant-subject-definition` | `:verified-by-inspection-before-dispatch` |
| `a96a04` | `:silent-api-drift-strengthening` | `:VERIFIED-against-mathlib-source-and-problem-md` |
| `a97a06` | `:module-path-defect` | `:VERIFIED-a02J05-holds-the-theorem-axiom-clean` |
| `a97a08` | `:blocked-on-shared-construction-target` | `:frontier-VERIFIED-against-current-mathlib` |
| `construction-deriv-ne-zero-of-injon` | `:mathlib-frontier-local-degree-assembly` | `:frontier-NARROWED-one-of-two-named-pieces-now-proved` |
| `rouche-root-count-transfer` | `:blocked-on-shared-construction-target` | `:frontier-VERIFIED-against-current-mathlib` |

27 rows. Read `:proposed-repair` on each — several are one-line fixes
with a named replacement. **Sorted machine-checked first**: a refutation Lean has
verified is a different kind of claim from one I argued, and the rows say which.

Three worth pulling out because the repair is mechanical:

- **`a02J06`** — `Complex.abs` is gone from Mathlib. Line 27 now elaborates to
  `sorry () = sorry () ^ 2`: **the file is accumulating `sorryAx` from a naming
  failure, not a mathematical one.** `Complex.abs z` → `‖z‖`,
  `Complex.abs.map_mul` → `norm_mul`.
- **`a01J03`** — imports use slashes instead of dots. *Not* a pure typo fix:
  `Mathlib.MeasureTheory.Function.Deriv` no longer exists and needs re-pointing.
- **`a96A04`** — `ContDiff ℝ ⊤` means *analytic*; the problem asks for *smooth*.
  `⊤` → `∞`. a01A03 hit the identical drift and was repaired this way.


---

## What went right

**Thirty problems closed** since yesterday morning (from a starting 63 clean / 172 sorries), plus one
construction target, plus one Mathlib frontier documented rather than churned.

**The memory loop closed on itself three times**, which is the result worth having:

1. **LemniscateComponents** — a solve-lane memory drafted from a01A08 supplied the
   *proof architecture* for closing the construction target two hours later.
2. **a01A07** — closed on its third attempt by the two **frontier** memories the
   scribe drafted from its *own* second attempt. The runner reports one of them
   "carried the proof". A record of what *remained* closed the thing it described.
3. **a02J05** — proved its entire convergence half using two memories drafted from
   its own failed attempt that morning.

**The recall repair holds under live fire.** Three fixes landed and were verified
end-to-end, including by mutation-testing the warrant gate. A runner receiving
`[dispatch-recall-outcome=timeout]` now copies it verbatim and explicitly declines
the terrain-gap inference — the exact error I made yesterday afternoon is now
structurally unavailable to it.

**Nine evidence amendments applied**, all append-only with proposer ≠ applier. The pipeline can promote but not amend; the
scribe proposed five amendments in hand-applicable form and I applied them as
append-only records (never mutating), preserving proposer ≠ applier.

**The scribe's judgement beat mine eight times out of eight.** It declined every
memory I proposed that would have duplicated an existing one, and when I asked for
a rule from a single case it gave a seven-part *hypothesis* instead — falsifiable
on the next frontier close, where a rule from n=1 would not have been.

## State

| | |
|---|---|
| problems closed overnight | **30** |
| corpus figure | take it when nothing is in flight — see trap 1 |
| queue | 22 untouched · 43 resolved · 4 held-out · **15 blocked-and-diagnosed** · 2 wontfix · 3 in flight |
| Codex usage | ~12 % against the 50 % stop |
| store | 6.9 GB, below restart thresholds |

In flight: a02J05 attempt 2 and a01A12, under `park-3e308208`, with the heartbeat
behind them.

**Check the loop is alive:**

    curl -s 'localhost:7070/api/alpha/parked?agent=claude-9' | python3 -m json.tool

Use the **agent-filtered** query — the bare `/parked` endpoint returns `[]` even
when a park is live, which would tell you the loop is dead when it isn't.
