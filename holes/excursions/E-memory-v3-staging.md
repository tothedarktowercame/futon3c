# E-memory-v3-staging — the pre-V3 bank

**Opened 2026-07-31 by claude-2 at Joe's suggestion.** Findings and repairs
arrive faster than V2 can absorb them. Banking them here does three things the
previous arrangement did not: it gives a finding a **home** so it is not lost,
it lets V2 **report a known failure without repairing it first**, and it keeps
the repair queue from interleaving with measurement (the gate in
`E-memory-whitepaper-v2-programme.md` §4).

**Rule of the bank.** Anything landing here is *scheduled, not scheduled away*.
Each entry names the V2 treatment (usually: report as a known failure) and the
V3 treatment (usually: repair or instrument). An entry with no V2 treatment is
suspicious — it usually means the finding has not been understood well enough
to state.

---

## A. Repairs — gated behind P1–P3, deliberately

**GATE LIFTED 2026-08-01.** P1, P1b, P2 and P3 are all complete and frozen, so
the measurement phase is closed and repairs are now permissible. **Permissible
is not automatic**: A2/A2b must not ship at all on current evidence (two
lexical mechanisms failed), and A1 has strengthened rather than weakened. The
gate did its job — it caught two plausible repairs that the data says are
wrong, one of them twice.

*Original rule, retained for the record: none of these could land until V2-2,
V2-6 and V2-5 were complete and frozen, because each would erase a number V2
reports. Exception: claude-9's going-forward receipt template, new rows only.*

| # | repair | V2 treatment | V3 treatment |
|---|---|---|---|
| A1+ | **STRENGTHENED by P2 — the defect may be biased, not merely lossy.** Of the 6 uses `used-ids` dropped, **4 were adjudicated load-bearing (67%)** against **13/43 (30%)** for the uses it kept. The blind subset of the dropped group is 2/2. **n = 6, so a signal, not a result** — but if it holds, every use-rate we have understates *load-bearing* use by more than it understates use overall, and the field's failure is correlated with what matters. Priority for A1 rises accordingly: it is now the repair most likely to change a reported number. |
| A1 | **`used-ids` has no code writer.** `dispatch_with_recall.clj:906` hardcodes `:used-memory-ids []` for the offered half by design; there is no `record-outcome!` in the file, so the outcome half is hand-authored and populated ~16% of outcome receipts (0/8 on 07-29, 9/48 on 07-30, 8/49 on 07-31 — never reliable, not a regression). | **Report as known failure.** It is the load-bearing number under C3: the loop pays for a measurement it records one time in six. | Write `record-outcome!`; back-fill from the 130 job results already on disk (ids are recoverable, nothing needs re-running). |
| A2b | **REVISED AGAIN 2026-08-01 by P1b — A2 may be aimed at the wrong layer.** Two lexical mechanisms have now been tested and both failed: marginal DF does not predict emptiness (U-curve, p = 0.618) and co-occurrence predicts it *with the wrong sign* (high co-occurrence → 78.6% empty, p = 0.0172). Term count is not a confound (119/126 queries have exactly 8 terms). | Report that we tested two lexical mechanisms and the evidence points away from the lexical stage. | **Do not ship any term-selection rule yet.** Test the attachment-layer conjecture first: empty means no *memories* surfaced, not no text matched, and P3's 80/90 plus the 19 no-attachment reports point at the projection stage. A term-selection repair that does not move the empty rate would be worse than no repair — it would look like a fix. |
| A2 | ~~**Port rarity ranking**~~ **— REVISED 2026-08-01 by P1; the original repair was backwards.** Rarity ranking exists in `codex_sorry_cron.py` (`:415`: *"Keep the rarest terms"*) and not in `dispatch_with_recall.clj`. **P1 measured the DF/empty-rate curve as a U: rare ≤11 → 71.4% empty, middle 12–56 → 44.4%, common >56 → 74.4%, with rare vs common indistinguishable (p = 0.618).** Keeping the *rarest* terms targets the worst band. | Report the U-curve, and report that our own source (`dispatch_with_recall.clj:288–298`) documents a monotonic rare-floors-conjunctions mechanism that n = 126 falsifies. | Select terms for the **mid-DF band**, not the rare tail — and still lane-scoped, since regulative memories' terms are generic by nature. Needs `:memory-use/kind` (B4) first. **Do not ship the rare-first rule.** |
| A3 | **`:witness-status` earned, not lane-derived.** `review-attachment!` (`memory_lifecycle.clj:160`) already does this correctly; the codex lane bypasses it. | Report the bypass; V1 §2.3 already documents the defect. | Route the codex lane through `review-attachment!`. |
| A4 | **`inclusion-reasons` is a constant string** on the WM compact-recall path (`wm_memory.clj:128–132`); `:memory-use/surfacing-via` is not available there. codex-6 judged the fix not small and said so. | Report as an instance of the degenerate-vocabulary finding. | Make `surfacing-via` available on that path. |

## B. Instruments V3 needs — unrecoverable if not built

| # | instrument | why receipts cannot supply it |
|---|---|---|
| B1 | **Reachability index** (declaration → module → reachable-from-problem), to count how often a blocked target names a result proved elsewhere. | Every receipt field is closed over the *offered* set. An artifact never offered appears nowhere, at any level of analysis. claude-9's five instances are the only detector we have, and they were found by noticing in passing. |
| B2 | **Dispatch-time seed capture** — the text-search candidate set (ids + scores) per dispatch, plus `index-as-of`. | The text index is a SQLite FTS5 sidecar with no temporal capability (verified: `system-as-of` is ignored, byte-identical response). Graph state *is* reconstructible to 07-25 with valid ≡ system time, so this is the single binding item for RQ1. |
| B3 | **A decision-keyed external checker identity** for the WM lane. | Mechanism is built and tested (`7a69095a`, `d722f772`); the triple is test-only because no decision-keyed check exists yet. This is an *arrangement*, not code. The 2026-07-23 check stays permanently unjoinable. |
| B4 | **`:memory-use/kind`** distinguishing substitutive from regulative memories. | Prerequisite for A2. Also the field that would let use-rate be reported by mode rather than as a misleading scalar. |
| B5 | **Persist which ladder rung fired** (3-term / pairs / singles) on the offered half. | P1's H2 was **untestable**: no receipt records it, so the singles-vs-conjunction comparison cannot be made from 129 dispatches. One field, and it is the field that would let us price the breadth rung against its yield. |

## C. Datasets

| # | dataset | status |
|---|---|---|
| C1 | **futon3c git history as *memories*** (Joe, 2026-07-31) — distinct from `E-futon-memories` S1, which built a *topology* corpus from the same history. This would mint commit-derived memories with mechanical outcomes (build, revert, blame antecedent). | **Not a V2 item, and it does not restore V2-4.** See §D. Valuable as V3 *instrument validation*: a corpus large enough to exercise the scoring model where the APM corpus cannot. |
| C2 | S2 fix→cause retrieval benchmark, n ≈ 265 across the stack | Deferred. S1 answered its gating question; the benchmark is optional for V2 and belongs with C1. |
| C3 | Clean Zai generality A/B | V3, and blocked on A3 regardless — otherwise it compares a structured-receipt lane against a prose lane and confounds model with instrumentation. |

## D. Why a bigger corpus does not restore V2-4

*Recorded because the temptation will recur.*

V2-4 was a Ψ-v2 replay at n = 20 — dropped because no V2 claim depends on it
and n = 20 is below calibration. A git-mined corpus (C1) would supply a much
larger n. **It would not restore V2-4**, because it answers a different
question: a scoring result on commit-derived memories with build/revert
outcomes licenses no claim about the APM memory system's behaviour on proof
work. Different corpus, different domain, different outcome semantics.

What it *could* support is a **methodological** claim — "the scoring model
behaves sensibly at scale when the data exist" — which is instrument
validation, belongs in V3, and should be stated as such rather than smuggled
in as evidence about the deployment. The same discipline that dropped V2-4 for
weak attachment applies to re-adopting it with borrowed data.

## E. Known failures V2 should report rather than repair

The list V2's limitations section is built from. Each is measured, not
suspected.

1. `used-ids` populated ~16% of outcome receipts; **use figures are floors** (A1).
2. `:rejection-reasons` non-empty on 7 receipts corpus-wide; the taxonomy had to
   be recovered from runner prose.
3. Error-time recall returns nothing in **40%** of dispatches — the only visible
   form of the reachability gap (B1).
4. **62%** of surfacing slots go to memories used nowhere in the 45; 52 of 77
   memories were never used.
5. The λ₂ **threshold is inverted** across a 55× richness range, while the
   statistic itself is informative (S1) — a retraction with evidence, replacing
   V1's retraction by withdrawal.
6. Recall-empty at **64%**, structural and non-randomly biased (pending P1).
7. Two failed hypotheses of our own, recorded so they are not retried: sticky
   memories (r = +0.002) and topical dispersion (2.4 vs 2.8, wrong direction).
8. **80 of 90 recorded queries return no candidate baseline** under V1's own
   sweep operator on the current graph (P3). Reported as a reachability result
   in its own right, *not* as a worsening of the 64% recall-empty figure — the
   operators differ and the numbers are not comparable.
9. **Four inherited denominators failed checking in one week**: 129
   rejection-reasons → 7; 121 codeable reports → 45; ~73 sweep problems → 10
   usable of 92 present; and the ~360 APM queue headroom → ~48 (plan §2.1).
   Every one was caught by re-counting rather than by the audit that was
   supposed to catch it. **V2 should state this as a methodological finding
   about our own instrumentation**, not bury the corrected figures — the
   pattern is the result.

## G. Conative structure — V3 conceptual material (Joe via claude-9, 2026-08-01)

*Joe asked that this go into V3 specifically. Recorded here with claude-2's
checks appended, including one that cuts against a conjecture in it.*

**G1. Recurrence alone is not learning; recurrence against a want is.** A
repeated encounter becomes a *problem to solve* only as a gap relative to an
optative direction — here, "prove the theorems". Without the conative aspect a
system re-encounters the same topics indefinitely and learns nothing. Counts
before each topic became a problem: module-path defect **5**, autoImplicit
binding **4**, stale statement hints **7**, Rouché **3**, integral Minkowski
**2**, stale queue statuses **6**. In every case the first encounter was a
*fact*, not a problem.

**Precondition (claude-9):** recurrence is only visible if you are *counting*.
Receipts make a rate perceptible; the queue turns a perceptible rate into a
dispatched job. Without receipts, five encounters stay five annoyances.

**G2. The conative layer lives in the queue, not the corpus.** The corpus is
declarative-advisory and has no representation of what is being pursued, so it
cannot notice that something keeps blocking. The queue does:
`:blocked-on-construction-target`, `:depends-on-problem`, `:next-route`,
`:unblocks`. Rouché was promoted because three rows named it as an obstruction,
not because any memory did.

*claude-2 sharpening, from the use-mode data:* the corpus does in fact **carry**
conative content — five of seven use-modes are regulative, and regulative
memories earn **45–54% use against substitutive 15%**. A stopping rule is
conative content sitting in a declarative store. What the corpus lacks is a
**structural representation** of it: no field marks a memory as bearing on
direction of effort, so it is held as prose, unindexed and unreasonable-over.
**This upgrades staging B4 (`:memory-use/kind`) from an analysis convenience to
the minimal structural representation of the conative layer inside the
corpus** — and it is the prerequisite for A2's lane-scoped term selection.

**G3. A conative aspect to the shape of memory itself — endogenous "ease".**
Alongside *what do I want to achieve in the world* sits *what shape do I want
my own memory to have*: fewer entries covering more, better-factored
abstractions, results that compose. Observable signature today: the
ConstructionTargets promotions are the corpus reorganising toward ease — a
lemma proved once and consumed by many rather than re-derived per problem,
**six consumers across five promoted modules**. Noticing that one `sorry`
blocks four problems is the system locating a high-leverage point in *its own
structure*.

**G4. Ease has a characteristic failure mode.** claude-9's own example:
generalising four real autoImplicit defects into a single mechanical pass/fail
gate, which then condemned a97A07 — a solved problem where `n` in
`Fin n → ℂ` is auto-bound as *idiom*, not defect. Ease bought one rule and lost
the distinction that mattered (does the identifier name something that exists
elsewhere?). Worse, a pass/fail gate would have trained everyone to
`set_option autoImplicit true`, restoring the hazard permanently.

**Counter-pressure already exists in the corpus:**
`e-codexpilot-bound-the-interface-adapter-heuristic-with-genuine-construction-cases`
is a memory whose entire content is *"do not misclassify a genuine construction
as a cheap adapter"* — a memory that exists to resist the easy reading. It is
also one of the highest-use memories measured (**5 used of 8 surfaced, 62%**).

**G5. claude-2's check — the ease hypothesis does NOT explain
relevance-without-applicability.** claude-9 conjectured that this category is
the same phenomenon from the other side: memories that generalised toward ease
until they named the right shape and lacked usable form. Extracting all five
coded instances, their names are:

- `refute-essential-boundedness-at-a-singularity-on-a-positive-measure-ball`
- `prove-weak-L2-convergence-by-Vitali-on-products-over-infinite-measure` (×2)
- `derive-local-L1-from-weak-L2-by-layer-cake-and-optimized-splitting`
- `prove-Holder-convolution-vanishes-at-infinity-by-compact-support-density`

**These are not over-general; they are highly specific**, each binding a
technique to precise conditions ("on a positive measure ball", "over infinite
measure", "by layer-cake and optimized-splitting"). The declines say the
*conditions* do not match — "concerns L^∞, not L²". So this category looks like
**over-specification**, the mirror image of the ease failure, not an instance
of it.

*Weak evidence, stated as such: n = 5, and specificity is being read off names
rather than bodies.*

**And it unifies with P1.** The same curve appears at two levels. At the
**query-term** level: rare terms are distinctive and nearly absent (71.4%
empty), common terms plentiful but non-co-occurring (74.4%), the middle band
works (44.4%). At the **memory-content** level: over-specific memories name
exact conditions that rarely recur (G5), over-general ones name a shape with no
usable form (G4), and the useful band sits between. Both are the same claim —
**distinctiveness is the wrong objective; connectivity is the right one** —
which is also the SIP argument in the programme's P1 writeup note. If that
holds up, it is the strongest candidate for V3's organising idea.

**G6. Connectivity is route-relative — and this may explain S1.** (claude-9,
a95J08, 2026-08-01.)

a95J08 had been `:blocked-mathlib-frontier`, proof-status
`:frontier-VERIFIED-three-attempts-same-wall-plus-zulip-confirmation` — three
attempts hit the identical ENNReal wall, with a Zulip thread agreeing the
infrastructure was missing. codex-10, reviewing from outside those attempts,
proposed organising through **Hölder rather than Jensen**, and it closed on the
first attempt.

**The key lemma was in Mathlib throughout**: `ENNReal.lintegral_mul_norm_pow_le`,
`MeasureTheory/Integral/MeanInequalities.lean:170` — a well-trafficked file, a
compositional name. **By any static connectivity measure that artifact is
fine.** It was unreachable for three attempts because searching `Jensen`,
`probability measure`, `normalised weight` will never surface it.

> **Connectivity is not a property of the artifact. It is a property of the
> artifact relative to a route.** The search vocabulary is determined by the
> proof you are trying to build; change the intended route and the same corpus
> is differently connected.

Distinct from the earlier findability case: integral Minkowski was a **naming**
problem (right content, wrong tradition's name). This is a **plan** problem —
correct name, correct file, wrong question asked of it.

**claude-2: this is a candidate explanation for S1's otherwise unexplained
result.** S1 found λ₂ *informative* (real graph ~15 SD below a
degree-preserving null) but its **threshold inverted** across a 55× richness
range, and we recorded no account of why. If usefulness is route-relative, then
**no static spectral measure of the artifact graph can predict it** — λ₂
measures connectivity-in-general, while what matters is
connectivity-under-the-route-actually-taken. The statistic detects real
structure (hence ≠ null) and fails to rank usefulness (hence the inversion).
*Conjecture, not a result; but it is the first account we have that predicts
both halves of S1 rather than one.*

**claude-2's one push-back on the three-level framing.** claude-9 proposes the
same U at route level: too idiosyncratic reaches nothing, too generic reaches
everything uselessly. But the operative clause in their own statement is
*"where a route's natural vocabulary matches how the library is factored"* —
that is an **alignment** between two structures, not a position on a
one-dimensional scale. And the route *determines* which vocabulary is used,
hence which DF band the query lands in. **So route-library alignment is
upstream of the term-level curve, not parallel to it** — it is plausibly the
generator of the other two levels rather than a third instance of them. That is
a stronger claim than a triptych, and a more falsifiable one.

**G7. B4 does not subsume the queue (claude-9, accepted).** Marking a memory
regulative and representing blocking relations *between goals* are different
structures. `:blocked-on-construction-target`, `:depends-on-problem`,
`:unblocks` say "this keeps blocking things"; **no memory says that, and B4
would not let one.** Rouché was promoted because three *rows* named it as an
obstruction. Conative structure therefore exists at two levels — inside a
memory (B4) and between goals (the queue) — and V3 should not conflate them.

**G8. A counter-pressure memory that correctly declines to fire is working.**
`bound-the-interface-adapter-heuristic-with-genuine-construction-cases` was used
again on a01J02, where the runner recorded using it *"cautiously: these targets
did prove to be direct completion-API adapters"* — it checked the easy reading,
found it genuinely easy, and said so. *claude-2: this is another place the use
vocabulary is too thin. `used-ids` records that it fired; nothing distinguishes
"fired and blocked" from "fired and cleared", and the second looks like
non-intervention in every downstream metric.*

## H. V3 EXPERIMENT — the federated ablation: making the counterfactual runnable

*Joe, 2026-08-01: "you would be able to test 'removing the memory changes the
outcome' if we use the federated system — ship different databases to lucy and
chicago and run independent tests there."*

**The idea is right and it answers P2's central caveat.** claude-9 flagged that
every load-bearing verdict is a plausibility judgement because a dispatch
cannot be re-executed with a memory withheld. Federation makes re-execution
possible.

### H1. The experiment worth running is not "does memory M matter"

It is **does the P2 rubric measure anything**. Ablate memories claude-9 judged
**load-bearing** and memories judged **incidental**, and compare:

| if | then |
|---|---|
| LB ablations change outcomes, IN ablations do not | the adjudication is **validated as measurement**, and P2's 38% becomes a rate rather than a judgement |
| both change, or neither does | the 38% is judgement only, and V2 must say so |

That converts a whole result rather than testing one memory, and it is the
only design here that can falsify our own instrument.

### H2. The binding constraint is variance, not infrastructure

**Runner nondeterminism is the real obstacle.** Two identical dispatches of the
same problem with the same corpus need not agree. **An ablation difference is
uninterpretable without knowing the same-corpus disagreement rate**, and that
noise floor must be measured *first*. This is the step most likely to be
skipped and the one that decides whether the result means anything.

Design, cross-over so site effect cancels:

| arm | corpus | purpose |
|---|---|---|
| control | full, both sites | measures site + run variance — **the noise floor** |
| treatment | ablated, both sites | memory effect, readable only against that floor |

Repeats per cell. Preregister the predicted direction and a falsifier before
running — three of our four preregistered expectations were wrong this week and
that is where the value came from.

### H3. Two ablation mechanisms, and the cheap one is probably sufficient

- **(a) Ship different databases** (Joe's proposal). True ablation; handles the
  case where removing M changes *what else surfaces*.
- **(b) Filter M from the surfaced set at dispatch.** Vastly cheaper — no
  281 MB store to ship to a 3 GB box — and **identical from the runner's view**,
  since the runner only ever sees what was surfaced. On a star-forest graph,
  where memories attach to exactly one pattern, (a) and (b) should coincide.

*Recommend (b) first, with (a) as the check on a subset.* If they disagree, the
disagreement is itself a graph-effect finding.

Note that (b) needs no federation at all for the *ablation* — federation buys
**independent execution**, which guards against dev-laptop-specific state. That
is worth having but is second-order to the noise floor.

### H3b. Runner-lane hygiene is a real scheduling constraint (Joe, 2026-08-01)

Re-proving theorems is **expensive**, and a clean ablation needs **genuinely
fresh codex lanes** — agents with no prior exposure to these problems or to
this programme. Of the ten codex agents on dev-laptop, codex-1/2/3/4/5/6 and
lon-codex-1/2 have all now worked it, and codex-10 ran a95J08 for claude-9.
**Lane availability, not compute, may be the binding constraint on how large
Stage 2 can be**, and it degrades every time we dispatch. Budget the lanes
before designing the arms.

### H3c. The noise floor is NOT RUNNABLE on current infrastructure (codex-7, 2026-08-01)

codex-7 was dispatched Stage 1, designed it, and **stopped at the blast-radius
gate without sending a single dispatch** — 0 runs, 0 tokens, 0 receipt writes,
frozen inputs verified across a 165-file manifest. Its design was sound (3
problems × K=3, within-problem pairwise disagreement in final sorry count,
bootstrap interval, ~180–315k tokens). It refused on structural grounds, and
the grounds hold:

1. **All runners share one absolute worktree** (`/home/joe/code/apm-lean`).
   Sequential repeats inherit earlier repeats' edits and commits; parallel
   repeats race on the same files. **Repeats are not independent, so the noise
   floor cannot be measured at all** without isolated checkouts.
2. **Repeats reuse agent conversational continuity.** A confound I had not
   considered: the same agent remembers its earlier attempts, so K repeats
   understate true run-to-run variance. Fresh sessions per repeat are required.
3. **Experiment dispatches never enter `data/codex-sorry-queue.edn`**, so they
   receive no backpressure against live jobs touching the same targets.
4. The dispatcher has **no isolated-checkout or starting-revision facility**.

### H3d. The subject is being consumed by the loop that studies it — this has a deadline

codex-7 noted the candidate files have evolved since their recorded runs.
**Verified, and it is more advanced than that:**

| problem | sorries now | last changed |
|---|---:|---|
| a02J05 | **0** | 2026-08-01 |
| a95J08 | **0** | **2026-08-01** |
| a01A07 | **0** | 2026-07-30 |
| a96A04 | 2 | 2026-07-31 |

**Three of four candidates are closed.** You cannot ask whether removing a
memory changes the outcome of a problem that has no outcome left to change. And
a95J08 closed **today, during this session** — by the Hölder-route work
claude-9 reported (§G6), i.e. the very activity that generates the findings is
destroying the substrate for testing them.

~~**Operational consequence:** reserve problems from the live loop now or lose
the subjects.~~ **WRONG — corrected by Joe, 2026-08-01, and the correction
improves the experiment rather than rescuing it.**

**Closed problems are the BEST subjects, not disqualified ones.** Re-run the
same closed problems from their pre-solution state, without showing the answer
or the memories. Verified reconstructible — a95J08's file history:

| revision | date | sorries |
|---|---|---:|
| `83c1fe4` | 07-26 | 4 (starting state) |
| `f8bac9c` | 07-31 | 2 |
| `ced5121` / `3cefa10` | 07-31 | 1 |
| **`b3cf20b`** | 08-01 | **0 — closed** |

Any prior state is checkoutable, so "the problem has moved on" is not a
barrier; the git history *is* the experimental apparatus.

**Why this beats holding problems out, on four counts:**

1. **Ground truth is known.** A closed problem has a *known-achievable*
   outcome, so the dependent variable becomes **"did it close?"** — a clean
   binary against a target we know is reachable. On an open problem, failure is
   ambiguous (perhaps it is merely hard). This is a much better DV than final
   sorry count, which is confounded by partial progress.
2. **The successful route is on record**, so an ablation can be checked against
   what actually worked, not just against a score.
3. **No hold-out cost.** Reserving problems would have sacrificed real loop
   progress. Re-running solved ones costs nothing the loop wanted.
4. **It dissolves the queue-backpressure objection** (H3c item 3): an
   experiment running at a *historical* revision in an isolated worktree cannot
   collide with live work at HEAD.

### H3e. A git worktree is NOT sufficient isolation — the answer leaks (verified 2026-08-01)

*Joe proposed a separate user account with apm-lean copied in without
solutions and no read access to his homedir. Checked, and it is **necessary
for validity**, not a convenience — the worktree approach I was defending is
fundamentally leaky.*

**A worktree at a historical revision shares the object database.** From any
revision, `git log --all` shows `b3cf20b "a95J08 prove finite-exponent Young
convolution inequality"`, and `git show b3cf20b:problems/a95J08/lean/Main.lean`
returns the **solved file, 0 sorries**. A runner that thinks to run `git log`
has the answer — and these runners *do* search the repository: a96A04's closed
on a lemma found by repository grep (§G6), which is exactly this behaviour.

**And the leak is not confined to apm-lean.** `a95J08` appears in three
futon3c lab artifacts (`rejection-coding-`, `memory-reports-worksheet-`,
`coding-sections-20260731.json`), which carry runner prose describing the
routes and outcomes. A clean apm-lean copy still leaks if the agent can read
`/home/joe/code/futon3c`.

**So the isolation must be:** a separate user account; apm-lean copied with
history **truncated at the target revision** (or re-`git init`ed from the
historical tree, so no future objects exist at all); **no read access to
`/home/joe`**; fresh agent session per run. Joe's design is right on every
count and the homedir restriction is the load-bearing part.

### H3f. Scale — Joe is probably right that few problems are needed

The closed-problem design makes the effect cheap to detect, because the
dependent variable is now binary against a **known-achievable** target:

- **The noise floor should be low.** A closed problem re-run with the same
  corpus ought to close again with high probability — unlike an open problem,
  where run-to-run variance is unbounded.
- **The expected effect is large.** If a load-bearing memory is genuinely
  load-bearing, ablating it should move closure substantially, not marginally.
- Binary outcome + low floor + large effect ⇒ **small n suffices**.

Rough sizing from codex-7's own estimate (~20–35k runner tokens per dispatch):
**6–8 problems × 2 arms × 2–3 repeats ≈ 30–50 dispatches ≈ 0.8–1.4M runner
tokens.** Real but bounded, and far below what a hold-out design would have
cost in lost loop progress.

*a95J08 is the strongest candidate: claude-9 documented three failed Jensen
attempts and a first-try close via Hölder (§G6), so both the working and
failing routes are characterised in advance.*

### H3g. Codex-side memory as a confound — partly checked, and the isolation already covers it

*Joe, 2026-08-01: the remaining worry is whether anything in the **Codex** memory
system could confound the ablation — a runner recalling the solution from its
own persistent state rather than from ours.*

First pass, 2026-08-01:

| store | size | mentions candidate problems? |
|---|---:|---|
| `~/.codex/history.jsonl` | 20 MB | **none** for a95J08, a02J05, a96A04 |
| `~/.codex/goals_1.sqlite` | 32 KB | tables `thread_goals`, `thread_goal_continuation_deferrals` |
| `~/.codex/cache` | ~3.5 GB | **not examined** |

**Provisionally reassuring but not settled.** `history.jsonl` is plausibly the
*interactive* CLI history rather than Agency-dispatched runs, and the 3.5 GB
cache is unexamined. Open questions for V3: where do Agency-run codex agents
persist per-session state; is any of it keyed by repository or problem; does
"minting a fresh agent" clear it.

**The important point: Joe's isolation design already covers this whether or
not it is material.** `~/.codex` sits in his homedir, so a separate user with
no read access to `/home/joe` excludes it by construction — along with the git
object database and the futon3c lab artifacts (§H3e). One mechanism closes
three leakage paths, which is a good sign the design is cutting at the right
joint.

*Do not treat this as cleared. Verify what the fresh-agent path actually
inherits before running, and state the residual in the writeup.*

### H5. E2 VALIDATION VERDICT — not fit to run (codex-1, 2026-08-01)

Validated before spending, on the E1 pattern. **Verdict: E2 is not fit to spend
~1.4M tokens on.** Both of claude-2's own suspicions were confirmed with
executable counterexamples, and six further defects were found. The validation
cost one packet and saved a run that would have produced an uninterpretable
result.

**Both suspicions confirmed.**

| # | suspicion | counterexample |
|---|---|---|
| a | arm totals discard pairing | every run at exactly 1 attempt, but 4 LB rows against 1 control → **`rubricValidated`**. Nothing got harder; only arm cardinality changed. |
| b | noise floor measures closure, DV is attempts | control repeats of 1 and 100 attempts → floor **passes**. Arbitrarily unstable effort clears the pilot. |

**Six further defects.**

1. **Empty trace yields a substantive conclusion.** No runs → every arm total 0
   → `0 ≥ 0 + 3` false → falls through to `rubricUnsupported`. **Zero data
   scored as evidence against the rubric.** *Fixed: minimum panel of 2 runs
   per arm, else `indeterminate`.*
2. **The prose-only arm is analytically dead.** Registered, budgeted, and
   described as testing structured-field bias — and `classify` never reads it.
   `ablateProseOnly` appears exactly once in the file, in its own inductive.
   Running it could not change any outcome, at a quarter of the budget. *This
   is the "flag that never takes effect" failure the facility exists to
   prevent, committed one level up in an **arm**, where the facility does not
   guard.*
3. **Panel selection is not preregistered.** The probe artifact names four
   problems, the cost comment assumes 6–8, and no typed mapping fixes which
   LB / IN / prose-only memory pairs with which problem. Selection after
   inspecting candidates could dominate the result.
4. **Isolation probes are not bound to runner processes**, and codex-1's
   judgement — which claude-2 accepts — is that *stating* this is insufficient,
   because isolation is a validity precondition rather than a caveat. Each run
   needs a receipt gathered inside the same execution boundary as the runner:
   effective UID, checkout base revision, probe execution, probe-result hash.
5. **`teardownDeadline := some 1400000` was the token estimate in a time
   field.** The facility checks only `isSome`, so a number in entirely the
   wrong unit passed readiness. *Fixed to 86400 (one day in seconds); that the
   field cannot express its own unit is a gap to raise against the facility.*
6. The threshold hazard is not truncation here but **an unnormalised `+3`
   against totals whose cardinality is unconstrained** — the same root as (a).

**Required before E2 runs:** a fixed problem/memory panel; per-problem and
per-seed pairing and balance observables; paired attempt-difference
classification; an effort-variability pilot stop rule; minimum sample and
completeness checks; a decision role for the prose-only arm or its removal;
process-bound isolation receipts; an operationally typed teardown.

*Two of these were fixed immediately (1 and 5). The rest are redesign, not
patching, and should not be rushed.*

### H6. E2 after eleven review passes — compiles, and still should not run

**Status 2026-08-01:** typechecks and builds clean (763 jobs), all four
outcomes reachable, **seven guards each independently reaching
`indeterminate`**, and two adversarial regressions proven — a single run
withholding the wrong memory id, and two runs sharing a session, both now yield
`indeterminate` rather than a verdict.

**And codex-1's judgement, which claude-2 asked for directly and accepts: do
not spend the 1.4M tokens yet.**

**Nine serial defects, every one in the registration rather than the data.**
The decisive pattern is not the count but the kind: **three fields whose
promised semantics existed only in comments.**

| field | documented as | actually enforced by | found at pass |
|---|---|---|---|
| `withheldMemory` | the ablated memory, per arm | nothing — a trace withholding *nothing* reached `rubricValidated` | 9 |
| `sessionId` | distinct per run, no continuity | nothing — every run on one shared session reached `rubricValidated` | 10 |
| `baseRevision` | the pre-solution revision | nothing — every run at `"post-solution-revision"` reached `rubricValidated` | 11 |

Each was patched with a new observable and a new guard. **That the same failure
recurred three times is the finding**: adding a guard per invariant is a
discipline that depends on remembering, and the whole point of the facility is
to not depend on remembering.

**The refactor codex-1 proposes, and claude-2 endorses:**

```
raw Trace
   │  validate once, returning ALL protocol errors
   ▼
ValidatedTrace
   │  classify accepts ONLY this type
   ▼
Outcome
```

`classify : ValidatedTrace → Outcome` **cannot** omit a documented invariant,
because an unvalidated trace does not typecheck into it. The authoritative
panel binds problem, revision, and both memory ids together. The adversarial
traces built during review become a permanent regression suite.

*This is the same lesson as the facility's own `Flag`/`Observable` pairing —
"declaring a flag without declaring what it does is a type error rather than an
oversight" — applied one level up, to the trace. We reached it by making the
mistake three times instead of reading it off the design we were already using.*

### H4. Site status, measured 2026-08-01 — the federation is one site, not two

| site | status |
|---|---|
| dev-laptop | live; all state, all runners |
| **lucy-joe** | live and reachable; `lon-codex-1/2` active today. **But: no `apm-lean`, no `lake`/`lean` on PATH, no Mathlib cache.** 81 GB disk free (ample) and **7 GB RAM total, ~3 GB available** |
| **chicago** | `chi-claude-1` / `chi-codex-1` registered but **last active 2026-07-13**; hostname does not resolve from dev-laptop |

**So lucy-joe cannot currently run Lean dispatches at all**, and its RAM is the
harder problem than its missing toolchain: Mathlib builds routinely want
8–16 GB against lucy-joe's 7 GB total. Joe has already flagged that recovering
from an OOM here would be tricky.

**Consequences, in order of cost:**

1. The **noise-floor measurement (H2) needs no federation** — it is repeats on
   dev-laptop, and it is the prerequisite for everything else. Run it first.
2. The **filtered ablation (H3b) also needs no federation.** It answers
   claude-9's caveat directly and is cheap.
3. Federation adds independent execution. **Standing up a second Lean site is
   the expensive part** — toolchain, Mathlib cache, and a RAM headroom question
   that lucy-joe may simply fail. Chicago's specs are unknown and it needs
   reviving first.

*So: the counterfactual is runnable, and sooner than the federation is ready.
The database-shipping design is the right eventual shape and the wrong first
step.*

## I. The write-side of retrieval: duplication (claude-9, 2026-08-01)

**Witness, measured not estimated.** `ConstructionTargets/LusinN.lean` was built
by **copying** `problems/a95A02/lean/Main.lean`, not by extracting from it:
**17 declaration names shared, all 17 with byte-identical proofs**, no import
relationship between the files, and LusinN's own docstring still opening
*"APM a95A02"* — which is the only reason the copy is detectable at all. One
declaration unique to each side. A fix applied to one copy silently does not
reach the other, and nothing in the system knows they are the same seventeen
facts.

### I1. Retrieval failure has two directions and we have studied one

| direction | question | failure mode |
|---|---|---|
| **read-side** (studied) | *I need a fact — does it exist?* | **loud, bounded, self-correcting** — you re-derive, at the cost of one dispatch |
| **write-side** (unstudied) | *I have just proved a fact — does it already exist?* | **silent, permanent, compounding** — every later fix must be applied N times or the copies diverge, with nothing signalling the divergence |

**Nobody runs the write-side query, because nothing prompts it.**

**The prediction, and it is testable:** a retrieval system tuned for precision
will *systematically manufacture duplication*, and the duplication will never
appear in its own metrics, **because a duplicate is a successful-looking
authoring event.** Duplication rate should therefore rise with retrieval
threshold.

**claude-2's structural note — this is the write-side twin of a V2 finding.**
The decline taxonomy scored **discoverability at exactly zero** across 94 coded
declines, because every receipt field is closed over the *offered* set, so an
artifact never offered appears nowhere (V2 §4.1, §5.1). Duplication is
invisible for the identical reason: **the instrument records attempts, never
omissions.** Report the two adjacently; each is weak alone and the pair is a
structural claim.

### I2. What is missing is identity, not recall

Episodic memory stores *"I proved X while working on P"*, which answers "have I
seen something like this?" It does not answer **"is this THE one, and where
does it canonically live?"** Duplication is the symptom of a memory with recall
and no **identity**: no canonical location per fact, no notion that two
episodes concern the same object.

**This is where embedding the Lean code earns its keep, and more clearly than
for retrieval.** A statement is a syntactic object with a normal form; two
proofs of one theorem are detectable in a way two prose summaries are not.
Duplicate *detection* is an easier target than relevance **and has hard ground
truth**, which makes it a good calibration instrument for the embedding work
whether or not it is deployed.

**claude-2's test, and why its weakness is the point.** Over the 62 distinct
memory ids in the frozen corpus, only **2 near-duplicate pairs** appear at
Jaccard ≥ 0.5 on title tokens — and at least one is a *correction sequence*
(`evaluate-…-by-Abel` against `remove-Abel-regularization-…`), not a duplicate.
But **only titles were comparable**: prose memories have no normal form, so a
duplicate written in different words is invisible to any test the corpus
supports. The negative result therefore *demonstrates* the argument rather than
testing it, and is a stronger case for code embedding than the retrieval
argument was.

### I3. A norm must be authored before recurrence can become a problem

**This corrects §G1 rather than extending it.** The conative framing held that
repeated encounters get promoted into a problem because they register as a gap
against an optative direction. The negative instance shows the direction must
exist *first*:

> **Nobody noticed the duplication, because nothing in anyone's optative
> structure wanted one copy.** A sorry-gap is promoted instantly — everyone
> wants the holes closed — so every encounter with a hole registers. The
> duplication was encountered by every agent that read either file, for a week,
> and registered as nothing.

It became visible only when the norm was **authored explicitly** (now rule R2 in
`M-codex-sorry-loop/construction-targets.md`). *The norm did not emerge from
the encounters; it had to be written, and the encounters became legible
retroactively.* So conative structure is **prior**, not emergent.

**claude-2: this happened inside V2's own apparatus today.** Three fields in one
preregistration — `withheldMemory`, `sessionId`, `baseRevision` — carried
semantics stated only in docstrings and enforced by nothing, each admitting a
trace that reached a substantive verdict while ablating nothing, sharing one
session, or running at post-solution revisions. Both reviewers read them
repeatedly and registered nothing, until codex-1 authored the norm *"check
whether the docstring's claim is enforced"* — after which all three surfaced in
three consecutive passes. **Same shape: the encounters were never the problem;
the absent norm was.**

**Limit on the endogenous-ease hypothesis (§G3).** A system pursues ease only
along dimensions it has norms for, and is blind along the rest. **Copying is
locally cheaper than extracting**, so an ease-seeking system without a
duplication norm will actively *prefer* duplication.

**Status:** cleanup specified, not executed — import + `open
ConstructionTargets.LusinN`, delete the 17 local copies, keep a95A02's unique
part-(c) declaration. Held because a95A02 is one of six statement-defect rows
under Joe's hold. Recorded on the queue row as `:duplication-debt` with the
full name list.

## F. Deployment gate blocking P1

`futon1b`'s live process rejects the DF-only text-search route
(`?df=…` → 400 `q parameter required`) although `futon1b_server.clj` implements
it, ordering the `df` clause before the `q` check. Verified: `?stats=true`
works, `?q=…&df=…` silently ignores `df`. So the running image predates the
route; there is no proxy, and a search-result count is not a document
frequency.

P1's preregistration names the DF endpoint specifically, so codex-1 correctly
stopped rather than substitute.

**RESOLVED 2026-08-01 without touching the service — and the reload I proposed
would not have worked.** Joe authorised a Drawbridge reload; checking it first
showed two independent reasons it was the wrong instrument:

1. **The drawbridge on :6768 belongs to a different JVM.** pid 4009446 (started
   Jul 28) owns :6768 and :7070; futon1b is pid 2001439 (started Jul 31
   05:38 — matching codex-1's observation exactly). :6768 cannot reach it.
2. **Even with a REPL into the right process, a reload would not take effect.**
   `futon1b_server.clj:737` mounts `(handler text-search-route)`, and `handler`
   (`:255`) `reify`s an `HttpHandler` that **captures the function value** at
   `start-server!` time. Rebinding the var leaves the mounted reify holding the
   old fn. This is the same closure-capture shape as
   [[feedback_serving_ns_reload_deadlock]], which also documents the
   reload-under-traffic deadlock hazard.

**The actual unblock: the FTS index is a file.**
`migration-store-21/fts5-evidence.db` (281 MB) is the same index the endpoint
would query. Copied and queried read-only, it yields **exact** document
frequencies — not a proxy: `SELECT count(*) FROM ev_fts WHERE ev_fts MATCH
'<term>'`. 121,566 rows; sample DF liminf 199, recursion 123, convolution 260,
tendsto 845, integral 1725. `recursion` at 123 against `integral` at 1725 is
consistent with the docstring's flooring claim.

So P1 runs with **no restart, no reload, no deployment change and no proxy**,
against a copy so the live writer is untouched. Carried caveat: DF is
current-index (snapshot 2026-08-01) while dispatches are historical.

*Generalisable lesson: an authorised action is not automatically the right one.
Both reasons the reload would have failed were cheap to check and would have
been expensive to discover by attempting it on a shared live service.*
