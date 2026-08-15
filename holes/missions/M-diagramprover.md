# Mission: DiagramProver — Pattern-Driven Proof Search
Status: active — programme of work adopted 2026-08-02 (see §Programme of Work below)

**Date:** 2026-04-01 (IDENTIFY), 2026-04-01 (MAP), 2026-04-01 (DERIVE),
2026-04-01 (ARGUE), 2026-04-01 (VERIFY begun)
**Status:** VERIFY
**Cross-ref:** M-apm-solutions (proof peripheral, pattern library, sorry boundaries),
futon5 (TPG, AIF loops), vsat.wiki/ukrn-demo (Bayesian pattern models),
M-distributed-frontiermath (superpod, LeanDojo)

## SOTA: What Is Known About AI Theorem Proving (April 2026)

**AxiomProver** (Axiom, axiommath.ai) is the headline system. Published
claims from press releases, investor materials, and news coverage:

- Putnam 2025: 12/12 (120/120). Only five humans have achieved this in
  98 years; median human score is 0.
- Four previously unsolved conjectures closed, including Chen-Gendron
  (algebraic geometry / number theory) and Fel's conjecture (syzygies
  of numerical semigroups, solved autonomously).
- Architecture (from UBOS coverage): "natural-language problem statements
  are parsed and translated into Lean code with type-consistency checking,
  then a custom transformer explores proof space with an RL loop rewarding
  progress and penalizing dead ends, and finally each candidate proof is
  fed to Lean for formal verification, with backtracking on rejection."
- Data flywheel: verified proofs fed back into training, avoiding model
  collapse from unverified AI-generated data.
- Transfer learning claims into code verification (thin specifics).

**What is NOT published:** model architecture, training data, RL reward
shaping details, search algorithm, Lean integration specifics. No papers.
No open-source components beyond API wrappers. The GitHub repos are
consumer-facing, not research artifacts.

**Our assessment:** The 3-stage pipeline (NL→Lean, RL search, Lean verify)
is a plausible rational reconstruction from the press coverage, but we
cannot verify whether the actual system is top-down (fixed axiom search)
or something more nuanced. The "top-down vs bottom-up" framing below is
our philosophical positioning, not a claim about AxiomProver's internals.

**Other systems:**
- **LeanAgent** (Kumarappan et al., ICLR 2025, arxiv 2410.06209v8):
  Lifelong learning for formal theorem proving. Curriculum learning
  (easy→medium→hard by `e^S` complexity), progressive training (1 epoch
  per repo to balance stability/plasticity), dynamic premise database
  that grows as proofs succeed. Proved 155 sorry theorems across 23 Lean
  repos. Key finding: backward transfer — learning new tasks improved
  performance on old tasks. Open-source, builds on LeanDojo/ReProver.
  **Directly relevant**: their curriculum learning = our triage lanes,
  their dynamic database = our local Mathlib extensions, their
  stability/plasticity balance = our pattern library (stable) +
  TPG evolution (plastic).
- **LeanDojo-v2** (Hsiang et al., NeurIPS 2025 Workshop, LeanDojo v2 paper):
  Unified infrastructure for Lean+AI. Data extraction from any Lean repo,
  Pantograph REPL for tactic execution with goal-state introspection,
  pluggable search (DFS, MCTS, custom), training pipelines (SFT, LoRA, RL
  via GRPO/PPO). The search agent is a base class — **we can plug TPG in
  as a tactic generator**. Supports API inference (HuggingFace) so laptop
  runs are viable — tactic generation calls the API, search + REPL run
  locally. Rob is setting this up on the superpod for fine-tuning and
  large-model local inference.
- **AlphaProof** (DeepMind, 2024): RL + Lean, IMO silver medal performance.
  Not open-source. Architecture published in more detail than AxiomProver.
- **Axplorer / PatternBoost** (Axiom, open-sourced March 2025; based on
  Charton, Ellenberg, Wagner & Williamson 2024): Self-improvement loop
  for combinatorial math discovery. Generate candidates → train
  transformer on best → sample new candidates → local search to fix →
  retain best → retrain. Key findings: (a) the model provides starting
  points, local search does the actual discovery; (b) sparse encoding
  (edges only, not adjacency matrix) gives 100x speedup; (c) "exploit
  first, then explore" beats usual exploration-first temperature
  scheduling. Runs on a laptop with GPU (MacBook Pro). **Our hypothesis:
  TPG replaces the transformer, eliminating the GPU requirement. The
  loop structure maps directly to DiagramProver.**
- **Curriculum-level provers** (various): Most benchmarks focus on
  competition math or miniF2F. Curriculum math (prelim-style) against a
  specific Mathlib API surface is underexplored — our 489-problem corpus
  may be a novel benchmark.

**The gap DiagramProver addresses:** All published systems treat each
theorem as an independent search problem. None exploit cross-problem
structure (sorry boundary clustering, pattern transfer, targeted Mathlib
extension as a strategic intervention). This is the Deleuzian contribution:
reading the *diagram* of failures across a corpus to guide where to extend
the *axiom system* (Mathlib) next.

## The Name

The name "DiagramProver" contrasts with "AxiomProver" at the level of
Deleuze vs Descartes — but this is our framing, not a claim about what
AxiomProver actually does internally.

DiagramProver works bottom-up: observed data → patterns → projected
interventions → evolve new capacity. The patterns are fluid; they emerge
from proof attempts, mutate through TPG evolution, and are validated
against real sorry boundaries. The diagram maps intensities (where is
the proof stuck? what's the shape of the gap?), creates new realities
(Mathlib extensions, novel tactic sequences), and resists the
reterritorialization of fixed axiom systems.

The diagram is not a proof — it's a map of the proof landscape that
shows where movement is possible. The axiom system is not wrong — it's
incomplete. DiagramProver extends it by reading the diagram of what's
missing.

## The Problem

We have 489 prelim problems. An overnight run produced 19 complete Lean
proofs, ~30 partial proofs with diagnosed sorry boundaries, and a
pattern library of 12 formalization patterns extracted from successes.

AxiomProver-style approaches (RL-trained transformer doing tactic search)
would treat each sorry as an independent search problem. This ignores
the structure we've already built:

- The pattern library tells us *what kind of move* each sorry needs
- The sorry boundary descriptions tell us *why* the move is blocked
- The Bayesian model (not yet built) could tell us *which extension
  to build next* for maximum impact across the corpus
- TPG (futon5) could evolve tactic-search programs that embody the
  patterns as executable strategies, not just recognition heuristics

The missing piece: a system that reads the diagram of sorry boundaries
and pattern successes/failures, projects where intervention will have
the most impact, executes that intervention, and feeds the result back
into the diagram.

## Scope

### In scope

- Sorry boundary atlas: structured data from all Pass 1 proof attempts
- Pattern-as-diagram induction: extract wiring diagrams from proved proofs
  via futon5 pattern-to-diagram translation (the category theory connection)
- Bayesian pattern model: at least model A (Beta-Binomial), possibly B/C,
  operating over diagram compositions not just flat patterns
- TPG integration: futon5 TPG pointed at Lean tactic search (binary fitness)
- LeanDojo integration: when available on superpod
- Pattern library feedback loop: successful TPG programs → new diagrams
- First concrete experiment: cluster → extend → re-run → measure

### Out of scope

- **Training a neural prover from scratch** — we use TPG/LeanDojo, not a
  custom transformer. AxiomProver's data flywheel is out of reach without
  their compute budget.
- **Competition math** — Putnam-style problems require different heuristics.
  We stay on curriculum math (prelims) where Mathlib coverage is the bottleneck,
  not mathematical creativity.
- **Replacing the proof peripheral** — DiagramProver augments the sorry-kick
  loop, it doesn't replace the observe/propose/execute discipline. The informal
  proofs remain the primary product.
- **Web UI** — all interaction is via REPL, Drawbridge, and Emacs.

## Completion Criteria

1. **Sorry boundary atlas** exists as queryable EDN with blocker-type clustering
   and cross-problem impact links for ≥100 problems.
2. **Bayesian model A** (Beta-Binomial) is implemented and ranks interventions.
   Ranking is validated against manual expert ranking on ≥20 sorry boundaries.
3. **At least one targeted Mathlib extension** has been written based on the
   model's recommendation and has closed ≥3 sorry across different problems
   in a re-run.
4. **TPG or LeanDojo** (whichever is available first) has been pointed at ≥10
   sorry boundaries and has closed ≥2 that the conductor alone could not.
5. **Pattern library has grown** from the initial 12 to ≥20 patterns, with at
   least 4 extracted automatically from TPG programs or LeanDojo traces.

## Owner and Dependencies

**Owner:** Joe (architecture + Bayesian model), with Claude/Codex for
implementation and proof search.
**Repos:** futon3c (conductor, pattern library), futon5 (TPG, AIF),
apm-lean (Lean proofs, Mathlib extensions), futon6 (frame workspaces).
**Blocking dependencies:**
- M-apm-solutions Pass 1 ≥200 problems (for sorry boundary data volume)
- futon5 TPG infrastructure reachable from futon3c (for Phase 3)
- Rob's LeanDojo on superpod (for Phase 4, non-blocking — Phase 3 can
  proceed with binary fitness)

## Architecture

### Layer 1: The Diagram (data)

The sorry boundary map. Each entry is:

```edn
{:problem-id "a00J01"
 :sorry-location "lower_bound"
 :goal-state "∀ᶠ p in atTop, α ≤ eLpNorm f p μ"
 :blocker {:type :api-gap
           :description "ENNReal rpow exponent continuity"
           :mathlib-api-tried ["ENNReal.rpow_zero" "continuousAt_const_rpow"]
           :what-would-close-it "Tendsto (t ^ ·) (𝓝 0) (𝓝 1) for 0 < t < ⊤ in ℝ≥0∞"
           :cross-problem-impact ["a00J03" "a93A03" "a97A04"]}
 :pattern-attempted :P-rpow-exponent-limit
 :pattern-outcome :partial  ;; pattern identified the right API but couldn't bridge coercions
 :subject :analysis}
```

Populated from: overnight run proof states, frame workspace changelogs,
conductor logs, pattern library match results.

### Layer 1.5: Pattern Diagrams (futon5 translation)

The pattern library currently stores patterns as prose + lists:

```edn
{:recognition "Union of connected sets sharing a common point → ..."
 :mathlib-api ["isPreconnected_sUnion" ...]
 :tactic-chain ["classical" "by_cases" ...]}
```

This is a good intention, not a composable artifact. futon5's deeper
contribution (beyond TPG) is the translation between natural-language
pattern descriptions and typed wiring diagrams. Each pattern becomes a
diagram with:

- **Input ports**: what the pattern consumes (goal state type, available
  hypotheses, Mathlib lemmas in scope)
- **Output ports**: what it produces (closed subgoal, new hypotheses,
  modified goal state)
- **Timescale**: single-tactic (one `apply`) vs multi-step composition
  (a `calc` block or `have` chain)
- **Closure condition**: when is the pattern "done"? (subgoal closed,
  sorry removed, `lake build` passes)
- **Exogeneity**: what comes from Mathlib (exogenous, stable) vs what
  the prover must construct (endogenous, search target)

**Composition**: two pattern diagrams can be wired together when the
output ports of one match the input ports of the other. This is
type-checked *before* running Lean — if P-measure-restrict-simplify
outputs a goal of type `eLpNorm f p μ ≤ eLpNorm f ⊤ μ` and
P-lp-norm-comparison takes that as input, the composition is valid.
If the types don't match, the Bayesian model shouldn't even consider
the pair.

**Induction from data (the category theory connection)**: The patterns
are not prescribed from theory — they are *induced from proved
problems*. Each proved problem is a specimen. We dissect its proof
into a wiring diagram: which lemmas were applied, in what order, with
what type transformations at each step. The diagram IS the pattern.

This connects to the category theory "bonus round" deferred in
M-apm-solutions: a proved Lean proof is a morphism in a category
whose objects are goal states and whose morphisms are tactic
applications. The wiring diagram of a proof is its string diagram
in that category. Pattern extraction is functor application: mapping
the concrete proof category into a pattern category where the objects
are goal-state *types* (not specific goals) and the morphisms are
reusable tactic strategies.

This makes the pattern library a *free category* generated by the
successful proofs — and new proofs extend the category. The Bayesian
model (Layer 2) then operates over morphisms in this category,
estimating which compositions are likely to succeed on unseen goals.

**Concrete first step**: For each of the 19 proved overnight proofs,
extract the tactic trace as a sequence of (goal-state-before,
tactic-applied, goal-state-after) triples. This is the raw data for
diagram induction. futon5's pattern-to-diagram translator compiles
these traces into typed wiring diagrams. The diagrams replace the
prose pattern library entries.

### Layer 2: The Bayesian Model (inference)

Prior: P(sorry closes | pattern, problem-type, Mathlib-coverage-area)
Updated by: each proof attempt (success or failure)
Outputs: ranked list of interventions by expected impact

Interventions are:
- **Write Mathlib extension X** — unblocks N sorry across M problems
- **Apply pattern P to problem Q** — high prior from similar successes
- **Evolve TPG program for tactic family T** — covers a cluster of sorry

The UKRN demo structure:
- Qualitative data (sorry descriptions) → coded themes (pattern types)
- Themes × observations → Bayesian network
- Network → projected intervention impact
- Intervention → execute → observe → update

### Layer 3: Search — TPG + LeanDojo-v2

Two tactic-search mechanisms, one laptop-runnable, one superpod-scale.

**TPG (laptop, futon5):** Tangled Program Graphs evolve tactic-search
programs. Population seeded from pattern diagrams (Layer 1.5).
Runs locally — no GPU required. Binary fitness initially (sorry
closed or not). Key advantage over neural search: evolved programs
are inspectable and become new pattern library entries automatically.

**LeanDojo-v2 (laptop + superpod):** The integration layer, not just
a search backend. Three roles:

1. **Data extraction**: Point at apm-lean repo, extract theorems,
   proofs, premises, dependencies. Builds the sorry boundary atlas
   (Layer 1) automatically from the Lean source.
2. **Pantograph REPL**: Python interface that executes a tactic and
   returns the updated goal state. This gives TPG its subgoal
   introspection for fitness evaluation (Path 1): after applying
   a TPG-generated tactic, Pantograph reports how many goals remain
   and what their types are. **This is the bridge between TPG and
   Lean.**
3. **Pluggable search**: The `BaseProver` class accepts any tactic
   generator. We plug in TPG as a tactic generator alongside
   LeanAgent/ReProver/API models. Same search framework, different
   tactic sources — direct A/B comparison.

**Laptop vs superpod split:**

| | Laptop | Superpod |
|---|---|---|
| TPG evolution | Yes — CPU-only, futon5 | Yes — faster population |
| Pantograph REPL | Yes — runs locally | Yes |
| LeanDojo data extraction | Yes — local apm-lean repo | Yes — larger repos |
| Tactic generation (small model) | Yes — API inference via HuggingFace | Yes — local inference |
| Fine-tuning (LoRA/SFT) | No — needs GPU | Yes |
| Large model local inference | No | Yes — DeepSeek-Prover-V2-7B+ |

The laptop runs the full DiagramProver loop at lower throughput:
TPG generates tactic candidates, Pantograph evaluates them, the
Bayesian model picks the next sorry to attack. The superpod adds
fine-tuned neural tactic generation and faster search.

**Curriculum learning connection (from LeanAgent):** LeanAgent's
progressive training — 1 epoch per repo, easy→medium→hard ordering,
dynamic premise database — maps to our architecture:

- Our triage lanes (quick/medium/hard) = their curriculum ordering
- Our local Mathlib extensions (ApmCanaries/Local/) = their dynamic
  premise database
- Their backward transfer finding (new tasks improve old performance)
  = our hypothesis that the pattern library compounds across batches

After each batch of proved problems, we should **actively reorder
the remaining problems** by updated difficulty estimates, not just
accumulate patterns. The Bayesian model (Layer 2) provides the
updated estimates; the triage is re-run using posteriors, not priors.

### Layer 4: Lean Verification (ground truth)

Every candidate proof is verified by `lake build`. No exceptions.
This is the one axiom we keep: Lean's type checker is the final
arbiter. The diagram maps possibilities; Lean confirms reality.

Pantograph (via LeanDojo-v2) provides *incremental* verification:
each tactic step is checked as it's applied, not just the final
proof. This means search can backtrack on the first failing tactic
rather than building a complete proof and checking at the end.

### The Loop

```
Observe sorry boundaries (Layer 1, auto-extracted by LeanDojo-v2)
  → Induce pattern diagrams (Layer 1.5, futon5 translation)
  → Infer highest-impact intervention (Layer 2, Bayesian model)
  → Generate tactic candidates (Layer 3, TPG or LeanDojo search)
  → Verify incrementally (Layer 4, Pantograph REPL)
  → Update diagram with result
  → Repeat
```

Each iteration either:
- Closes a sorry → pattern extracted, Bayesian prior updated
- Fails with new information → sorry boundary refined, search narrowed
- Produces a Mathlib extension → unblocks multiple sorry at once

## Relation to Existing Infrastructure

| Component | Exists | Where |
|-----------|--------|-------|
| Sorry boundary data | Yes (partial) | proof-state/*.edn, frame changelogs |
| Pattern library | Yes (12 patterns) | data/apm-formalization-patterns.edn |
| Pattern injection into prompts | Yes | apm_conductor_v2.clj |
| Bayesian model | No | To build (cf. vsat.wiki/ukrn-demo) |
| TPG infrastructure | Yes (futon5) | ~/code/futon5/ |
| TPG → Lean integration | No | To build |
| LeanDojo on superpod | In progress | Rob's work, ~/code/futon5/ superpod |
| Lean verification loop | Yes (manual) | lake build + conductor sorry-kick |
| AIF loop structure | Yes (futon5) | To wire to proof domain |

## What Makes This Different from AxiomProver

| | AxiomProver | DiagramProver |
|---|---|---|
| Search | RL-trained transformer | TPG-evolved tactic programs |
| Training data | Self-generated proof flywheel | 489 prelim problems + sorry boundaries |
| Patterns | Implicit (in model weights) | Explicit (inspectable, teachable) |
| Strategy | Maximize proof rate | Maximize *learning* from proof attempts |
| Failure mode | Black-box search exhaustion | Diagnosed sorry → Bayesian → targeted extension |
| Transfer | To similar problems (implicit) | To students (explicit patterns + breakpoints) |
| Philosophy | Cartesian: deduce within axioms | Deleuzian: map the diagram, extend the territory |

The key difference: AxiomProver is trying to prove theorems. DiagramProver
is trying to *understand why theorems are hard to prove*, and use that
understanding to make the next theorem easier. The proofs are a byproduct
of the understanding, not the goal.

## Open Design Questions

### Layer 2: Bayesian Model — candidate structures

The model must answer: "given this sorry boundary, which intervention
has the highest expected impact?" Three candidate structures:

**A. Beta-Binomial per pattern-blocker pair (simplest).**
Each (pattern, blocker-type) pair gets a Beta(α, β) prior.
α increments when the pattern closes a sorry of that blocker type,
β increments when it fails. Posterior mean = α/(α+β) = success rate.
Intervention ranking: pick the pair with highest posterior mean ×
cross-problem count (expected sorry closed).

Pro: trivially implementable now from existing data (12 patterns ×
~5 blocker types = 60 cells). Con: no sharing of information across
similar patterns or subjects. The topology P-connected-union pattern
learns nothing from the analysis P-measure-restrict pattern, even
if both involve "rewrite μ(univ) for a restricted measure."

**B. Hierarchical model with partial pooling across subjects.**
Pattern success rates are drawn from a subject-level distribution:
θ_{p,s} ~ Beta(α_s, β_s), where (α_s, β_s) are estimated per
subject. Analysis patterns share strength; topology patterns share
strength. A new pattern in analysis starts with the analysis-level
prior, not a uniform prior.

Pro: better estimates with sparse data (most patterns have 1-3
observations). Con: requires choosing the pooling structure — do
we pool by subject, by blocker type, or both? Needs ~50+ observations
to reliably estimate the hyperparameters.

**C. NPT-style model (cf. UKRN demo).**
A directed Bayesian network where nodes are: problem-subject,
blocker-type, pattern-attempted, Mathlib-coverage-area, outcome.
The conditional probability tables are estimated from data. The
network structure encodes domain knowledge: subject influences
which patterns are relevant, Mathlib coverage influences whether
a pattern can succeed, blocker type determines which intervention
is needed.

Pro: richest model, can answer counterfactual questions ("if we
added ENNReal rpow-exponent continuity to Mathlib, how many sorry
would close?"). Con: requires the most data and the most structural
assumptions. Probably Phase 2b after the simpler models are validated.

**Phase 2 starts by comparing A and B on the existing 489-problem
data** (once Pass 1 has enough sorry boundary observations). Model C
is deferred to Phase 2b.

### Layer 3: TPG fitness — subgoal measurement

"Partial progress (subgoals reduced)" requires introspecting Lean's
proof state between tactic steps. Two paths:

**Path 1: LeanDojo proof-state extraction.** LeanDojo exposes the
tactic state (goals, hypotheses, types) at each step. Fitness =
(initial sorry count - final sorry count) + 0.1 × (initial subgoal
count - final subgoal count). This makes Phase 3 dependent on Phase 4
(LeanDojo integration). Honest dependency — note it.

**Path 2: Binary fitness only.** Fitness = 1 if sorry closes, 0
otherwise. No partial progress signal. Simpler, no LeanDojo
dependency. TPG evolves by finding *any* tactic sequence that works,
without gradient toward partial solutions. May be sufficient for
prelim-level problems where the search space is narrow enough that
binary signal suffices.

**Start with Path 2. Move to Path 1 when LeanDojo is available.**

### Cross-problem impact — population strategy

The `:cross-problem-impact` field in the sorry boundary EDN is
populated in three stages:

1. **Manual (now):** From the overnight run analysis, a human reads
   sorry descriptions and notes obvious connections. ("a00J01 and
   a93A03 both need ENNReal rpow-exponent continuity.")

2. **String-matching (Phase 1):** Cluster sorry boundaries by
   `:blocker.description` similarity (TF-IDF or embedding cosine).
   Automatic but noisy — catches "rpow" appearing in multiple
   descriptions but may miss semantic connections.

3. **Bayesian model (Phase 2):** The model infers cross-problem
   impact from the posterior: if closing sorry X with pattern P
   updates the posterior for sorry Y (because they share a
   blocker-type node), then X and Y are cross-linked. No circularity
   — the model *discovers* connections, it doesn't assume them.

## First Concrete Experiment

**Prerequisite:** existing data only (12 patterns, ~30 diagnosed
sorry boundaries from the overnight run). No Bayesian model, no TPG.

**Steps:**

1. **Cluster sorry by blocker type.** Read all sorry boundary
   descriptions from proof-state EDN files. Group by hand into
   blocker categories: api-gap (missing Mathlib lemma), coercion-
   bridge (ℝ↔ℝ≥0∞ type wiring), tactic-composition (right lemmas
   known but can't chain them), structural-gap (no Mathlib coverage
   for the proof technique at all).

2. **Pick the highest-count cluster.** From the overnight run,
   "coercion-bridge" and "api-gap" are likely the largest clusters.

3. **Write one targeted Mathlib extension.** For the highest-count
   cluster, write one lemma or tactic macro that addresses the common
   blocker. E.g., if 5 sorry need `ENNReal.rpow` exponent continuity,
   write `ENNReal.tendsto_rpow_atTop` and add it to
   `apm-lean/ApmCanaries/Local/`.

4. **Re-run the conductor on that cluster.** Use the v2 conductor
   with the local extension available. Measure: how many sorry close?

5. **Evaluate.** If K sorry close from 1 extension, that's the
   empirical cross-problem impact for this blocker type. Record it.
   This becomes the first data point for the Bayesian model.

**Expected timeline:** One weekend with what exists now.
**Expected outcome:** 3-8 sorry closed from 1 targeted extension,
plus the first calibration data for the Bayesian model.

## Scope (revised)

### Phase 0: First Concrete Experiment (above)

Cluster, extend, re-run, measure. No new infrastructure.

### Checkpoint 1: PatternBoost-on-a-Dell

**Goal:** Demonstrate that the Axplorer/PatternBoost self-improvement
loop works for Lean tactic search on commodity hardware (no GPU),
using TPG in place of the transformer.

**Motivation:** Axiom's Axplorer (open-sourced March 2025, based on
PatternBoost by Charton, Ellenberg, Wagner & Williamson 2024) runs a
generate→train→sample→local-search→select cycle for combinatorial
math discovery. They got 100x efficiency over brute force on an L4
GPU. The core insight: the AI model provides good starting points
for local search; it doesn't do the discovery itself. Our hypothesis:
TPG can replace the transformer in this loop, eliminating the GPU
requirement entirely.

**The loop on a Dell:**

```
1. Score initial candidates
   → lake build on 39 sorry boundaries (binary: closed or not)

2. "Train" (evolve TPG population)
   → Seed from 12 pattern library entries
   → Each TPG program is a tactic-sequence generator
   → Fitness: did the generated tactics close the sorry?
   → Selection + crossover + mutation (CPU-only, no backprop)

3. Sample new tactic candidates from evolved TPG programs
   → Each program generates a tactic sequence for a target sorry

4. Local search (Pantograph REPL)
   → Apply tactics step-by-step
   → Backtrack on failure
   → This is the "sculptor" — TPG is the "apprentice"

5. Retain best, re-evolve
   → Successful tactic sequences → extract as new patterns
   → Add to pattern library → seed next TPG generation
   → Bayesian model updates priors on blocker types
```

**Sparse encoding (from Axplorer's key insight):** Don't encode the
full proof state. Encode only: (a) the sorry goal type, (b) the
available hypotheses, (c) the pattern hint. This is our sorry boundary
EDN — already sparse by design.

**Temperature analogy:** Axplorer found "exploit first, then explore"
beats the usual exploration-first wisdom. For TPG: start with low
mutation rate (exploit known patterns), increase mutation once easy
sorry are closed and duplicates appear in the population. This matches
LeanAgent's curriculum: easy→medium→hard.

**Hardware:** Dell laptop, no GPU. TPG evolution is CPU-only.
Pantograph REPL runs locally. Tactic generation is program execution,
not neural inference. The only external dependency is LeanDojo-v2
for Pantograph (Phase 3a).

**Success criteria:**
- TPG evolves at least one tactic program that closes a sorry the
  conductor could not close
- The closed sorry yields a new pattern not in the original 12
- Total compute time < 24 hours on the laptop
- No GPU used at any point

**Expected timeline:** After Phase 3a (LeanDojo-v2 installed) +
Phase 3b (TPG wired as BaseProver). Possibly 1-2 weeks of
TPG evolution runs.

---

### Phase 1: Sorry Boundary Atlas + Diagram Induction (IDENTIFY → MAP)

Extract structured sorry-boundary data from all Pass 1 results.
Build the diagram. Automate clustering (string-matching on blocker
descriptions). Populate cross-problem impact links.

In parallel: for each proved problem, extract the tactic trace as
(goal-before, tactic, goal-after) triples. Compile into typed wiring
diagrams via futon5's pattern-to-diagram translator. These diagrams
replace the prose pattern library entries and become the objects that
the Bayesian model reasons over.

### Phase 2a: Bayesian Pattern Model — Beta-Binomial (DERIVE)

Implement model A (Beta-Binomial per pattern-blocker pair).
Rank interventions by expected impact. Compare with manual
ranking from Phase 0.

### Phase 2b: Bayesian Pattern Model — Hierarchical (ARGUE)

Implement model B (partial pooling across subjects).
Compare with 2a. If data supports it, implement model C (NPT).

### Phase 3a: LeanDojo-v2 Integration — Laptop Layer (VERIFY)

Install LeanDojo-v2 locally. Point data extraction at apm-lean repo.
Verify Pantograph REPL works against our sorry theorems. This gives us:
- Automatic sorry boundary atlas extraction (replaces manual EDN)
- Incremental tactic evaluation for TPG fitness (Path 1)
- API inference for tactic generation (HuggingFace, no local GPU)

This is the foundation — everything else in Layer 3 builds on it.

### Phase 3b: TPG as Tactic Generator (VERIFY → INSTANTIATE)

Implement a LeanDojo-v2 `BaseProver` subclass that uses futon5 TPG
as the tactic generator. Binary fitness initially (sorry closed or
not), then upgrade to subgoal-counting fitness via Pantograph.
Seed TPG population from pattern diagrams (Layer 1.5). Evolve
against sorry boundaries. Extract new pattern diagrams from
successful programs.

Runs entirely on laptop — TPG is CPU-only, Pantograph is local,
tactic evaluation uses the REPL.

### Phase 4: Superpod Scale-Up

When Rob has LeanDojo-v2 on the superpod:
- Fine-tune DeepSeek-Prover-V2-7B on our apm-lean data (SFT/LoRA)
- Run neural tactic generation locally (no API latency)
- Compare: TPG tactic generator vs fine-tuned neural generator vs
  API inference — same LeanDojo search framework, different sources
- Re-run curriculum with updated difficulty estimates (LeanAgent-style
  progressive training: re-rank problems after each batch by
  Bayesian posterior, not initial triage)

## MAP — Infrastructure Survey (2026-04-01)

### Inventory: ready vs missing

| Component | State | Location | Notes |
|-----------|-------|----------|-------|
| Pattern library | **READY** | `futon3c/data/apm-formalization-patterns.edn` | 12 patterns, EDN, loadable, injected into v2 conductor prompts |
| Sorry boundary data | **READY** | `futon3c/data/proof-state/` | 76 proof state files, 39 partial with diagnosed sorry boundaries |
| Proved Lean files | **READY** | `apm-lean/ApmCanaries/Frames/` | 70 frame dirs, 31 sorry-free files with real theorem declarations |
| Frame workspaces | **READY** | `futon6/.state/proof-frames/` | 78 problem frames, 211 metadata JSON files |
| Conductor pattern injection | **READY** | `futon3c/.../apm_conductor_v2.clj` | `format-patterns-for-prompt` wired into `make-solve-prompt` |
| futon5 TPG | **READY** | `futon5/` | `tpg_render.clj`, evolution scripts, phenotype output, `best-tpg.edn` |
| UKRN Bayesian model | **READY** | `vsat.wiki/ukrn-demo/` | Clojure NPT model, geometric-mean gates, mode classification |
| apm-lean Local extensions | **EMPTY** | `apm-lean/ApmCanaries/Local/` | Directory exists, no files. First extension is Phase 0 deliverable |
| LeanDojo-v2 | **MISSING** | — | Not installed. Python package, needs pip + Lean toolchain |
| Pantograph | **MISSING** | — | Not installed. Required by LeanDojo-v2 for REPL |

### MAP questions and answers

**Q1: Is the sorry boundary data rich enough for Phase 0?**
Yes. 39 partial proof states with diagnosed blockers. The `:proof/output`
field in each EDN contains the sorry count, which Mathlib APIs were tried,
and what would close the gap. This is sufficient to cluster by blocker type
and pick the highest-count cluster for the first targeted extension.

**Q2: Can the pattern library be loaded and used programmatically?**
Yes. `load-pattern-library` and `patterns-for-subject` are working
functions in the v2 conductor. The format (EDN with `:id`, `:recognition`,
`:mathlib-api`, `:tactic-chain`, `:subjects`, `:difficulty`) is stable.
Pattern diagrams (Layer 1.5) will extend this format, not replace it.

**Q3: Can futon5 TPG be pointed at Lean tactic search?**
Partially. TPG evolution infrastructure exists (`tpg_coupling_evolve.clj`,
`tpg_pattern_evolution.clj`). The fitness function and action space need
to be adapted: currently TPG operates on futon5's own domain, not Lean
tactics. The adaptation requires: (a) defining the action space (Lean
tactic vocabulary), (b) connecting fitness evaluation to Pantograph REPL,
(c) seeding initial population from pattern library entries. This is
Phase 3b work.

**Q4: Can we run LeanDojo-v2 on the laptop?**
Unknown — not yet installed. The paper says API inference (HuggingFace)
eliminates the need for local GPU, so the laptop should handle search +
REPL. Rob can confirm. Installation is Phase 3a.

**Q5: Is the UKRN Bayesian model transferable to the proof domain?**
Structurally yes. The UKRN model uses NPT (Noisy-OR + geometric-mean
gates) over pattern-strength scores. The same structure works for
pattern × blocker-type × outcome, replacing "institution support factors"
with "Mathlib coverage factors." The assumptions.edn format can encode
our pattern priors. Adaptation is Phase 2a work.

**Q6: How many tactic traces can we extract from proved proofs?**
31 sorry-free Lean files with real theorem declarations. Each can be
traced to produce (goal-before, tactic, goal-after) triples. LeanDojo-v2
data extraction does this automatically (when installed). Manual
extraction is possible but tedious — ~30 minutes per file. LeanDojo
installation is the gating dependency for efficient trace extraction.

**Q7: What is the sorry blocker distribution?**
Not yet formally clustered. From manual overnight-run analysis:
- API gap (missing Mathlib lemma): ~15 problems (e.g., ENNReal rpow)
- Coercion bridge (ℝ↔ℝ≥0∞ wiring): ~8 problems
- Structural gap (no Mathlib coverage for proof technique): ~10 problems
- Tactic composition (right lemmas known, can't chain): ~6 problems
Formal clustering is Phase 1 work.

### Surprises

1. **31 sorry-free proofs, not 19.** The reclassification pass found 19,
   but continued codex-1 runs added more. The pattern library was
   extracted from the original 19 — the additional 12 may contain new
   patterns not yet catalogued.

2. **TPG phenotypes already exist.** `futon5/out/tpg-runs/` contains
   rendered TPG phenotypes from prior evolution runs. The rendering
   pipeline (`tpg_render.clj`) includes routing summaries, operator
   frequency analysis, and entropy metrics — all potentially useful for
   visualizing evolved tactic programs.

3. **apm-lean Local extensions directory is empty.** Despite being
   referenced in frame workspace prompts ("promote reusable lemmas into
   ApmCanaries/Local"), no lemmas have actually been promoted. The first
   targeted Mathlib extension (Phase 0) will be the first occupant.

4. **The UKRN model uses geometric-mean gates.** This is a bottleneck
   model — the weakest factor dominates. For proof search, this means
   the hardest step (the gate) determines whether a pattern succeeds,
   which matches reality: a 5-step tactic chain fails if any one step
   is impossible. This structure may be more appropriate than the
   Beta-Binomial model for Phase 2.

## DERIVE — Design (2026-04-01)

### Entity types

| Entity | Source | Type | Repo |
|--------|--------|------|------|
| **SorryBoundary** | Extracted from proof-state EDN | Derived (from proof attempts) | futon3c |
| **PatternDiagram** | Induced from proved Lean traces | Derived (from successful proofs) | futon3c + futon5 |
| **InformalPattern** | Pre-existing flexiarg library | Authored (human-written) | futon3 |
| **TacticProgram** | Evolved by TPG | Derived (from evolution) | futon5 |
| **BlockerCluster** | Grouped sorry boundaries | Derived (from clustering) | futon3c |
| **MathLibExtension** | Written to close a cluster | Authored (human or agent) | apm-lean |
| **InterventionRanking** | Output of Bayesian model | Derived (from inference) | futon3c |
| **ProofTrace** | Extracted from Lean via LeanDojo | Derived (from Lean source) | apm-lean |
| **FitnessEvaluation** | Pantograph REPL result | Derived (from tactic execution) | apm-lean |

### Relation types

| Relation | From | To | Type |
|----------|------|----|------|
| `:blocks` | SorryBoundary | ProofTrace | The sorry prevents the proof from completing |
| `:addresses` | PatternDiagram | BlockerCluster | The pattern's output ports match the cluster's goal type |
| `:closes` | TacticProgram | SorryBoundary | The program generates tactics that close the sorry |
| `:induced-from` | PatternDiagram | ProofTrace | The diagram was extracted from this proved proof |
| `:instantiates` | PatternDiagram | InformalPattern | The formal diagram is a concrete instance of an informal heuristic |
| `:unblocks` | MathLibExtension | BlockerCluster | The extension closes sorry across this cluster |
| `:recommends` | InterventionRanking | MathLibExtension | The Bayesian model ranked this intervention highest |
| `:seeded-by` | TacticProgram | PatternDiagram | The TPG population was initialised from this pattern |
| `:evolved-into` | TacticProgram | PatternDiagram | A successful program was extracted as a new pattern |

### Invariant rules

Expressible as core.logic relations:

```clojure
;; A PatternDiagram's output ports must type-match the goal of any
;; SorryBoundary it claims to address.
(defn addresses-valid? [pattern sorry]
  (port-type-compatible? (:output-ports pattern) (:goal-type sorry)))

;; A TacticProgram that :closes a SorryBoundary must have been verified
;; by Pantograph REPL — no self-reported closures.
(defn closure-verified? [program sorry]
  (some? (:pantograph-verification (get-closure program sorry))))

;; An InterventionRanking must be traceable to Bayesian posterior
;; computation — no ad hoc rankings.
(defn ranking-traceable? [ranking]
  (some? (:posterior-computation ranking)))

;; The :induced-from relation is monotone: once a pattern is induced
;; from a proof, it cannot be un-induced. The pattern library only grows.
(defn pattern-library-monotone? [library-before library-after]
  (every? (set library-after) library-before))
```

### Data flow

```
futon3c conductor (Pass 1)
  → proof-state/*.edn (SorryBoundary data)
  → apm-lean/ApmCanaries/Frames/*/Main.lean (ProofTrace source)
       ↓
LeanDojo-v2 data extraction (Phase 3a)
  → (goal-before, tactic, goal-after) triples
       ↓
futon5 pattern-to-diagram translator (Phase 1)
  → PatternDiagram entries (typed wiring diagrams)
       ↓
Bayesian model (Phase 2)
  ← PatternDiagram + SorryBoundary + BlockerCluster
  → InterventionRanking
       ↓
TPG evolution (Phase 3b) / LeanDojo search (Phase 4)
  ← PatternDiagram (seeds population)
  ← SorryBoundary (fitness targets)
  → TacticProgram candidates
       ↓
Pantograph REPL (Layer 4)
  ← TacticProgram
  → FitnessEvaluation (sorry closed? subgoals reduced?)
       ↓
Feedback:
  - Closed sorry → new ProofTrace → new PatternDiagram (library grows)
  - Failed attempt → updated Bayesian posterior (model learns)
  - New MathLibExtension → unblocks BlockerCluster → re-run conductor
```

### Design decisions (IF/HOWEVER/THEN/BECAUSE)

**D1: TPG replaces transformer in the PatternBoost loop.**
IF the Axplorer/PatternBoost self-improvement cycle requires a
generative model to produce candidate tactic sequences, HOWEVER
transformers require GPU and gradient-based training, THEN we use
TPG (evolved programs) as the generative model, BECAUSE TPG is
CPU-only, produces inspectable programs, and the programs can be
directly extracted as new patterns for the library. Trade-off:
TPG may have weaker generalization than a transformer, but the
pattern library provides strong priors that narrow the search space.

**D2: Sparse encoding for sorry boundaries.**
IF each sorry boundary has a full Lean proof context (hypotheses,
imports, sibling lemmas), HOWEVER full-context encoding is quadratic
in attention cost for transformers and combinatorially explosive for
TPG, THEN we encode only: goal type + available hypotheses + pattern
hint (the matching PatternDiagram), BECAUSE Axplorer's key efficiency
finding is that sparse encoding gives 100x speedup over dense encoding.
The pattern hint is the "prior distribution" that transforms brute-force
search into informed search.

**D3: Geometric-mean gate model for Bayesian inference.**
IF we need to estimate P(sorry closes | pattern, blocker-type),
HOWEVER the Beta-Binomial treats each factor independently while
proof tactics fail at the weakest link, THEN we use the UKRN demo's
geometric-mean gate model where the weakest factor dominates,
BECAUSE a 5-step tactic chain fails if any one step is impossible —
the bottleneck structure matches the mathematical reality. The
Beta-Binomial is the comparison baseline.

**D4: futon3 informal math patterns as the human-authored seed.**
IF we need to seed the pattern library with high-quality recognition
heuristics, HOWEVER our current 12 formalization patterns are
extracted from a small sample of proved proofs, THEN we also load
the 31 informal math patterns and 7 strategy patterns from
`futon3/library/math-informal/` and `futon3/library/math-strategy/`
as the informal backbone of the pattern library, BECAUSE these
patterns encode decades of mathematical practice in IF/HOWEVER/
THEN/BECAUSE form with NEXT-STEPS, and they cover exactly the
reasoning moves (argue-by-contradiction, compose-independent-lemmas,
estimate-by-bounding, pass-to-a-subsequence, etc.) that drive
prelim proofs. The formalization patterns are their Lean-specific
instantiations; the informal patterns are the recognition heuristics
that select which formalization to attempt.

Concretely:
- `argue-by-contradiction.flexiarg` → when TPG encounters a sorry
  whose goal is negated or existential, this pattern fires
- `compose-independent-lemmas.flexiarg` → when a sorry has multiple
  independent subgoals, decompose before searching
- `estimate-by-bounding.flexiarg` → analysis sorry involving
  inequalities → reach for `nlinarith`, `gcongr`, `calc`
- `pass-to-a-subsequence.flexiarg` → convergence sorry where the
  full sequence doesn't converge → `Filter.Tendsto` + `atTop`

The `:instantiates` relation links each PatternDiagram to the
InformalPattern it concretises.

**D5: Monotone pattern library (append-only).**
IF patterns could be deleted or modified as the system evolves,
HOWEVER this would break the stability property (backward transfer)
and make the Bayesian model's priors unreliable, THEN the pattern
library is append-only — new patterns are added, old patterns are
never removed or modified, BECAUSE LeanAgent's key finding is that
stability (retaining old knowledge) is more important than
plasticity (adapting to new data) for lifelong theorem proving.
Patterns can be *superseded* (marked as having a better alternative)
but never deleted.

### Views and projections

**REPL (primary):** All interaction via Drawbridge eval + Emacs REPL.
```clojure
;; Check sorry boundary atlas
(v2/sorry-boundary-atlas)

;; Run Bayesian ranking
(v2/rank-interventions)

;; Evolve TPG population for 100 generations against a sorry cluster
(dp/evolve-tactic-programs! :cluster :api-gap :generations 100)

;; Inspect an evolved program as a tactic sequence
(dp/program->tactics best-program sorry-boundary)
```

**PDF cheatsheet projection:** Each proved problem generates a
two-column cheatsheet (from M-apm-solutions rendering pipeline).
DiagramProver adds a third annotation layer: the wiring diagram of
the proof, showing which patterns were applied, in what order, with
what type transformations. This is the "proof anatomy" view — how
the proof *works*, not just what it says.

**Pattern evolution visualisation:** futon5's `tpg_render.clj`
already renders TPG phenotypes as PNGs. Extend to show the tactic
program structure alongside the sorry boundary it addresses.

### Wiring diagram (pilot)

The DERIVE wiring diagram for DiagramProver itself is deferred to
ARGUE (it's the mission's own architecture, not a proof diagram).

However, a **pilot** is needed now: take 2-3 proved proofs and 1-2
partials from the overnight run and manually construct their wiring
diagrams. This validates that:
1. The (goal-before, tactic, goal-after) triple representation works
2. Pattern diagrams can be induced from the triples
3. The `:instantiates` relation to informal patterns is meaningful
4. Partial proofs produce diagrams with open ports (sorry = open port)

Pilot candidates:
- **t92J01** (proved, 1 theorem, topology) — clean contradiction via
  isPreconnected_sUnion, should yield a simple linear diagram
- **t94A02** (proved, 10 theorems, topology) — multi-lemma composition,
  should yield a rich dependency diagram
- **a00J01** (partial, 2 sorry, analysis) — open ports at the
  ENNReal rpow boundary, validates partial-diagram representation

### Fidelity contract

This mission is greenfield, not a port. Fidelity is to the futon6
devmap vision (`futon3/holes/futon6.devmap`):

> "FUTON6 is a comprehensive mathematics dictionary where informal and
> formal arguments coexist, indexed by patterns at multiple levels of
> abstraction."

DiagramProver contributes to this vision by:
1. **Populating the formal argument layer** — each proved Lean proof
   is a formal entry in the dictionary
2. **Linking formal to informal** — the `:instantiates` relation
   connects PatternDiagrams to math-informal flexiarg patterns
3. **Indexing by patterns** — the pattern library IS the index; the
   Bayesian model ranks patterns by effectiveness
4. **Multiple levels of abstraction** — informal patterns (recognition
   heuristic) → formalization patterns (Lean API + tactic chain) →
   wiring diagrams (typed ports, composition) → TPG programs (executable)

The 12 new `math-formalization/*.flexiarg` files in futon3/library/
sit alongside the existing 31 `math-informal/` and 7 `math-strategy/`
patterns, completing the three-level pattern architecture:
- **math-informal**: when to try a strategy (recognition)
- **math-strategy**: how to structure the argument (composition)
- **math-formalization**: how to wire the Lean proof (execution)

## ARGUE — Why This Design Is Right (2026-04-01)

### Pattern cross-references

Seven futon3/library patterns apply directly to DiagramProver's design:

**1. `futon-theory/baldwin-cycle` [🔃/三]**
*Where it applies:* The entire DiagramProver feedback loop.
Phase 1 (EXPLORATION): TPG evolves tactic programs, tries variants
against sorry boundaries, preserves evidence of what was tried.
Phase 2 (ASSIMILATION): successful programs are extracted as new
PatternDiagram entries — learned behaviour fixed into genotype.
Phase 3 (CANALIZATION): the Bayesian model narrows the search space
by removing low-probability patterns from the candidate set. What
started as runtime search becomes compile-time structure (the pattern
library).

**2. `meta/baldwin-ratchet-defeats-darkroom` [🔃/今 🌏/甲]**
*Where it applies:* The conductor's tendency to accept scaffolds as
"proved" was a dark-room response — reducing observation (skip Lean
verification) to avoid the friction of sorry. The formal-alignment
gate, the artifact-content check, and the sorry-kick loop are all
Baldwin ratchets: they internalise the pressure into the agent's
exotype and require demonstrated behaviour (zero sorry, substantive
declarations) rather than claims.

**3. `aif/candidate-pattern-action-space` [📥/力]**
*Where it applies:* Layer 3 (TPG search). The action space for TPG
is not "any Lean tactic at any time" — it's a bounded candidate set
of pattern-informed tactic sequences. The Bayesian model (Layer 2)
constructs this candidate set by filtering patterns whose output
ports type-match the sorry goal. This prevents the search from being
"unstructured and dominated by recency" (the pattern's warning).

**4. `enrichment/rational-reconstruction` [日/引]**
*Where it applies:* The sorry boundary atlas (Layer 1). We don't
do a one-shot static analysis of all 489 problems. We build the
atlas incrementally — each conductor batch adds sorry boundary
observations, each is timestamped and correctable. The 31→19→31
reclassification sequence is exactly the "corrections between layers"
the pattern describes. Early layers WILL have errors; later layers
fix them. The correction history shows where the system's self-image
was wrong.

**5. `realtime/learn-as-you-go` [🌂/日]**
*Where it applies:* The pattern library's append-only growth (D5).
Each proof attempt — success or failure — generates a realtime
learning: "this pattern worked on this sorry type" or "this pattern
failed because of this coercion gap." These are the `{:works-well
:doesnt-work :evidence}` entries the pattern prescribes. Without
them, the loop stalls — the system keeps trying the same patterns
on the same blockers.

**6. `math-strategy/compose-independent-lemmas` [🧩/合]**
*Where it applies:* Layer 1.5 (pattern diagrams as wiring diagrams).
The diagram representation IS the dependency graph from this pattern.
Each PatternDiagram decomposes a proof into independent pieces,
verifies each is self-contained, and checks the composition step.
The invariant rule `addresses-valid?` (output ports type-match input
ports) is the formal version of "the composition step does not
silently introduce new claims."

**7. `math-informal/argue-by-contradiction` [🔺/今]**
*Where it applies:* As the most common informal pattern in the
overnight run (t92J01, topology sorry boundaries). The `:instantiates`
relation links the formalization pattern `connected-union-via-common-
point.flexiarg` to this informal pattern. This validates the
three-level architecture: the informal pattern tells you *when*
(existential/impossibility goal), the strategy pattern tells you
*how* (decompose, find the known fact violated), the formalization
pattern tells you *which Lean API* (isPreconnected_sUnion + classical).

### Theoretical coherence

The IDENTIFY theoretical anchoring was Deleuze's diagram vs axiom.
Does the DERIVE design serve it?

Yes: the diagram IS the sorry boundary atlas with its pattern overlays.
It maps intensities (which sorry boundaries cluster, which patterns
succeed where, which Mathlib APIs are the bottleneck). The axiom
system IS Lean's type checker + Mathlib. DiagramProver extends the
axiom system (writes Mathlib extensions) by reading the diagram
(Bayesian model identifies highest-impact intervention). The
reterritorialization pressure (fixed Mathlib, fixed tactic vocabulary)
is resisted by the deterritorialization force (TPG evolving new tactic
programs, pattern library growing, sorry boundaries remapping the
landscape after each extension).

The Baldwin cycle provides the mechanism: the diagram is the
phenotype (plastic, exploratory), the pattern library is the
genotype (stable, accumulated), and canalization narrows the
search space without losing the capacity to explore new territory.

### Trade-off summary

| We chose | Over | Because |
|----------|------|---------|
| TPG (CPU, inspectable) | Transformer (GPU, black-box) | Laptop-runnable, patterns extractable, no training data needed |
| Sparse encoding | Full proof context | 100x efficiency (Axplorer finding), pattern library provides the prior |
| Geometric-mean gates | Beta-Binomial | Matches tactic-chain bottleneck structure (weakest link dominates) |
| Append-only patterns | Mutable pattern library | Stability > plasticity (LeanAgent finding), corrections via supersession not deletion |
| futon3 flexiarg format | Custom EDN-only format | Integrates with existing 38 math patterns, IF/HOWEVER/THEN/BECAUSE structure reusable |

What we give up:
- **Neural generalization**: TPG may not generalize as well as a
  fine-tuned transformer to unseen tactic sequences. Mitigated by
  the pattern library providing strong priors.
- **Training data flywheel**: We can't self-train on our own proofs
  the way AxiomProver does. Mitigated by the Bayesian model updating
  on proof attempt outcomes (not gradient updates, but belief updates).
- **Speed**: TPG evolution is slower than neural inference for
  candidate generation. Mitigated by the bounded candidate set
  (pattern action space) keeping the search small.

### Generalisation notes

The design generalises beyond prelim mathematics:

- **Any Lean repository with sorry**: DiagramProver can be pointed at
  any Lean project, not just APM problems. The sorry boundary atlas,
  pattern library, and TPG search are domain-agnostic.
- **Other ITPs**: The architecture (sorry atlas → patterns → Bayesian →
  search → verify) works for Coq, Isabelle, or Agda with different
  Layer 4 verification backends.
- **Software verification**: Axiom claims transfer learning to code
  verification. Our pattern architecture (informal → strategy →
  formalization) could map to (design intent → architecture pattern →
  implementation tactic) for program correctness.
- **Pedagogy**: The breakpoint library (M-apm-solutions planned
  excursion) is a direct projection of DiagramProver's sorry boundary
  atlas into a student-facing format. The pattern library becomes the
  tutor's knowledge base.

### Plain-language argument

DiagramProver looks at where proofs get stuck, figures out which
stuck-points are related, and writes targeted fixes that unstick
multiple proofs at once. It gets better over time because each
success and failure updates its estimate of what works where.
It runs on a laptop because it uses evolved programs instead of
neural networks for search. The patterns it discovers are readable
by humans, so they double as teaching material. The whole thing is
grounded in Lean's type checker — no proof is accepted unless the
machine verifies it.

## VERIFY — Structural Check and Pilot (2026-04-01)

### Completion criteria pre-check

| Criterion (from IDENTIFY) | DERIVE addresses it? | Risk |
|---|---|---|
| Sorry atlas ≥100 problems | Yes (Layer 1, Phase 1) | Low — Pass 1 is running |
| Bayesian model A validated on ≥20 | Yes (Layer 2, Phase 2a) | Medium — needs sorry data volume |
| ≥3 sorry closed from 1 extension | Yes (Phase 0) | Low — can start now |
| TPG/LeanDojo closes ≥2 sorry | Yes (Checkpoint 1) | **HIGH — see pilot below** |
| Pattern library ≥20 with ≥4 auto-extracted | Yes (Layer 1.5 + Layer 3) | **HIGH — pattern-to-diagram gap** |

### Critical gap: pattern-to-diagram translation does not exist

The DERIVE design describes Layer 1.5 as "futon5's pattern-to-diagram
translator compiles traces into typed wiring diagrams." This code does
not exist. futon5's TPG infrastructure operates on MMCA cellular
automata with hexagram-aligned operators. It has:

- TPG core: teams, programs, routing, evolution (**ready**)
- Exotype representation: 6x6 matrices, hexagram lifting (**ready**)
- Diagnostics: feature vectors for decision-making (**ready**)
- Phenotype rendering (**ready**)

It does NOT have:
- Pattern → wiring diagram translation (**missing**)
- Typed ports for pattern composition (**missing**)
- String diagram / category theory formalism (**missing**)
- Any connection to Lean tactics (**missing**)

This means:
- Layer 1.5 (pattern diagrams) requires **new code**, not adaptation
- The `:instantiates` relation needs a formal representation of
  "instantiation" that doesn't yet exist
- TPG as a tactic generator (Phase 3b) requires defining a new
  action space, not reusing the hexagram operators

**This is the riskiest part of the design.** The ARGUE section claims
the Baldwin cycle operates via pattern library → TPG → new patterns.
But the bridge between patterns (flexiarg, prose) and TPG (programs,
feature vectors) is unbuilt.

### Pilot specification

The pilot tests both directions of the pattern-to-diagram bridge,
starting small enough to validate the concept before building
infrastructure.

**Pilot A: Proof → diagram (3 specimens)**

For each proved proof, manually construct the wiring diagram:
1. Read the Lean file
2. List each tactic step as (goal-before, tactic, goal-after)
3. Draw the diagram: nodes = goal states, edges = tactics
4. Identify which formalization pattern each step instantiates
5. Check: can the diagram be composed from existing patterns?

| Specimen | Type | What it tests |
|----------|------|---------------|
| t92J01 (1 theorem, topology) | Simple linear | Does the contradiction pattern yield a clean diagram? |
| t94A02 (10 theorems, topology) | Multi-lemma DAG | Do independent lemma compositions show up as parallel branches? |
| a00J01 (2 sorry, analysis) | Open ports | Do sorry boundaries appear as open output ports in the diagram? |

**Pilot B: Pattern → diagram → improved proof (2 attempts)**

For each pattern, manually construct a wiring diagram with typed
ports, then apply it to a sorry boundary where the ports should match:

1. Take a formalization pattern (e.g., `rpow-exponent-limit.flexiarg`)
2. Define input ports: {goal : `Tendsto (t ^ ·) ... (𝓝 1)`, hyp : `0 < t`}
3. Define output ports: {closed-goal : `True`}
4. Find a sorry boundary whose goal matches the input port type
5. Manually apply the pattern's tactic chain to the sorry
6. Record: did it close? If not, where did the ports mismatch?

| Pattern | Target sorry | What it tests |
|---------|-------------|---------------|
| `rpow-exponent-limit` | a00J01 lower_bound | Does the Real rpow limit bridge to the ENNReal context? |
| `measure-restrict-simplify` | any analysis sorry involving volume.restrict | Does the μ(univ)=1 rewrite chain transfer? |

**Pilot B is the real test.** If the pattern's typed ports predict
where it will succeed and fail, the diagram representation is
validated. If the ports say "match" but the proof fails anyway
(because of coercion gaps, universe issues, or API changes), the
port typing is too coarse and needs refinement.

### Pilot C: Tension analysis → new patterns (completed)

The VERIFY pilot's most productive direction was not diagram
construction but tension identification. Three proof failures were
analysed using `constraint-tension-resolution` methodology:

| Failure | Tension | Pattern induced |
|---------|---------|----------------|
| a00J01: proved limit in ℝ, can't lift to ℝ≥0∞ | Math representation ≠ API representation. Coercion chain is directed (lossy), not isomorphic | `coercion-bridge.flexiarg` [🌉/渡] |
| a93A03: know ‖h‖²-2‖h‖²+‖h‖²=0 but rw breaks Tendsto goal | Algebraic rewrite obligation conflicts with filter-structure preservation | `tactic-algebra-interference.flexiarg` [⚡/衝] |
| a96J01: "tent functions" is 1 line on paper, 50 in Lean | Trivial mathematical insight, substantial Lean construction engineering | `construction-cost-asymmetry.flexiarg` [🏗️/造] |

Each pattern was induced from a real failure, linked to its informal
parent via `:instantiates`, and documents the specific tension that
the informal pattern doesn't capture. This validates the three-level
architecture: the informal pattern tells you *what to try*, the
tension tells you *why it's hard in Lean*, the formalization pattern
tells you *how to work around it*.

**Pilot C finding:** Pattern induction from failures is more
productive than pattern induction from successes. Successful proofs
yield patterns that describe what worked; failed proofs yield
patterns that describe what's *hard* — and the hard parts are where
the pattern library needs to grow. This inverts the expected value:
the sorry boundaries are not just targets for search, they're the
raw material for pattern discovery.

This is the DiagramProver thesis in miniature: the diagram of
failures (sorry boundary atlas) produces the patterns (library growth)
that drive the search (TPG/LeanDojo) that closes the sorry (new
proofs) that updates the diagram. The loop works.

Pattern library: 15 formalization + 31 informal + 7 strategy = 53 total.

### Decision log

1. **Pattern-to-diagram is new code, not futon5 adaptation.**
   Originally scoped as "futon5's translator." Revised: the
   translator must be written. futon5 provides TPG evolution
   infrastructure but not the pattern representation layer.
   The pilot will determine the right representation before
   committing to implementation.

2. **TPG action space for Lean is new, not hexagram-based.**
   The MMCA hexagram operators (expansion, conservation, adaptation,
   etc.) don't map to Lean tactics. A new operator table must be
   designed: {apply, exact, rw, simp, calc, have, intro, cases, ...}
   with the feature vector being the sorry goal state. The pilot
   doesn't need TPG — it uses manual pattern application — but the
   action space design must precede Phase 3b.

3. **The wiring diagram pilot is manual, not automated.**
   Automated extraction requires LeanDojo-v2 (Pantograph for
   goal-state introspection at each tactic step). The pilot is
   done by hand to validate the representation before depending
   on tooling that isn't installed yet.

## Deferred Until

- Phase 0 can start **now** with existing data (39 partials, 12 patterns)
- Phase 1 requires Pass 1 ≥200 problems (currently ~76)
- Phase 2 requires ~50+ sorry boundary observations for model A
- Phase 3a requires LeanDojo-v2 installation (check with Rob)
- Phase 3b requires futon5 TPG + Pantograph integration
- Phase 4 requires superpod GPU access (non-blocking)

---

## Programme of Work (2026-08-02) — concept → build

Everything above this line is the April 2026 concept (IDENTIFY→VERIFY,
parked at VERIFY). This section supersedes the phasing above where they
conflict; the April analysis remains the design record. Per the revision
contract discipline, changes land as dated deltas, not silent edits.

### What changed since April

1. **The data problem is solved.** M-codex-sorry-loop ran a full campaign
   (137/145 modules closed; queue audits in `scripts/queue_audit.py`;
   residual taxonomy in `holes/labs/M-codex-sorry-loop/
   known-residual-20260801.md`). The "diagram of failures" this mission
   wanted to read now exists at scale, with process telemetry (runner
   legs, verification failures, statement-defect classes) the April
   overnight run never had. The 489-prelim corpus remains available as a
   second dataset.

2. **The missing formalism has been chosen.** Decision of 2026-08-02
   (memory: `causal-engine-target2`): build a **generic deep-rewriting
   engine** (diagrams as hypergraphs, DPO rewriting modulo SMC axioms —
   the chyp/quantomatic discipline) with the target set to the
   categorical causal-inference line (Fong's causal theories, Fritz's
   Markov categories, Jacobs–Kissinger–Zanasi surgery). This is exactly
   the "string diagram in a category of goal states" formalism Layer 1.5
   gestured at — VERIFY's #1 HIGH risk ("pattern-to-diagram translation
   does not exist") now has a concrete build target instead of a gap.
   Context: Kissinger grant decision ~Aug 2026; we build from public
   materials only, aiming at a prototype he'd find interesting.

3. **Layer 2's counterfactual ambition gets real machinery.** Model C
   wanted to answer "if we added ENNReal rpow-exponent continuity, how
   many sorry would close?" — that is an interventional query,
   `do(extension)`, and the ad hoc NPT route is superseded by doing it
   properly: author the proof pipeline as a causal DAG, check
   identification with do-calculus/d-separation, *then* estimate.
   Models A/B survive as the estimation layer under the causal spec;
   Model C as originally scoped is retired.

4. **Rob's Pearl/Neo4j port is the compatibility baseline.** The DAG
   level (variables, evidenced arrows, the JSON spec format of
   `docs/memory-causal-graph-spec.json`) is the interchange format.
   Anything our engine derives that dagitty/Y0/Rob's port can also
   compute must agree (differential testing); divergence is a bug or a
   result. Rob's own bar — "success on a real non-toy problem" — is met
   by modelling our systems, not the Book of Why figures.

### Workstreams (target: working prototype in ~1 month)

**WS-A: Engine core — LANDED 2026-08-02, reviewed and accepted**
(commits 52cb3fae, d07e4442, 07115ef8; review by claude-10 same day:
gates re-run, all 7 spec test expectations verified, REPL transcript
reproduced over Drawbridge). Clojure. Diagrams as hypergraphs; regime is
**SDRT-II convex DPO (plain SMC)** — matching + DPO rewriting modulo
symmetric monoidal structure ONLY; the commutative-comonoid regime a
Markov category needs is *not* covered (see WS-B.0). Rule sets pluggable
(Tom-style separation); rule application returns all legal rewrites as a
lazy relation. Porting material was chyp (Apache-2.0, ported).
Quantomatic is unmaintained — historical reference only; PyZX is a
performance reference, not a rule-set source (delta D7).

**WS-B.0: Rewriting-regime decision — DECIDED 2026-08-02 (delta D1,
adopted by Joe).** Route (b): keep the convex-SMC kernel and carry
copy/discard as explicit generators + rules, with the quotient gap
contained three ways rather than merely documented:
(i) **canonical-form invariant at ingest** — the DAG-ingest functor
emits a fixed copy-comb normal form (ordered branches), and WS-B rules
preserve it; receipts are stated as valid *for canonically-ingested
diagrams*, which is the entire v1 population;
(ii) **executable gap marker** — a pinned test asserting that two
diagrams equal modulo cocommutativity are NOT identified by find-iso;
it documents the gap and doubles as the acceptance test for the MPZ
extension;
(iii) **MPZ extension (route (a)) is a NAMED MONTH-2 OBLIGATION, not
scope** — Milosavljevic–Piedeleu–Zanasi, LMCS 21(1) 2025
(arXiv:2204.04274), the sound-and-complete DPO characterisation for the
comonoid regime; the kernel's validity-predicate seam (matcher `:convex?`
option) is the extension point, so route (a) later is an extension, not
a rewrite. Entry condition per Joe: explore once the working system
answers the receipts we asked of Rob on the memory-system DAG.
**WS-B.0 read — DONE 2026-08-02** (claude-10, LMCS 21(1):12, §3–4 to
decision grade). Findings: (1) MPZ absorb the (co)monoid into the
*representation* — diagrams become **right-monogamous acyclic cospans**
(Thm 3.21), i.e. wire-sharing at nodes, not explicit copy edges;
(2) **matching stays convex** (Def 4.7, unchanged from the SDRT-II
kernel we ported) — what changes is the pushout complement: **weak
boundary complements** (Def 4.6, conditions A–D) allowing boundary-node
identification on the monoid side, not necessarily unique, so one match
can induce several rewrites; "weakly convex DPO" (Def 4.9) is sound and
complete (Thm 4.10). Consequences: (a) route (a) later is confirmed an
extension at existing seams — matcher untouched, `rewrite.clj`'s
pushout-complement step generalises, plus relaxing one-sided monogamy
in the graph model and switching `diagram.clj` from explicit copy-combs
to node-sharing; (b) route (b) is confirmed safe for month 1 trivially:
the soundness condition constrains *rewriting steps*, and stage 1
performs none — surgery and d-separation are DAG-level algorithms.
Non-obvious cost surfaced for month 2: complement non-uniqueness widens
the rewrite relation, which the receipts machinery must enumerate, not
assume unique. Rationale in full: the month's receipts are
computed by algorithmic passes (surgery = one deterministic
transformation; d-separation = reachability), not free rewriting search,
so the modulo-comonoid machinery is not load-bearing until equational
derivations (JKZ disintegration proofs) enter — month 2.

**WS-B: Causal rule set + receipts** (weeks 2–3). Markov-category
structure (copy/discard as explicit generators per WS-B.0), interventions
as JKZ surgery, d-separation (Fritz–Klingler characterisation). DAG
ingest from the interchange JSON (canonical form, per WS-B.0). ALSO
(delta D2): enumerate testable implications and run each authored spec
through a falsification pass — dagitty impliedConditionalIndependencies
+ localTests, y0 verma.py, DoWhy gcm.falsify_graph (permutation-baselined)
— reported *with* the spec, including the survives-only-because-data-
are-thin case. Acceptance: derive the three receipts on the
memory-system DAG — Q1 cohort identification, Q2 E2-surgery + mediation,
Q3 filter-equivalence d-separation on both corpus topologies — and agree
with dagitty/y0 oracles (causaleffect as independent ID/IDC cross-check).
Priced-in validation limits (census): there is NO oracle for
mediation-under-surgery, so Q2's receipt is checkable only piecewise;
and DAG-level oracle agreement cannot catch a categorical-layer bug that
projects correctly — state both when the receipts land. Q3's falsifiable
divergence prediction is the prototype's first real output.

**WS-B stages 1+2 — DONE 2026-08-02** (codex-1 authored, claude-10
reviewed; stage 1: `8ed1d561`+`251d6bf6`+`028abf71`, stage 2:
`2e50339d`+`504be25c`). Stage 1: validated ingest of both spec JSONs,
leak variants, surgery, canonical copy-comb rendering with round-trip,
d-separation with named witnesses, bounded implied-independency
enumeration (209 minimal implications on the memory spec at k≤2,
independently recounted by the reviewer's from-scratch implementation —
exact agreement), pinned cocommutativity-gap test in place. Stage 2:
`receipts.clj` computing Q1/Q2/Q3 (memory) + R1 (lean) as pure EDN with
computed verdicts and named paths. Q3 delivered its predicted
divergence pair: star_forest separated / populated_graph connected via
[:M-in-store :shared-patterns :V12-minus-M] — mechanism (b)
filter-at-dispatch is a valid substitute for (a) only on the current
star topology; the divergence on populated_graph is the preregistered
finding for the multi-attachment repair. R1 formalized E7 selection
with time-indexed :P10-pre (avoiding post-treatment adjustment and
cycles), computed the open selection backdoor, and verified
{:P01 :P10-pre} closes it. Review re-ran all gates (26 tests / 95
assertions green; kondo and check-parens clean with negative control),
re-ran receipts over Drawbridge, and hand-verified Q1/Q3/R1 verdicts
with an independent d-separation implementation from the raw JSONs.
**Stage 3 (delta D2 oracle pass) — DONE 2026-08-02, WS-B CLOSED**
(codex-1 authored `59ee1f3f`, claude-10 reviewed; artifacts + REPORT.md
in `holes/labs/M-diagramprover/oracle-pass/`, one-command repro via
`run.sh`). Structure-level results, all verified by the reviewer
re-running the pass end-to-end (regenerated artifacts byte-identical to
committed — determinism confirmed): engine's 209 implications 209/209
against NetworkX AND 209/209 against dagitty 0.3-4 (installed with V8;
bnlearn fallback not needed); converse direction 1,382/1,382 — every CI
in dagitty's emitted basis (157 non-adjacent pairs, multiple separating
sets each) confirmed by the engine; Q3 divergence pair + V18
corollaries 4/4 in both oracles; y0 finds Q1 P(V18|do(V06)), R1
P(P16|do(P20)), and the R1 IDC conditional query all identifiable
(encoding caveat stated in REPORT.md: fully-observed directed DAG, no
bidirected arcs — the receipts' backdoor verdicts remain separate
computed claims). Zero disagreements anywhere. Deferred with reason,
verbatim in REPORT.md: dagitty localTests and DoWhy gcm.falsify_graph
are data-dependent (await M-memory-retrieval cohort data);
Q2 mediation-under-surgery has no structure-level oracle (priced-in
limit, restated). DAG-level agreement still cannot catch a
categorical-layer bug that projects correctly — that limit is
permanent, not deferred.

**Stage 4 (WS-C completion: R2+R3, lean falsification, dosearch) —
DONE 2026-08-02** (codex-1 `2a9807fb`+`aba7e01c`, claude-10 reviewed:
gates re-run, run.sh byte-identical on re-run, R2/R3 verdicts
hand-verified independently from raw JSON). **R2 ANSWERED**: K1–K4
opened paths named + per-leak severance verified; provenance asymmetry
CONFIRMED as computed outcome (copied-class do(withhold P19) → null,
content survives via the K2-byte-copy channel, named; extracted-class →
effect through module-import); duplication debt = the recorded contrast
between do(withhold-module) [no effect] and do(remove-content)
[effect]. **R3 ANSWERED, with a headline refutation of our own spec
prose**: T04 insufficient for progress (confirmed, path named) BUT the
preregistered T05-retirement case is NOT confirmed — T05 as a
measurement child of P10 cannot screen off T04 (d-sep: conditioning on
a noisy child never blocks the parent's other paths); the reviewer's
independent check adds the constructive form: conditioning on
P10-at-k ITSELF screens off T04. Reformulated retirement case: record
the dependency SET as an artifact (lossless), or argue T05≡P10 via
pipeline determinism — which is exactly the D6 caveat (deterministic
CI is invisible to plain d-sep) now biting in our favor. **Lean
falsification DONE**: 202/202 implications × NetworkX and × dagitty;
converse 426/426; R2 3/3 and R3 2/2 × both oracles; zero
disagreements anywhere. **dosearch (D3) EXERCISED TO A BOUNDARY**:
installed (1.0.12), correct refusal — the faithful 18-node ancestral
reduction becomes 36 internal intervention nodes > its hard limit 30;
no proxy substituted; exact query strings in REPORT.md. Open follow-up
(small): rerun on the latent projection onto the query-relevant
variable set (sound: identifiability is preserved under latent
projection; lands well under the limit). NDE/NIE remains outside
dosearch's syntax — Q2 mediation stays answered piecewise.

**WS-C: Lean-pipeline causal spec, example #2 — AUTHORED 2026-08-02**
(`docs/lean-proof-pipeline-causal-spec.json` + `.md`: 20 variables, 31
evidenced arrows, conjecture CJ1, receipts R1–R3; valid JSON, acyclic).
Variables from the sorry-loop mechanism chain, sensors with missingness
(stale-olean and wrong-namespace failures as *measurement-error nodes*),
interventions `do(add extension)` / `do(withhold module)` /
`do(full statement audit)`. This upgrades Phase 0's "write one
extension, measure closures" into a causal experiment with
identification checked by the engine before the run.
Adopted deltas: **(D3)** any identification question routing through the
missingness/measurement layer is oracled by **dosearch**
(arXiv:1902.01073) — y0's tree does not clearly cover missing-data
identification; dosearch cloned into `diagramprover-refs/`. **(D6)**
Methodological statement: WS-C authors the DAG and checks
identification; it does NOT run constraint-based structure discovery on
pipeline traces — the pipeline is deterministic, determinism generates
CIs beyond the Markov condition, and PC-family algorithms are unsound
under it. Model revision proceeds by hard intervention, for which
intervention-immediacy faithfulness suffices (Mazaheri–Zhang–Uhler, UAI
2026); the interventional program-model precedent is CPDA (JSS 2024).
This is also the transpiler use case's citable ancestor. **(D2)** The
falsification pass applies to this spec too, with the stated caveat that
LMC-violation statistics behave differently on deterministic pipeline
runs than in the i.i.d. setting.

**WS-D: Proofs as diagrams** (weeks 3–4, then ongoing — the mission's
original thesis, now on a real substrate). Tactic traces as string
diagrams in the engine's representation; typed ports = goal-state types;
composition type-checked before Lean runs (the `addresses-valid?`
invariant, for real). Re-run Pilot A/B specimens (t92J01, t94A02,
a00J01) through the engine instead of by hand. Pattern induction
(including from failures, per Pilot C's finding) becomes diagram
extraction. Adopted delta **(D5)**: trace extraction is via Lean 4's
**`InfoTree`/`TacticInfo`** (the Paperproof approach) — not manual
transcription, not LeanDojo/Pantograph, which stay parked. **Represent
the extracted structure as a DAG, not a tree**: Paperproof's Gentzen
tree discards exactly the sharing (hypotheses and `have`s used more than
once) that makes a proof a diagram rather than a derivation. Cheap
companion read: LeanTree (arXiv:2507.14722) — on its abstract, the
closest existing work to the typed-ports invariant.

**WS-E: Mission-wiring verification — ADOPTED 2026-08-15 as a matter of
urgency (operator ruling, Joe).** Trigger: M-apm-demonstration built and
REPL-bashed a cycle machine with no process-level verifier, accumulating
sixteen written-but-not-wired instances plus three independent-audit
findings before the class was caught — while scoring 4-for-4 on the
wiring-required test added the same day to `futon4/holes/
mission-lifecycle.md` §Specification Bill of Materials. Operator ruling,
recorded: *"we need this sorted out as a matter of urgency, not just
recorded. M-diagramprover will have two jobs: solving this at a
sufficient level of generality for other missions, and proving it — not
'self-certifying' but at least demonstrating that it itself follows
known best practice."*

Job 1 — **solve, generally.** An ingest functor from process configs to
the engine's typed-diagram representation (the WS-B DAG-ingest functor
is the worked precedent; WS-D's typed ports are the same invariant on a
different source), plus structural checks over the result. First
sources: futon3c `CycleDomainConfig` (problem peripheral) + the round-1
registration EDN; futon5 exotype `.edn`. Checks, minimum three: every
wire has both ends (no orphan writer/reader), every box is reachable
(no ghost phase), every wire leaves exactly one box (single writer /
declared ownership). Generality bar, per shape-first discipline: the
same functor + checks run on **≥2 non-APM configs** — the proof
peripheral's domain config is sibling #1; sibling #2 from the WM flight
loop or the futon3b gate pipeline. If only the APM instance ever works,
record `:special-case true` as a finding, not a success.

Job 2 — **prove it, without self-certification.** Warrants:
(i) *ground truth that predates the tool* — the APM reconstruction as
replay corpus. Sensitivity: the verifier must catch, on pre-fix states,
the unreachable `:close` (`44d8fe6d`), the unread registration pins,
the injectable `:retrieval-probes`, and the producer-less validator
field (`TN-problem-peripheral-RC-fable-review.md`, findings established
by other parties before WS-E existed). Specificity: it must pass the
post-fix states as negative control. (ii) *author ≠ reviewer*, per
mission ownership — Codex authors, Claude owner reviews with
independent re-runs (the WS-B pattern that produced three-way oracle
agreement). (iii) M-diagramprover fills **its own BOM** at VERIFY
(futon4 lifecycle §SBOM), including running WS-E over WS-E's own
pipeline config — flagged as *demonstration*, never certificate; the
certificate-grade warrant is (i)+(ii).

Discharges: the priced hole in `futon4/holes/mission-lifecycle.md`
§Specification Bill of Materials (tool-status note). When acceptance
lands, re-point the lifecycle's process row at WS-E and close the hole.

**WS-E slices 1–4 LANDED + reviewed 2026-08-15** (codex-4 authored,
claude-7 reviewed with gates re-run and adversarial probes per slice):
`a7131e61` ingest + written-never-read; `ad256571` read-never-written +
multiply-written; `7aa8e8df` site conformance (+ review fix `77cca3c9`:
unreadable site is a finding, not an exception); `c474470f` phase-chain
checks; `0a26ea42` the live declared map + findings snapshot
(`holes/labs/M-apm-demonstration/problem-wiring.edn`,
`wiring-findings-20260815.edn`). **Sensitivity: demonstrated on all four
recorded defect classes** — unread pins (fixture; and on the first live
run the conformance check caught the MAP AUTHOR's stale belief that the
pins were still unread — they had been made load-bearing by the "Fable
RC review" commit); missing producer (`:retrieval-probes`, live);
double-writer (`:environment-checkouts` payload fallback, live — the one
open seam, flagged to the peripheral owners); ghost phase (pre-`44d8fe6d`
fixture). **Specificity: post-fix states clean** — live snapshot:
written-never-read [], phase-chain [] (phases derived from the live
config at check time), conformance []. **Remaining for acceptance: the
generality leg** — the same functor + checks on ≥2 non-APM configs
(proof peripheral first).

**Generality sibling #1 (proof peripheral) — DONE 2026-08-15**
(`holes/labs/M-diagramprover/proof-wiring.edn` +
`proof-wiring-findings-20260815.edn`). Same functor and all five checks,
zero new machinery. The authoring-plus-checking process surfaced **three
real latent findings in a config the tool was not built from**:
(1) `proof-domain-config` never opts into `:enforce-required-outputs?`,
so the canonical CR-1..8 contract gates nothing at advance time;
(2) `proof_logic.clj` holds a private duplicate of `phase-order` and
`phase-required-outputs` that has **drifted** — 9 phases, missing
`:target-check` and its three outputs; (3) the duplicate is the copy the
only live output-checking consumes (`query-missing-phase-outputs`), so
enforcement-that-exists reads the stale contract. All three flagged to
the proof peripheral's owners; fixes change cycle behavior and are not
applied from WS-E. Also the second and third instances of the checker
out-auditing its own author: the conformance pass exposed a broken
authoring grep (`\b` after `?`), which is how the duplicate was found.
Final truthful-map verdicts: all five checks clean. **Sibling #2 to be
picked next (operator's call: WM flight loop or futon3b gate pipeline).**

**Generality sibling #2 (War Machine belief path) — DONE 2026-08-15;
WS-E ACCEPTANCE MET** (`holes/labs/M-diagramprover/wm-wiring.edn` +
`wm-wiring-findings-20260815.edn`). Referent pinned first, at the
operator's insistence: the PLoP paper (`p4ng/plop-2026.tex`) describes
the **futon2** implementation — entry point
`futon2/scripts/wm_scheduled_run.clj` (scheduled loop), belief path
`futon2/src/futon2/aif/belief.clj`, production seam
`futon2/scripts/futon2/report/war_machine.clj` — NOT the futon3c pilot
peripheral; mapping the pilot would have been the day's third
two-machines specimen. Map covers the paper's A/B/D `model-manifest`
seam (the four trace stamps); the three content hashes are write-mostly
by design with the paper-cited live-wiring test as their load-bearing
reader — declared, not manufactured into a defect. All five checks
clean, cross-repo, on a non-cycle-engine machine — the strongest form
of the generality bar. **Acceptance summary: sensitivity (four recorded
defect classes, three on live artifacts), specificity (post-fix states
clean), generality (proof peripheral + futon2 War Machine). The
lifecycle's priced hole is discharged in the same push.** Honest grade:
the checks are detect-grade over declared+conformance-kept maps;
by-construction unconstructibility (V.12's F1 lesson) remains the
Lean/engine layer's territory.

### Explicitly parked / superseded

- **TPG as tactic generator** (Phase 3b) — parked, off the critical
  path. The engine replaces the missing pattern-representation layer;
  whether TPG re-enters as a search strategy is a post-prototype
  question.
- **Model C (NPT)** — superseded by WS-B/WS-C (causal formulation).
- **LeanDojo-v2 / Pantograph / superpod** (Phases 3a, 4) — parked and now
  fully off WS-D's critical path (InfoTree extraction replaces the
  "manual until then" fallback, delta D5).
- **PatternBoost-on-a-Dell** (Checkpoint 1) — parked with TPG.
- **Causal abstraction (HELD, not parked — delta D4).** Lorenz–Tull
  2026 (arXiv:2602.16612, successor to the 2023 paper WS-B builds on)
  supplies the formalism for "is our coarse DAG a sound abstraction of
  the fine mechanism?" and for the concrete-`do` → abstract-`do` lift
  WS-C performs informally (a specific Mathlib lemma read as
  `do(add extension)` — their *upward abstraction*). Named obligation;
  entry condition: WS-B's receipts landed AND WS-C's spec used for at
  least one real intervention.
- **MPZ comonoid-regime extension — OPENED 2026-08-02 (route a).**
  Entry condition met and accepted by Joe same day: all six receipts
  self-answered with three-way oracle agreement. Architectural
  decision (claude-10): the MPZ regime lands ALONGSIDE the plain
  kernel, not in place of it — new right-monogamous graph model + RM
  rendering in new namespaces; graph.clj/matcher.clj/rewrite.clj/
  diagram.clj and the whole route-(b) causal pipeline stay untouched
  and remain the delivered system. The pinned cocommutativity-gap test
  STAYS (it documents the plain kernel's quotient gap); acceptance of
  the extension is a COMPANION flip test: the same cocommuted pair,
  rendered right-monogamously, is identified (the gap closes by
  representation, per MPZ Thm 3.21 — copy absorbed into node-sharing).
  Slices: **A1 DONE 2026-08-02** (codex-1 `1deee0de`+`9a3970f4`,
  corrected in `74a3475d` after review found two defects — see
  E-ratchet-probe cycle 1: canonicalize's tied-class fallback crashed
  on first execution (min-key over string keys, never exercised by
  singleton-partition tests), and the flip test was vacuous
  (difference erased upstream of the RM mechanism). Post-correction:
  41/156 green, tied-class permutation path forced by tests, flip test
  constructs the cocommuted pair at RM level with an explicit
  non-vacuity witness, reviewer's counterexample probes pass over
  Drawbridge, protected paths untouched throughout.) A2 = convex
  matching over RM + weak boundary complement ENUMERATION (Def 4.6
  A–D; non-unique, so enumerate — the priced-in cost from the WS-B.0
  read) + weakly convex DPO (Thm 4.10). A2 doubles as the ratchet
  probe's measurement leg (no rule restatement in its bell).

### Acceptance for the month

1. Engine core rewrites diagrams under a pluggable rule set (WS-A).
   **CLEARED 2026-08-02** (landed + reviewed).
2. Receipts Q1–Q3 derived on the memory-system DAG, oracle-checked
   (WS-B). Q3 verdict delivered to the M-memory-retrieval cohort
   registration. Per delta D2, each authored spec also ships with its
   falsification-pass report (dagitty localTests + y0 verma + DoWhy
   falsify_graph), including the too-thin-to-falsify case; per the
   census, Q2's mediation-under-surgery has no oracle and is checked
   piecewise — stated with the receipt.
3. `lean-proof-pipeline-causal-spec.json` v1 exists and its first
   identification question is answered by the engine (WS-C). Spec
   **AUTHORED 2026-08-02**; **R1 ANSWERED 2026-08-02** (stage-2 receipt:
   controlled regime identified with ∅, E7 selection regime refused
   with the named backdoor, {:P01 :P10-pre} verified as the closing
   adjustment; y0 ID + IDC agreement in the stage-3 oracle pass).
   Remaining WS-C: R2, R3; lean-spec structure-level falsification
   pass; dosearch (D3) on missingness-routed questions.
4. ≥1 proved Lean proof and ≥1 partial rendered as typed string
   diagrams by the engine, open ports = sorry (WS-D).

### Decision record — 2026-08-02 (deep-research deltas)

All seven deltas from `holes/labs/M-diagramprover/deep-research-deltas.md`
adopted by Joe, with D1 resolved as **route (b), hardened** (see WS-B.0
for the three containment measures and the month-2 entry condition for
route (a)/MPZ). D4 and the MPZ extension enter as HELD named obligations,
not scope. Census: `deep-research-census.md`, same directory.

Ownership: architecture + review = Claude owner; substantial coding
slices = Codex via bell+park; specs and gates per AGENTS.md.

## Application to theorem-proving capability construction (2026-08-03, Joe + claude-10)

M-diagramprover's machinery is not only for proving APM/BPM theorems —
it is the right frame for holding the whole "co-proof" programme
together, because the programme's own capability claim is itself a
theorem to be proved **by construction**.

### The top-level claim, read constructively

Take the premise "**all APM problems are solved**." Read
constructively (BHK), this is not a census of solved problems — it is
the claim that a PROCEDURE exists which, handed any APM problem,
produces a solution, together with witnesses that each component of
the procedure does its job. The relay pipeline (starter → boundary
artifact → closer chain, with memory and desk research in the loop)
is the constructive content of the claim. The proof updates as
certificates land; it is never re-argued, only re-certified.

### The skeleton: sub-claims as typed holes with contracts

The high-level proof is deliberately small (~a dozen nodes). Each
sub-claim is a hole with a contract: a statement, a current warrant,
attached certificates, and a named upgrade path. Initial skeleton with
the certificates already banked on 2026-08-03:

1. **Extra resources can fill Mathlib holes.** Witnesses: a96J04
   (interval-decomposition gap, proved locally in ~15 min, commit
   33575db, 6-step gate); a96J07 (local Liouville from the two-pole
   result, commit 462b48a, non-circularity verified by import
   analysis). Open instance: a96J08 (rectangular-contour residue
   theorem; boundary documented, commit 37192e1). Warrant: n=2
   positive, 1 open hole.
2. **Work transports between agents.** Mechanism: the
   boundary-comment artifact (the relay's transport layer). Witnesses:
   E9→J04 (the closer's 15-minute path was paved by the starter's
   boundary comment), a92J06→a96J07 (cross-problem reuse, cited in
   source), protocol now specified in the store
   (e-e9-a96j04-localize-an-observed-blocker-at-one-sorry). Warrant:
   n=3.
3. **The memory store records learning.** Witnesses: 12 reviewed,
   tagged, attached memories from one day's runs; scribe protocol with
   hunger audit (demand-side tagging); approval discipline
   (author≠reviewer, operator approval retired 08-03). Warrant:
   write-side strong; consumption-side n=1 (one retrieval correctly
   graded marginal in E10 phase A).
4. **Agents consult memory when instructed.** Witness: E10 (same
   agent: 0 lookups under invitation, 21 under the two-part frame —
   the propensity gap closes with task framing). Warrant: n=1
   controlled contrast, preregistered (E9-pull-probe-prereg.md).
5. **Retrieval serves the need when consulted.** Current warrant:
   WEAK — the four-layer anatomy (propensity/framing/affordance/
   index-reach) shows layers 2–4 are mechanical; repairs 2–4 in
   claude-12's queue; hunger audit + demand-side tagging are the
   upgrade path. Honest state: the relay currently runs on
   repo-memory; the store amortizes ACROSS chains, not within them.
6. **Capability transports from APM to held-out BPM.** This is the
   headline meter and it is formally a TRANSPORTABILITY claim
   (selection diagrams; E-book-of-why-complete B1, promoted 08-03).
   Warrant: none yet; identification requirement named.
7. **Outcomes are mechanically scoreable.** Contract =
   endpoint-preregistration-draft.md (delta-form endpoint,
   executed-witness-only, statement-fidelity with voiding); capture in
   flight (§5 fields). Warrant: designed, capture pending.
8. **The process learns at the ability level.** Mechanism: practice
   memories → packet-template deltas (the delta contract generalized
   from specs to behavior). Warrant: designed 08-03, not yet built;
   today's ability transfers (event-anchored lookups, ignore-and-
   move-on, boundary protocol) all traveled through hand-authored
   packets.

### Heterogeneous warrants are the point, not a defect

Some nodes are discharged mechanically (lake exit 0, axiom-clean);
some by replication (the E8 rank rerun — "frozen and replicated
license different things"); some by scientific induction with honesty
bounds (n=1/n=2 markers); some are registered candidates; some are
refusals (proved-impossible vs not-yet-capable, kept distinct). A
capability proof whose nodes carry graded warrants is not a weaker
proof — it is an honest one (the etik reading: the document marks
absence of certification precisely). Inductions enter the
construction AS constructions, with their evidence grade on their
sleeve.

### The engine's role

The claim-dependency structure is a DAG spec in the same interchange
format as the retrieval-stage spec; certificate updates follow the
deltas-not-silent-edits revision contract (the retrieval spec's
ten-delta trail on 2026-08-03 is the working preview of a capability
proof revising itself under evidence); causal sub-claims (arm
contrasts, transport, mediation) get their identification receipts
from the engine, with its refusal discipline. The sequencing receipts
(v3-sequencing-receipts.md) are this section's method applied to the
experiment plan itself: instruments before arms, ordered by computed
constraint.

### Update discipline

As runs land, their receipts attach to the relevant nodes; a node's
warrant upgrades only by certificate, never by narrative. The
pipeline generates certificates as a byproduct of ordinary work (the
chase and the cohort are the same instrumented process). The document
that instantiates this section — the capability-proof skeleton with
live certificate links — is the next M-diagramprover artifact
(queued 2026-08-03; the morning opener).
