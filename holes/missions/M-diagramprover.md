# Mission: DiagramProver — Pattern-Driven Proof Search

**Date:** 2026-04-01 (IDENTIFY), 2026-04-01 (MAP begun)
**Status:** MAP
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

## Deferred Until

- Phase 0 can start **now** with existing data (39 partials, 12 patterns)
- Phase 1 requires Pass 1 ≥200 problems (currently ~76)
- Phase 2 requires ~50+ sorry boundary observations for model A
- Phase 3a requires LeanDojo-v2 installation (check with Rob)
- Phase 3b requires futon5 TPG + Pantograph integration
- Phase 4 requires superpod GPU access (non-blocking)
