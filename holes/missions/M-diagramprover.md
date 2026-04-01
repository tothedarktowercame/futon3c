# Mission: DiagramProver — Pattern-Driven Proof Search

**Date:** 2026-04-01 (IDENTIFY)
**Status:** IDENTIFY
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
- **LeanDojo** (open-source): tree search over Lean tactic states.
  Proof-state extraction + neural tactic suggestion. Available. Rob is
  setting this up on the superpod.
- **AlphaProof** (DeepMind, 2024): RL + Lean, IMO silver medal performance.
  Not open-source. Architecture published in more detail than AxiomProver.
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
- Bayesian pattern model: at least model A (Beta-Binomial), possibly B/C
- TPG integration: futon5 TPG pointed at Lean tactic search (binary fitness)
- LeanDojo integration: when available on superpod
- Pattern library feedback loop: successful TPG programs → new patterns
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

### Layer 3: TPG Evolution (search)

Tangled Program Graphs from futon5 evolve tactic-search programs.

Population seeded from: pattern library (each pattern → initial program)
Fitness: sorry closed (binary) + partial progress (subgoals reduced)
Selection: programs that close sorry in one problem are tested on
similar problems (cross-problem transfer)

Key advantage over RL: evolved programs are inspectable. A TPG program
that closes a topology sorry can be read as "first try exact?, then
unfold IsConnected, then apply isPreconnected_sUnion, then..." — that's
a new pattern for the library, extracted automatically.

### Layer 4: Lean Verification (ground truth)

Every candidate proof is verified by `lake build`. No exceptions.
This is the one axiom we keep: Lean's type checker is the final
arbiter. The diagram maps possibilities; Lean confirms reality.

### The Loop

```
Observe sorry boundaries (Layer 1)
  → Infer highest-impact intervention (Layer 2)
  → Generate tactic candidates (Layer 3 / TPG)
  → Verify with Lean (Layer 4)
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

### Phase 1: Sorry Boundary Atlas (IDENTIFY → MAP)

Extract structured sorry-boundary data from all Pass 1 results.
Build the diagram. Automate clustering (string-matching on blocker
descriptions). Populate cross-problem impact links.

### Phase 2a: Bayesian Pattern Model — Beta-Binomial (DERIVE)

Implement model A (Beta-Binomial per pattern-blocker pair).
Rank interventions by expected impact. Compare with manual
ranking from Phase 0.

### Phase 2b: Bayesian Pattern Model — Hierarchical (ARGUE)

Implement model B (partial pooling across subjects).
Compare with 2a. If data supports it, implement model C (NPT).

### Phase 3: TPG Tactic Evolution (VERIFY → INSTANTIATE)

Connect futon5 TPG to Lean tactic search. Binary fitness (Path 2).
Seed population from pattern library. Evolve against sorry boundaries.
Extract new patterns from successful programs.

### Phase 4: LeanDojo Integration

When available on superpod: integrate as search backend, enable
Path 1 fitness (subgoal introspection), compare TPG vs LeanDojo
vs RL (if replicable).

## Deferred Until

- M-apm-solutions Pass 1 has completed at least 200 problems
  (Phase 0 can start now with existing 53-problem data)
- Sorry boundary data is rich enough to train Bayesian models
  (~50+ observations for model A, ~200+ for model B)
- futon5 TPG infrastructure is available for Lean integration
- LeanDojo is running on the superpod (Rob's timeline)
