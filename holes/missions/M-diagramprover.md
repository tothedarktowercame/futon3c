# Mission: DiagramProver — Pattern-Driven Proof Search

**Date:** 2026-04-01 (IDENTIFY)
**Status:** IDENTIFY
**Cross-ref:** M-apm-solutions (proof peripheral, pattern library, sorry boundaries),
futon5 (TPG, AIF loops), vsat.wiki/ukrn-demo (Bayesian pattern models),
M-distributed-frontiermath (superpod, LeanDojo)

## The Name

AxiomProver works top-down: axioms → rules → search within rules → verify.
The axioms are fixed; the prover explores within their constraints. This is
Cartesian: clear and distinct rules, mechanical deduction, convergence to
a single correct proof.

DiagramProver works bottom-up: observed data → patterns → projected
interventions → evolve new capacity. The patterns are fluid; they emerge
from proof attempts, mutate through TPG evolution, and are validated
against real sorry boundaries. This is Deleuzian: the diagram maps
intensities (where is the proof stuck? what's the shape of the gap?),
creates new realities (Mathlib extensions, novel tactic sequences), and
resists the reterritorialization of fixed axiom systems.

The diagram is not a proof — it's a map of the proof landscape that
shows where movement is possible. The axiom is not wrong — it's
incomplete. DiagramProver extends the axiom system by reading the
diagram of what's missing.

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

## Scope

### Phase 1: Sorry Boundary Atlas (IDENTIFY → MAP)

Extract structured sorry-boundary data from all overnight run results.
Build the diagram. Identify clusters of sorry that share the same blocker.

### Phase 2: Bayesian Pattern Model (DERIVE → ARGUE)

Build the Bayesian model over pattern × sorry-type × outcome.
Rank interventions. Identify the Mathlib extension with highest
expected cross-problem impact.

### Phase 3: TPG Tactic Evolution (VERIFY → INSTANTIATE)

Connect futon5 TPG to Lean tactic search. Seed population from
pattern library. Evolve against sorry boundaries. Extract new patterns
from successful programs.

### Phase 4: LeanDojo Integration

When Rob has LeanDojo running on the superpod, integrate it as an
alternative search backend alongside TPG. Compare: TPG-evolved
programs vs LeanDojo tree search vs AxiomProver-style RL (if we
can replicate it).

## Deferred Until

- M-apm-solutions Pass 1 has completed at least 200 problems
- Sorry boundary data is rich enough to train the Bayesian model
- futon5 TPG infrastructure is available for Lean integration
- LeanDojo is running on the superpod (Rob's timeline)
