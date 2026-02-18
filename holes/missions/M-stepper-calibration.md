# Mission: Stepper Calibration on First Proof Problems

**Date:** 2026-02-18
**Status:** IN PROGRESS — P1 complete (RED), P7 golden path written (awaiting calibration)
**Depends on:** proof peripheral (operational), `:corpus-check` tool (landed)

## Motivation

The proof stepper (proof peripheral + `:corpus-check` + evidence facets)
is designed to catch framing errors before they propagate into full proofs.
First Proof provides 10 problems with known official answers — a perfect
calibration set.

We need to trace each problem through the stepper to determine:
1. **Would the stepper have caught the error?** (for wrong answers: P1, P7)
2. **Would the stepper have prevented problem substitution?** (P3)
3. **Would the stepper have surfaced the missing technique?** (P4, P6)
4. **What does a successful stepper trace look like?** (P2, P8, P9, P10)
5. **What corpus data is needed** to make each check effective?

This calibrates the stepper *and* defines requirements for the ArXiv
pipeline — we'll know exactly what data is needed, not guess.

## Problem Triage

| Problem | Result | Error type | Stepper target | Priority |
|---------|--------|------------|----------------|----------|
| **P1** | WRONG (YES→NO) | False premise (mu ~ mu_0) | Framing gate | **Done** |
| **P7** | WRONG (YES→NO) | False premise (CW→manifold) | Framing gate | **Next** |
| **P3** | Wrong variant | Problem substitution (standard→interpolation) | Claim typing | High |
| **P4** | Incomplete | Missing technique (hyperbolic poly Hessians) | Propose-phase search | Medium |
| **P6** | Incomplete | Missing technique (modified barrier function) | Propose-phase search | Medium |
| **P5** | Incomplete | Needs rigor tightening | Low (correct framework) | Low |
| **P2** | Correct | — | Baseline (success trace) | Low |
| **P8** | Correct | — | Baseline (success trace) | Low |
| **P9** | Correct | — | Baseline (success trace) | Low |
| **P10** | Correct | — | Baseline (success trace) | Low |

## Three error classes the stepper must handle

### Class A: Foundational error (P1, P7)

Shape: assert a premise without checking → build valid proof on it →
wrong conclusion. Both P1 and P7 answered YES when the answer was NO.

Stepper mechanism: **framing-first gate**. The `L-preconditions` and
`L-obstruction-scan` obligations must be resolved before `L-bridge`
is unblocked. `:corpus-check` fires during observe/propose to surface
known obstructions.

Discipline signal (available now): patterns like `evidence-over-assertion`
fire even without domain data, forcing the agent to acknowledge the
assumption is unverified.

Domain signal (needs corpus): threads/papers about Hairer's regularity
structures (P1) or the Novikov conjecture (P7) would surface the
specific obstruction.

### Class B: Problem substitution (P3)

Shape: solve a known related problem instead of the actual one. We solved
standard ASEP polynomials instead of the interpolation variant.

Stepper mechanism: **claim typing**. The `L-claim-type` obligation forces
"classify exactly which variant we're solving" before any bridge argument.
`:corpus-check` during observe phase queries for the distinction between
standard and interpolation variants.

This is a different kind of corpus query — not "is my premise false?" but
"is the problem I'm solving the same as the problem I was asked?"

### Class C: Missing technique (P4, P6)

Shape: correct answer, correct direction, but can't close the proof
because the key technique isn't in the agent's repertoire.

Stepper mechanism: **propose-phase search**. When the agent proposes an
approach and it stalls (e.g., our SOS attempts on P4 hit degree
explosion), `:corpus-check` fires with the structural obstruction to
find threads/papers that solved similar problems differently.

The structural obstruction from `:failed-route-add` becomes the query:
"Interior zero with strict constraints violates Putinar certificate
preconditions" → search for alternative approaches to positivity
certification.

## Completed work

### P1 — Phi^4_3 measure equivalence (Class A)

- Golden path script: `scripts/proof-p1-stepper-golden-path.clj`
- Calibration report: `holes/qa/issue-11-stepper-calibration.md`
- Result: **RED** — pattern index has no domain signal for Phi^4_3.
  Discipline signal (framing-first gate) would have flagged the
  unverified assumption but not provided the specific answer.
- Minimum ArXiv seed: Hairer 2014, Barashkov-Gubinelli 2020,
  Gubinelli-Imkeller-Perkowski 2015.
- Commits: `11db0b2` (`:corpus-check` tool), `12005ba` / `9bbac22`
  (golden path + framing-first refactor), `8c21423` (calibration)

## Planned work

### P7 — Lattice with 2-torsion (Class A)

Same error pattern as P1: YES bias + missed deep obstruction. We
correctly identified Fowler's criterion for finite CW complexes but
assumed the manifold upgrade would work. The Novikov conjecture
(assembly map injectivity for lattices in semisimple Lie groups)
provides the definitive obstruction.

Steps:
1. Write `scripts/proof-p7-stepper-golden-path.clj`
2. Ledger: `L-claim-type` (CW complex vs manifold?), `L-preconditions`
   (does Fowler extend to manifolds?), `L-obstruction-scan` (Novikov),
   `L-bridge`, `L-conclusion`
3. Corpus queries: "finite CW complex vs closed manifold upgrade",
   "Novikov conjecture obstructs surgery realization", "lattice
   rational acyclicity manifold existence"
4. Record failed route (our wrong approach: treated surgery obstruction
   as "possibly vanishing")
5. Calibrate: GREEN/YELLOW/RED

### P3 — ASEP polynomial Markov chain (Class B)

Different error type: we solved the standard variant instead of the
interpolation variant. The stepper needs to catch problem substitution.

Steps:
1. Write `scripts/proof-p3-stepper-golden-path.clj`
2. Ledger: `L-claim-type` (which polynomial variant?), `L-variant-match`
   (does AMW Theorem 1.1 cover F*_mu?), `L-bridge`, `L-conclusion`
3. Corpus queries: "interpolation vs standard ASEP polynomials",
   "AMW theorem scope: standard vs starred Macdonald polynomials",
   "signed two-line queue construction"
4. Record failed route (our wrong approach: cited AMW for F_eta, problem
   asks for F*_mu)
5. Calibrate: GREEN/YELLOW/RED

### P4 — Finite free Stam inequality (Class C)

We proved n=2 and n=3 but couldn't close the general case. The missing
technique: hyperbolic polynomial Hessians (Bauschke et al.).

Steps:
1. Write `scripts/proof-p4-stepper-strategy-search.clj`
   (different from existing `proof-p4-observe.clj` which tracks the
   obligation DAG — this tests whether `:corpus-check` surfaces the
   missing technique)
2. Corpus queries using failed route obstructions: "SOS infeasible due
   to interior zero", "alternative to Putinar for positivity on
   semialgebraic set", "Hessian connection to hyperbolic polynomials"
3. Calibrate: would the corpus have surfaced hyperbolic polynomial theory?

### P6 — Epsilon-light subsets (Class C)

We proved K_n with c=1/3 but couldn't close general graphs. Missing:
modified barrier function Phi^u_sigma (restricting to top-sigma eigenvalues).

Steps:
1. Write `scripts/proof-p6-stepper-strategy-search.clj`
2. Corpus queries: "modified BSS barrier function eigenvalue restriction",
   "leverage score barrier for graph sparsification"
3. Calibrate: would the corpus have surfaced the modified barrier?

### Baselines (P2, P5, P8, P9, P10)

Low priority but valuable: trace the successful problems through the
stepper to show what a clean framing-first trace looks like. This
provides positive calibration — the stepper shouldn't *block* correct
approaches.

Steps (per problem):
1. Write golden path script
2. Verify that framing-first obligations resolve cleanly
3. Verify that `:corpus-check` returns supportive (not conflicting)
   signal for the correct approach

## Success criteria

1. All 10 problems have stepper traces (golden path scripts)
2. Each trace has a calibration result (GREEN/YELLOW/RED per query)
3. The two wrong answers (P1, P7) demonstrate that the framing gate
   would have caught the error (at least at discipline level)
4. Problem substitution (P3) demonstrates that claim typing catches
   the variant mismatch
5. Missing techniques (P4, P6) identify specific ArXiv papers that
   would make propose-phase search effective
6. Successful problems (P2, P5, P8, P9, P10) pass cleanly through
   the framing gate without false blocks
7. Aggregate: minimum ArXiv seed set for all 10 problems identified

## Relationship to other work

- **Superpod run** (futon6): produces the wiring diagram embeddings
  that upgrade `:corpus-check` from text-only to structural search.
  When the superpod tarballs come back, re-run all calibrations.
- **futon3a upgrade**: index superpod R-GCN model + FAISS into
  futon3a's ANN store. The `:corpus-check` tool doesn't change.
- **Evidence facets** (`docs/evidence-facets.md`): stepper traces
  produce faceted evidence (§"Application: Proof Stepper Ancillary
  Evidence"). Cross-problem queries enable learning.
- **First Proof Batch 2**: the calibrated stepper is what we run
  live during the next competition. "No derivation from unchecked
  premises" is the operating principle.
