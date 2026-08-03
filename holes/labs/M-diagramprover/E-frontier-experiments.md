# E-frontier-experiments — demos + direction from the capability frontier

Status: **DESIGNED 2026-08-02 (Joe + claude-10). Not yet dispatched.**
Purpose, per Joe: (a) independently show Rob that the receipts pipeline
works as an alternative to his direct port of *The Book of Why*;
(b) decide M-diagramprover's next direction by building out from the
frontier — the experiments MAP the frontier, and the refusals they
produce are the direction signal. claude-7 meanwhile uses the delivered
system "as needed" (Joe, same date) — no gate on our side.

Current frontier (what the engine does today, all oracle-checked):
DAG ingest/validation/surgery; d-separation with named witnesses;
bounded implied-CI enumeration; **backdoor-only identification**;
receipts as computed EDN with honest refusals; deterministic repro;
live guard pattern (topology observation → license verdict); RM
representation with iso-complete canonicalization (route (a) A1).
Known gaps: front-door and general ID (rung 2 beyond backdoor),
counterfactuals (rung 3), rewriting in the RM regime (A2, specced),
data-contact falsification (tools installed, never run on data).

## X1 — Book-of-Why fixtures (the Rob demo)

Encode 5–6 canonical BoW examples as interchange JSONs with
`requested_receipts`, run the full receipts + oracle pass on each, one
run.sh, one REPORT — the same artifact style as the delivered system,
on ground Rob recognizes:

1. **Simpson/kidney-stones** — adjustment reverses the marginal
   verdict; receipt = both adjusted and unadjusted claims with the
   backdoor path named. (Engine: fully in scope today.)
2. **Sprinkler/wet-grass** — collider conditioning creates dependence;
   receipt = the d-sep pair before/after conditioning. (In scope.)
3. **Smoking → tar → cancer with latent confounder** — backdoor FAILS;
   identification requires FRONT-DOOR. Engine today must emit an
   honest refusal; y0 identifies it. **This refusal is the point**: it
   marks the frontier precisely and is priced as the most likely next
   capability (see Direction below).
4. **Monty Hall as collider** — d-sep account of the paradox. (In
   scope; pure demo value.)
5. **Firing squad** — rung-3 counterfactual ("had soldier A not
   fired…"). Engine refuses (no counterfactual machinery); the refusal
   marks the rung-3 boundary honestly. y0's ID* could oracle a future
   implementation.
6. (Optional) **napkin problem** — needs full do-calculus; same
   refusal-marks-frontier logic.

Acceptance: every in-scope fixture's receipts oracle-agree (NetworkX +
dagitty + y0 where applicable); every out-of-scope fixture produces a
computed refusal naming WHAT is missing (not a crash, not silence);
REPORT presents the coverage table — "computes + triangulates + knows
what it cannot do" IS the alternative-to-a-port pitch. Prediction to
preregister: refusals exactly on {front-door, napkin, rung-3}.
Cost: small (pure, no new theory, fixture authoring + receipts reuse).
**Dispatch note: this is the natural ratchet-probe measurement leg**
(fixture acceptance tests offer organic recurrence opportunities for
R-A/R-B; the bell must not restate either rule).

## X2 — Code as a crime scene, causally (with claude-2's thread)

Tornhill's forensic repo analysis (hotspots = churn×complexity,
temporal coupling, knowledge maps) is correlational; the causal
formulation is exactly our R1 shape: *do hot files get refactoring
attention BECAUSE they are defective* (selection confounding), and
does do(refactor-hotspot) reduce defect rate?

- Author `crime-scene-causal-spec.json` over repo/process variables
  (churn, complexity, temporal coupling, ownership diffusion, review
  latency → defect incidence; defect proxy = fix/revert commits).
- Receipts: identification of do(refactor) with the selection backdoor
  named (R1's log-at-selection-time lesson transfers verbatim);
  temporal coupling as a confounding channel; implied-CI enumeration.
- **The unique leg: first data contact.** Git history IS observational
  data — the deferred data-dependent falsification tools (dagitty
  localTests, DoWhy falsify_graph) finally run, against a spec we
  authored. This closes the last never-exercised leg of delta D2 and
  tests whether our authored DAGs survive real statistics.
- Candidate corpora: a futon repo's full history; or the sorry-loop
  campaign window (dense, well-understood, defect-labels cheap).
- Prior art contrast for the writeup: Tornhill (static/correlational)
  vs receipts (causal, refusal-honest). Note: the old E-codebase-
  manifold idea (M-aif2 §6, 2026-05-31) already lists Tornhill as
  contrast prior-art — claude-2's thread and that excursion should be
  synced before speccing. SYNC REQUIRED: pull claude-2's crime-scene
  notes before authoring the spec.

Cost: medium (data extraction + spec authoring + falsification run).

## X3 — AIF² : causal arguments (with claude-4's thread)

**Disambiguation (recorded in memory): this AIF² = Argument
Interchange Format × Active Inference Framework (Joe/claude-4,
2026-08), NOT the 2026-05 M-aif2 (WM niche construction).**

Setting: AIF argument maps (I-nodes, S-nodes/schemes) given causal
semantics, so that arguments ABOUT causal claims and arguments AS
causal structures both become engine objects:

- do(withdraw premise P) → is conclusion C's support cut? (surgery +
  d-connection receipt with the support path named);
- "load-bearing premise" = d-connection of C on P given the rest —
  idle premises computably identified;
- undercutter vs rebutter = edge-surgery vs node-surgery (a clean
  formal distinction the AIF literature states informally);
- the active-inference side (claude-4's half): argument moves as
  expected-free-energy-reducing actions over the map — which premise
  to attack/support next = the EFE-ranked intervention. (Scope with
  claude-4; do not build the AI side speculatively.)
- **RM representation is the natural substrate**: premise sharing (one
  premise supports many conclusions) IS node-sharing — route (a)'s
  representation, giving A2's rewriting a consumer (argument-map
  rewriting = scheme application).

First concrete artifact: encode ONE argument we already own — the R3
refutation ("T05 cannot screen off T04, therefore record the
dependency set") — as an AIF map with causal receipts. It is small,
real, self-referential in the good way (the engine analyzing the
argument that repaired its own spec), and directly Rob-shaped (his
definition-repair loop is an argument process).

Cost: medium-high (new functor AIF→causal graph + semantics decisions;
theory sync with claude-4 first). Frontier value: highest — new
setting, feeds the categorical/compositional grant story and the
argumentation lineage.

## Direction logic (b)

Sequencing recommendation: **X1 → X2 → X3.** X1 is days and produces
the Rob artifact + the preregistered refusal map; X2 adds the
only-real-data leg; X3 needs a claude-4 sync and opens new theory.
The A2-vs-identification decision is DEFERRED until X1's refusal map
exists: if the front-door/ID refusals are the ones that hurt (likely,
given all three fixtures that refuse are identification-shaped), the
next engine capability is front-door + ID (y0-oracled, native
implementation) BEFORE RM rewriting; A2 then follows with X3 as its
consumer. The ratchet probe leg rides whichever codex dispatch comes
first (X1 by this sequencing).
