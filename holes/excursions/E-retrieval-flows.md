# E-retrieval-flows — memory retrieval as an evolving-operator flow

**Status:** OPENED 2026-07-25 (Joe's direction). Theory recovered from existing
documents, not reinvented; v0 operator-sweep prototype run same day.

## Provenance

Three bodies of existing work say the same thing at different granularities,
and Rob's note (below) is the bridge:

1. **E-dynamic-queries** (this repo): retrieval state
   \(s_t = (x_t, \theta_t, F_t, B_t)\), propagation operator
   \(\Delta_\theta = \sum_r \theta_r \Delta_r\), coupled updates
   \(x_{t+1} = \Phi(x_t, \Delta_{\theta_t}, q)\),
   \(\theta_{t+1} = \Psi(\theta_t, x_{t+1}, q)\).
2. **MetaCA / draft5** (`futon5/holes/tech-notes/paper/draft5.tex`): the
   **phenotype** field \(X_x(t)\) (binary states) evolves under a rule; the
   **genotype** field \(g_x(t)\) — *a rule per site, itself evolving* — is a
   location-dependent operator field. One coupled MetaCA step = express the
   current rule, then evolve the rules (Box `box:metaca-step`). The paper's
   central question — which writings/propagators produce non-trivial
   structure (§"A Sampled Edge of Chaos"; combining peaks at a literal edge
   of chaos; the gcd-1-sustains verdict) — is the classification question.
3. **M-typed-memories** (mission): the pattern ladder (P1b mint lane) is
   pattern formation as a first-class store event; patterns exist at any
   abstraction level (Joe, 2026-07-25).

## Rob's note (2026-07-25, verbatim)

> I read through this - very cool stuff - it is vaguely reminiscent of some
> things (this is no surprise given our talks - identifying we are oddly
> thinking about similar things) I have in this RH proof. One though I had
> looks over actually the more standard update rule over states, especially
> the way you wrote it, is that it could kind of be reinterpreted as a
> "coarse" version of a heat/wave equation finite difference update. So like
> a typical one - since I have graph laplacian on the brain would like like
> X_{t+1}(x) - X_t(x) = \Delta_{x}(X_t(x))  I use \Delta for graph laplacian.
> Now that allows real numbers to evolve, but the state here is binary so
> that's what I mean by "coarse". Given that then the thought I had about
> what you are doing with the rules now evolving is more like updating
> "which" laplacian - or put another way maybe - a general sturm louiville
> operator (I just mean a second order differential operator that has
> coefficients that are location dependent) and then seeing which ones
> produce "non trivial" structure over time. Very neat stuff. I have zero
> insight as to why 🙂 but those are the structures I recognized.

## The correspondence, made exact

| MetaCA (draft5, coarse/binary) | Retrieval flow (fine/real) | Rob's frame |
|---|---|---|
| Phenotype \(X_x(t)\), binary | Activation \(x_t\) over the typed memory graph | The "coarse" state of the heat/wave FD update |
| Genotype \(g_x(t)\): one rule per site, evolving | \(\theta_t\) (relation conductances) + pattern activation: coefficients per edge-type/node, evolving via \(\Psi\) | "Updating **which** Laplacian" — Sturm–Liouville coefficients \(p(x), q(x)\), location-dependent |
| Coupled MetaCA step (express rule, evolve rules) | \(\Phi\) then \(\Psi\) (E-dynamic-queries) | State update, then operator update |
| Writings/propagators; the 8! bijective core | The family \(\{\Delta_\theta\}\); admissible \(\theta\) region | The operator family being selected within |
| Cycle-structure classification; gcd 1 sustains | (open) spectral classification of \(\Delta_\theta\) | "Which ones produce non-trivial structure over time" |
| Edge of chaos: combining peaks at transition | Sustained multi-level pattern formation between heat-death and collapse | Non-trivial structure |
| Typed boxes / abstraction levels | Patterns at any abstraction level (rules → patterns → pattern-of-patterns) | — |

Degenerate regimes on the retrieval side are already named elsewhere in the
stack: **collapse** (all activation on one node) is Rung 4's confirmation
collapse; **dissipation** (flat activation) is retrieval that ranks nothing.
The E-dynamic-queries exploration-mass floor is a coefficient constraint that
keeps \(\Delta_\theta\) inside the sustaining band — an edge-of-chaos
regulator stated operationally before this correspondence named it.

## v0 prototype (2026-07-25): operator sweep over the phase-4 graph

`holes/labs/M-typed-memories/retrieval_flow_sweep.bb` builds
\(\Delta_\theta = \sum_r \theta_r \Delta_r\) from
`phase4-wm-corpus.edn` (13 nodes, 5 typed edges, 4 relation types), runs heat
(\(x_{t+1} = x_t + \varepsilon\Delta_\theta x_t\)) and wave
(\(x_{t+1} = 2x_t - x_{t-1} + \varepsilon\Delta_\theta x_t\)) forms over a
\(\theta\)-grid (uniform / support-only / challenge-heavy / repairs-heavy)
× \(\varepsilon \in \{0.1, 0.3, 0.6\}\), 60 steps, classifying each run by
tail entropy/participation. Results:
`retrieval-flow-sweep-results.edn`.

**Finding (honest, and itself informative): the dynamics are
component-limited.** Every heat run converges to uniform-on-component
(H → ln 3; participation → 3.0 — the seed's component has exactly three
nodes); wave runs oscillate within the same component. Operator choice
barely differentiates because **the current typed graph is too sparse and
disconnected for the operator to matter** — a disjoint union of small stars
has trivial flow no matter which Sturm–Liouville coefficients you pick.

Three consequences:

1. **The food problem has a spectral form.** "Not enough memories" concretely
   means: the graph's components are too small for retrieval *dynamics* to
   add anything over direct lookup. Connectivity (shared patterns, distills
   edges, cross-problem links) is what makes the operator family
   non-degenerate — the zai-learning loop's scribe output (rules sharing
   trigger-class patterns and problems) is precisely connectivity food.
2. **The classification tooling wants the spectrum.** The MetaCA paper
   classifies its operator core by cycle structure; the graph analogue is the
   spectrum of \(\Delta_\theta\) as \(\theta\) varies (algebraic connectivity
   \(\lambda_2\) per component predicts diffusion; spectral gaps predict
   sustain-vs-collapse). Next prototype iteration computes spectra alongside
   the trajectory classification.
3. **Genotype evolution (\(\Psi\)) is not yet exercised here** — this sweep
   holds \(\theta\) fixed per run (phenotype-only, like a fixed-rule CA). The
   coupled version — \(\theta\) updating from outcomes while \(x\) flows — is
   Rung 4's charter, and the zai-learning loop is its live coefficient
   source.

## v1 prototype (same day): the combined rules graph differentiates

`retrieval_flow_sweep_v1.bb` rebuilds the graph from the six draft rewrite
rules with their natural hubs — shared problem `a95A04` (degree 6), shared
tactics, trigger-class patterns — 23 nodes, 27 typed edges, with cycles.
Results: `retrieval-flow-sweep-v1-results.edn`.

**The operator family is no longer degenerate.** Participation at heat
ε=0.3: uniform θ → 10.0 effective nodes; prescribe-heavy → 3.9 (activation
concentrates on the fix-side tactic cluster); uses-heavy → 4.2 (symptom
side); hub-off → 11.0 (removing `:mined-from` spreads flow). All three
regime characters appear across the grid: one dissipated run (uniform,
ε=0.1, H→0.99·max), twelve sustained-structured, eleven
sustained-oscillating (wave forms). Which neighborhood retrieval
concentrates on is now a *choice of coefficients* — Rob's "which operator"
question, live on 23 nodes.

Confirms v0's diagnosis in the positive direction: the six rules' shared
hubs (one problem, one tactic triple) were enough connectivity to make
\(\Delta_\theta\) expressive. Scribe output IS operator food.

**Corpus upgrade discovered en route**: zai self-talk is already persisted in
the Evidence Landscape as `:coordination`/`:turn-round` entries (per-round,
with turn-id and round number, `:profile :zaif`) — back to at least
2026-07-18 across zai-3/4/5, and dense for zai-1 today (incl. the a95A07
Basel completion, commit 1d622b0, round 83). The scribe should mine the
store, not buffer scrapes, and rules should cite turn-round evidence ids as
provenance.

## Next steps

- v1 sweep on the combined graph (phase-4 + live bootstrap attachments + the
  six draft rewrite rules linked via shared trigger-class patterns and the
  a95A04 problem) — first graph dense enough for operator-dependence.
- ~~Spectral classification per \(\theta\) beside trajectory classes.~~
  **DONE 2026-07-28** (M-memory-retrieval WS2, Codex-4 `07aa3af`/`be93d0f`,
  owner review PASS) — see §v3 below.
- Wire the sustaining-band language into Rung 4's battery spec (the
  exploration-mass floor is the coefficient-space constraint).
  [Rung 4 landed 2026-07-27 with the floor + collapse battery;
  E-dynamic-queries §Rung 4 verification.]
- Show this note + the correspondence table to Rob — being executed as the
  M-memory-retrieval WS6 whitepaper (`docs/retrieval-whitepaper.md`);
  the Panopticon rule-rewriting conversation and this operator-evolution
  frame are plausibly the same object at different levels.

## v3 (2026-07-28): spectra, the connectivity meter, and a stability lesson

WS2 delivered (`retrieval_flow_sweep_v2.bb`, `connectivity_meter.bb`;
results note `ws2-results-note.md`):

- **The food problem in spectral form, literally**: v0's four components
  are four zero modes of \(\Delta_\theta\) — no conductance choice moves
  activation between them. v1's λ₂ by grid: uniform 0.185, hub-off 0.154,
  prescribe-heavy 0.058, uses-heavy 0.058; spectra separate coefficient
  choices, connectivity support still dominates.
- **First live connectivity reading: `:component-limited`** (preregistered
  criterion). 62 `:memory/assert` rows → 83 nodes, 51 pattern attachments
  (45 current+reviewed); largest reviewed component = 6 nodes, ONE edge
  type, λ₂ ≈ 1.0 (small stars mix internally; stars are isolated from
  each other). This is the baseline against which cohort exhaust
  ("operator food") is now measurable per cohort.
- **A stability lesson with teeth.** The preregistered λ₂-vs-time check
  FAILED at ε=0.3 (ρ = 0.0) — and the owner-review diagnosis shows why:
  three of four grid points sit past the explicit-Euler stability
  boundary ε < 2/λ_max, so their dynamics are power iteration
  (concentration), not diffusion. The preregistered re-test at ε=0.1
  (all points stable) CONFIRMED λ₂ ranking with ρ = −0.8
  (`ws2-owner-stability-check.bb`). Consequence: **step size is part of
  the operator** — ε must be reported against 2/λ_max, and the v1
  ε=0.3 "sustained-structured" classes partly reflect the scheme's
  concentration regime, not the graph alone. This feeds Rung 4's audit
  discipline directly.

## v2 (2026-07-27): the LIVE instantiation — theory objects → shipped code

The M-zai-learning-loop recall stack (cohorts 1–2) is the first deployed
shadow of the evolving-operator frame. The mapping, term by term:

| theory object | live counterpart | where |
|---|---|---|
| `x_t` propagation over typed graph | pattern-mediated recall: FTS seed → reviewed memory/assert edge → pattern endpoint → attached memories | `memory_recall/propose-patterns-by-query` + `recall-by-endpoint` |
| `Δ_θ` coupling structure (which edges conduct) | pattern attachments + pattern DESCRIPTIONS (lexical bridge rows); editing descriptions = editing conductances | `wire_math_memory_patterns.clj` (7776cfc, e382487) |
| query source term `q` | problem-file term extraction (problem.md/proof-outline.md nouns, tuned stopwords) | `dispatch_with_recall/problem-term-sources` |
| boundary conditions `B_t` | the warrant discipline: only REVIEWED edges conduct; descriptions/embeddings propose, never warrant | P1 acceptance rule; charter S5 |
| **Ψ (the θ update — the "updating Laplacian")** | **S6 receipt-informed ranking: per-memory multiplier (1 + α·used/offered) from use-receipts; the first LIVE Ψ** | `dispatch_with_recall` receipt-stats (d4f0f5d); receipts as the observation channel |
| forcing `F_t` (external drive) | curriculum lane: coverage map + memory-proposed construction targets select what the system experiences next | cohort-2 prereg (041a49f) |
| widened proposal support | S5 semantic lane (MiniLM proposals, feasible-later) | charter §S5 |

Honest deltas from the theory: live Ψ is a scalar per-memory reweighting,
not the rung-2 per-edge-type operator update — θ_t currently has one
coefficient per memory, not per relation type; the dark rungs (1–4 in
E-dynamic-queries) remain the richer dynamics this deployment should grow
toward. The observation channel, however, is exactly as drawn: receipts
(offered/used/outcome) are the y_t that Ψ consumes, and they are live.

Cross-refs: M-typed-memories (the store/dialogue-acts substrate this flows
over) · M-zai-learning-loop charter §Second derivative (Ψ as the first
outer-loop mechanism) · algorithms/zai-learning-loop.md (operational
reproduction recipe).
