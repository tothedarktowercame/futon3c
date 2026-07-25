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

## Next steps

- v1 sweep on the combined graph (phase-4 + live bootstrap attachments + the
  six draft rewrite rules linked via shared trigger-class patterns and the
  a95A04 problem) — first graph dense enough for operator-dependence.
- Spectral classification per \(\theta\) beside trajectory classes.
- Wire the sustaining-band language into Rung 4's battery spec (the
  exploration-mass floor is the coefficient-space constraint).
- Show this note + the correspondence table to Rob; the Panopticon
  rule-rewriting conversation and this operator-evolution frame are
  plausibly the same object at different levels.
