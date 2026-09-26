# E-book-of-why-complete — capability backlog surfaced by the mfuton sweep

Status: **BACKLOG, authored 2026-08-03 (Joe + claude-10).** Source: the
20 graph-only rows of the mfuton 60-fixture sweep
(`mfuton-sweep/REPORT-mfuton-sweep.md`, commit `a378762a`) — each
conversion boundary is a capability the engine lacks, priced against a
real corpus. These return as straightforward codex dispatches, each
with the sweep as its regression harness (acceptance = the affected
fixtures flip from graph-only to fully-converted with oracle
agreement, everything already-green stays green). NOT prerequisites
for the V3/retrieval work (discrete/structural machinery suffices
there); pull from this list when a slice's fixtures matter or the
demo needs the class.

## B1 — Transportability / selection diagrams (5 fixtures)
transport-{boston,honolulu,los-angeles,san-francisco,toronto}-*.
Their `selection` annotations are Bareinboim–Pearl S-nodes; the
capability is the transport formula (do-calculus + selection nodes)
over our ADMG layer. Oracle: causaleffect (R) does transportability;
y0 partially. Natural slice shape: S-node encoding in admg.clj +
transport recursion in idalg.clj + the 5 fixtures as known answers.
Biggest single win: it is ALSO the formal home of "does this result
carry from star-forest to populated-graph" — the Q3/E2 topology
question is a transportability question, so B1 may get pulled in by
the retrieval thread after all.

## B2 — Finite probability tables / categorical domains (6 fixtures)
airport-bag-posterior, vaccination, fertilizer-{target,improper,
randomized}, tourniquet-selection. Capability: a finite CPT evaluator
(forward inference + numeric do()) beside scm.clj's Boolean layer.
Oracle: pgmpy (pip) or hand-enumeration. Turns rung-1/2 verdicts
numeric where tables exist.

## B3 — Linear/affine SCMs, path coefficients (7 fixtures)
wright-{guinea-pig,puppy,supply,price}-*, linear-mediation,
generic-linear-IV, alice-education-salary. Capability: Wright's path
rules (trek sums), IV estimands, affine counterfactuals (the alice
fixture is rung-3 affine — extends scm.clj's determinism to linear
algebra). Oracle: sympy independent solve; semopy for path models.
Historical resonance for the demo: Wright's guinea pigs are where
path analysis started.

## B4 — Probabilities of causation (1 fixture converted but shallow)
climate-change-probabilities-of-causation converted fully, but its
natural queries (PN/PS bounds, Tian–Pearl) are stochastic rung-3 —
currently our named refusal class. Capability: PN/PS bound
computation. This is the principled successor to the deterministic-SCM
slice.

## B5 — Numeric observational distributions (2 fixtures)
cholestyramine-noncompliance, vaccination carry actual numbers —
usable as micro data-contact tests for the falsification tools
independent of cohort data.

## Non-goals
Rob's engine itself stays unconsumed (Joe 2026-08-03: our
oracle-triangulation beats textbook-porting); the corpus is data, the
per-fixture pytest expectations are extractable ground truth where
cheap. The nonlinear-threshold-mediation fixture (non-fixed-grammar
expressions) stays out until B3 exists to host it.
