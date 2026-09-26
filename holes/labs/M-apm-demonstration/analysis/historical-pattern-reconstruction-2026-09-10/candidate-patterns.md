# Candidate missing-method drafts (unpublished; at most three)

Status: drafts within this packet only. No canonical pattern/memory edits,
no promotion, no attachment edges created. Each has two distinct problem
witnesses from this packet's frozen selection.

## D1. left-inverse-injectivity-obstruction

**Definition.** Given continuous i : A → X with a left inverse r (r∘i = id_A)
and a functor F from spaces to groups/modules that respects identities and
composition, F(i) is injective. Consequently, if F(i) is zero (or has
nontrivial kernel) while F(A) ≠ 0, no such retraction exists; if moreover a
hypothesized equivalence of X transports the retraction data to another
space, the same obstruction applies there.

**Exact applicability conditions.**
1. An explicit left-inverse equation r∘i = id (proved pointwise, then
   packaged as `ContinuousMap` composition equality).
2. F strictly functorial (map_comp / map_id available); basepoint handling
   for groupoid-valued F (fix the basepoint once; π₁ needs a chosen point,
   H₁ does not).
3. A *nontriviality witness* in F(A): an element x ≠ 0 (or ≠ 1), or
   subsingleton F(X) at the relevant basepoint.

**Construction supplied.** Push the left-inverse equation through F to get
F(i)∘F(r) = id, hence F(i) injective; exhibit the killed nontrivial element;
contradict. For transported settings, conjugate i and r across the
hypothetical equivalence first.

**Countercondition / failure examples.**
- F(A) trivial (zero group): injectivity of a zero map gives no
  contradiction — the obstruction is empty (census T3's boundary case; also
  why t01A03's final theorem needs the `Nontrivial (H₁ Circle)` instance as a
  hypothesis rather than deriving it).
- A one-sided preservation argument (only i-then-r on objects) descends a
  map but supplies no inverse and no injectivity.
- An abstract isomorphism F(A) ≅ G without identifying F(i) does not
  transfer the obstruction.

**Overlap.** Parent: `math-strategy/structural-obstruction-as-theorem`
(failure-of-method framing, no construction). `transport-across-isomorphism`
(consume an isomorphism; here we *produce* injectivity and transport
retraction data). Prior draft: census T3 (prior work; this draft adds
conditions and failure contrasts).

**Witnesses.** t01A03 (H₁ variant + π₁ variant;
`H1map_injective_of_retraction`,
`fundamentalGroupMap_injective_of_retraction`, sha256 in manifest) and
t91J02 (`fundamentalGroupMap_injective_of_retract`, marked in-source as
reused from sibling t91A05 — a third consumer, not re-inspected here).

**Historical failed attempts (evidenced, not invented).** t91J02's closer-hop
comments record that sibling partials t01J03/t91A05/t02A06/t95J04/t01A05 all
stopped at the missing invariant computations (π₁/H₁ of circle and sphere)
— the functorial step was never the blocker; the pattern helps exactly by
isolating which single computation remains.

## D2. null-bad-values-vs-positive-measure-target

**Definition.** To produce a value with a regularity property (regular
value, avoided value, transverse target), show the bad-value set is null for
a measure under which an available test set has positive measure; take a
countable union if a countable family of constraints must be satisfied
simultaneously; contradict containment; choose a point outside and translate
avoidance into the required certificate.

**Exact applicability conditions.**
1. Bad-value nullity in the actual coordinate model (e.g. manifold Sard on
   the composed map into a Euclidean chart target; no global manifold
   measure is assumed).
2. Positive-measure test set in the *same* measure (e.g. ℝ^m with volume;
   open Euclidean chart images for the fixed-chart route).
3. Countable (not arbitrary) family for the simultaneous case —
   `measure_iUnion_null` needs countability.

**Construction supplied.** Union the null sets, contradict `univ`-containment
(`volume univ ≠ 0`), pick b outside; then convert regularity of the
surrogate map (q∘f regular at b) back through the chain rule and an explicit
linear-algebra bridge (range ⊔ span(ker direction) = ⊤) to the certificate
on the original map.

**Countercondition / failure examples.**
- Uncountable families: a union of null singletons can exhaust the target;
  the countability hypothesis is not removable.
- Nullity of critical values ≠ nullity of the whole image; the argument
  cannot produce a value outside the image.
- A test set of measure zero (e.g. a lower-dimensional slice as the *target*
  measure space) gives no contradiction.

**Overlap.** `exhaustion-as-theorem`, `local-to-global` are generic parents
that do not state the measure-theoretic conditions. Prior draft census T2
(prior work; corroborating witnesses t94J05/t95J04 via
`FixedChartNullForcesNonsurjectivity` were cited there, not re-inspected).

**Witnesses.** t93A04 (`T93A04ManifoldWork.null_critical` + avoidance of the
critical-value image; linear bridge lemma) and t92J07
(`exists_common_regularValue_of_null`, the countable-union assembly as the
problem's own named lemma).

## D3. summable-dominator-then-identify

**Definition.** To transfer a regularity property (absolute continuity,
a.e. differentiability with a given derivative) to an object defined by a
limiting process (boundary value, infinite sum), (i) construct an auxiliary
object that has the property by construction from summable/integrable data;
(ii) justify the termwise/boundary interchange by exhibiting a summable
dominating family; (iii) identify the auxiliary object with the target by a
uniqueness-of-limits argument; (iv) transfer the property by rewriting the
definition through the identification.

**Exact applicability conditions.**
1. A summable dominator exists in the correct sense: for a97J04, global
   interval-integrability of deriv f (from bounded variation — local AC
   alone does not supply it); for a94A02, summability of the monotone
   increments B n = f_n(x+1) − f_n(x) (from Σ f_n(1) < ∞ plus monotonicity
   and nonnegativity).
2. Nonnegative domination (needed for the tsum/measure interchange lemmas).
3. A uniqueness/identification handle: continuity at the boundary point, or
   a.e. equality of measures/densities.

**Construction supplied.** a97J04: g = f 0 + ∫ f′ is AC; f = g on [0,1] via
the sequence a n = x/n, FTC on [a n, x], additivity of adjacent integrals,
and uniqueness of limits; rewrite AC's ε–δ through f = g. a94A02: rightLim
interchange by `tendsto_tsum_of_dominated_convergence`; the Stieltjes
measure of the sum is the sum of measures; Lebesgue-decompose each and sum
the densities; rnDeriv identification gives f′ = Σ f_n′ a.e.

**Countercondition / failure examples.**
- No summable dominator: a pointwise-convergent series of monotone functions
  without Σ f_n(1) < ∞ can fail termwise interchange (jumps can accumulate).
- Local AC without BV on the closed interval: deriv f need not be
  integrable across the endpoint, and the primitive route stalls — the
  memory's own boundary note.
- Identification without continuity/uniqueness: the auxiliary object may
  differ from the target at the bad point/measure-zero set, so the property
  transfers only up to that set; state which one.

**Overlap.** The reviewed memory
`e-codexpilot-extend-local-absolute-continuity-to-an-endpoint-via-the-derivative-primitive`
documents the a97J04 route in full (kind feedback, approved; runner codex) —
this draft generalizes it to a two-witness method rather than replacing it.
Parents: `construct-auxiliary-object`, `local-to-global` (their warnings —
not every property is local/assembles — are exactly conditions 1–2).

**Witnesses.** a97J04 (solve cites the memory in-source) and a94A02
(`tendsto_tsum_of_dominated_convergence` step and measure-sum
identification). Distinct problems, distinct stress points (identification
vs interchange), same cascade.

---

**Not proposed** (single-example or out-of-scope, retained as hypotheses):
the linear-algebra bridge of t93A04 (`range_sup_span…`) alone; the
connectedness-transfer step of t01A03; a93A01's finite-net/threshold pair
(already drafted in prelim-development.md and owned by the pilot's area).
