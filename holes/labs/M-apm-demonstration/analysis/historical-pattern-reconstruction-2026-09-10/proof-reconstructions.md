# Six compact structured proof reconstructions

Format follows the ta-cascade schema idea (statement → obligation tree →
method attribution per node). Method attribution categories: **L** ordinary
logical/library step; **P** existing canonical pattern (futon3
`library/math-*`, cited by exact ID); **M** reviewed memory/example
attachment (cited by evidence id); **G** proposed missing-method gap (draft
in `candidate-patterns.md`). What is attributed is the *construction that
makes the intermediate step possible*, not theorem names occurring in it.

No fresh Lean elaboration was run for this packet; reconstructions are read
from the pinned sources (see `selection-manifest.md`), whose `#print axioms`
lines and zero sorry counts were checked by source inspection only.

## Pair A — retraction ⇒ invariant injectivity ⇒ nonexistence

### t01A03 (three retraction problems on a genus-two surface)

Obligation: given hypotheses (T2 compact path-connected 2-manifold S with
hasGenusTwo S; embedded middle circle with H₁-map 0; embedded wedge with
noninjective π₁ map; embedded disjoint two-circle union), conclude ¬retraction
for each of the three inclusions.

- 〈1〉 Connectedness obstruction for the two-circle inclusion. [L]
  Surjectivity of r (from r∘i=id) transports `ConnectedSpace S` to the
  disjoint union `Sum Circle Circle`; a clopen range argument then forces
  `Sum.inr 1 ∈ range Sum.inl`, contradiction. Intermediate construction:
  *retraction ⇒ surjectivity ⇒ connectivity transfer*. [L; generic]
- 〈2〉 π₁ obstruction for the wedge. [**G-A**] Lemma
  `fundamentalGroupMap_injective_of_retraction`: apply the fundamental
  groupoid functor to r∘i = id; functoriality (map_comp, map_id) rewrites
  (π₁i)∘(π₁r) = id, so π₁i is injective; contradiction with the noninjectivity
  hypothesis. Intermediate construction: **left-inverse equation pushed
  through a functor to force injectivity**.
- 〈3〉 H₁ obstruction for the middle circle. [**G-A**, same construction at
  H₁] `H1map_injective_of_retraction` (identical categorical argument with
  the singular-homology functor), then `noRetraction_of_H1map_zero`: a zero
  induced map from a space with H₁ ≠ 0 cannot be injective.
- 〈4〉 Assembly. [L] The theorem is **conditional**: the nontriviality of
  H₁(Circle) enters as a `Nontrivial` instance hypothesis
  (`noRetraction_circle_of_H1_nontrivial`); the genus-two hypotheses are not
  used to discharge it. Faithful statement of what is proved: reduction, not
  the full informal claim for arbitrary genus-two surfaces.

### t91J02 (no torus–sphere homeomorphism)

Obligation: `¬Nonempty(Torus ≃ₜ S²)`.

- 〈1〉 Explicit retraction data on the torus: i(z)=(z,1), r=fst; rfl gives
  r∘i = id. [L]
- 〈2〉 Transport: a hypothetical homeomorphism e conjugates the retraction
  to a circle retract of the metric sphere. [L; parent
  `math-informal/transport-across-isomorphism` describes the surrounding
  move, but not this conjugation-of-left-inverse construction]
- 〈3〉 Functorial injectivity of the retract inclusion's π₁ map. [**G-A**]
  `fundamentalGroupMap_injective_of_retract` — the source file itself marks
  it "Reused from the functorial argument in sibling t91A05".
- 〈4〉 Invariant obstruction: subsingleton π₁ at every sphere basepoint +
  Nontrivial π₁(Circle) ⇒ the injection kills a nontrivial element. [L]
- 〈5〉 The two genuine invariant computations are supplied by
  ConstructionTargets imports (`CircleUniversalCover`, simply-connected
  spheres) — library completions, not part of either pair's method. [L]

Shared construction (pair witness): the *same* lemma shape proved twice in
two problems (H₁ in t01A03, π₁ in t91J02, plus the sibling reuse note), i.e.
two distinct problem consumers.

## Pair B — null bad-value set vs positive-measure target

### t93A04 (a line with smooth inverse image)

Obligation: for any smooth f : M → ℝ³ (M a genuine finite-dim manifold),
exhibit a line ℓ whose preimage is certified smooth (transversality:
range(mfderiv f x) ⊔ span(v) = ⊤ for all x ∈ f⁻¹(ℓ)).

- 〈1〉 Compose with the coordinate projection q : ℝ³ → ℝ² killing the first
  coordinate; ker q = span(lineDirection). [L]
- 〈2〉 Manifold Sard: the critical values of q∘f are null. [L, imported
  `SardGenuineSmoothManifoldVector` producer]
- 〈3〉 **Null set cannot cover ℝ²**: volume(CV) = 0 but volume(univ) ≠ 0,
  so some b avoids the critical values. [**G-B**]
- 〈4〉 Choose p with q p = b; set ℓ = p + ℝ·lineDirection. For x ∈ f⁻¹(ℓ):
  q(f x) = b is regular, chain rule rewrites surjectivity of mfderiv(q∘f)
  into surjectivity of q∘mfderiv(f), and the linear-algebra bridge lemma
  (`range_sup_span_eq_top_of_projection_surjective`) converts it to
  range ⊔ span(v) = ⊤. [L, with an explicit bridge lemma — the conversion
  step is ordinary linear algebra]
- Encoded-predicate caveat preserved: the "smooth line preimage" is a
  transversality certificate on the actual preimage, not a Mathlib
  embedded-submanifold predicate.

### t92J07 (countably many common regular fibers)

Obligation: for a countable family of smooth manifolds M_i and maps
f_i : M_i → ℝ^m, find a value x regular for every f_i.

- 〈1〉 Regular value expressed as pointwise surjectivity of mfderiv;
  criticalValues(f) = non-regular values. [L]
- 〈2〉 Zero-dimensional codomain degenerate case: every value regular
  (codomain subsingleton). [L]
- 〈3〉 **Countable union of null sets is null, and cannot cover ℝ^m**:
  `measure_iUnion_null` over the countable index, univ ≠ null ⇒ common
  regular value exists. [**G-B**] — this is the problem's own named lemma
  `exists_common_regularValue_of_null`.
- 〈4〉 Assemble per-member regular-fiber certificates. [L]

Shared construction: both solves decide "∃ a good value" by contradicting
containment of a positive-measure test set (ℝ² resp. ℝ^m with Lebesgue
volume) in a null bad-value set (Sard critical values resp. countable union
of critical-value sets). The prior all-topology census (T2, prior work) also
cites t94J05/t95J04 via `FixedChartNullForcesNonsurjectivity` — recorded as
additional prior witnesses, not re-inspected here.

## Pair C — summable dominator licenses interchange; then identify

### a97J04 (local AC ⇒ global AC on [0,1])

Obligation: f continuous + BV on [0,1], AC on every [δ,1] ⇒ AC on [0,1].

- 〈1〉 BV ⇒ deriv f interval-integrable on [0,1]. [L,
  `BoundedVariationOn.intervalIntegrable_deriv`; this is the bridge local AC
  alone cannot supply]
- 〈2〉 Construct g(x) = f 0 + ∫₀ˣ f′; g is AC (integral of an L¹ function
  plus a constant). [**M** + **G-C**; parent
  `math-informal/construct-auxiliary-object`]
- 〈3〉 Identify f = g pointwise: fix x > 0, take a n = x/n → 0; on [a n, x]
  local AC gives FTC; splitting the primitive at a n makes f(a n) − g(a n)
  equal the constant f x − g x eventually, while continuity of f and AC of g
  make it tend to f 0 − g 0 = 0; uniqueness of limits closes it. [**M**:
  memory `e-codexpilot-extend-local-absolute-continuity-to-an-endpoint-via-
  the-derivative-primitive` documents exactly this endpoint-identification
  cascade; the solve cites it in-source]
- 〈4〉 Transfer AC of g to f by rewriting the ε–δ definition through the
  pointwise equality. [L]

### a94A02 (derivative of a sum of monotone AC functions)

Obligation: f = Σ f_n, f_n nondecreasing AC, Σ f_n(1) < ∞ ⇒ g = Σ f_n′
integrable and f′ = g a.e.

- 〈1〉 Pointwise sum of monotone functions is monotone (tsum_le_tsum). [L]
- 〈2〉 **Right-limit interchange**: rightLim f = Σ rightLim f_n, proved by
  dominated convergence for series (`tendsto_tsum_of_dominated_convergence`)
  with the summable dominator B n = f_n(x+1) − f_n(x) (summable because
  Σ f_n(x+1), Σ f_n(x) both converge, using monotonicity and f_n ≥ 0). The
  jump terms are nonnegative and dominated by B n. [**G-C**]
- 〈3〉 Measure side: the Stieltjes measure of Σ f_n equals Measure.sum of
  the individual Stieltjes measures (proved interval-wise via 〈2〉 and
  `ofReal_tsum_of_nonneg`); decompose each into singular part + density;
  the summed density d is the rnDeriv of the total measure
  (`eq_rnDeriv` with the mutually-singular sum). [L]
- 〈4〉 Conclude HasDerivAt f (Σ f_n′) a.e.; integrability/finite a.e. from
  rnDeriv < ⊤ a.e. [L]

Shared construction: an infinite family is handled termwise because a
*summable dominating family* licenses the interchange (of an integral/FTC
identification in a97J04 via the L¹ dominator deriv f; of right-limits and
measures in a94A02 via B n); a separately-constructed regular object (g,
resp. the density d) is then identified with the target (f, resp. f′) by a
uniqueness/limit argument. In a97J04 the identification is the hard half; in
a94A02 the interchange is. Same cascade shape, different stress points.
