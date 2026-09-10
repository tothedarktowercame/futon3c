# Canonical mathematical-pattern census: family representatives

Generated from frozen.json. Counts are committed canonical declarations, not admission decisions. The exact full texts, metadata, IF/HOWEVER/THEN spans and hashes for all 136 patterns are in that file. The quotations below are bounded verbatim excerpts; absent IF means no explicit `+ IF:` section, not absence of prerequisites.

Examples below are exact-ID attachments in the four retained f211 snapshots. Their reviewed status is retained evidence, not a fresh graph check or certification that they implement the representative method. No name similarity or namespace migration creates an attachment here. Where none are found, examples remain unestablished within this population.

## math-formalization: 31

Representative: `math-formalization/additive-principal-parts-from-order-germs`. Source: `futon3/library/math-formalization/additive-principal-parts-from-order-germs.flexiarg`; SHA-256 `d589b7f18ba75fe1adbb9cb31f19958268adb89ff1515b0506e0880c371bfedb`.

IF (line 22):

> Every boundary pole has order exactly `-1`, the pole set is finite, and the goal is `f = g + ∑ ζ ∈ P, c ζ / (z - ζ)` with `g` holomorphic on a larger disk (e.g. to bound Taylor coefficients).

HOWEVER (line 26):

> Searches of `CanonicalDecomposition`, `FactorizedRational`, and `MeromorphicOn.extract_zeros_poles` find only multiplicative factorizations; the additive form is absent from the library.

THEN (line 30):

> (1) At a pole, `(meromorphicOrderAt_eq_int_iff hF).1 horder` gives `q` analytic with `q ζ ≠ 0` and `F =ᶠ (z - ζ)^(-1) • q`; shift with `comp_sub` and apply `AnalyticAt.exists_eq_sum_add_pow_mul 1` to write `q z = q ζ + (z - ζ) * H z`, so `F =ᶠ[𝓝[≠] ζ] H + q ζ / (z - ζ)`. (2) `choose c hc` over all poles. (3) The finite sum `S` of principal parts is `AnalyticAt` off `P` (`Finset.analyticAt_fun_sum`, `AnalyticAt.div`), so at `z ∉ P` use `meromorphicOrderAt_add` and at `z ∈ P` use `meromorphicOrderAt_congr` with `Finset.sum_erase_add` to see `F - S` has nonnegative order. (4) Normalize and thicke …

Available metadata: @title, @keywords, @why, @see-also, @how. Raw body and file identity are also available. Preconditions are prose; these fields do not prove them.

No exact-ID attached example in the frozen 364-memory population.

## math-formalization-CA: 23

Representative: `math-formalization-CA/ode-gronwall-api`. Source: `futon3/library/math-formalization-CA/ode-gronwall-api.flexiarg`; SHA-256 `e851771dea23953da24158d26045f082e75b77f518832dedf4ba3e7074da8e0b`.

IF (line 22):

> The goal is already expressed as distance between two trajectories, use `dist_le_of_trajectories_ODE`. If it is expressed as the norm of their difference and derivative bounds are readily available, use `norm_le_gronwallBound_of_norm_deriv_right_le` and simplify the resulting zero-error bound with `gronwallBound_ε0`.

HOWEVER (line 29):

> These theorems are naturally forward-time statements. Applying them directly when `t < 0` creates endpoint-order and sign obligations that obscure the estimate. Rewriting absolute values alone does not turn a backward trajectory into the forward trajectory expected by the API.

THEN (line 35):

> Split on the sign of `t`. For nonnegative time, apply the matching Grönwall theorem to `s ↦ phi s a - phi s b`, or directly to the pair of trajectories. For negative time define the reversed field `(s,y) ↦ -f (-s) y` and reversed trajectories `s ↦ phi (-s) a` and `s ↦ phi (-s) b`; prove the same Lipschitz and derivative hypotheses, and apply the forward theorem at `-t`. Normalize distance, norm and absolute value only after the analytic estimate is obtained.

Available metadata: @title, @keywords, @why, @see-also, @how. Raw body and file identity are also available. Preconditions are prose; these fields do not prove them.

No exact-ID attached example in the frozen 364-memory population.

## math-formalization-CV: 7

Representative: `math-formalization-CV/complex-liouville-periodicity`. Source: `futon3/library/math-formalization-CV/complex-liouville-periodicity.flexiarg`; SHA-256 `bf0aaf1e3402d26d47cc800f55237686c44673f33927751a88f7b74cfc2fc92b`.

IF (line 20):

> The periods generate a lattice, then the function's whole range is already attained on one fundamental domain, and a fundamental domain can be taken compact. Continuity on a compact set gives a bound, periodicity transports that bound to the plane, and `Liouville` in its bounded-entire-implies- constant form closes the goal.

HOWEVER (line 27):

> The reduction is the content, and it is easy to state as if it were free. Two obligations have to be discharged explicitly: that every point of the plane is a lattice translate of a point of the chosen domain, and that the chosen domain is compact. Neither follows from writing down a parallelogram; both are ordinary but real work, and the transport of the bound is a separate step from the bound itself.

THEN (line 35):

> Choose the fundamental domain and prove it compact. Prove that every point is a lattice translate of one of its points. Bound the function on the domain by continuity, transport the bound by periodicity, then apply the bounded-entire-implies-constant API.

Available metadata: @title, @keywords, @why, @see-also. Raw body and file identity are also available. Preconditions are prose; these fields do not prove them.

No exact-ID attached example in the frozen 364-memory population.

## math-formalization-FA: 3

Representative: `math-formalization-FA/weak-convergence-hilbert`. Source: `futon3/library/math-formalization-FA/weak-convergence-hilbert.flexiarg`; SHA-256 `9712cded92858e5bf6dd2cdb6da6348964f42f81e37eb8271f91bc0c8fc55aa8`.

IF (line 20):

> The route is taken through the squared norm, then each step is already in the library and the proof is assembly: expand `‖xₙ - x‖²` into norms and inner products, pass the inner-product term to its weak limit, cancel against the norm hypothesis, and take the square root at the end. The analogous `Lᵖ` route runs through Fatou to an a.e. limit and then along the same chain.

HOWEVER (line 28):

> The `liminf` steps carry hypotheses the classical argument never states. A `liminf` bound in Mathlib is conditional on the sequence being cobounded under the relevant filter — the theorem is false without it, because an unbounded-above gap makes `liminf` degenerate — and these side conditions surface only when the rewrite fails, at which point they look like a tooling obstruction rather than the mathematics they are. They also do not all discharge the same way. An eventual lower bound, a `liminf ≤` from a `≤ 0` bound, and a `le_liminf` under an unbounded-above gap each want a different witness …

THEN (line 48):

> Write the chain out as named steps before proving any of them — `norm_sub_sq` expansion, weak limit of the inner product, cancellation, square root — and treat each side condition as its own obligation with its own witness. Establish coboundedness where the `liminf` step needs it, and say which variant of the `liminf` API you are using and why that one. Use norm powers only for the scalar facts, and return to the original objects before the last step.

Available metadata: @title, @keywords, @why, @see-also. Raw body and file identity are also available. Preconditions are prose; these fields do not prove them.

Retained attached examples (up to two shown):

- `e-apm-promotion-27c9fba17df273a4fd03390524e691a5` — Recover complex Fourier coefficients with conjugate-symmetric paired probes; source `m02A02`; review `e-apm-promotion-review-c36c66c11a3d7e08e0944a4fbe8105ad`.
- `e-apm-promotion-3cd1a2080b57e8df4ba88f9099560274` — memlp-smul-holdertriple-ascribe-result-type; source `m01J04`; review `e-apm-promotion-review-a1f41fbf1b39bb94b14ee935401ebe2c`.

## math-formalization-GN: 2

Representative: `math-formalization-GN/connected-union-via-common-point`. Source: `futon3/library/math-formalization-GN/connected-union-via-common-point.flexiarg`; SHA-256 `8010202f2f48809fd82bf2f4c656682315ab1205961ef86917223e8456b80d1a`.

IF (line 20):

> The goal involves IsConnected or IsPreconnected applied to a union, and the hypothesis provides a common point in all components.

HOWEVER (line 24):

> "Union of connected sets is connected" is false in general — the common point hypothesis is essential. Without it, disjoint connected sets are a trivial counterexample.

THEN (line 29):

> Use isPreconnected_sUnion from Mathlib. The key step: rewrite the indexed union as a set-indexed union via sUnion_range, then apply the combinator with the common point and individual connectedness. + LEAN: api[isPreconnected_sUnion] api[Set.range] api[Set.not_nonempty_iff] tactic[classical → by_cases → simpa [sUnion_range]]

Available metadata: @title, @keywords. Raw body and file identity are also available. Preconditions are prose; these fields do not prove them.

No exact-ID attached example in the frozen 364-memory population.

## math-formalization-GR: 2

Representative: `math-formalization-GR/alternating-generation-via-preprimitive-three-cycle`. Source: `futon3/library/math-formalization-GR/alternating-generation-via-preprimitive-three-cycle.flexiarg`; SHA-256 `239e933ff0c88424bff4ab0688a49f2b5ab17fcf2bb4339129ea3b1a328545e7`.

IF (line 14):

> A goal says that the subgroup closure of explicit even permutations is the whole alternating group, while direct computation of closure membership is unavailable.

HOWEVER (line 16):

> Finiteness of the ambient permutation group does not make `Subgroup.closure` computationally decidable, so `decide` or `native_decide` cannot evaluate the generation equality directly.

THEN (line 18):

> Map the closure into the full permutation group; prove its action pretransitive, upgrade to preprimitive when the degree is prime, prove that a generator is a three-cycle, and apply `Equiv.Perm.alternatingGroup_le_of_isPreprimitive_of_isThreeCycle_mem`. Pull the resulting inclusion back through the subtype map and finish with `top_unique`. + LEAN: api[MulAction.isPretransitive_iff_base] api[MulAction.IsPreprimitive.of_prime_card] api[Equiv.Perm.isThreeCycle_swap_mul_swap_same] api[Equiv.Perm.alternatingGroup_le_of_isPreprimitive_of_isThreeCycle_mem] api[Subgroup.map]

Available metadata: @title, @keywords. Raw body and file identity are also available. Preconditions are prose; these fields do not prove them.

No exact-ID attached example in the frozen 364-memory population.

## math-formalization-MG: 1

Representative: `math-formalization-MG/chart-a-polytope-sphere-by-perimeter-walk`. Source: `futon3/library/math-formalization-MG/chart-a-polytope-sphere-by-perimeter-walk.flexiarg`; SHA-256 `6f9a0e84146d27084de8366e8eabeb2c74018860286ae4f99ea5e64af9baa80c`.

IF: no explicit section. Read the full context/conclusion before extracting conditions.

HOWEVER (line 15):

> Prove edge coverage, seam agreement, and injectivity on the chosen half-open parameter interval; a geometric picture does not discharge these.

THEN (line 18):

> Enumerate oriented faces or edges, parameterize each affinely, weld the endpoints, and rotate the parameter cut when a miss-a-point argument requires the excluded point to lie at the seam.

Available metadata: @title, @keywords, @provenance. Raw body and file identity are also available. Preconditions are prose; these fields do not prove them.

No exact-ID attached example in the frozen 364-memory population.

## math-informal: 24

Representative: `math-informal/unfold-the-definition`. Source: `futon3/library/math-informal/unfold-the-definition.flexiarg`; SHA-256 `641cb4452c1ed7e0be21f6df4a48f2e9abc429a86796c5616c9c337b37f7e182`.

IF (line 16):

> The statement involves a defined concept (compact, continuous, exact, measurable, adjoint) and you are treating it as a black box instead of engaging with its content.

HOWEVER (line 20):

> Definitions can unfold into multiple equivalent characterisations, and not all unfoldings are equally useful. Choosing the wrong characterisation can make the problem harder.

THEN (line 24):

> Write out the definition of every technical term in the statement. Replace defined terms with their meanings. The proof obligation often becomes clear once the definitions are fully expanded. If multiple equivalent definitions exist, try each one.

Available metadata: @title, @keywords, @why, @how. Raw body and file identity are also available. Preconditions are prose; these fields do not prove them.

No exact-ID attached example in the frozen 364-memory population.

## math-informal-CA: 6

Representative: `math-informal-CA/epsilon-of-room`. Source: `futon3/library/math-informal-CA/epsilon-of-room.flexiarg`; SHA-256 `536614e3e38bf1dd27993ec237cd40514bed0f404895a84d89fd5329739e4308`.

IF (line 16):

> The exact target is hard, but the *relaxed* target — `X ≤ ε`, or `X ≤ Y + ε`, or "within ε of the limit" — is reachable for each fixed ε > 0 (typically because some definition hands you an ε–δ budget: continuity, absolute continuity, measure zero, convergence).

HOWEVER (line 21):

> The relaxed bound must hold for *all* ε > 0 with the slack genuinely controlled by ε (the bound's right side must tend to the target as ε → 0), and the quantity must live where "≤ ε for every ε > 0 ⇒ ≤ 0" is valid. Watch that any δ, cover, or index chosen to meet one ε does not secretly depend on the thing you are bounding.

THEN (line 27):

> Fix an arbitrary ε > 0. Cash in the ε–δ budget from the relevant definition to prove the slack statement. Since ε was arbitrary, take ε → 0 (or inf over ε) to collapse the slack and conclude the sharp statement.

Available metadata: @title, @keywords. Raw body and file identity are also available. Preconditions are prose; these fields do not prove them.

No exact-ID attached example in the frozen 364-memory population.

## math-informal-CO: 2

Representative: `math-informal-CO/count-over-a-decomposition`. Source: `futon3/library/math-informal-CO/count-over-a-decomposition.flexiarg`; SHA-256 `b9c43ef3b1d7d8a37bfc3c1e4f44b4050399c11afa763e082f6cdc88452c2db6`.

IF (line 16):

> The quantity decomposes additively over a partition (orbits, conjugacy classes, cosets, cases, a cover), and you control all-but-one of the parts via a uniform property — each shares a divisibility, a congruence, a bound, or simply vanishes — so the distinguished part is forced.

HOWEVER (line 22):

> The decomposition must be **exhaustive and disjoint** (no part double-counted, none missed), and the shared constraint must genuinely hold on *every* part you are not solving for — one uncounted orbit or one exception breaks the conclusion.

THEN (line 27):

> Choose the decomposition that isolates the term you want (the center as the singleton classes; the fixed points as the size-1 orbits). Establish the uniform constraint on the other parts. Subtract / take congruences to force the distinguished term.

Available metadata: @title, @keywords. Raw body and file identity are also available. Preconditions are prose; these fields do not prove them.

No exact-ID attached example in the frozen 364-memory population.

## math-informal-CT: 7

Representative: `math-informal-CT/chase-the-diagram`. Source: `futon3/library/math-informal-CT/chase-the-diagram.flexiarg`; SHA-256 `50968510599d65b0ee04b96339576d53b558707e1b1edfba7e0899bedd515cb7`.

IF (line 19):

> The claim is an equation between composites, every square you need is already known to commute or is a hypothesis, and the diagram can be pasted from those known-commuting pieces. Then the equation follows from the pasting alone, with no information about the objects required.

HOWEVER (line 25):

> "By a diagram chase" is the most-abused phrase in the subject, and it hides three distinct failures. The pasting may not be *determined*: if two faces are filled by different 2-cells, the outer boundary depends on which, and the chase silently picks one. The diagram may not commute where you assumed it did — naturality in one variable is routinely used as naturality in both. And an element-style chase (`take x ∈ A, push it around`) needs an ambient abelian or concrete setting; in a general category there are no elements to take, and the same words no longer denote an argument. A chase whose st …

THEN (line 37):

> Name every cell you paste and say why each commutes — hypothesis, naturality, a previous lemma, or a universal property. Give the two routes around the boundary explicitly. If the argument is element-style, state the setting that licenses elements. If it is genuinely mechanical, say so and cite the general result (Kelly's doctrinal adjunction, the mates correspondence) rather than re-deriving it.

Available metadata: @title, @keywords. Raw body and file identity are also available. Preconditions are prose; these fields do not prove them.

No exact-ID attached example in the frozen 364-memory population.

## math-informal-LO: 1

Representative: `math-informal-LO/complexity-classification`. Source: `futon3/library/math-informal-LO/complexity-classification.flexiarg`; SHA-256 `86baa7a90e0255e13bfdbfc21d996be04215be66cf2c3ffdede332762b0182ff`.

IF (line 21):

> You can construct a polynomial-time (or appropriate) reduction from a complexity-class-complete problem to P, or vice versa, establishing membership / hardness with a sharp boundary.

HOWEVER (line 26):

> Complexity-classification is *one specific subtype* of `characterization-result` — its analogue in non-computational settings is `classification-result` (e.g., classifying KMS states or AFD outer actions). The reasoning shape is "list-the-cases-and-show-exhaustion" but the deliverable is "the problem belongs to class C" rather than "the structure has these instances." Use this leaf only when the result is genuinely complexity-theoretic.

THEN (line 35):

> 1. Construct the reduction (typically polynomial-time many-one or Turing) from a complexity-class-complete problem to P (for hardness) or from P to a problem already in C (for membership). 2. Verify the reduction is correct and within the resource bounds for the target class. 3. State the classification theorem: P is C-complete (or C-hard, or in C, as appropriate).

Available metadata: @title, @keywords. Raw body and file identity are also available. Preconditions are prose; these fields do not prove them.

No exact-ID attached example in the frozen 364-memory population.

## math-informal-NA: 2

Representative: `math-informal-NA/hybrid-certification`. Source: `futon3/library/math-informal-NA/hybrid-certification.flexiarg`; SHA-256 `ce070a9f45f28bafd47bde128b10d01f51180556cbb109e1ccddc6003208d4a5`.

IF (line 19):

> The exact method's bottleneck is a specific subroutine (polynomial GCD, resultant, matrix factorization) that scales badly with coefficient size or degree, but the qualitative structure of the answer (number of roots, sign pattern) can be determined cheaply by approximate computation.

HOWEVER (line 25):

> The hybrid method must be provably correct. The numerical step identifies candidates; the exact step certifies them. If the certification step fails (e.g., the approximate root is too far from the true root for sign determination), the method must detect this and fall back rather than produce a wrong answer. + FAILURE-MODES: - Double roots: if two roots are very close, the numerical step may merge them and the sign-change count will be wrong. Mitigation: compare numerical root count with exact Sturm count on a COARSE interval first. - Coefficient explosion: the exact evaluation at a rational t …

THEN (line 40):

> 1. Use fast numerical computation (numpy, LAPACK, floating-point) to compute approximate answers: root locations, eigenvalues, sign patterns. 2. Design an exact verification step that checks the numerical answer: evaluate the polynomial at rational test points, verify sign conditions, count roots by IVT. 3. The exact step should be O(n) in the problem size (not O(n³) like the numerical step), since it's certifying known answers, not discovering them. 4. If the exact step disagrees with the numerical step, investigate rather than trust either.

Available metadata: @title, @keywords. Raw body and file identity are also available. Preconditions are prose; these fields do not prove them.

No exact-ID attached example in the frozen 364-memory population.

## math-informal-RA: 2

Representative: `math-informal-RA/construct-through-a-finite-correspondence`. Source: `futon3/library/math-informal-RA/construct-through-a-finite-correspondence.flexiarg`; SHA-256 `d9228e5de3761f953b4d9480db94a3f1bceeeaeadd889159e660138cca63a863`.

IF (line 26):

> A correspondence theorem (Galois, Pontryagin, Stone) maps your target's constraint lattice anti-isomorphically onto a FINITE, computable side (subgroups of (ZMod n)ˣ), and a concrete realization (cyclotomic field, product group) makes the finite side explicit.

HOWEVER (line 31):

> The finite-side work "by itself does not resolve the implementation blockage" (the runner's own verdict): the transport step — reinterpreting fixed fields, matching instances, reindexing — is where formalization time actually goes, and it needs its own techniques (instance-diamond transport, lift/reflect). Budget the transport as the major cost, not an afterthought.

THEN (line 38):

> (1) realize the ambient object concretely; (2) state and prove every lattice fact on the finite side as standalone lemmas — meet, join, orders, indices; (3) transport through the correspondence one named lemma at a time; (4) read degrees/dimensions off indices via the correspondence's numerology.

Available metadata: @title, @keywords, @provenance. Raw body and file identity are also available. Preconditions are prose; these fields do not prove them.

No exact-ID attached example in the frozen 364-memory population.

## math-strategy: 20

Representative: `math-strategy/hypothesis-category-check`. Source: `futon3/library/math-strategy/hypothesis-category-check.flexiarg`; SHA-256 `d3a67ff4fd65f5917d98d761afe43b86dcecade7e52806e2b027cb3a1f4da735`.

IF (line 20):

> The theorem you want to invoke is stated for objects in category C (e.g., smooth manifolds, algebraic varieties, measurable functions), and your objects live in a different category C' (e.g., PL complexes, schemes, continuous functions).

HOWEVER (line 26):

> Category mismatches are a common source of silent errors in proofs that cross domain boundaries. A theorem about smooth Lagrangian intersections does not apply to polyhedral creases. A result about algebraic varieties may fail for general schemes. The mismatch may be bridgeable (via a smoothing lemma, GAGA, regularity bootstrap) but the bridge itself requires proof.

THEN (line 34):

> (a) State the category of the theorem's hypotheses explicitly. (b) State the category of your objects explicitly. (c) If they differ, provide a bridge: a smoothing lemma, a regularity result, a comparison theorem, or a specialization argument that places your objects in the required category. (d) If no bridge exists, the theorem does not apply -- find an alternative or prove a new version in your category.

Available metadata: @title, @keywords, @why, @how. Raw body and file identity are also available. Preconditions are prose; these fields do not prove them.

No exact-ID attached example in the frozen 364-memory population.

## proof-search: 3

Representative: `proof-search/typed-hole-as-frontier`. Source: `futon3/library/proof-search/typed-hole-as-frontier.flexiarg`; SHA-256 `5f342980b21e8043d5cb186050bc66e108292e6e2edcd61bfdb02ff3f63e18d7`.

IF (line 16):

> A piece of formal work is incomplete and must be recorded, handed off, or prioritized.

HOWEVER (line 20):

> "Incomplete" as prose is unqueryable: nobody can list the gaps, rank them, cluster them, or hand one to an agent — the knowledge of what's missing lives in heads and scrollback.

THEN (line 25):

> Write the hole with its type: WANT (the signature or codomain of what's missing — what would have to be true/constructed), HAVE (the endpoints in hand), and the discharge condition. Store holes where they can be enumerated (a ledger, a corpus of sorry records), so the frontier is a queryable set, not an impression.

Available metadata: @title, @keywords. Raw body and file identity are also available. Preconditions are prose; these fields do not prove them.

No exact-ID attached example in the frozen 364-memory population.
