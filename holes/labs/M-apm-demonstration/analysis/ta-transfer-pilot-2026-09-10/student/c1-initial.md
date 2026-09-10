# C1 INITIAL — zai-student-transfer-20260910

## Pattern use (search: see c1-initial-tools.txt)

- **Retrieved:** 5 patterns (ids above). **Actually read (2):**
  - `math-formalization-CA/uniform-continuity-boundedness` @ `a24aa6c9…7272c` — "bound the image before/instead of mixing modulus and magnitude; boundedness of the image is where the domain's boundedness enters."
  - `math-informal/local-to-global` @ `ac568c2e…fb63` — "cover with pieces where the property is manageable, prove on each, assemble globally; here no overlap-compatibility condition is needed, just max over centers."
- Claimed used = proof-supported used = exactly these two. Not read: metric-cauchy-convergence, estimate-by-bounding, unfold-the-definition (titles only). No memory store consulted.

## Premises

Required:
1. X nonempty, totally bounded — **given**.
2. Y normed real vector space (only vector-space/norm facts used: triangle inequality, ||·|| homogeneity-free lower bound 0) — **given**.
3. F nonempty, **uniformly equicontinuous**: ∀ε>0 ∃δ>0 ∀f∈F ∀x,y: d(x,y)<δ ⇒ ‖f(x)−f(y)‖<ε — **given**.
4. Pointwise bounded: ∀x ∃B_x<∞ ∀f: ‖f(x)‖≤B_x — **given**.

Not needed / absent: completeness of X, compactness (total boundedness alone suffices — no sequential argument), any structure on Y beyond the norm. Unchecked: nothing external; the proof is self-contained elementary estimates.

## Theorem

∃M<∞ such that ‖f(x)‖ ≤ M for all f∈F, x∈X. (Equivalently: the family F is uniformly bounded.)

## Proof

Fix ε = 1. By uniform equicontinuity there is δ > 0 such that

  (★) for all f∈F and all x,y∈X with d(x,y) < δ:  ‖f(x) − f(y)‖ < 1.

**Step 1 (finite net).** Since X is totally bounded, the open cover {B(x,δ) : x∈X} admits a finite subcover: there exist x₁,…,x_n ∈ X with X = ⋃_{i=1}^n B(x_i, δ). This is where the domain hypothesis enters — exactly the entry point flagged by `uniform-continuity-boundedness` ("the step between [continuity and boundedness] is exactly where the domain's boundedness enters").

**Step 2 (bound on the net).** By pointwise boundedness, for each i choose finite B_i with ‖f(x_i)‖ ≤ B_i for all f∈F. Set

  M := max_{1≤i≤n} B_i + 1.

M is finite (max of finitely many finite numbers, plus 1).

**Step 3 (local-to-global patch).** Let f∈F and x∈X be arbitrary. By Step 1 there is i with d(x, x_i) < δ. Then, by the triangle inequality in Y and (★),

  ‖f(x)‖ ≤ ‖f(x) − f(x_i)‖ + ‖f(x_i)‖ < 1 + B_i ≤ 1 + M′ ≤ M,

where M′ := max_i B_i, so M = M′ + 1. Since f and x were arbitrary, M bounds ‖f(x)‖ over all of F × X. ∎

Note on patching (per `local-to-global`'s HOWEVER clause): here the local bounds need no compatibility on overlaps — each x is assigned to *some* net point, and the same uniform δ works everywhere; the assembly is a max, not a gluing, so no overlap check is required. That clause was deliberately discharged rather than ignored.

## Does "each f uniformly continuous" suffice?

**No.** Uniform equicontinuity is strictly stronger: it requires one δ to serve all f∈F simultaneously, whereas "each f uniformly continuous" permits δ = δ(f, ε) depending on f.

**Counterexample.** Take X = {0} ⊂ ℝ (nonempty, totally bounded), Y = ℝ, and F = {f_n : n ∈ ℕ} with f_n(0) = n. Every f_n is (trivially) uniformly continuous — on a one-point space *every* map is uniformly continuous. But F is not even pointwise bounded at 0 (B_0 would have to dominate every n), so pointwise boundedness already fails, and a fortiori no finite M exists: ‖f_n(0)‖ = n → ∞.

If one wants a counterexample preserving pointwise boundedness but failing the conclusion, that is impossible on totally bounded X — that is exactly the theorem just proved — so pointwise-bounded failure must use a non-totally-bounded domain; but the question as posed ("does the weaker per-f hypothesis suffice to derive the conclusion?") is answered negatively by the example above, where all per-f uniform continuity holds and the conclusion is false. (Uniform equicontinuity also fails there, for ε=1 no single δ consideration can help since boundedness fails outright — consistent with the pattern's observation that the modulus says nothing about the *size* of values.)

## Difficulty / limitations

- **Difficulty observed:** none of substance. The 1-net + triangle-inequality estimate is the standard proof of the "uniformly bounded" half of Arzelà–Ascoli-type results; total boundedness replaces compactness cleanly because no limit extraction is needed. My main hesitation was checking that no completeness/compactness is smuggled in — it is not: Steps 1–3 use only total boundedness, equicontinuity, pointwise boundedness, and the norm axioms.
- Limitation: the bound M produced is non-constructive only in choosing which finite subcover/net to use (fine, X arbitrary metric); each B_x is assumed given, not computed.
- Honest boundary of the counterexample: on a one-point space even *uniform equicontinuity* holds trivially, so the one-point example shows pointwise boundedness is indispensable, not that per-f uniform continuity is weaker. The clean demonstration of the latter keeps pointwise boundedness and drops total boundedness: X = ℕ ⊂ ℝ (discrete, so not totally bounded), Y = ℝ, f_n(k) = n if k = n, else 0. Each f_n is uniformly continuous (two-valued on a discrete space); F is pointwise bounded (sup_n ‖f_n(k)‖ = k < ∞ for each fixed k); yet sup over F×X of ‖f(x)‖ is infinite — no uniform bound. So with only per-f uniform continuity, pointwise boundedness does not upgrade to uniform boundedness unless the domain is totally bounded; and under the stated hypotheses of the theorem, equicontinuity is what lets one fixed δ ride along the finite net.
