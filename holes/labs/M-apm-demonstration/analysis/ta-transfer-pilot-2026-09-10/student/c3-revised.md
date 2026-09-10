# C3 REVISED — precision repairs — zai-student-transfer-20260910

Fresh reads this round: **none** (session context only: c3-initial.md, the two pattern bodies read there). Retrieval/reads stand as recorded in c3-initial.md; no new query, no memory/network/agents.

## Repaired vs. already proved

Already proved and unchanged: existence (Cauchy transfer + completeness of Y), extension F|_A=f, uniqueness, non-necessity of X's completeness. Repaired below: (b) metric-only well-definedness (no subtraction in a general metric space; interleaving removed — it was unused as flagged); (d) strict-inearity of uniform continuity with ε/2 bookkeeping, "same modulus" claim withdrawn; density made explicit with a radius sequence; X empty case.

## (0) Setup, density, and edge cases

If X = ∅ then A = ∅ ⊆ X is vacuously dense, and there is exactly one map F:∅→Y (the empty map); it is uniformly continuous and unique — the theorem holds trivially. Assume henceforth X ≠ ∅.

Density with explicit radii: for each x∈X and each k≥1, the ball B(x, 1/k) ∩ A ≠ ∅; choose (using countable choice) a point a^(x)_k ∈ A with d(a^(x)_k, x) < 1/k. Then a^(x)_k → x, so approximating sequences exist for every x, with the explicit radius sequence r_k = 1/k. Define F(x) := lim_k f(a^(x)_k) (existence by part (a), unchanged).

## (b) Well-definedness — metric-only

Let (a_n), (b_n) ⊂ A both converge to x ∈ X. Fix ε > 0. By **uniform** continuity of f there is δ > 0 such that for all p,q ∈ A with d(p,q) < δ: ρ(f(p), f(q)) < ε/2. Choose N such that for n ≥ N: d(a_n, x) < δ/2 and d(b_n, x) < δ/2; then

  d(a_n, b_n) ≤ d(a_n, x) + d(x, b_n) < δ  ⇒  ρ(f(a_n), f(b_n)) < ε/2 for all n ≥ N.

Let u := lim f(a_n), v := lim f(b_n) (in Y, by (a)). The metric ρ is continuous in each argument, so letting n→∞:

  ρ(u, v) = lim_n ρ(u, f(b_n)) ≤ limsup_n [ ρ(u, f(a_n)) + ρ(f(a_n), f(b_n)) ] ≤ 0 + ε/2 ≤ ε/2 < ∞.

More explicitly: for n ≥ N, ρ(u,v) ≤ ρ(u, f(a_n)) + ρ(f(a_n), f(b_n)) + ρ(f(b_n), v) ≤ ρ(u,f(a_n)) + ε/2 + ρ(f(b_n),v); letting n→∞ each outer term → 0, giving ρ(u,v) ≤ ε/2. Since ε > 0 was arbitrary, ρ(u,v) = 0, hence u = v (metric axiom). No subtraction, no interleaving: the triangle inequality alone bounds the distance between the two candidate limits, and "distance ≤ every ε > 0" forces equality. F(x) is well-defined.

## (d) Uniform continuity of F — strict version, quantified

**Claim.** For every ε > 0 there is δ' > 0 such that for all u,v ∈ X with d(u,v) < δ': ρ(F(u), F(v)) < ε. Moreover, given the modulus of f, one may take δ' to be the δ that f assigns to ε/2; so F's tolerance for ε is *at least as good as* f's tolerance for ε/2. (I withdraw the initial "same modulus" phrasing: the initial argument proved only ρ ≤ ε for the δ of ε, which does not give strict inequality, and the ε/2 shift is what buys strictness.)

Proof. Fix ε > 0. By uniform continuity of f choose δ > 0 such that p,q ∈ A with d(p,q) < δ ⇒ ρ(f(p), f(q)) < ε/2. **Set δ' := δ.** Let u,v ∈ X with d(u,v) < δ'. Using the sequences from (0), a_k → u and b_k → v in A. For all k large enough: d(a_k, u) < (δ − d(u,v))/2 and d(b_k, v) < (δ − d(u,v))/2 — note δ − d(u,v) > 0 — so

  d(a_k, b_k) ≤ d(a_k, u) + d(u,v) + d(v, b_k) < δ,  hence ρ(f(a_k), f(b_k)) < ε/2.

For such k, the triangle inequality gives ρ(F(u), F(v)) ≤ ρ(F(u), f(a_k)) + ρ(f(a_k), f(b_k)) + ρ(f(b_k), F(v)) < ρ(F(u), f(a_k)) + ε/2 + ρ(f(b_k), F(v)). Let k → ∞: the first and third terms tend to 0 (F(u)=lim f(a_k), F(v)=lim f(b_k), metric continuity), so ρ(F(u), F(v)) ≤ ε/2 < ε. ∎ (Strict inequality throughout, final δ' = δ = f's tolerance for ε/2, quantified.)

## What was repaired

1. (b): removed limit subtraction (undefined for general metric Y) and the unused half-interleaving; replaced by explicit triangle-inequality bound ρ(u,v) ≤ ε/2 for all ε, forcing ρ(u,v)=0.
2. (d): initial ≤ε conclusion upgraded to strict <ε via ε/2 chosen *inside* the modulus application; final δ' stated explicitly; the "unchanged strict modulus" claim retracted and replaced by the precise ε/2-shift statement.
3. Density: explicit radius sequence r_k = 1/k and choice of a^(x)_k ∈ B(x,1/k)∩A.
4. X = ∅ handled (trivial case).

Unchanged: existence via Cauchy transfer (`metric-cauchy-convergence` @ e9c6b826… used there), extension on A, uniqueness, completeness of X not required.
