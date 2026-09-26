# C3 INITIAL — extension by density — zai-student-transfer-20260910

## Bounded search record (1 of 3 queries; bodies read: 2 of 2)

Query 1 (exact): `python3 -c` printing full `p['text']` and `p['sha256']` for the two selected ids from patterns.json. Full output preserved in this transcript (see below); nothing else queried.
- Retrieved: all 5 ids known from C1 receipt.
- Actually read (this round): `math-formalization-CA/metric-cauchy-convergence` @ `e9c6b826610b1dc516c5b86bdff78c865e062e323fe6c0543fae5efc3e26f8c1`; `math-informal/unfold-the-definition` @ `641cb4452c1ed7e0be21f6df4a48f2e9abc429a86796c5616c9c337b37f7e182`.
- Claimed use: cauchy-convergence (convergence-via-Cauchy strategy); unfold-the-definition (write out definitions before estimating).
- Proof-supported use: both, in the places cited inline. Prior exposure (not fresh reads): uniform-continuity-boundedness, local-to-global (C1), source-lesson.md (C1/C2 rounds). No memory calls. Gap claims below refer only to this bounded corpus+search, not proven library absence.

## Required conditions

1. A ⊆ X dense (every x∈X is a limit of a sequence/net in A — for the construction I use sequences: metric spaces are first-countable) — **given**.
2. f:A→Y uniformly continuous — **given**.
3. **Y complete** — **given; essential** (limits of Cauchy images must exist in Y).
4. X metric — **given**.
- **Completeness of X: NOT required** (shown below). A itself need not be complete; density in X is what matters.

## Construction

For x∈X define F(x) := lim_{n→∞} f(a_n), where (a_n) ⊂ A is any sequence with a_n → x (exists by density; for x∈A take a_n ≡ x).

## Proofs

**(a) The limit exists.** Let (a_n)→x in X. Then (a_n) is Cauchy in X, hence in A. Uniform continuity of f maps Cauchy sequences to Cauchy sequences: given ε>0 pick δ>0 with d(a,b)<δ ⇒ ρ(f(a),f(b))<ε (uniformly); pick N with d(a_m,a_n)<δ for m,n≥N; then ρ(f(a_m),f(a_n))<ε for m,n≥N. So (f(a_n)) is Cauchy in Y; **Y complete** ⇒ the limit exists. [Pattern `e9c6b826…`: "prove convergence by showing Cauchy"; here the Cauchy transfer is the crux and completeness of the target supplies the limit. Independence of the Lean-specific api/tactic fields — not used on paper.]

**(b) Independence of choices.** Let (a_n),(b_n) ⊂ A both → x. Interleave: c_{2n}=a_n, c_{2n}→x too, and ρ(f(a_n), f(b_n)) → 0 by uniform continuity of f applied to d(a_n,b_n) ≤ d(a_n,x)+d(x,b_n) → 0 (ε-δ: for ε>0 get δ; eventually d(a_n,b_n)<δ, so ρ<ε). Hence lim f(a_n) − lim f(b_n) has distance ≤ limsup ρ(f(a_n),f(b_n)) = 0, i.e. the two limits coincide in the metric ρ (distance 0 ⇒ equal). So F(x) is well-defined, independent of the approximating sequence.

**(c) Extension: F|_A = f.** For x∈A take a_n ≡ x: F(x) = lim f(x) = f(x). ✓

**(d) F uniformly continuous, with the same modulus.** Claim: for all u,v∈X with d(u,v)<δ (the δ chosen for ε), ρ(F(u),F(v)) ≤ ε. Fix ε>0, get δ from uniform continuity of f. Take a_n→u, b_n→v in A. For n large, d(a_n,b_n) ≤ d(a_n,u)+d(u,v)+d(v,b_n) < δ, so ρ(f(a_n),f(b_n)) ≤ ε. Passing to the limit (metric is continuous): ρ(F(u),F(v)) = lim ρ(f(a_n),f(b_n)) ≤ ε. (If one wants strict <ε throughout, bookkeep with ε/2.) Hence F is uniformly continuous; indeed its modulus is no worse than f's.

**(e) Uniqueness.** Suppose G:X→Y is continuous with G|_A = f. For x∈X and a_n→x: G(x) = lim G(a_n) = lim f(a_n) = F(x). Any continuous extension equals F; a fortiori any *uniformly* continuous one. (So uniqueness needs only continuity of the competitor.)

## Is completeness of X required?

**No.** Example: X = ℚ (not complete), A = ℚ, f = identity into Y = ℝ (complete). F = id:ℚ→ℝ is the unique uniformly continuous extension, existing with no completeness of X anywhere. The construction only ever takes limits of the *images* in Y; sequences in A converging in X are used only through d(a_n, b_n)→0-type facts, not through convergence of anything in X to a point of X beyond the given x itself. What is essential is **completeness of Y** — drop it and the construction dies at step (a): e.g. A = ℚ, X = ℝ, Y = ℚ, f = id. Then F(√2) would have to be lim a_n in ℚ with a_n→√2 — no such limit exists, and indeed no continuous extension ℝ→ℚ exists (ℝ connected, image of dense ℚ unbounded… simplest: a continuous map ℝ→ℚ is constant, but f=id_ℚ is not constant).

## What the patterns supply vs. leave to me

- `metric-cauchy-convergence`: supplies the strategy "get convergence by proving Cauchy + completeness" — applied in step (a). Leaves to me: the Cauchy-transfer lemma (uniform continuity ⇒ Cauchy-preserving) — the corpus's *single-map* Cauchy content doesn't state it — and everything else (well-definedness, modulus preservation, uniqueness).
- `unfold-the-definition`: supplies the discipline of writing out density, uniform continuity, completeness explicitly before estimating — this is how steps (b),(d) were arranged. No mathematical content beyond that.
- Apparent gap in this bounded corpus: no pattern addresses *extension/density/gluing of a function to limit points*, nor *well-definedness of a definition via approximating sequences* (local-to-global covers covers/patching but not sequential limits). Restated: within these five patterns and my three-query budget, the extension-of-uniformly-continuous-maps theme is absent; this is only a bounded-corpus observation, not proven library absence. No publication made.

## Difficulty (honest)

Low-to-moderate; no blockers. Two points needed care: (i) well-definedness must use *uniform* continuity — merely continuous f can fail (f(x)=1/x on A=(0,1)∩ℚ has no continuous extension to 0), and my first impulse to prove (b) via continuity of f fails since x∉A; uniform continuity of the pair (a_n,b_n) is the fix. (ii) The strict-vs-nonstrict ε bookkeeping in (d). Neither stalled the proof.
