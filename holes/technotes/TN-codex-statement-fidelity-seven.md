# Fresh statement-fidelity review of seven rejected-manifest problems

Date: 2026-09-06

Reviewer: codex-7

Scope: review only; no Lean source or status was changed.

## Result

All seven current Lean artifacts are **FAITHFUL** to the mathematical asks in
their `problem.md` Raw TeX. The `bridges-rejected` and `tier-a-rejected`
manifest verdicts therefore do not describe a defect present in the current
files. This review cannot reconstruct why those verdicts were originally
entered because their reasons were not recorded.

| Problem | Manifest verdict | Fresh verdict |
|---|---|---|
| `a01A12` | `bridges-rejected` | **FAITHFUL** |
| `a92J01` | `bridges-rejected` | **FAITHFUL** |
| `a01J05` | `bridges-rejected` | **FAITHFUL** |
| `a96J08` | `bridges-rejected` | **FAITHFUL** |
| `a01J06` | `tier-a-rejected` | **FAITHFUL** |
| `a98J02` | `tier-a-rejected` | **FAITHFUL** |
| `a02J04` | `bridges-rejected` | **FAITHFUL** |

I read each Raw TeX ask before its Lean file. I then inspected the target and
the definitions occurring in it, checked for omitted parts and surrogate or
tautological conclusions, searched the seven files for `axiom`, `opaque`, and
`native_decide`, and elaborated each current source with `#print axioms` on its
target declaration. Every target reported exactly
`[propext, Classical.choice, Quot.sound]`.

## `a01A12` — FAITHFUL

The Raw TeX has two requirements: construct a one-to-one conformal map from the
upper half-plane onto the slit angle, and describe all such maps
(`apm-lean/problems/a01A12/problem.md:21` and `:26`).

The Lean definitions represent the same source and target:

> `def apm_a01A12_H : Set ℂ := {z : ℂ | 0 < z.im}`
>
> `def apm_a01A12_wedgeSlit : Set ℂ :=`
> `{z : ℂ | z ≠ 0 ∧ |z.arg| < Real.pi / 4} \`
> `{z : ℂ | 0 ≤ z.re ∧ z.re ≤ 1 ∧ z.im = 0}`

These occur at `apm-lean/problems/a01A12/lean/Main.lean:28` and `:33`. The
chosen map is `(1 - z ^ 2) ^ (1 / 4 : ℂ)` (`:37`). Conformality is not reduced
to image containment: `apm_a01A12_ConformalBijOn` explicitly requires `BijOn`,
differentiability, and nonzero derivative (`:43`).

The main theorem states both requested parts. It proves the displayed map is a
conformal bijection and classifies every other conformal bijection as its
composition with a positive-determinant real Möbius automorphism of the upper
half-plane (`apm_a01A12` at `:1397-1405`). Thus neither injectivity,
surjectivity, nor the “all mappings” clause is absent. `#print axioms
apm_a01a12` reported only the three permitted axioms.

## `a92J01` — FAITHFUL

The Raw TeX asks for truth values and proofs or counterexamples for four
inclusions (`apm-lean/problems/a92J01/problem.md:25-37`). The Lean theorem has
four matching conjuncts (`apm-lean/problems/a92J01/lean/Main.lean:298-311`):

1. It proves `L^p([0,1]) ⊆ L^q([0,1])` for `p > q ≥ 1` (`:299-301`).
2. It gives an explicit sequence in `ℓ^p` but not `ℓ^q` (`:302-304`), thereby
   refuting the stated inclusion.
3. It gives an explicit real-line function in `L^p` but not `L^q` (`:305-307`).
4. It gives an explicit member of `L^p([0,1])` that is not continuous
   (`:308-311`).

The counterexamples are concrete power laws and an interval indicator, not
opaque answer functions (`:25-32`). Their membership and non-membership are
proved in the bridge theorems; for example, the sequence bridge states both
`MemLp ... p` and `¬MemLp ... q` (`:121-126`). `#print axioms apm_a92j01`
reported only the three permitted axioms.

## `a01J05` — FAITHFUL

The Raw TeX asks that an analytic function of boundary modulus one with `N`
interior zeros have exactly `N-1` interior critical points, all multiplicities
counted (`apm-lean/problems/a01J05/problem.md:18-23`).

Lean defines the interior as the open unit ball and the boundary as its sphere
(`apm-lean/problems/a01J05/lean/Main.lean:22-26`). Its multiplicity is the sum
of `analyticOrderAt` over points in that open ball (`:28-32`), so the theorem is
not merely counting a hand-written list. The target assumes analyticity on a
slightly larger closed ball, boundary norm one, and total multiplicity `N`, and
concludes:

> `apm_a01J05_zeroMultiplicity (deriv f) = (N - 1 : ℕ)`

at `apm-lean/problems/a01J05/lean/Main.lean:1917-1923`. The slightly-larger-ball
form is a concrete encoding of an open neighborhood of the compact closed unit
disk, not a change of theorem. The proof reaches the result through divisor and
logarithmic-derivative identities (`:1871-1915`), rather than unfolding the
conclusion against itself. `#print axioms apm_a01j05` reported only the three
permitted axioms.

## `a96J08` — FAITHFUL

The Raw TeX asks to evaluate the whole-line expression under
`0 < Re(ω) < 1` (`apm-lean/problems/a96J08/problem.md:46-47`). Because the
integrand has a pole at zero, an ordinary improper integral does not exist; the
symmetric Cauchy principal value is the mathematically meaningful reading.

The target states precisely that symmetric truncation limit and the value
`π * cos (π * ω) / sin (π * ω)` (`apm-lean/problems/a96J08/lean/Main.lean:726-729`).
This is `+π cot(πω)`, with the corrected sign documented at `:713-725`. I agree
with the prior review that this is faithful. I also agree that the main proof’s

> `exact apm_a96J08_bridge_1 ω hre_pos hre_lt`

at `:759` is an unnecessary wrapper around an identically stated theorem
(`:693-699`), but it is not circular and does not weaken the target: the bridge
itself has a substantive proof at `:700-711`. `#print axioms apm_a96J08`
reported only the three permitted axioms.

## `a01J06` — FAITHFUL

The Raw TeX asks for convergence of the weighted zero sequence of a nonzero
entire function of exponential type for every `α > 1`
(`apm-lean/problems/a01J06/problem.md:18-23`).

The growth predicate is exactly `‖f z‖ ≤ B * exp (A * ‖z‖)`
(`apm-lean/problems/a01J06/lean/Main.lean:22-23`). More importantly, the
sequence is tied to the actual analytic divisor: every fiber is finite and its
cardinality equals `analyticOrderNatAt f z` (`:25-28`). Thus the conclusion
cannot be proved using a fabricated list unrelated to `f`.

The main theorem assumes entire differentiability, `f ≠ 0`, positive `A` and
`B`, the growth bound, and that exact enumeration predicate, then concludes

> `∀ α : ℝ, 1 < α → Summable (fun n : ℕ => Real.rpow (1 + ‖ω n‖) (-α))`

at `apm-lean/problems/a01J06/lean/Main.lean:646-654`. Making `f ≠ 0` explicit
correctly formalizes what “zeros listed with appropriate multiplicity” already
presupposes. `#print axioms apm_a01j06` reported only the three permitted
axioms.

## `a98J02` — FAITHFUL

The Raw TeX asks for the uniform small-interval estimate for an absolutely
continuous function whose derivative is square-integrable
(`apm-lean/problems/a98J02/problem.md:25-28`). The source prints
`|f(x)-f(y)|2`; in context, the standard claim and the supplied denominator
make the missing superscript marker unambiguously a square.

Lean states absolute continuity on `[0,1]`, integrability of
`|derivWithin f [0,1] x|^2`, and, for every positive epsilon, a positive delta
such that

> `|f x - f y| ^ 2 / |x - y| < ε`

for every `0 ≤ y < x ≤ 1` with `|x-y| < δ`
(`apm-lean/problems/a98J02/lean/Main.lean:139-146`). This is the full uniform
claim, not a pointwise-in-center surrogate. The proof uses a uniform
small-measure-set integral estimate and Cauchy–Schwarz (`:207-250`). `#print
axioms apm_a98j02` reported only the three permitted axioms.

## `a02J04` — FAITHFUL

The Raw TeX defines a triangular planar density, pushes the resulting measure
forward under first projection, and asks for the Lebesgue decomposition and
Radon–Nikodym derivative of Lebesgue measure with respect to that pushforward
(`apm-lean/problems/a02J04/problem.md:18-24`).

I agree with the prior review. The supposedly missing objects are explicitly
defined: `g` and `μ` at `apm-lean/problems/a02J04/lean/Main.lean:22-26`, `P` at
`:28`, and `τ := Measure.map P μ` at `:32-33`. Lean derives the marginal
density `2x` on `[0,1]` (`:35-36`, `:100-102`). The main theorem then states:

- the derived density of `τ` (`:196-198`);
- absolute continuity of Lebesgue measure restricted to `[0,1]` with respect
  to `τ` (`:199`);
- mutual singularity of Lebesgue measure on the complement with `τ` (`:200`);
- Radon–Nikodym derivative `1/(2x)`, `τ`-almost everywhere (`:201-203`).

Those are exactly the absolutely continuous and singular pieces of Lebesgue
measure relative to a measure supported on `[0,1]`, together with the requested
derivative. The endpoint at zero is correctly handled as a null exception in
the proof (`:165-194`). `#print axioms apm_a02j04` reported only the three
permitted axioms.

## Limits of this review

This review establishes statement fidelity and current target axiom
dependencies. It does not reconstruct the absent historical reasons for the
manifest rejections, change those verdicts, rerun C-square, audit runtime frame
receipts, or make any claim that the status/manifest update machinery is
correct. No corrected Lean statement is supplied because none of the seven was
classified **DEFECTIVE** or **PARTIAL**.
