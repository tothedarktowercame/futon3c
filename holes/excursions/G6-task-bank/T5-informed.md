minus40 memories injected before round 1 for this task:

--- memory lean-top-is-analytic (cc:-home-joe) ---
`ContDiff ℝ ⊤ f`, `ContMDiff I I' ⊤ f`, `IsManifold I ⊤ M` mean C^ω (analytic) in current Mathlib; smooth is `∞` = `((⊤ : ℕ∞) : WithTop ℕ∞)`. `ContDiffMapSupportedIn E F n K` takes `n : ℕ∞`, so `⊤` there is smooth. `open scoped ContDiff` gives `∞`/`ω` notation but clashes with a variable named `ω`.

2026-10-13 (found by lemma-4 on t96A08): an audit of all Main.lean files found
- t96A08 (registered faithful, analytic in fact): repaired to ∞, re-graded faithful, re-admitted;
- vacuous closed problems (compactly supported analytic test functions are 0): m93J04, m02J06, m95J02 (proofs survived ⊤→∞), m01J05 (narrower; survived);
- m01J04, m03J01, m95J05 REOPENED: their proofs used the collapse; collapse lemmas deleted;
- t01A05, t99J03 (held open) fixed in form.

**Why:** registry grading missed it; "closed" counts included vacuous statements.

**How to apply:** when grading or writing a statement, check every smoothness order; grep `ContDiff\w* ℝ ⊤`, `IsManifold .* ⊤`, `(⊤ : WithTop ℕ∞)`. Side goals like `2 ≤ ↑⊤` close with `by norm_cast`. Related: [[no-narrower-stand-ins]], [[blueprint-parallel-lemmas]].

Related trap (10-13/14, t99J04 and t95A04): `ContDiffAt ℝ ∞ f a` does NOT give one neighbourhood on which f is C^∞ (`ContDiffAt.contDiffOn` needs a finite order). A local inverse that must be a `PartialDiffeomorph` needs `ContDiffOn ∞` on an open set: state the hypothesis that way, or assume global smoothness.
--- end of injected memories ---

Write the Lean 4 statement, for current Mathlib, of: "f : ℝ → ℝ is infinitely differentiable" (C^∞, smooth; not analytic), using ContDiff. Statement only, as a `theorem` with `sorry`. Reply with the Lean in one code block and one sentence on the smoothness argument you passed.
