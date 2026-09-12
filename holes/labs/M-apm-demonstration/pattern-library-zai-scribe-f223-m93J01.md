# Zai Scribe pattern library — f223 (m93J01 mining)

Patterns authored this frame from Student self-corrections. Stated without
Lean identifiers where possible; trigger/move/why form.

## math-formalization/apply-iff-lemma-as-term-when-rw-instance-mismatch

- **Trigger:** `rw` with a characterisation lemma (`rw [some_nat_iff_...]`)
  fails on an iff whose generality carries different typeclass instances than
  the concrete goal (e.g. a general-field normed-space instance vs the real
  line's).
- **Move:** stop rewriting; apply the iff as a term with explicit named
  arguments `(f := ...) (n := ...)` so unification is directed by the goal's
  instances rather than by the lemma head.
- **Why:** `rw` unifies the lemma's statement head-first and picks the most
  general instance; named projection of the same lemma forces the desired
  specialization.

## math-formalization/prove-iterated-derivative-composition-via-function-iterate-add

- **Trigger:** need `iteratedDeriv m (iteratedDeriv n f) = iteratedDeriv (m+n) f`
  and the library has no such composition lemma.
- **Move:** rewrite both sides with the lemma equating iterated derivative to
  iterated `deriv`, then `Function.iterate_add_apply`; small numeral sums are
  definitionally equal, so a trailing `rfl` (or `omega`-free defeq) closes it.
  Expect `simp only` to leave a composition not unfolded — append `rfl`.
- **Why:** iterated derivatives are compositions; function-iterate algebra is
  the composition algebra.

## math-formalization/discharge-order-cast-goals-in-withtop-with-norm-num

- **Trigger:** a goal like `(k : WithTop ℕ∞) ≤ n` arising from downgrading a
  smoothness hypothesis (`ContDiff ℝ n f` to `ContDiff ℝ k f` via `.of_le`).
- **Move:** prove the side goal with `by norm_num`; pass the weaker lemma via
  `hyp.of_le (by norm_num)` rather than feeding the stronger hypothesis
  directly.
- **Why:** `omega` reports spurious counterexamples on cast goals between
  `WithTop ℕ∞` and `simp` can make no progress on numeric order goals there;
  `norm_num` evaluates the numerals directly.

## math-formalization/close-polynomial-shape-gaps-with-convert-funext-ring

- **Trigger:** an `IsBigO.add`/sum of remainder terms compiles, but the summed
  expression is an expanded polynomial shape the goal statement does not share
  syntactically.
- **Move:** `convert <proof> using 1; funext <var>; ring` — equate the two
  shapes pointwise rather than trying to rewrite one into the other.
- **Why:** asymptotic statements are per-point predicates under a filter;
  `convert` at depth 1 moves the mismatch into an equality of functions that
  `ring` closes pointwise.

## math-formalization/dot-notation-unavailable-on-def-unfolded-predicates-use-named-lemma

- **Trigger:** `hyp.deriv`-style dot notation on a hypothesis whose type is a
  `def` that unfolds to an `Exists` structure fails with "invalid field".
- **Move:** use the fully named lemma of the same area that produces the
  conclusion for iterated derivatives (`ContDiff.iterate_deriv' n k hyp`),
  plus the rewrite equating iterated derivative with iterated `deriv`.
- **Why:** dot notation resolves fields of the head constant as declared; a
  `def`-wrapped predicate has no such fields, and the projection lemma may
  not exist under the short name at all in the given Mathlib version.
