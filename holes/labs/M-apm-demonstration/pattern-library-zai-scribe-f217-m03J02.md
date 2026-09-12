# Zai Scribe pattern library — f217 / m03J02

Added because the reviewed mathematics-memory search found no coherent parent
for two self-corrections mined from this frame's student attempts.

## math-formalization/name-the-existence-witness-before-refining-unique-existence

- **Trigger:** The goal is an `ExistsUnique` (or any nested `∃` inside a
  uniqueness field) and `refine ⟨?a, ?b, ?_⟩` fails with metavariable-identity
  errors ("sorry ≠ sorry") or intro/binder errors because the lambda under the
  third field is already applied.
- **Move:** Obtain the existence witness first — `obtain ⟨w, hw⟩ : ∃ w, P w := by ...`
  (even under a temporary `sorry`) — then close the goal with an explicit
  anonymous constructor whose uniqueness component is a plain lambda taking
  the competitor and its hypothesis, flipping the uniqueness lemma's equation
  with `.symm` as needed.
- **Why it works:** Naming the witness turns the metavariable in the existence
  component into an ordinary local, so the uniqueness lambda elaborates against
  concrete terms instead of competing `sorryAx` metavariables.

## math-formalization/normalize-lipschitz-constants-before-applying-gronwall

- **Trigger:** Combining named Mathlib Lipschitz facts (`Real.lipschitzWith_cos`,
  `LipschitzWith.id`, `LipschitzWith.add`) yields a constant like `1 + 1` while
  the target theorem (Gronwall-style uniqueness) demands a literal such as `2`,
  and `norm_num` cannot close the NNReal equality in context.
- **Move:** Bridge with an explicit `have h2 : (2 : NNReal) = 1 + 1 := by rw [one_add_one_eq_two]`
  and rewrite; keep the triangle-inequality step elementary via
  `abs_sub a b : |a - b| ≤ |a| + |b|` (there is no `abs_add` under that name).
- **Why it works:** Tactic automation treats the syntactic constant `1 + 1`
  and the literal `2` in NNReal as different terms; a single definitional
  rewrite closes the gap that `norm_num` leaves open inside a dependent
  hypothesis.
