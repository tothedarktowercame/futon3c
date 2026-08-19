# f13-guide working notes — m99J06

Cycle `m99J06-a0a723b1aef69731014d0b797f7f8ff0da1a8be419e8f064649dd606e8dbff0f`.
Not frozen material. Written so the analysis survives a pouch eviction.

## Solver dispatch 1

- action-id `f13-guide/dispatch-solver/1`, at version 9 -> 10.
- job `invoke-1787138965830-5047-ae5ec610`, recipient `f13-solver`.
- park `park-c016b9a7-6172-45ec-b363-b6f47a6a0aab`, deadline +3600s.
- **`[dispatch-recall-outcome=completed-with-memories]`** — first non-empty
  recall in the f9..f13 series. Two memories surfaced, both f12 harvest:
  - `e-1b72bb47-1575-4a08-ba3b-2a40735c2b86`
    propagate-local-api-mismatches-to-global-theorem-semantics (rewrite-rule)
  - `e-7c6631c9-caf0-4ee1-a1ce-2c7b7c6b88a6`
    audit-elaborated-regularity-semantics-before-proof-search (technique)
- The packet demands an inhabitation witness as first deliverable, WITHOUT
  supplying the argument below. Deliberate: whether the solver finds it (with
  or without the two surfaced memories) is the measurement.

## HYPOTHESIS (mine, NOT COMPILED): `apm_m99J06_H01Model H` is uninhabited

If this holds, `theorem apm_m99j06` is VACUOUSLY TRUE for every `H`, and the
frame confirms `:problem-closed-on-artifact` while refuting `:problem-solved`
— the f12 pair, by a different mechanism.

Argument. Everything `apm_m99J06_isH01Pair u du` requires constrains `u` only
on `[0,1]`:
- `AbsolutelyContinuousOnInterval u 0 1` is stated over `uIcc 0 1` only
  (Mathlib/MeasureTheory/Function/AbsolutelyContinuous.lean);
- `MemLp u 2 (volume.restrict (Icc 0 1))` sees only `[0,1]`;
- `HasDerivAt u (duRep x) x` for a.e. `x` in `[0,1]` is LOCAL, so for a point
  of `(0,1)` it constrains `u` only on a small neighbourhood inside `(0,1)`;
- `u 0 = 0`, `u 1 = 0`.

So take `u = 0` and `u' = Set.indicator {2} (fun _ => 1)`, both with `du = 0`.
`u'` is identically `0` on a neighbourhood of every point of `[0,1]` (the ball
of radius 1/2 avoids `2`), so both are `isH01Pair _ 0`.

`M.realizes` then gives `w w' : H` with `M.val w = u`, `M.val w' = u'`,
`M.weakDeriv w = M.weakDeriv w' = 0` — and `val w = u` is POINTWISE function
equality on all of `ℝ`.

`M.inner_eq` integrates over `Icc 0 1` only, where `u = u' = 0`, so
`⟪w,w⟫ = ⟪w,w'⟫ = ⟪w',w'⟫ = 0`, hence `‖w - w'‖² = 0`, hence `w = w'`, hence
`M.val w = M.val w'`, hence `0 = u' 2 = 1`. Contradiction.

The defect class: `realizes` pins `val` POINTWISE on all of `ℝ`, while
`inner_eq` measures only on `[0,1]`, so the structure demands an isometry
separate points that its own inner product identifies.

**Corroboration from the file's own header.** The "Statement repairs" note says
the former model "stored weak derivatives as literal functions even though its
inner product observed them only almost everywhere, making the hypothesized
structure inconsistent", and repaired it by moving `weakDeriv` into `Lp`. That
is EXACTLY this defect — and the repair was applied to `weakDeriv` and not to
`val`, which is still a raw `H → ℝ → ℝ`.

Status: HYPOTHESIS. Not compiled by me (card forbids scratch-file evals).
The solver owes a compiled witness in either direction. If it returns (A) with
a genuine model, this note is wrong and that is the better outcome.

If the solver misses it, this is the content to deliver at `:intervene` as an
`:answer` to its reported residual — content is permitted there because it
changes the route.

## Machine findings so far

- **The engine-owned park did not fire.** `park-dispatch` only posts when the
  cycle context carries `:agent`; `/api/alpha/parked` was empty after the
  dispatch, so I parked by hand. Report at close.
