# Frame: m93J06 (frame-15)

## Target
`problems/m93J06/lean/Main.lean` is 265 lines and carries exactly one executable
`sorry`, at line 147, discharging the whole of `theorem apm_m93j06`.

The frozen theorem is a conjunction of FIVE clauses about ODE flows on `ℝ`:

1. **Picard–Lindelöf.** For continuous `f` with a global Lipschitz constant in
   the second argument, every `y₀` admits a UNIQUE global solution
   (`∃! y, apm_m93j06_Solves f y₀ y`).
2. **C¹ flow regularity.** For `ContDiff ℝ 1` data and a flow `φ`, there is
   `ε > 0` such that `φ t` is `C¹` for `|t| < ε`.
3. **Global existence under linear growth.** `|f t y| ≤ C * |y|` plus a global
   Lipschitz bound yields a flow.
4. **Hölder non-uniqueness** — a NEGATED universal. **ALREADY PROVED IN THE
   FROZEN FILE**: cite `apm_m93j06_not_holder_unique` (`Main.lean:132`), which
   rests on `apm_m93j06_zero_solves_sqrt` and
   `apm_m93j06_signedSquare_solves_sqrt`. This conjunct is free. Do not
   reconstruct it.
5. **Exponential Lipschitz bound on the flow**:
   `|φ t a - φ t b| ≤ exp (L * |t|) * |a - b|`.

So the real work is FOUR conjuncts, and the frame is registered saying so.

## What is different about this frame, and you should know it
**This problem was PROBED SOUND before the frame opened.** The three preceding
frames each drew a defective formalisation — a statement that was FALSE, one
that was VACUOUS, and a model that was UNINHABITED — and each defect was found
only by trying to prove the theorem. So m93J06 was probed first, in one dispatch
(`probe/m93J06-soundness`, commit `9ba574a6`): ten axiom-clean witnesses in
`problems/m93J06/lean/Soundness.lean` exhibit an inhabitant AND a non-inhabitant
for each of `apm_m93j06_Solves` and `apm_m93j06_IsFlow`, show that the all-of-`ℝ`
solution formulation permits genuine uniqueness, and confirm `ContDiff` occurs
only as `ContDiff ℝ 1`.

**This is therefore a frame about SOLVING, not about detecting a broken
problem.** That has not been true since frame 10.

If you nevertheless find a defect, that is a reportable result and not a failure
— but do not go looking for one in preference to proving the theorem.

## Contract
Close the `sorry`, **or reduce it to strictly less residual and say precisely
what remains.**

Named per-clause residuals are an accepted result even if no clause closes.
Mathlib supplies `IsPicardLindelof`, `ODE_solution_unique_univ` and
`dist_le_of_trajectories_ODE`; the frozen file's own boundary note says clauses
1 and 3 need solutions glued across an exhausting family of compact time
intervals, and clause 2 needs differentiable dependence on initial data rather
than regularity of each trajectory. Read that note before starting.

If the library lacks a notion you need, that is **not** a reason to stop: name it
as a construction target and build it if budget allows. See your card.

Any definition you introduce needs a proof it takes a non-trivial value in a
concrete case.

## Acceptance
- The frozen statement of `apm_m93j06` is unchanged.
- Any close is axiom-clean; the `sorry` count strictly decreases, **or** the
  bundled `sorry` becomes named per-clause residuals.
- Whatever remains open is localised, with nearest API and empty searches
  recorded beside it.
- Do not modify `problems/m93J06/lean/Soundness.lean`; it belongs to the probe.
