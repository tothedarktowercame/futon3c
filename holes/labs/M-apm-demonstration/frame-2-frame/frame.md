# Frame: t00A05 (frame-2, round 1 measured unit)

## Target
`problems/t00A05/lean/Main.lean` carries exactly one executable `sorry`
(compiler-verified at the pin): the final conjunct of `apm_t00a05` — the
line integral of the winding form over ANY C2 closed curve (per
`apm_t00a05_IsC2`) equals -2π. The two supporting lemmas
(`apm_t00a05_dOmega_eq_zero`, `apm_t00a05_lineIntegral_C1`) are already
proved. The bundle's `proof-outline.md` and `informal-solution.md` are yours
to use — the intended route is the deformation/change-of-variables argument
sketched in the hole's comment.

## Contract
Work ONLY in your checkout (ENVIRONMENT block below); commit ONLY to the
frame branch. Do not touch files outside `problems/t00A05/`. Verify the hole
state against YOUR BRANCH before working.

## Acceptance
1. `lake env lean problems/t00A05/lean/Main.lean` → exit 0, ZERO
   "declaration uses `sorry`" warnings.
2. `#print axioms apm_t00a05` → at most [propext, Classical.choice, Quot.sound].
3. Commit to the frame branch; reply with summary, commit shas, verbatim
   axiom output.

## Reporting honesty
If the statement is defective or the target unreachable, SAY SO — honest
refusals are valid completed attempts (frame-1 precedent: a defect report
led to an operator-ruled repair and two independent proofs).
