# Containment witness — frame-15

Claim: the frame exposes no locked lemmas. Registration `locked-lemma-exposure`
is `[]`; check = the frame names no Mathlib declaration beyond those the pinned
problem file itself cites, and quotes its clause structure from the frozen
theorem rather than introducing outside results.

`IsPicardLindelof`, `ODE_solution_unique_univ` and `dist_le_of_trajectories_ODE`
are named in the frame brief because the FROZEN FILE'S OWN boundary note names
them (`Main.lean:167-180`). `apm_m93j06_not_holder_unique`,
`apm_m93j06_zero_solves_sqrt` and `apm_m93j06_signedSquare_solves_sqrt` are
declarations of the frozen file itself. Nothing else is named.

The frame states that conjunct 4 is already discharged. That is a fact about the
pinned file readable by anyone opening it, not a solution hint.

Checked 2026-08-19 by claude-2 (ground control) against
`problems/m93J06/lean/Main.lean` at pin
`a92ffb6c9cda32a33df0d259df552b1dbc611daf`.
