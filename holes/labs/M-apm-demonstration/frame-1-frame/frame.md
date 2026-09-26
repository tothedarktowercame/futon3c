# Frame: t94J02 (frame-1, round 1 measured unit)

## Target
`problems/t94J02/lean/Main.lean` in your checkout carries exactly one
executable `sorry` (compiler-verified at the pin: one "declaration uses
`sorry`" warning, line 18). The theorem `apm_t94j02`: for a compact Hausdorff
topology σ on X, no strictly finer topology is compact (and the stated
companion clause) — maximal-compact/minimal-Hausdorff territory. The bundle's
`proof-outline.md` (9.3K) and `informal-solution.md` (5.6K) are yours to use.

## Contract
Work ONLY in your checkout (path in the ENVIRONMENT block below); commit ONLY
to the frame branch. Do not touch files outside `problems/t94J02/`. Verify
the hole state against YOUR BRANCH before working — dispatch text can be
stale; the branch is the truth.

## Acceptance
1. `lake env lean problems/t94J02/lean/Main.lean` → exit 0, ZERO
   "declaration uses `sorry`" warnings. (NOT root `lake build` — it never
   compiles problem files.)
2. `#print axioms apm_t94j02` → at most [propext, Classical.choice, Quot.sound].
3. Commit to the frame branch; reply with summary, commit shas, and the
   verbatim axiom output.

## Reporting honesty
If the target has no executable hole or the statement is defective, SAY SO —
an honest stale-premise or defective-statement report is a valid completed
attempt.
