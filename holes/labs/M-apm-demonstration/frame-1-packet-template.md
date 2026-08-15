# Frame-1 dispatch packet template (t94J02) — pre-flight, 2026-08-15

Prepared during the attested-system pre-flight. The conductor fills the
bracketed fields at dispatch time; everything else is frozen text. The
acceptance bar reflects rehearsal finding 9: root `lake build` is VACUOUS for
problem files (no `lean_lib` for `problems/`), so the bar is the direct
compile plus the axiom probe.

## Target
`problems/t94J02/lean/Main.lean` in your checkout carries exactly one
executable `sorry` (compiler-verified at the pin: one "declaration uses
'sorry'" warning, line 18). The bundle's `proof-outline.md` (9.3K) and
`informal-solution.md` (5.6K) are yours to use.

## Contract
Work ONLY in your checkout: [CHECKOUT — injected by dispatcher per Fix C].
Commit ONLY to the frame branch. Do not touch files outside
`problems/t94J02/`. Verify the hole state against YOUR BRANCH before working
(memory e-907281cd: dispatch text can be stale; the branch is the truth).

## Acceptance
1. `lake env lean problems/t94J02/lean/Main.lean` → exit 0 with ZERO
   "declaration uses `sorry`" warnings. (NOT root `lake build` — that never
   compiles problem files; memory e-2d8f82c7.)
2. `#print axioms` on the closing theorem → at most
   [propext, Classical.choice, Quot.sound].
3. Commit to the frame branch; reply with summary, commit shas, and the
   verbatim axiom output.

## Reporting honesty
If the target has no executable hole, or the statement is defective, SAY SO —
a stale-premise or defective-statement report is a valid completed attempt
(rehearsal-1 precedent: two honest refusals, both counted).
