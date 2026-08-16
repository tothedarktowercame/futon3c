# Frame: a01A06 (rehearsal-1)

## Target
`problems/a01A06/lean/Main.lean` in your checkout carries exactly one `sorry`:
the core analytical estimate — exponential decay from the Orlicz bound
(`∫ gf < A` whenever `∫ e^f ≤ 1`) via test-function optimization. The
surrounding structure (hypothesis, distribution-function conclusion, L²
conclusion) is already stated with the proof strategy documented in the file
header and `proof-outline.md`.

## Contract
Work ONLY in your checkout; commit on the frame branch only. The bundle's
`proof-outline.md` and `informal-solution.md` are yours to use. Do not touch
files outside `problems/a01A06/`.

## Acceptance
`lake build` clean from the checkout root; zero `sorry` in Main.lean;
`#print axioms` on the closing theorem shows at most
[propext, Classical.choice, Quot.sound].
