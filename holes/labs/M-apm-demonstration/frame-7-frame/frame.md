# Frame: a98A01 (frame-7, first fresh-seat conducted frame)

## Target
`problems/a98A01/lean/Main.lean` in your checkout carries exactly one
executable `sorry` (verified at the pin a92ffb6c: one hole, closing theorem
`apm_a98a01`). The dilation family `F_k(x) = k^α f(k|x|)` on
`EuclideanSpace ℝ (Fin n)`, with `f ~ t` near 0 and `f ~ t⁻⁴` at infinity:
prove the three equivalences — uniform convergence to 0 ⟺ `α < 0`;
almost-everywhere convergence ⟺ `α < 4`; `L¹` convergence ⟺ `n ≤ 3 ∧ α < n`.
The statement was verified defect-free at the pin (W.37); the file's own
comments sketch the intended route (sup scale-invariance; tail asymptotics;
polar-coordinate `L¹` computation).

## Contract
Work ONLY in your checkout (path in the ENVIRONMENT block of your dispatch);
commit ONLY to the frame branch. Do not touch files outside
`problems/a98A01/`. Verify the hole state against YOUR BRANCH before working
— dispatch text can be stale; the branch is the truth.

## Acceptance
1. `lake env lean problems/a98A01/lean/Main.lean` → exit 0 with ZERO
   "declaration uses `sorry`" warnings (NOT root `lake build` — vacuous for
   problem files).
2. `#print axioms apm_a98a01` → at most [propext, Classical.choice,
   Quot.sound].
3. Commit to the frame branch; reply with summary, commit shas, and the
   verbatim axiom output.

## Reporting honesty
A defective-statement or stale-premise report is a valid completed attempt.
Commit partial lemmas if the final theorem remains open; report the exact
residual in Lean terms — your reported residual is authoritative for the
next dispatch.
