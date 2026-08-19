# Frame: m99J06 (frame-13)

## Target
`problems/m99J06/lean/Main.lean` is 101 lines and carries exactly one executable
`sorry`, the last line (101), discharging the whole of `theorem apm_m99j06`.

The frozen theorem is a conjunction of TWO clauses, over a real Hilbert space `H`
with an `apm_m99J06_H01Model` and `f ∈ L²`:

1. **Weak ⟺ strong.** For every `u`, `weakSolution M f u ↔ strongSolution M f u`.
2. **The Galerkin clause.** For any increasing chain of finite-dimensional
   subspaces `V n` with dense union: each `V n` contains a UNIQUE solution of the
   Galerkin equation, and any sequence of such solutions is bounded by
   `‖f‖_{L²}` — with the convergence statement following.

## Contract
Close the `sorry`, **or reduce it to strictly less residual and say precisely
what remains.**

The two clauses are independent in difficulty: clause 1 is an equivalence that
usually turns on integration by parts and a density argument; clause 2 is a
Lax–Milgram / finite-dimensional existence argument plus a uniform bound.
**Named per-clause residuals are an accepted result** even if neither closes.

If the library lacks a notion you need, that is **not** a reason to stop: name it
as a construction target and build it if budget allows. See your card. Note the
Sobolev/`H¹₀` setting here overlaps `ConstructionTargets`; check what is already
built before defining anything new.

Any definition you introduce needs a proof it takes a non-trivial value in a
concrete case.

No statement defect was found by a prior pass. If you find one, that is a
reportable result, not a failure.

## Acceptance
- The frozen statement of `apm_m99j06` is unchanged.
- Any close is axiom-clean; the `sorry` count strictly decreases, **or** the
  bundled `sorry` becomes named per-clause residuals.
- Whatever remains open is localised, with nearest API and empty searches
  recorded beside it.
