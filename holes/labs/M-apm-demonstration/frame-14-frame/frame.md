# Frame: m93J06 (frame-14)

## Target
`problems/m93J06/lean/Main.lean` is 265 lines and carries exactly one executable
`sorry`, the last line (265), discharging the whole of `theorem apm_m93j06`.

The frozen theorem is a conjunction of **FIVE OR MORE** clauses about ODE flows:

1. **Picard–Lindelöf.** Continuous `f` with a global Lipschitz constant gives a
   unique solution of the Cauchy problem for every initial value.
2. **Flow smoothness.** `C¹` right-hand side and `IsFlow f φ` gives an `ε` on
   which each `φ t` is `C¹`.
3. **Global flow.** Linear growth `|f t y| ≤ C|y|` plus Lipschitz gives a flow.
4. **A NEGATIVE clause.** `¬ ∀ α ∈ (0,1)`, Hölder-`α` continuity implies
   uniqueness. This asks for a COUNTEREXAMPLE — the standard `y' = y^α`
   non-uniqueness — and is a different kind of obligation from the other four.
5. Further flow/bound clauses after these.

## Contract
Close the `sorry`, **or reduce it to strictly less residual and say precisely
what remains.**

This is the most bundled residual in the batch: one `sorry` for five-plus clauses
of genuinely different character, including one NEGATION that requires exhibiting
a counterexample rather than proving a universal. **Splitting into named
per-clause `have`s is strongly preferred and counts as a real result** even if
none closes. Do not treat "I did not close the theorem" as failure; treat
leaving the residual un-localised as failure.

Clause 4 deserves care. A negation is proved by construction, and the immediately
preceding frames show why this matters: one frame proved a frozen clause FALSE by
exhibiting a two-point counterexample, and that was its most valuable output. If
any clause here turns out to be false as stated, say so — that is a statement
defect and a reportable result.

If the library lacks a notion, name it as a construction target and build it if
budget allows. See your card. Any definition you introduce needs a proof it takes
a non-trivial value in a concrete case.

## Acceptance
- The frozen statement of `apm_m93j06` is unchanged.
- Any close is axiom-clean; the `sorry` count strictly decreases, **or** the
  bundled `sorry` becomes named per-clause residuals that together imply it.
- Whatever remains open is localised, with nearest API and empty searches
  recorded beside it.
