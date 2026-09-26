# Frame: a01J06 (frame-9, first frame with an Analyst seat and the memory cascade)

## Target
`problems/a01J06/lean/Main.lean` in your checkout carries exactly one
executable `sorry` (line 128, closing theorem `apm_a01j06`; verified by
elaboration at the pin, exit 0 with a single `declaration uses \`sorry\``
diagnostic). The theorem: for `f` entire, non-zero, of exponential type
(`apm_a01J06_ExpTypeBound f A B`), with `ω` enumerating its zeros
(`apm_a01J06_EnumeratesZeros f ω`), the series
`∑ n, (1 + ‖ω n‖)^(-α)` is summable for every `α > 1`.

The residual is already isolated IN-FILE into two exact contracts, neither
introduced as a declaration because neither was proved:

- `apm_a01J06_linear_zero_count` — Jensen plus finite divisor aggregation:
  `∃ C ≥ 0, ∀ R ≥ 1, Finite {n // ‖ω n‖ ≤ R} ∧ card {n // ‖ω n‖ ≤ R} ≤ C * R`.
- `apm_a01J06_summable_of_linear_count` — the dyadic-shell geometric
  comparison taking that linear count to summability for `α > 1`.

Their composition closes the frozen theorem. Scaffolding already proved
in-file: `apm_a01J06_finite_preimage_of_enumeratesZeros`,
`apm_a01J06_card_preimage_eq_finsum_analyticOrder` (the exact identity
`card {n | ω n ∈ s} = ∑ z ∈ s, analyticOrderNatAt f z`), and
`apm_a01J06_finite_closedDisk_indices`.

## Contract
Close the `sorry` at line 128, or reduce it to strictly less residual and
say precisely what remains. The prior pass recorded its searches
(`LEMMA-INDEX.md`, all `ConstructionTargets/` modules, Mathlib's
Jensen/log-counting, locally finite divisor, set-cardinality, finite-union,
geometric-series and summability sources) and found the ingredients but no
declaration proving either contract in this encoding. **Do not repeat that
search blindly** — either use what it found, or record why its conclusion
was wrong.

No statement defect was found by the prior pass. If you find one, that is a
reportable result and not a failure to solve.

## Acceptance
- The frozen statement of `apm_a01j06` is unchanged.
- Any close is axiom-clean; `sorry` count strictly decreases or the residual
  is named exactly.
- Whatever remains open is localised at ONE `sorry` with the bridge, the
  nearest API and the searches that came back empty recorded beside it.
