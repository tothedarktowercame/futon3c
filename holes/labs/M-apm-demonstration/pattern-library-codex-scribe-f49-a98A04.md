# Pattern library additions — codex-scribe, frame f49 promote-solver (a98A04)

Created because the reviewed mathematics memory corpus returned no fit for the
mined rules (search receipt from `scripts/apm-search-memory.py`, frame f49,
2026-08-27): the corpus's measure-theory entries concern approximation in
integral norm, not a.e.-robust existence witnesses or series superpositions.
Ingested explicitly by `scripts/apm-ingest-coined-pattern-files.sh` (direct
`publish-file!` call, coiner `f49-codex-scribe`); ids below are pattern ids for
attachment.

## math-formalization-CA/superlevel-open-subinterval-for-ae-witness

Trigger: the goal asks for a point witness `∃ x ∈ s, M < g x` where `g` is only
known almost-everyly equal to a constructed `f` (`g =ᵐ[μ] f`), so a single
point where `f` is large proves nothing — the adversary can redefine `g` there.
Move: upgrade the height assertion to one that survives null-set surgery.
Prove the superlevel set of the constructed function *contains a nonempty open
interval* (`Ioo c d ⊆ {x | M < f x}`), not merely that it is nonempty; then the
superlevel set has strictly positive measure, so after restricting the a.e.
filter to that interval (`volume.restrict (Ioo c d)` with `ae_restrict_neBot`
from `volume (Ioo c d) ≠ 0` via `Real.volume_Ioo`), the conjunction
"`x ∈ Ioo c d` ∧ `g x = f x`" holds almost everywhere on a positive-measure set
and `Filter.Eventually.exists` extracts a concrete point that simultaneously
lies in the interval and satisfies the bound for `g` itself. Reason: an
existential over an a.e.-equivalence class is only well-defined on sets of
positive measure; "nonempty" upgrades to "positive measure" for free when the
witness set is open, and `restrict` + `Eventually.exists` is the Mathlib path
that cashes an a.e. statement into a point without choosing a representative.

## math-formalization-CA/ioc-integrand-to-indicator-interval-integral

Trigger: an integrand is defined by cases on an open interval (`if a < x ∧
x < b then φ x else 0`) and the goal is its Lebesgue integrability and exact
integral value; direct `integral` computation on the if-expression stalls.
Move: rewrite the function as `(Ioo a b).indicator φ` (`funext`, `if_pos` /
`if_neg` on membership), then pass through the indicator API
(`integrable_indicator_iff`, `integral_indicator`, both needing
`measurableSet_Ioo`) and the bridge
`intervalIntegrable_iff_integrableOn_Ioo_of_le` / `integral_Ioc_eq_integral_Ioo`
to land in `IntervalIntegrable` territory, where translated power-function
facts (`intervalIntegral.integrableOn_Ioo_rpow_iff`, `integral_rpow`,
`intervalIntegral.integral_comp_sub_right`) compute the exact value. Reason:
Mathlib's exact-integral lemmas for singular power integrands live on the
interval-integral side; the indicator rewrite is the toll bridge between a
Set-level definition and that API, and translation is moved by
`IntervalIntegrable.comp_sub_right` rather than by substituting bounds.

## math-formalization-CA/countable-superposition-via-encode-weights

Trigger: a construction needs a countable family of nonnegative terms indexed
by a countable type (e.g. `ℚ × ℚ`) summed with summable positive weights, and
the analysis API (`tsum`, a.e. summability, `Integrable.mono'`) is stated over
`ℕ`. Move: index the family by `Encodable.encode : ι → ℕ` with the weight
`(1/2) ^ encode i` (summable via `summable_geometric_two_encode`); linearise to
a sequence over ℕ by decoding (`Encodable.decode`) with a self-check
`encode i = n` so duplicates and out-of-range indices contribute zero — the
check makes `seq (encode i) = term i` definitional by `simp`. For
integrability of the pointwise series, first make the norm series integrable:
bound each `∫ ‖seq n‖ₑ` by the geometric weight, use `lintegral_tsum` /
`ae_lt_top'` with `AEMeasurable.ennreal_tsum` to get a.e. finiteness of
`∑' ‖seq n x‖₊`, then dominate with `Integrable.mono'` and
`norm_tsum_le_tsum_norm`. Reason: `∑'` over an arbitrary encodable type has no
integration API, but `2 ^ -encode` is summable by fiat of the encoding, and
absolute summability of norms converts every later pointwise step (a.e.
summability, termwise lower bounds) into one-line consequences.
