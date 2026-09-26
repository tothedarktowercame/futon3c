# Frame: m93J02 (frame-10, second frame of the Analyst's N=2 tenure)

## Target
`problems/m93J02/lean/Main.lean` in your checkout carries exactly one
executable `sorry` (line 213; verified by elaboration at the pin, exit 0 with
a single ``declaration uses `sorry``` diagnostic).

The frozen theorem `apm_m93j02`: for `U ⊆ (Fin n → ℝ)` compact, connected and
nontrivial, and `α ∈ (0,1)`, an eight-fold conjunction relating the LOCAL
Hölder seminorm (a nested infinitesimal `ENNReal` `limsup`) to the GLOBAL one.

**Read this before planning: the residual here is not one bridge.** Unlike a
single isolated contract, the `sorry` at line 213 discharges `hremaining`,
which bundles **six of the eight conjuncts at once**:

1. local seminorm finite → global seminorm finite (compactness / cluster point);
2. ¬∃ finite `K` with `global ≤ K * local` — needs an explicit nonconstant
   witness on an *arbitrary* compact connected nontrivial `U`;
3. `isNormOn` for `localNorm`;   4. ¬`isCompleteOn` for `localNorm`;
5. `isNormOn` for `globalNorm`;  6. `isCompleteOn` for `globalNorm`.

Two conjuncts are already proved in-file and are NOT your problem: `hle`
(local ≤ global) and `hglobal_local` (global finite → local finite). Also
already compiled: the whole zero-function battery (`hquotient_zero`,
`hglobal_zero`, `hsup_zero`, `hlocal_zero`, `hlocalNorm_zero`,
`hglobalNorm_zero`), `apm_m93J02_eq_zero_of_supNorm_eq_zero`, and
`apm_m93J02_eq_zero_of_toReal_add_eq_zero` — which together reduce
definiteness for BOTH candidate norms to finiteness of their encoded sums on
the Hölder carrier.

## Contract
Close the `sorry` at line 213 **or reduce it to strictly less residual and say
precisely what remains.**

Because the one `sorry` covers six independent conjuncts, **splitting it into
named per-conjunct `have`s — each with its own `sorry` and its own recorded
search — is itself a real result**, even if none of the six closes. It
converts one opaque hole into six localised ones. Do not treat "I did not
close the theorem" as failure; treat leaving the residual un-localised as
failure.

Three prior passes recorded their searches in the file's own comment blocks:
Mathlib's `HolderOnWith`, `ContDiffPointwiseHolderAt`, compact-image /
connected-image, `ENNReal` `iSup`/`toReal`, and filter `limsup` APIs. Their
conclusion: the packaged Hölder predicates use a **fixed global constant** and
do not convert to or from this frozen nested infinitesimal `ENNReal` `limsup`.
They also noted a route for arbitrary compact connected `U` — push through
`x ↦ dist x x₀`, whose compact connected image contains an interval — but
found that transporting the Lipschitz-zero local seminorm and the incomplete
sequence through that map still needs substantial new `limsup` development.
**Do not repeat that search blindly** — either use what it found, or record
why its conclusion was wrong.

No statement defect was found by the prior passes. If you find one, that is a
reportable result and not a failure to solve.

## Acceptance
- The frozen statement of `apm_m93j02` is unchanged.
- Any close is axiom-clean; the executable `sorry` count strictly decreases,
  **or** the single bundled `sorry` is replaced by named per-conjunct
  residuals that together imply `hremaining`.
- Whatever remains open is localised, with the nearest API and the searches
  that came back empty recorded beside it.
