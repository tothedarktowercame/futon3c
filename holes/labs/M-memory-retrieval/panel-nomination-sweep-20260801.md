# E2 full closed-problem panel nomination sweep — 2026-08-01

## Verdict

The full sweep yields **one strong pair**, not the requested 6–10. This is an honest
small-panel result: 50 distinct used memory/problem pairs across the 137 hole-free
canonical problems were screened, and only one has a complete, dual-time-valid
failure → reviewed arrival → use → closure chain while also surviving repository-search
cancellation.

| problem | memory | recommendation | reason |
|---|---|---|---|
| a95j01 | `e-codexpilot-analytic-order-at-least-two-implies-local-noninjectivity` | nominate-strong | All four criteria pass; exact missing local-degree bridge was absent from the base tree and Mathlib. |

The pair's `:score-varies?` is **true**: the memory supplied the exact local-degree
bridge at which the earlier memory-free attempt stopped, and that bridge was absent
from both the base problem tree and pinned Mathlib under direct and close-paraphrase
queries.

## Fully worked timeline 1 — nominated a95J01 pair

1. **Memory-free failure.** `invoke-1785393010025-303-f9f19596` ran at
   `2026-07-30T06:30:10Z` and committed `51b6bc00dcc1c6cfeaecc79cdcf0fc8d2b720f03`. Its report explicitly says
   no memories were supplied or surfaced, and stops at the leaf
   `IsOpen U → DifferentiableOn ℂ g U → InjOn g U → z ∈ U → deriv g z ≠ 0`.
2. **Historical attachment.** Frozen graph export captured at `2026-07-30T19:00:54Z`
   records review evidence
   `e-review-codex-5-analytic-order-at-least-two-implies-local-noninjectivity`,
   reviewed at `2026-07-30T17:26:30.049396801Z`. This predates both memory-bearing
   runs, so the arrival is valid-time safe rather than inferred from current graph state.
3. **Arrival and attributed use.** `invoke-1785519091300-552-1f8d8a48` at
   `2026-07-31T17:31:31Z` reports `e-codexpilot-analytic-order-at-least-two-implies-local-noninjectivity` used to identify the exact
   local-degree obstruction. `invoke-1785521408563-571-76fa2f7b` then reports the same memory used when
   importing the completed `ConstructionTargets.UnivalentDeriv` bridge.
4. **Construction and closure.** `953a06fd734e3d0bab57c8776bdffa86b2498d6b`
   proves the nonvanishing-derivative construction target. `invoke-1785582866229-633-5750d1ef` closes
   a95J01 at `016bf0fb5b7f196abb61770d0d6e953e97305036` with zero sorries.
5. **Rerun revisions.** Base `51b6bc00dcc1c6cfeaecc79cdcf0fc8d2b720f03`; observed closure `016bf0fb5b7f196abb61770d0d6e953e97305036`.

Reachability commands (all returned no matches):

```text
$ git grep -n -i -E 'analyticOrderAt|not_injOn_nhds|deriv_ne_zero_of_injOn' 51b6bc00dcc1c6cfeaecc79cdcf0fc8d2b720f03 -- problems/a95J01
$ git grep -n -i -E 'analyticOrderAt.*not_inj|not_inj.*analyticOrderAt|deriv_ne_zero_of_inj|analytic.*local.*noninject|univalent.*deriv.*nonzero' 51b6bc00dcc1c6cfeaecc79cdcf0fc8d2b720f03 -- ':!problems/a95J01'
$ rg -n -i 'analyticOrderAt.*not_inj|not_inj.*analyticOrderAt|deriv_ne_zero_of_inj|analytic.*local.*noninject|univalent.*deriv.*nonzero' .lake/packages/mathlib/Mathlib
```

## Fully worked timeline 2 — rejected a96A04 cancellation

This case has a real failure and later memory use, but fails criterion (a).

1. `invoke-1785098957336-167-75dd4cab` committed `f614856` with three
   remaining sorries after proving only Gaussian normalization.
2. `invoke-1785470457961-468-ad547147` later reported using
   `e-codexpilot-derive-integrable-from-nonzero-bochner-integral` and
   `e-codexpilot-distinguish-ContDiff-top-analytic-from-ContDiff-infinity-smooth`.
3. Cancellation is decisive: pinned Mathlib contains the exact declaration
   `integrable_of_integral_eq_one` in
   `Mathlib/MeasureTheory/Integral/Bochner/Basic.lean`, and
   `Mathlib/Analysis/Calculus/ContDiff/Defs.lean` explicitly documents that
   `ContDiff ... ⊤` is analytic while `ContDiff ... ∞` is smooth. The operative
   content is therefore repository-searchable; this is not a strong isolation pair.

The reproducer verifies those cancellation hits with:

```text
$ rg -n 'theorem integrable_of_integral_eq_one' .lake/packages/mathlib/Mathlib/MeasureTheory/Integral/Bochner/Basic.lean
$ rg -n 'contDiffWithinAt_omega_iff_analyticWithinAt|ContDiffWithinAt.*∞' .lake/packages/mathlib/Mathlib/Analysis/Calculus/ContDiff/Defs.lean
```

## Incidental-arm candidates for the nominated problem

The arrival/use receipts record the following surfaced-and-ignored memories on a95J01:

- `e-codexpilot-combine-Schwarz-rigidity-with-power-series-uniqueness`
- `e-codexpilot-derive-the-unit-ball-volume-recursion-through-Wallis-parity-formulas`
- `e-codexpilot-package-closed-ball-holomorphicity-as-DiffContOnCl-for-Cauchy-estimates`
- `e-codexpilot-specialize-affine-dslope-equality-at-the-disk-center`

`e-88a1af39-53d5-4ac8-a01b-04137a559619` is not placed in the incidental arm:
the runner marked it used, but only as confirmation of the already-present Schwarz API,
so it is excluded from LB nomination rather than relabelled IN.

## Honest gaps and interpretation

- Most closures do not have all four temporal events. Some have use without a
  memory-free failed baseline; some statement-repair jobs cannot supply a same-target
  baseline; others used memories whose content is already in the base tree or Mathlib.
- The frozen graph export is sufficient for the nominated pair because its review time
  predates arrival. Current attachment state was not projected backward.
- The full census is a use census, not an LB/IN adjudication source. The EDN records each
  pair's screen verdict; it does not import labels from
  `load-bearing-candidates-20260731.jsonl`.
- A one-pair panel is too fragile for the intended E2 comparison. More strong pairs need
  a fresh preregistered window that records memory-free baselines and reviewed attachment
  times before dispatch, rather than retrospective label manufacture.

## Reproduction and frozen inputs

Run:

```text
python3 holes/labs/M-memory-retrieval/derive-panel-nomination-sweep-20260801.py
sha256sum holes/labs/M-memory-retrieval/panel-nomination-sweep-20260801.edn \
  holes/labs/M-memory-retrieval/panel-nomination-sweep-20260801.md
```

Inputs: receipts `0cc527e23c3678a4cc7d8053d6636d0cde556dab15fcc3ce69bedf0b659820b3`; graph `e8b683b6f825dca0b75af772c236ac3f8d086d1b233d85d3ae272d98ed4d6565`; queue
`d6b61d04a90db54fb7e6f48bb38c414318ff0de6abfdfb57d6b9081107a5a3a7`; pinned apm-lean `82f98e81258a0e5ac49b7bfc74f8e35d4c9b7964`. The script reads the frozen
local ledger and performs no network calls or store writes.
