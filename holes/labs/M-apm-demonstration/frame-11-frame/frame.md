# Frame: t01A05 (frame-11, first frame on the repaired write path)

## Target
`problems/t01A05/lean/Main.lean` in your checkout is 97 lines and carries
exactly one executable `sorry` — the last line of the file, at line 97 —
discharging the **whole** of `theorem apm_t01a05` (verified by elaboration at
the pin: exit 0, one ``declaration uses `sorry``` diagnostic, zero errors).

The frozen theorem is a conjunction of two clauses about `k`-sheeted covers and
orientations, encoded through top integral singular homology:

1. **A concrete case.** For every `k > 0`, `t01A05TorusCover k` is a
   `k`-sheeted cover of the torus, and every orientation `o` of the torus lifts
   to some `ot` satisfying `t01A05FundamentalClassRelation (k := k)`.
2. **The general case.** For all `n`, `k > 0`, and closed manifolds `X`, `Xtilde`
   (`T2Space`, `CompactSpace`, `ChartedSpace (T01A05Euclidean n)`,
   `IsManifold (T01A05Model n) ⊤`) with `π : C(Xtilde, X)` a `k`-sheeted cover:
   every orientation of `X` lifts to an orientation of `Xtilde` satisfying the
   same relation.

The scaffolding you have is definitional only — `T01A05Orientation` (a structure
over `T01A05TopHomology`), `t01A05FundamentalClass`,
`t01A05FundamentalClassRelation`, `t01A05IsKSheetedCover`, `t01A05TorusCover`,
and the `T01A05Euclidean` / `T01A05Model` / `T01A05Torus` abbreviations.

**TWO CORRECTIONS TO THE RECORDED PRIOR WORK. Read these before planning; both
were checked against the pinned file by ground control, and both are places
where the write-up and the artefact disagree.**

- `proof-outline.md` lists four lemmas under "Formal progress"
  (`t01A05_pullback_integral_eq`, `t01A05_preimage_intersectionNumber_eq`,
  `t01A05_preimageSubmanifold_compact_of_t2`,
  `t01A05_preimageSubmanifold_compact_of_isClosed`). **None of them exists in
  `Main.lean` — zero occurrences each.** The file contains no auxiliary lemma of
  any kind. Do not plan around reusing them.
- The same outline states its central obstruction as: *"the frozen hypotheses
  omit `T2Space X`"*, and concludes that the standard compact-source /
  Hausdorff-target argument "cannot be applied to the statement as given".
  **The frozen statement includes `[T2Space X]` (line 86) and
  `[T2Space Xtilde]` (line 89).** That obstruction, as written, does not hold
  against this file. Its other stated obstacles — no sphere or torus singular
  homology computation in Mathlib, and `Nat.card fiber = 0` not establishing
  finiteness — were not checked by ground control and may well stand.

## Contract
Close the `sorry` at line 97, **or reduce it to strictly less residual and say
precisely what remains.**

Because one `sorry` covers both clauses, **splitting it into named per-clause
`have`s — each with its own `sorry` and its own recorded search — is itself a
real result**, even if neither closes. The concrete torus clause and the general
covering clause have different difficulty and different missing API; collapsing
them into one hole hides which is which. Do not treat "I did not close the
theorem" as failure; treat leaving the residual un-localised as failure.

If a recorded obstruction turns out to be wrong — as two above already are —
**say so explicitly and move on**; that is a reportable result. A prior pass's
cost estimate is evidence, not a verdict: in the immediately preceding frame a
residual written off as needing "substantial new development" turned out to be
one eight-line lemma plus one short lemma.

No statement defect was found by the prior pass. If you find one, that is a
reportable result and not a failure to solve.

## Acceptance
- The frozen statement of `apm_t01a05` is unchanged.
- Any close is axiom-clean; the executable `sorry` count strictly decreases,
  **or** the single bundled `sorry` is replaced by named per-clause residuals
  that together imply the theorem.
- Whatever remains open is localised, with the nearest API and the searches that
  came back empty recorded beside it.
