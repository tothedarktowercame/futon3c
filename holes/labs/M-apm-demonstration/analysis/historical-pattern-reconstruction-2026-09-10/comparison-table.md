# Existing-pattern / memory comparison

## Searches actually performed (bounded; a miss is not exhaustive absence)

Canonical library (futon3 `a376e404`, families math-informal,
math-informal-CA, math-formalization, math-formalization-CA, math-strategy,
proof-search — directory listing of every file; plus keyword greps over
those trees for `retract`, `sard`, `summable`, `fundamental group`,
`critical value`, `null`, `measure zero`, `absolute contin`, `dominated`):

- math-informal: 24 files listed; relevant near-misses only:
  `local-to-global`, `construct-auxiliary-object`,
  `transport-across-isomorphism`, `reduce-to-known-result`.
- math-strategy: `structural-obstruction-as-theorem`, `exhaustion-as-theorem`,
  `isolate-computational-kernel-before-transport`.
- math-formalization (31 listed): nothing on retractions, Sard, or
  series-of-measures; nearest is
  `lift-prove-upstairs-reflect-by-injectivity` (about lifting through
  injections, not functorial injectivity from a left inverse).
- No canonical file in any math family mentions retraction-injectivity,
  Sard/critical values, or termwise interchange under a summable dominator.

Retained memory population (prior census `frozen.json`, 364 memories, and
`result.json`, `census.md` in pattern-first-development-2026-09-10):

- `sard` 0 hits; `regular value` 0; `critical value` 0.
- `retract` 12 hits — all inspected via a structured walk: they concern the
  b94J01 finite-commutative-group cyclic-factor split (memories
  `e-apm-promotion-2395e8e…`, `e-apm-promotion-5c7349ba…`, pattern
  `math-formalization/split-a-maximal-cyclic-factor-by-character-extension`).
  Unrelated to topological retracts.
- `absolute contin` 3 hits; `extend-local-absolute-continuity…` **0 hits**:
  the a97J04 memory (evidence id
  `e-codexpilot-extend-local-absolute-continuity-to-an-endpoint-via-the-derivative-primitive`)
  exists in the live store (read in full for this packet) but is absent from
  the 364-memory retained snapshot and carries no pattern-id attachment.
- Futon3c evidence store: `memory_search` by tags {t01A03, t91J02, t92J02,
  t93A04, t92J07, t94J02} and by {t01A03}, {t93A04} — empty; no recorded
  evidence entries for these solves in this store.
- Prior drafts compared (not republished): all-topology census drafts T2/T3
  (`../pattern-construction-2026-09-10/topology-patterns.md`, apm-lean pin
  `f053ab59…` there) and the two a93A01 candidate abstractions in
  `../pattern-construction-2026-09-10/prelim-development.md`.

## Classification per finding

| Finding | Existing coverage | Classification |
|---|---|---|
| Left-inverse ⇒ functorial-invariant injectivity (pair A) | `math-strategy/structural-obstruction-as-theorem` is a parent (name a failure mode of a method, does not supply the construction); `transport-across-isomorphism` assumes an isomorphism in hand. Prior census draft T3 already drafted this (single packet, topology-only). No canonical or retained-memory description found. | **genuinely candidate new method within inspected scope** (two fresh problem witnesses here; census T3 corroborates with t01A03/t91J02 plus boundary case t00A04) |
| Null bad-value set vs positive-measure target (pair B) | No Sard/critical-value/measure-zero pattern in any math family or in the 364-memory snapshot. Generic parents `exhaustion-as-theorem` / `local-to-global` do not state the decisive countability + positive-measure-test-set conditions. Prior census draft T2 drafted it. | **genuinely candidate new method within inspected scope** (t93A04 + t92J07 here; census T2 adds t94J05/t95J04) |
| Summable-dominator interchange + identification (pair C) | A reviewed memory exists for the exact a97J04 route (`e-codexpilot-extend-local-absolute-continuity…`, approved 2026-07-30) but is single-example, has no pattern-id attachment, and is missing from the retained snapshot corpus. `local-to-global` and `construct-auxiliary-object` are parents. | **discoverability / example-link gap** plus **existing method needs clearer conditions** (when is the dominator summable? a94A02's B n construction vs a97J04's BV-integrability are different sufficient conditions) |

No finding was classified "reusable as written": each intermediate
construction is absent from, or strictly under-specified by, every canonical
description examined in the bounded scope above.

## Renaming check

Draft T2/T3 of the prior census are acknowledged as prior work; the drafts
below restate them with the commission's required fields (definitions, exact
conditions, failure contrast, overlap) plus the new pair-B/pair-C witness
detail and the authorship/repair provenance. Pair C is new relative to the
prior census (which was topology-only).
