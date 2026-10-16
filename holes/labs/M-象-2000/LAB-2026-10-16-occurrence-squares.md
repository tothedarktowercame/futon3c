# LAB 2026-10-16 — a 象 citation as a blend square; four citations as a cascade

Joe (turn-zZ52wx, 2026-10-16): a cascade has the same shape as a pattern,
applied recursively; codex-68 is working with cascades on the War Machine, so
show the 象/m40 side can produce the same data structures.

## Rule applied

CASCADE-SPEC-v3 §2 as settled 2026-10-16 (futon2 e5c3a4604): a pattern's
square is its four written parts under IF = I1, HOWEVER = I2, BECAUSE = G,
THEN = B; an occurrence is the four maps (situation element + source per
part); an unmatched part voids the citation; a square whose texts are not the
pattern's parts is refused at the producer.

## What was run

`futon3c/scripts/occurrence_square.py` (new). `parts` prints a flexiarg's
four parts; `square` takes an occurrences file (turn, pattern, four maps)
and emits `:wm/three-halves-square-v1` records: object text = the pattern's
part, pinned to the library file's sha256; object theory = the one
situation element the map names; a1/a2/b1/b2 from G and the inputs to B.
Occurrences sharing a situation element (same name, any role) are glued
there; one component = cascade, more = bag (spec §3.1).

Input: `data/occurrences/occurrences-2026-10-16.json`, four citations:
the 2026-10-01 象 citation of `ukrns/publication-cadence` on turn-w3xIu6
(maps from the spec's c17 reading), and the three patterns retrieval
attached to turn-zZ52wx, mapped to this turn's situation (the WM square
producer, the two defaulted templates, the spec rules, the codex-2 packet).

Output: `data/occurrences/occurrences-2026-10-16.edn`.

| result | value |
|---|---|
| occurrences emitted | 4 |
| void | 0 |
| glued pairs | 2 |
| components | 2 |
| verdict | bag |

Glued at "the parts-check refusal at the producer": cite-the-source-bytes/B
with pattern-becomes-peripheral/B. Glued at "CASCADE-SPEC-v3 §2 five rules":
lift-when-three-align/G with pattern-becomes-peripheral/I1. The
publication-cadence occurrence shares nothing with the other three, so the
four are a bag; the three from this turn alone are one connected cascade.

`data/occurrences/void-example-2026-10-16.json`: publication-cadence cited
where HOWEVER's costs name nothing in the situation → `void
HOWEVER:no-situation-element`, no square emitted.

## Cross-side check

All four emitted squares pass the War Machine's own validator,
`futon2.aif.three-halves-square/validate` (futon2 main at e5c3a4604), run
once from a fresh process (`~/notes/validate-squares-2026-10-16.clj`, log
`~/notes/validate-squares-2026-10-16.log`): 4 × `:valid`. Their texts are
the pattern's parts, so they also meet §2 rule 5, which the WM's two
templates on the 2026-10-14 trace do not (packet to codex-2, job
invoke-1792193935835-15912-5c902ffb).

## Limits

- The validator's commutation and coverage checks are trivial on singleton
  theories; what it certifies here is schema, pinning and map direction.
- Gluing is by equal element name. The spec's gluing object is an object of
  some occurrence; name equality is the 象-side stand-in until elements have
  ids (m40 entities are the candidate).
- The recursive step is not shown: a connected cascade restated as one
  square (NOTE-outer-cascade §2a) needs a rule for which occurrence's parts
  become the outer IF/HOWEVER/BECAUSE/THEN. Next, once codex-2's check lands.
- The four maps were authored by me for this turn; nothing yet writes them
  at citation time. That is the producer change on the 象 side.
