# E9 scribe pass: a96J04

Operator-review drafts from the single zai-1 session for APM problem
`a96J04`. Nothing in this directory has been written to the memory store.

## Corpus boundary and method

The pass applies the S1-pilot comparison method to three views of the same
work:

1. the committed `Main.lean`, `status.json`, and `proof-outline.md` at
   apm-lean commit `1d014930e85e1b9a8a21dfc197cad7876baf468a`;
2. the 100 zai-1 `turn-round` entries returned by the read-only evidence query
   `author=zai-1&since=2026-08-03T13:57:00Z&before=2026-08-03T14:17:00Z&limit=500`;
3. the runner's final summary, evidence
   `e-afcdc7ee-2507-473b-95ac-8e74c2a7de67`.

The final summary was independently read from the durable job record
`invoke-1785765468643-936-8155fcf8`; its result text and artifact reference
`1d01493` agree with the final turn-round evidence.

The returned evidence range is
`2026-08-03T13:57:58.741358867Z` through
`2026-08-03T14:15:56.674847194Z`, all authored by `zai-1`, session
`zai-bec940299024470eb815607f8b13b650`. The final artifact compiles with exit
0 and one `sorry` in `ac_monotone_maps_null_to_null`.

## Drafts

| File | Requested level | Confidence | Review state |
| --- | --- | --- | --- |
| [open-set-interval-decomposition-gap.md](open-set-interval-decomposition-gap.md) | lemma-location | single instance (`n=1`) | pending Joe |
| [monotone-image-interval-containment.md](monotone-image-interval-containment.md) | tactic | single instance (`n=1`) | pending Joe |
| [null-image-via-open-cover-and-finite-ac.md](null-image-via-open-cover-and-finite-ac.md) | strategy | single instance (`n=1`) | pending Joe |
| [localize-an-observed-blocker-at-one-sorry.md](localize-an-observed-blocker-at-one-sorry.md) | process | single instance (`n=1`) | pending Joe |

## Near-duplicate findings

The read-only near-duplicate scan covered all 527 current `type=memory`
entries.

- `e-c924ebba-3fff-4a01-b281-d3e90a0a09bd`,
  `checkpoint-a-compiling-lemma-layer-before-final-assembly`, is close to the
  process draft. It concerns durable compiling checkpoints before cap-risk
  assembly. The new draft is narrower: after an observed dependency gap, put
  the only `sorry` at that exact bridge and record the proven prefix, searched
  API, next proof steps, and uninvestigated alternatives.
- `e-codexpilot-treat-not-in-mathlib-comments-as-revision-scoped-search-claims`
  is also close. It prevents timeless library-absence claims. The new process
  draft incorporates that scope discipline but adds precise proof-boundary
  localization. The lemma-location draft likewise says only that the direct
  packaged lemma was not found in this session and checked revision.
- `e-codexpilot-refuse-sorry-relocation-when-no-axiom-clean-partial-exists`
  is related but not a duplicate: it rejects moving `sorryAx` without a proved
  reusable leaf, whereas a96J04 did produce two compiling reusable helpers.

No current memory matched the monotone interval-image helper or the complete
open-cover/finite-AC proof architecture. The four files remain drafts awaiting
operator judgment; this pass performed no evidence or memory writes.
