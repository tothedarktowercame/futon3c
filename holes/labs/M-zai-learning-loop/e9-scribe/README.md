# E9 scribe pass: a96J04

Reviewed memories from the single zai-1 session for APM problem `a96J04`, plus
the separately evidenced closer addendum. The five entries were promoted on
2026-08-03 after claude-10's owner review/commission; their pattern attachments
remain proposed for a separately authored attachment review.

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

| File | Requested level | Confidence | Memory ID |
| --- | --- | --- | --- |
| [open-set-interval-decomposition-gap.md](open-set-interval-decomposition-gap.md) | lemma-location | single instance (`n=1`) | `e-e9-a96j04-open-set-interval-decomposition-gap` |
| [monotone-image-interval-containment.md](monotone-image-interval-containment.md) | tactic | single instance (`n=1`) | `e-e9-a96j04-monotone-image-interval-containment` |
| [null-image-via-open-cover-and-finite-ac.md](null-image-via-open-cover-and-finite-ac.md) | strategy | single instance (`n=1`) | `e-e9-a96j04-null-image-via-open-cover-and-finite-ac` |
| [localize-an-observed-blocker-at-one-sorry.md](localize-an-observed-blocker-at-one-sorry.md) | process | single instance (`n=1`) | `e-e9-a96j04-localize-an-observed-blocker-at-one-sorry` |
| [closer-open-component-decomposition.md](closer-open-component-decomposition.md) | lemma-location | single instance (`n=1`) | `e-e9-a96j04-closer-open-component-decomposition` |

The first four evidence bodies contain the reviewed Markdown byte-for-byte.
The fifth cites closer commit
`33575db8ea2271641c75b1eda91c78ab72606150` and job
`invoke-1785770371655-945-fa70bee1`.

## Proposed attachment table

| Memory | Proposed pattern | Justification |
| --- | --- | --- |
| `open-set-interval-decomposition-gap` | `math/missing-dependency-protocol` | A bounded Mathlib search exposed a missing theorem/API bridge and an explicit local-construction frontier. |
| `monotone-image-interval-containment` | `math/measure-integration-api` | The compiled helper is an interval-image step used inside a Lebesgue-null image argument. |
| `null-image-via-open-cover-and-finite-ac` | `math/measure-integration-api` | The record is explicitly a measure/integration proof architecture using null covers and finite absolute continuity. |
| `localize-an-observed-blocker-at-one-sorry` | `math/missing-dependency-protocol` | The process rule governs how to preserve a compiling frontier when a searched dependency is missing. |
| `closer-open-component-decomposition` | `math/missing-dependency-protocol` | The closer turns the same missing packaged dependency into a concrete local construction recipe. |

All five `memory/assert` edges read back with `:attachment-status :proposed`.
The exact cross-author review commands are in
[APPROVALS.md](APPROVALS.md).

## Tag verification

The promotion report records memory-search-equivalent AND queries over the
durable evidence tags. Every query returned its expected entry or entries:

| Tags | Returned new memories |
| --- | --- |
| `[open-set interval-decomposition Lebesgue null-set]` | decomposition gap; closer addendum |
| `[monotone interval-image MonotoneOn Icc]` | monotone interval containment |
| `[Lebesgue null-set absolute-continuity finite-intervals]` | open-cover/finite-AC strategy |
| `[Lean sorry proof-boundary dependency-blocker]` | honest-boundary process |

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

No pre-existing memory matched the monotone interval-image helper or the
complete open-cover/finite-AC proof architecture. The near-duplicate findings
above therefore remain unchanged by promotion.
