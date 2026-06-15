# DISCHARGE adapter report

Adapter: `scripts/adapters/discharge_adapter.py`
Single runtime fill: `scripts/fill.py` via `fill.fill(..., kind=fill.KIND_DISCHARGE)`

- mode: APPLY/live ArSE writes
- sorry registry: `/home/joe/code/futon2/resources/sorrys.edn`
- schema: `:schema-version 1`; v2-in-comments over :schema-version 1; optional :kind; :status distinguishes :open from discharged/addressed states
- selected sorries: 5
- filled/discharged sorries posted: 3
- ArSE manifest before entity_count: 87
- ArSE manifest after entity_count: 90

| sorry | status | filler | filled | thread-id | question evidence | answer evidence |
|---|---|---|---:|---|---|---|
| `sorry/handler-closure-route-rebinding` | `addressed` | resolution=Resolution candidates (a)+(c) executed — the pairing the sorry's own text na... | true | `ask-1781553303-87` | `arse-q-ask-1781553303-87` | `arse-a-ask-1781553303-87` |
| `sorry/mission-aif-head-not-served` | `addressed` | resolution=Addressed via bridge candidate (c): the WM head now READS the mission-AIF he... | true | `ask-1781553303-88` | `arse-q-ask-1781553303-88` | `arse-a-ask-1781553303-88` |
| `sorry/pattern-two-projections-of-one-quantity` | `addressed` | resolution=Pattern minted: futon3/library/structure/two-projections-of-one-quantity.fle... | true | `ask-1781553303-89` | `arse-q-ask-1781553303-89` | `arse-a-ask-1781553303-89` |
| `sorry/pattern-measure-never-target` | `open` | OPEN | false | `` | `` | `` |
| `sorry/pattern-typed-theme-liberates-the-solo` | `open` | OPEN | false | `` | `` | `` |

Verification:

- The adapter imports `fill.py`; it does not issue ArSE ask/answer calls directly.
- Open sorries route through `fill(..., filler=None)` and produce no ArSE witness.
- Live run checks `entity_count` delta equals the number of filled records.
- Live run checks posted threads are not left unanswered.
