# compose adapter report

Runner: `scripts/adapters/compose_adapter.py`

Input shape:

- BV comb typing path: `/home/joe/code/futon6/holes/bv-comb-typing.edn`
- Shape used: `:typed-missions` entries with `:shape :linear-chain`, `:bv-type {:bv/seq [...]}`, and `:composes-typing` rows of `[:from :to :typed-monotone|:gap-no-boundary-type]`.
- Mapping: each `:composes` row is a COMPOSE hole. `:typed-monotone` rows get a `sub-process:<mission>:<from>-><to>` filler with matching `bv/seq-boundary`; gap rows are recorded OPEN with `filler=None`.

- ArSE manifest before entity_count: 83
- ArSE manifest after entity_count: 87
- selected compose holes: 5
- filled compose holes posted: 4
- total ArSE writes: 8 (4 asks + 4 answers)

| hole | verdict | filler | thread-id | question evidence | answer evidence |
|---|---|---|---|---|---|
| `compose:M-futon1a-rebuild::ckpt-03->:ckpt-04` | `:typed-monotone` | `sub-process:M-futon1a-rebuild::ckpt-03->:ckpt-04` | `ask-1781553276-83` | `arse-q-ask-1781553276-83` | `arse-a-ask-1781553276-83` |
| `compose:M-operational-readiness::ckpt-00->:ckpt-01` | `:typed-monotone` | `sub-process:M-operational-readiness::ckpt-00->:ckpt-01` | `ask-1781553276-84` | `arse-q-ask-1781553276-84` | `arse-a-ask-1781553276-84` |
| `compose:M-operational-readiness::ckpt-01->:ckpt-02` | `:typed-monotone` | `sub-process:M-operational-readiness::ckpt-01->:ckpt-02` | `ask-1781553276-85` | `arse-q-ask-1781553276-85` | `arse-a-ask-1781553276-85` |
| `compose:M-operational-readiness::ckpt-02->:ckpt-03` | `:typed-monotone` | `sub-process:M-operational-readiness::ckpt-02->:ckpt-03` | `ask-1781553276-86` | `arse-q-ask-1781553276-86` | `arse-a-ask-1781553276-86` |
| `compose:M-first-flights::ckpt-00->:ckpt-01` | `:gap-no-boundary-type` | OPEN | - | - | - |

Verification:

- Imports `scripts/fill.py` and uses `fill.fill` with `kind=fill.KIND_COMPOSE`.
- Open COMPOSE holes return `FillRecord(filled=False)` and do not write ArSE, preserving I2 without a fake witness.
- Filled COMPOSE holes enforce the adapter boundary string as `filler_type` before discharge.
