# ground adapter report

Runner: `scripts/adapters/ground_adapter.py`

- paper: `0704.0502`
- store: `/home/joe/code/futon6/data/substrate-2a/hx/0704.0502.edn`
- ArSE manifest before entity_count: 79
- ArSE manifest after entity_count: 83
- filled holes posted: 4
- open holes recorded locally: 1

| subject | hole-id | filler | filled | thread-id | question evidence | answer evidence |
|---|---|---|---|---|---|---|
| `O-H_0-simeq-mathfrak-S-_2` | `ground:0704.0502:edge-7:O-H_0-simeq-mathfrak-S-_2` | `and-nnexus-and` | yes | `ask-1781553243-79` | `arse-q-ask-1781553243-79` | `arse-a-ask-1781553243-79` |
| `alpha` | `ground:0704.0502:edge-8:alpha` | `element-nnexus-element` | yes | `ask-1781553243-80` | `arse-q-ask-1781553243-80` | `arse-a-ask-1781553243-80` |
| `f-V-rightarrow-W` | `ground:0704.0502:edge-12:f-V-rightarrow-W` | `linear-map-nnexus-linear-map` | yes | `ask-1781553243-81` | `arse-q-ask-1781553243-81` | `arse-a-ask-1781553243-81` |
| `f_n-V_n-rightarrow-W` | `ground:0704.0502:edge-14:f_n-V_n-rightarrow-W` | `linear-map-nnexus-linear-map` | yes | `ask-1781553243-82` | `arse-q-ask-1781553243-82` | `arse-a-ask-1781553243-82` |
| `V` | `ground:0704.0502:edge-1:V` | OPEN | no |  |  |  |

Verification:

- Adapter imports `fill.py` and calls `fill.fill(..., kind=fill.KIND_GROUND)`.
- Open `:concept :?` is passed as `filler=None`; no ArSE write is made for the open hole.
- Every filled thread was absent from `/api/alpha/arse/unanswered` after the run.
