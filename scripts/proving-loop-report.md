# proving-loop report

Runner: `scripts/proving_loop.py`

- ArSE manifest before entity_count: 74
- ArSE manifest after entity_count: 79
- scopes posted: 5
- total ArSE writes: 10 (5 asks + 5 answers)

| paper | scope | thread-id | question evidence | answer evidence | witnessed fills | open holes |
|---|---|---|---|---|---|---:|
| 0704.0502 | `{:kind :bind (:subject :A) (:concept ?c)}` | `ask-1781553025-74` | `arse-q-ask-1781553025-74` | `arse-a-ask-1781553025-74` | `canonical-generator-of-F_A-V`, `morphism`, `rules-of-composition-for-the-morphisms` | 10 |
| 0704.0502 | `{:kind :bind (:subject :E) (:concept ?c)}` | `ask-1781553025-75` | `arse-q-ask-1781553025-75` | `arse-a-ask-1781553025-75` | `category`, `dimension-of-the-vector-space`, `full-subcategory-of-E-f-having-as-objects-the` | 0 |
| 0705.0452 | `{:kind :bind (:subject :G) (:concept ?c)}` | `ask-1781553025-76` | `arse-q-ask-1781553025-76` | `arse-a-ask-1781553025-76` | `Lie-group-nlab-1598` | 3 |
| 0705.0452 | `{:kind :bind (:subject :K) (:concept ?c)}` | `ask-1781553025-77` | `arse-q-ask-1781553025-77` | `arse-a-ask-1781553025-77` | `display-defined-K` | 3 |
| 0708.2659 | `{:kind :bind (:subject :Set) (:concept ?c)}` | `ask-1781553026-78` | `arse-q-ask-1781553026-78` | `arse-a-ask-1781553026-78` | `comma-category` | 4 |

Verification:

- `?` was filtered out of witnessed fill text and counted separately.
- Every posted thread was absent from `/api/alpha/arse/unanswered` after answering.
- Witnessed fills are exactly `ScopeQuery.answers(... ?c)` values with `?` removed.
