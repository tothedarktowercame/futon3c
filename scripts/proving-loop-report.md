# proving-loop report

Runner: `scripts/proving_loop.py`

- ArSE manifest before entity_count: 69
- ArSE manifest after entity_count: 74
- scopes posted: 5
- total ArSE writes: 10 (5 asks + 5 answers)

| paper | scope | thread-id | question evidence | answer evidence | witnessed fills | open holes |
|---|---|---|---|---|---|---:|
| 0704.0502 | `{:kind :bind (:subject :A) (:concept ?c)}` | `ask-1781552434-69` | `arse-q-ask-1781552434-69` | `arse-a-ask-1781552434-69` | `canonical-generator-of-F_A-V`, `morphism`, `rules-of-composition-for-the-morphisms` | 10 |
| 0704.0502 | `{:kind :bind (:subject :E) (:concept ?c)}` | `ask-1781552434-70` | `arse-q-ask-1781552434-70` | `arse-a-ask-1781552434-70` | `category`, `dimension-of-the-vector-space`, `full-subcategory-of-E-f-having-as-objects-the` | 0 |
| 0705.0452 | `{:kind :bind (:subject :G) (:concept ?c)}` | `ask-1781552435-71` | `arse-q-ask-1781552435-71` | `arse-a-ask-1781552435-71` | `Lie-group-nlab-1598` | 3 |
| 0705.0452 | `{:kind :bind (:subject :K) (:concept ?c)}` | `ask-1781552435-72` | `arse-q-ask-1781552435-72` | `arse-a-ask-1781552435-72` | `display-defined-K` | 3 |
| 0708.2659 | `{:kind :bind (:subject :Set) (:concept ?c)}` | `ask-1781552435-73` | `arse-q-ask-1781552435-73` | `arse-a-ask-1781552435-73` | `comma-category` | 4 |

Verification:

- `?` was filtered out of witnessed fill text and counted separately.
- Every posted thread was absent from `/api/alpha/arse/unanswered` after answering.
- Witnessed fills are exactly `ScopeQuery.answers(... ?c)` values with `?` removed.
