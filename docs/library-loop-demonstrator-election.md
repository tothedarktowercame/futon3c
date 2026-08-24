# Library Loop demonstrator election

Status: **proposed; awaiting Joe's confirmation before any real run**.

The files-only Library Loop's hard-five slate is ordered by independently
elected construction program.  This is an operator-facing integration record;
the schema-1 runtime ledger in `data/apm-lane/demonstrators.edn` intentionally
contains only problem identity and ordinary status-recompute state.

| Construction program | Proposed primary | Alternate |
|---|---|---|
| Concrete singular homology | `t01A03` | `t02A04` |
| Fundamental groups and van Kampen | `t91A05` | `t93A03` |
| Covering classification and monodromy | `t00A02` | `t03J03` |
| Homological orientation and transverse-preimage duality | `t00J02` | `t02A08` |
| Intersection theory and product pairings | `t96J06` | `t03J05` |

The ordering in `demonstrators.edn` is therefore `t01A03`, `t91A05`,
`t00A02`, `t00J02`, `t96J06`.  Every entry remains initialized with
`:success? false` and `:last-ruling nil`.  Inclusion in this file is not a
success ruling: only the existing ordinary status-recompute path may set a
problem successful, and only when its ruling is `:closed`.

## Provenance

The construction-area election was authored and independently reviewed on
the apm-lean branch `codex/construction-area-map`:

- `161f1f1c` — five construction-program slots, their dependency boundaries,
  expanded qualification slate, and local promotion requirements;
- `ac2983e9` — primary/alternate problem election after inspecting each
  candidate's frozen statement, `status.json`, proof outline, Lean source,
  exact residual declaration, and sorry sites.

The election deliberately replaces the earlier provisional set.  In
particular, it adds a direct concrete-singular-homology qualification, uses a
less cross-program covering primary, and avoids spending two hard slots on the
same orientation/singular-homology dependency cluster.  Alternates are not
automatic successors: changing a primary requires a new election record with
the failed promotion condition and current repository evidence.

## Remaining operator decision

Joe must explicitly confirm (or revise) this proposed hard-five slate before
the first real Codex turn, apm-lean bank, or 20-turn qualification run.  That
confirmation belongs in operator-facing documentation or run authorization,
not as an unvalidated extension to the runtime ledger schema.  Until then the
EDN is a proposed scoreboard initialized to no successes and no rulings.
