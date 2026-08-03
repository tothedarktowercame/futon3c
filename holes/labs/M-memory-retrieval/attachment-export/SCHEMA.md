# Attachment-state export schema

`attachment-state.json` is a read-only SEQ-0.3 snapshot. It is deterministic for the store-content hashes and timestamps in `snapshot-watermark`; a new live run necessarily has a new watermark.

## Top-level fields

| field | meaning |
|---|---|
| `schema-version` | Closed schema identifier, currently `seq-0.3/attachment-state-v1`. |
| `read-only` | Always true; the exporter performs evidence GETs and coherent projection POST queries only. |
| `snapshot-watermark` | Read interval, store URL, entry/edge/component counts, projection audits, and canonical input hashes. |
| `read-counts` | Entry counts for every evidence or hyperedge type consumed. |
| `scope` | Exact endpoint-discovery, edge-read, density rule, and known coverage boundary. |
| `status-counts` | Counts by `edge-state/attachment-status`. |
| `attachments` | One row per returned `(memory/assert edge, pattern endpoint)` pair, including non-current rows. |
| `pattern-aggregates` | One row per discovered endpoint; `reviewed-attachment-count` is attachment density. |
| `density-distribution` | Sparsity summary across all discovered endpoints, including zeroes. |
| `worked-join-examples` | Three joins to the frozen 129-dispatch receipt corpus. |

## Attachment row fields

| field | source and meaning |
|---|---|
| `edge-id` | Store `:hx/id` of the `memory/assert` edge. |
| `memory-id` | `:hx/props :roles :entry`. |
| `pattern-id` | One value from `:hx/props :roles :patterns`; the density endpoint. |
| `edge-state` | Store edge state, e.g. `current` or `superseded`. |
| `attachment-status` | Store review projection, e.g. `proposed` or `reviewed`. |
| `asserted-at` | `:evidence/at` of the joined memory evidence entry. Null means unavailable. |
| `asserted-at-source` | `memory-evidence-entry` when `asserted-at` was available; otherwise null. |
| `reviewed-at` | `:hx/props :review :reviewed-at`. Null means unavailable/not reviewed. |
| `review-evidence-id` | Evidence id named by the projected review. Null when unavailable. |
| `review-verdict` | Projected review verdict. Null when unavailable. |
| `reviewer` | Projected reviewer. Null when unavailable. |
| `system-time` | Edge `:hx/props :system-time`. Null when unavailable. |

No assertion timestamp, review timestamp, or historical state is inferred when the corresponding store field is absent.

## Determinism boundary

Two live reads have different `read-started-at`, `read-completed-at`, and derived staleness intervals even if the store is unchanged. Run `compare_modulo_watermark.py FIRST SECOND`: it removes only those watermark-derived fields and requires every input hash, edge, aggregate, and joined record to remain identical.
