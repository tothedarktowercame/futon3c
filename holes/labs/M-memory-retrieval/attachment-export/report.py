#!/usr/bin/env python3
import hashlib
import json
from pathlib import Path

HERE = Path("holes/labs/M-memory-retrieval/attachment-export")
ARTIFACT = HERE / "attachment-state.json"
data = json.loads(ARTIFACT.read_text())

schema = """# Attachment-state export schema

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
"""
(HERE / "SCHEMA.md").write_text(schema)

lines = ["# Joining dispatches to the attachment snapshot", "",
"For an offered dispatch receipt, take `evidence/body.job-id`, `evidence/at`, and the surfaced memory ids. Join each surfaced id to `attachments.memory-id`; each matching row supplies its endpoint and the endpoint's snapshot density from `pattern-aggregates`.", "",
"This is a snapshot join, not an as-of reconstruction. For a dispatch before the watermark, `staleness-bounds-seconds.lower=0` and `.upper` is the elapsed dispatch-to-snapshot interval. `reviewed-by-dispatch=true` only proves that the recorded review timestamp is no later than the dispatch; it does not prove that the edge remained unchanged throughout the interval. A causal cohort should export at dispatch time or use a future bitemporal as-of export.", "",
"## Worked examples (verbatim artifact records)", ""]
for example in data["worked-join-examples"]:
    lines += [f"### `{example['job-id']}`", "", "```json",
              json.dumps(example, indent=2, sort_keys=True), "```", ""]
(HERE / "JOIN.md").write_text("\n".join(lines))

def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()

outputs = ["attachment-state.json", "SCHEMA.md", "JOIN.md"]
(HERE / "SHA256SUMS").write_text(
    "".join(f"{sha(HERE / name)}  {name}\n" for name in outputs)
)
