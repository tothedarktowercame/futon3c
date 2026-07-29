# S8 — error-time recall

`scripts/error_recall.bb` moves arc-memory lookup to the compile failure
that supplies its retrieval key. It preserves Lean identifiers verbatim,
stopwords ordinary error vocabulary, queries at most eight store results,
prints at most three memories, and ranks arc-lane before solve-lane.

The runner performs bounded reads only. Every query or miss is appended to
`.state/error-recall/<row-id>.jsonl`; ground control joins row to Agency job
from the queue and remains the only receipt writer.

## Motivating live demo

Command:

`bb scripts/error_recall.bb --row s8-demo-a95A01 'setIntegral_mono requires a global pointwise inequality'`

Output:

```text
switch-to-setIntegral-mono-on-for-cell-local-bounds | When the comparison is inherently restricted to the set being integrated over, select the `_on` monotonicity theorem rather than strengthening a local bound into an unnecessary global one. | e-codexpilot-switch-to-setIntegral-mono-on-for-cell-local-bounds
```

The local log records terms
`["setIntegral_mono","global","pointwise","inequality"]` and that one
surfaced memory id.

## Store-down demo

With `ERROR_RECALL_BASE=http://127.0.0.1:1`, the same invocation returned
exit `0`, printed zero bytes, and still logged the query with
`"surfaced-memory-ids":[]`.

The packet now tells runners to recall on each compile error, consult Loogle
and LeanSearch before unbounded source grepping, and distinguish
dispatch-time from error-time memory use in the final report.
