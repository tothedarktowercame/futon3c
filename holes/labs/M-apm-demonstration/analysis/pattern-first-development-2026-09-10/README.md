# Offline pattern-first APM development packet

Report: `holes/technotes/TN-APM-pattern-first-development-2026-09-10.md`.

- `frozen.json`: 136 canonical patterns, all full texts and section spans;
  wider committed-source census, deposits, source hashes and family history;
  original catalog rows; six analyst-authored queries; 364 sanitized retained
  memory rows. Snapshot `memory_keys` reference SHA-256-addressed `memory_rows`
  so shared rows are stored once. Source descriptions, examples and review
  status remain distinct. No inferred pattern–memory edge is added.
- `result.json`: corpus counts, exact retained attachment pairs, top-ten
  candidates/scores/pointers for three retrieval arms and designated candidate
  ranks. Null rank means absent from the top ten, not absent from the corpus.
- `census.md`: representative real ID, literal condition/move excerpts and
  recorded attached examples for each of 16 families. Full spans for every
  pattern remain in `frozen.json.patterns`.
- `probe.py`: freeze inputs and replay the local SQLite/catalog probe.
- `sanitize_snapshot.bb`: whitelist mathematical/provenance fields from one
  explicit retained snapshot on stdin; it does not open packets or services.
- `check_catalog.bb`: read the pinned Git transport source, extract only its
  pure catalog scoring function and compare its output with the Python arm.
  It does not load the transport namespace or call an endpoint.
- `checks.txt`: completed validation, with failed initial checks distinguished.

Replay from futon3c, without a running service:

```sh
python3 holes/labs/M-apm-demonstration/analysis/pattern-first-development-2026-09-10/probe.py replay --output /tmp/pattern-first.json
cmp holes/labs/M-apm-demonstration/analysis/pattern-first-development-2026-09-10/result.json /tmp/pattern-first.json
bb holes/labs/M-apm-demonstration/analysis/pattern-first-development-2026-09-10/check_catalog.bb
```

Python's standard library and SQLite FTS5 suffice for replay. Babashka and local
pinned Git objects are required for the catalog parity check. `freeze` additionally
reads the four named local f211 snapshots; these are retained untracked inputs,
not committed source. It pins each source hash and refuses a read-time change.
It reads committed mathematical declarations only on the explicit watcher repo
list, fixes futon3c to the handoff revision and records other repo revisions.
Regeneration against changed revisions is a new development packet. Replay
does not refresh them.

This is not the live Student reviewed-memory endpoint, a held-out evaluation,
a fresh Student attempt or evidence that higher ranking causes transfer.
The tuned endpoint query is labelled separately from statement queries. Both
negative prerequisite cases remain negative even when their patterns rank high.
Unjudged top-ten alternatives are candidates, not validated applications.
