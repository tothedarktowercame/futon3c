# F224 store-read timeout investigation

Observed window: 2026-09-11 18:02–18:18 UTC. Export with journalctl --user
-u futon1b-zone --since '2026-09-11 18:02:00 UTC' --until
'2026-09-11 18:18:00 UTC' -o short-iso --no-pager.
Run python3 analyze.py EXPORT to reproduce the timing summary. Full journal
remains local at /tmp/apm-f224-timeout-audit/futon1b.log; committed excerpts
exclude other users' query contents.

469 endpoint reads: median 2171 ms, p95 3771 ms, p99 4515 ms. Two ended with
client disconnect at 5202 and 5360 ms; both had zero worker-queue delay.
Their IDs belong to F224's 407-memory guide-2 snapshot (digest
7135690144b1441905f3e0d372ce273f22777d7c4b316e9b29cce1d05d40cf40).
F224 publication certificate 1789150635425-0-4a9d7862-3b7a-47a2-8fb7-8ed56b97d46b
records a timeout and retry at 18:27:15.388 UTC. Publication failure was
recorded at 18:17:15.425; that is not the individual read's start time.
The five-second read bound is unchanged. Membership/timing are strong
correlation, not a historically authenticated client identity.

There were 6149 executor rejection log records across the service. They
do not identify the pool/client, so cannot be assigned to these reads or
treated as proof of main-pool starvation. Their successful zero-delay
admission separately establishes the worker wait.

The server measures the entire expensive-read callback, including response
writing. It does not separately time endpoint selection, document hydration,
connection acquisition, or serialization. These logs do not yet establish
the underlying database-cost cause. Source inspection finds an endpoint
unnest/filter query followed by hydration; no query-plan profiling is claimed.

## Executed logging repair

Commit e2d7aea9, live-loaded from canonical futon3c without restart:
substrate GETs now carry trace IDs; failure exceptions preserve trace, URL,
deadline and elapsed time; memory visibility failures retain a whitelisted
diagnostic and emit [apm-store-read-failure] to the service journal.
Bodies and arbitrary exception data are excluded.
Tests: client 3/15; memory snapshot 23/101; all pass, lint and parens pass.
The captured read-only probe joins HTTP and permit logs under one trace:
client elapsed 1802 ms, server 1797 ms, permit callback 1796 ms.

This repairs a logging gap, not query performance. The next diagnostic
boundary is select/hydrate/connection timing for the matched endpoint query.
Any indexed or batched replacement must preserve the current reviewed
attachment checks.
