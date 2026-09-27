# P0 retrieval ordering — API prerequisite (2026-09-27)

Owner request: replace the remaining retrieval-ordering detail stub using P6
system timestamps, pin the answer, and reject a rehashed snapshot whose two
system times are swapped. Status: **blocked on source exposure**, not completed.
MAP-Q5 and the historical mission claim are unchanged.

## What the available records establish

- Retrieval evidence `e-9c5a5211-25c5-42a5-984c-d0e5d48bac5b` has event-at
  `2026-09-24T16:22:48.527475353Z`.
- Turn-commits evidence `emacs-19af606bf41e8e62417c17410be3ab8c` has event-at
  `2026-09-24T16:22:46.007027481Z`, references futon3c commit
  `80428193c9f221276dc6769ab2d539120561af73`, and carries committed-at
  `2026-09-24T16:22:34+00:00`.
- Git commit-at is 14 seconds earlier at whole-second display precision;
  the retrieval evidence event timestamp is precisely 14.527475353 seconds
  after that second-resolution Git timestamp. Invoke completion is a separate
  event; it cannot supply either record's XTDB system time.

These values were inspected in the existing raw P0 snapshot at
`storage/m-xiang-2000/p0-snapshot-2026-09-27/evidence.jsonl` and expected fixture.
Neither record there has a system timestamp. No store ordering follows from
this absence, and no backfill provenance has been established by this review.

## Missing contract

`futon1b/API-CONTRACT.md:217` documents P6 system-as-of/valid-as-of **selection**.
It explicitly distinguishes caller event-at from XTDB insertion/valid time.
It does not document a returned per-record system timestamp.
`futon1b/futon1b_evidence.clj:157` hydrates using `SELECT * FROM evidence` with
temporal FROM clauses; it does not explicitly select `_system_from` or
`_system_to`. `public-doc` at line104 removes only :xt/id. Neither this module
nor its API contract defines an exposed system-from field for P0 to consume.
A query pin is the reader's knowledge bound, not the record's insertion time.

Required structural change: expose authoritative system-version timestamp(s)
from the same temporally pinned selection/hydration, with documented field
names and meaning (version start versus earliest insertion, especially for
backfill/replacement). Test that returned fields follow system-as-of and then
make P0 capture the raw response and compare the two records. This avoids
adding an admin-only store query or inventing timestamps in P0. If a separate
existing supported endpoint already exposes them, supply that contract instead.

## Execution and acceptance state

A fresh read-only P0 capture under timeout failed with HTTP503 before writing
its requested `/tmp/p0-retrieval-before-review` snapshot. A narrower pinned
list probe also timed out; no new record contents were obtained. No JVM reload,
restart, evidence write or existing snapshot mutation was performed. Commands
finished; no background process remains.

No P0 source or expected fixture was changed: a passing fixture must not pretend
that the requested two-system-times query and swapped-times regression exist.
No new detail line or successful live --check output is claimed. Python gates
are deferred until an implementation can use the real fields; this artifact is
the sourced prerequisite for the requesting owner, not a completed P0 packet.
