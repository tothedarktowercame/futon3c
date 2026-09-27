# P0 retrieval ordering — API prerequisite (2026-09-27)

**Update:** the prerequisite below was resolved using existing LIST visibility
queries, not an API change. See the second-pass result below.

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

## Second pass: bracket query implemented

claude-17 supplied the bounded bisection probe in
[bisect_system_time_probe.py](bisect_system_time_probe.py). P0 uses its **fixed
brackets**, not a fresh bisection, to keep additional store load at four
sequential LIST reads. Turn-commits probes are 16:22:46.034Z/.036Z; retrieval
probes are 16:22:48.564Z/.565Z, all on 2026-09-24. Each read retains author,
exact session, event-at lower filter and limit1000. Request parameters and raw
responses are saved in `retrieval-ordering.json`, covered by the manifest and
captured request log. All probe times must be at or before the capture pin.

Replay validates complete non-paginated responses, matching scope, absence at
lo, exactly one identical target record at hi, and positive width <=10ms.
The interval is **(lo, hi]**, not an exact insertion timestamp. The ordering is
computed from tc.hi < retrieval.lo; otherwise it says `not ordered`. These
observations place record visibility near the original event, without replacing
event time with system time or asserting unobserved ingestion provenance.
Git commit-at remains separately sourced from Git and the turn-commits record.
The retrieval storage gap to commit-at is (14.564,14.565] seconds. This resolves
the old six-second claim without modifying historical MAP-Q5.

**Route limitation:** the owner probe found BY-ID ignores system-as-of. The API
contract limits temporal reads to LIST/count; BY-ID must not be used for this
purpose. P0 uses only LIST. Rejecting unsupported temporal BY-ID parameters is
a futon1b contract-hardening gap, outside this packet.

The previous 'API change required' conclusion was too strong: exact timestamps
are not exposed, but the existing temporal selector supplies bounded visibility
evidence. The snapshot test fixture preserves the real responses, not fabricated
timestamp fields. HTTP transient failures are retried sequentially, at most
three attempts, with 2s/4s backoff and stderr diagnostics; nontransient errors
fail immediately. No JVM reload/restart or store write is needed.

### Validation

- Live `python3 scripts/xiang2000_p0.py --snapshot /tmp/p0-ordering-checked-v2
  --check`: `stubs: 0 of 12`, `check: PASS`.
- Two `--from-snapshot /tmp/p0-ordering-checked-v2 --check` outputs are
  byte-identical to the live output (including capture pins).
- `P0_ORDERING_SNAPSHOT=/tmp/p0-ordering-checked-v2 python3 -m unittest discover
  -s scripts -p 'test_xiang2000_p0*.py'`: **16 tests, OK**, no skips.
- The CLI mutation test copies the snapshot, swaps only the two records' raw
  bracket responses, recomputes manifest hashes, and checks nonzero exit naming
  `retrieval-ordering detail`. A second rehashed copy makes lo contain the target
  and is also refused. Unit controls pin the real answer, reject incomplete
  absence, derive `not ordered` for overlapping bounds, and exercise 503 backoff.
- `py_compile` passes for P0 and the new test; `git diff --check` clean.
- One intermediate capture got HTTP503 on an existing hyperedge read and exposed
  a new retry name collision with P0's local `time` string. Fixed by module alias,
  with a regression; the final checked capture succeeded. No restart/reload.

Old snapshots lacking `retrieval-ordering.json` cannot prove this added detail
and are refused as missing required inputs; no synthetic migration is applied.
The committed compact fixture contains the actual four responses and joined
records. The full successful capture remains at the temporary path above.
