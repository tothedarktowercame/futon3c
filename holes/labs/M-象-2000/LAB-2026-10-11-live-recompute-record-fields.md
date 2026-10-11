# Live recompute of record fields — 2026-10-11

Read/reload window: 2026-10-11 17:11–17:54 UTC. This run reloaded master and
recomputed only the settled records belonging to sessions current in the
Agency registry at the selection point. It did not backfill `reply_at`, induce
a session transition, or write record files directly.

## Reload receipt

The shared JVM was reloaded through `scripts/proof-eval.sh`, in this order:

1. `futon3c.logic.xiang`
2. `futon3c.xiang.turn-acts`
3. `futon3c.xiang.turn-service`
4. `futon3c.agency.registry`
5. `futon3c.transport.http`

The evaluation returned:

```clojure
{:ok true,
 :value {:rules-version "2026-10-11.2",
         :listener? true,
         :has-in-force true,
         :has-promulgated true,
         :in-force ["receipt-turn-f-s1-0"],
         :promulgated ["receipt-turn-f-s1-0"]}}
```

Thus the running adapter has rules version `2026-10-11.2`, the registry's
`add-session-end-listener!` var resolves, and a synthetic `turn-ports` call
returns both new standings projections.

## Frozen population and method

The registry exposed 124 current session IDs. Eighty-one had settled record
files, comprising 2,167 records. The exact agent/session mapping and per-session
record counts are in
`data/live-record-fields-recompute-2026-10-11.sessions.json`. The before and
after record data are in `data/live-record-fields-recompute-2026-10-11.tsv`.

Records were ordered by session and `created_at`. The existing live
`compute-ports!` was called with `{:session-cap 10000}`, with request starts at
least one second apart. Each result was read back before proceeding. All 2,167
calls completed; the status timestamps range from 17:12:46Z to 17:53:47Z.

## Results

| Measure | Before | After |
|---|---:|---:|
| Records with `acts` | 1 | 2,159 |
| Records with `standings` | 1 | 2,159 |
| Records with `obligations` | 1 | 2,159 |
| Records with `ports_status` | 1 | 2,167 |
| Sum of `still_open` at 81 frozen session tails | 2,585 | 2,597 |

The 2,159 computed records carry all four projected fields. The eight records
which cannot produce acts carry `ports_status.state = "unknown"`, reason
`"no-acts"`, and rules version `2026-10-11.2`:

- `turn-IPuc7i`
- `turn-xy0WXk`
- `turn-wDJHml`
- `turn-iezmrd`
- `turn-CG3sw4`
- `turn-T8Kkuu`
- `turn-XthaXw`
- `turn-X9T8gs`

No other record refused.

Open counts changed on four records, all in the active codex-68 session
`01a123e4-5da0-7130-bcc2-d8be5a21d32c`:

| Record | Before | After |
|---|---:|---:|
| `turn-TC1iro` | 42 | 51 |
| `turn-pQ8VCG` | 40 | 51 |
| `turn-Z3ZOCn` | 41 | 52 |
| `turn-BYo43M` | 41 | 53 |

That session's frozen tail is `turn-BYo43M`, accounting for the entire tail
aggregate increase of 12. None of these records has `reply_at`; this batch did
not change linking, kinds, or their act times. The supported explanation is
that the full chronological replay refreshed stale/in-flight snapshots in a
session which remained active during the 41-minute run. The other 80 frozen
session tails were unchanged.

## Natural post-reload happened observation

`turn-c1TN4b` was created at 17:18:11Z and naturally received its happened
payload at `reply_at = 2026-10-11T17:25:52Z`, after the reload. Its persisted
record has four reply acts; every one has `at = 2026-10-11T17:25:52Z`. This
confirms both the HTTP pass-through/service timestamp and the adapter's use of
the reply's own time. No historical `reply_at` was synthesized by this run.

## Session-end outbox

`~/.emacs-graph/session-turn-analysis/session-ends.jsonl` exists. At the final
read it was 1,860 bytes with mtime 2026-10-11 17:30:58.513162811Z. The check
was read-only; no session change was triggered.

## Reproduction

The stdlib-only driver is
`scripts/live_record_fields_recompute.py`; it was run under `python3 -I`.
`python3 -m py_compile` succeeds. The result checkpoint was an uncommitted
execution artifact in `/tmp`; the durable TSV contains the verified outcome.
