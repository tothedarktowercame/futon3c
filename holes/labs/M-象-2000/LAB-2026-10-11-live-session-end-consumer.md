# Live reload and recompute: session-end consumer — 2026-10-11

Reload/recompute/observation window: 2026-10-11 UTC. This run reloaded the
session-end consumer from master, recomputed the records belonging to the
Agency registry's current sessions, and passively watched for a real session
end. It did not induce a reset, replacement, deregistration, or reap.

## Outbox preflight

Before reload:

```text
-rw-rw-r-- 1 joe joe 1860 Oct 11 17:30 session-ends.jsonl.test-pollution-2026-10-11
```

The live `session-ends.jsonl` and its cursor were absent. The quarantined
1,860-byte pollution file was left untouched. Since there were zero live
lines, there were no transition agents to compare with the 127-agent roster
and no unsafe test identity to refuse.

## Reload receipt

The shared JVM loaded, in order:

1. `futon3c.agency.registry`
2. `futon3c.logic.xiang`
3. `futon3c.xiang.turn-store`
4. `futon3c.xiang.turn-acts`
5. `futon3c.xiang.turn-service`
6. `futon3c.transport.http`

The proof evaluation then explicitly called idempotent `start!` on the service
preserved in HTTP's service atom. It returned:

```clojure
{:ok true,
 :value {:rules-version "2026-10-11.3",
         :consume? true,
         :service? true,
         :listener? true,
         :outbox-path "/home/joe/.emacs-graph/session-turn-analysis/session-ends.jsonl"}}
```

The outbox was empty, so replay had nothing to acknowledge and correctly did
not create `session-ends.jsonl.cursor`.

## Frozen population and replay

The selection found 124 current session IDs, of which 81 had settled turn
records. The frozen population was 2,181 records. Exact session membership and
counts are in `data/live-session-end-recompute-2026-10-11.sessions.json`; the
record-level before/after values are in
`data/live-session-end-recompute-2026-10-11.tsv`.

The records were processed per session in `created_at` order. Every call used
the live `compute-ports!` with `{:session-cap 10000}`, with starts at least one
second apart and read-back after every call.

| Result | Count |
|---|---:|
| Calls completed | 2,181 |
| Computed | 2,173 |
| Expected `unknown/no-acts` | 8 |
| Other refusals | 0 |

The eight unchanged no-act records were `turn-IPuc7i`, `turn-xy0WXk`,
`turn-wDJHml`, `turn-iezmrd`, `turn-CG3sw4`, `turn-T8Kkuu`, `turn-XthaXw`,
and `turn-X9T8gs`.

The sum of `still_open` at the 81 frozen session tails was **2,610 before and
2,610 after**. No tail changed.

One non-tail record changed: `turn-c1TN4b` went from 5 to 6. It is a codex-72
agent-origin turn whose reply ended at 17:25:52Z. The next bell turn began at
17:24:13Z, inside that earlier turn's operator-to-reply interval. The replay
therefore included the overlapping later bell ask
`invoke-1791739453321-5677-80295469-b-0` in the earlier turn's through-time.
This is a concurrent-turn timing/stale-snapshot finding, not a session-end
consumer effect.

## Passive session-end observation

After recompute, the live outbox was polled for a full 15 minutes. No real
session end occurred. At the end of the window:

- `session-ends.jsonl` was still absent;
- `session-ends.jsonl.cursor` was still absent;
- no `turn-end-*` record was available to inspect.

Accordingly there is no live envelope/lapse/mark example from this window.
The listener registration receipt above confirms subscription, but runtime
consumption awaits the next genuine registry transition.

## Reproduction

The stdlib-only driver is `scripts/live_session_end_recompute.py`, run with
`python3 -I`. Its per-call checkpoint was retained only as an execution
artifact under `/tmp`; the committed TSV is the durable audit.
