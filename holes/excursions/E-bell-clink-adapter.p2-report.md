# E-bell-clink-adapter P2 report

## Commit

- Implementation: `66858f5a` (`Add Clink inbox consumption acknowledgements`)

This report is committed separately because a commit cannot contain its own
final SHA.

## Files touched

- `src/futon3c/agency/inbox.clj`
- `src/futon3c/agency/registry.clj`
- `src/futon3c/transport/http.clj`
- `test/futon3c/agency/inbox_test.clj`
- `scripts/clink-inbox.py`
- `holes/excursions/E-bell-clink-adapter.p2-report.md` (this report)

## Acceptance test

Exact command:

```text
env FUTON3C_TYPED_BELLS=0 FUTON3C_AGENCY_INBOX_DIR=/tmp/futon3c-agency-inbox-test-p2-final clojure -M:test -n futon3c.agency.inbox-test
```

Exact output:

```text
WARNING: Unknown module: org.apache.arrow.memory.core specified to --add-opens

Running tests in #{"test"}

Testing futon3c.agency.inbox-test
[invoke-trace] at=2026-08-26T11:21:46.145970052Z agent=push-seat-3380340e-9bbd-4c87-92d3-c90e844aba22 msg-id= thread=clojure-agent-send-off-pool-1 preview="--- CURRENT TURN ---\nSurface: bell\nFrom: test-caller\nTo: push-seat-3380340e-9bbd-4c87-92d3-c90e844aba22\nOrigin: agent\nEd"
[federation] announce skipped {:agent-id push-seat-3380340e-9bbd-4c87-92d3-c90e844aba22, :reason :no-peers, :peers [], :self-url nil}

Ran 2 tests containing 40 assertions.
0 failures, 0 errors.
```

The test covers missing-job 404, non-inbox 409, first ack, second-ack 409,
receipt preservation, `acked` event note, inbox-to-consumed file move, per-seat
count/age, aggregate health count/worst seat, and the older-unread/newer-acked
alarm case.

## Required regression suites

```text
clojure -M:test -n futon3c.agency.registry-test
Ran 51 tests containing 192 assertions.
0 failures, 0 errors.
```

```text
env FUTON3C_TYPED_BELLS=0 FUTON3C_AGENCY_INBOX_DIR=/tmp/futon3c-agency-inbox-test-p2 clojure -M:test -n futon3c.transport.auto-bellback-test
Ran 30 tests containing 93 assertions.
0 failures, 0 errors.
```

The auto-bellback run also printed two existing asynchronous fixture warnings
about a nil persistence file; they did not produce test failures or errors.

## Known timeout baseline

Exact command:

```text
env FUTON3C_TYPED_BELLS=0 FUTON3C_AGENCY_INBOX_DIR=/tmp/futon3c-agency-inbox-test-p2 clojure -M:test -n futon3c.transport.job-timeout-test
```

Final output summary:

```text
Ran 20 tests containing 54 assertions.
8 failures, 0 errors.
```

This matches the reviewed baseline. An immediately preceding run had 9
failures: the additional failure was
`reaper-and-supervisor-race-finalizes-once`, which observed two bellbacks. It
did not reproduce on rerun; the final run returned to the same eight existing
overrun/ceiling failures. P2 does not change the reaper or supervisor paths.

## Static gates

Exact commands and output:

```text
clj-kondo --lint src/futon3c/agency/inbox.clj src/futon3c/agency/registry.clj src/futon3c/transport/http.clj test/futon3c/agency/inbox_test.clj
src/futon3c/agency/registry.clj:1455:67: info: Redundant boolean coercion: expression already has type boolean
src/futon3c/agency/registry.clj:1457:71: info: Redundant boolean coercion: expression already has type boolean
src/futon3c/agency/registry.clj:1465:58: info: Redundant boolean coercion: expression already has type boolean
src/futon3c/transport/http.clj:6721:18: info: Redundant boolean coercion: expression already has type boolean
linting took 302ms, errors: 0, warnings: 0
```

```text
emacs -Q --batch -l ../futon4/dev/check-parens.el --eval '(arxana-check-parens-cli)' -- src/futon3c/agency/inbox.clj src/futon3c/agency/registry.clj src/futon3c/transport/http.clj test/futon3c/agency/inbox_test.clj
OK
```

```text
python3 -m py_compile scripts/clink-inbox.py
```

The Python compile produced no output and exited 0. `git diff --check` also
produced no output and exited 0.

## CLI smoke test

Against a test-generated unread delivery:

```text
env FUTON3C_AGENCY_INBOX_DIR=/tmp/futon3c-agency-inbox-test-p2 python3 scripts/clink-inbox.py --agent ack-seat-26469b9a-5db0-4b74-916c-e404fcf80740 list
invoke-1787743160529-3-fb4880ea test-caller 22489ms older backlog item
```

`read` printed the complete JSON payload, including its stored
`"ack-url": "/api/alpha/invoke/jobs/invoke-1787743160529-3-fb4880ea/ack"`.
The `ack` command loads that field from the file and joins it to `--base-url`;
it does not reconstruct the endpoint.

## Spec findings and decisions

- Nothing in the spec was impossible or required a workaround.
- The age fold is over the in-memory ledger via `active-invoke-job-counts`.
  `/health` reuses the projected registry result rather than scanning the
  ledger a second time or touching the filesystem.
- The finalizer is the single-winner guard. Only a successful
  `delivered`-to-`done` transition records `inbox-consumed-ack` and appends the
  `acked` event, so repeated or concurrent acks cannot rewrite the receipt.
- The consumed-file move is attempted only after the authoritative ledger
  transition and is caught/logged on failure, so a filesystem problem cannot
  make the consumption ack fail.
- Aggregate `/health` fields are `unconsumed-count`,
  `oldest-unconsumed-age-ms`, and `oldest-unconsumed-agent-id`.
- No shared JVM was restarted or live-loaded.
