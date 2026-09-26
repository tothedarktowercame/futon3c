# E-bell-clink-adapter P1 report

## Commit

- Implementation: `f6462999` (`Add pull-only Agency inbox delivery`)

The report is committed separately because a commit cannot contain its own final SHA.

## Files touched

- `src/futon3c/agency/registry.clj`
- `src/futon3c/agency/inbox.clj`
- `src/futon3c/agency/roster_store.clj`
- `src/futon3c/transport/http.clj`
- `test/futon3c/agency/inbox_test.clj`
- `holes/excursions/E-bell-clink-adapter.p1-report.md` (this report)

`roster_store.clj` and the HTTP register/auto-register/restore boundaries carry
`delivery-mode` so a persisted pull-only seat cannot return after restart as push-mode.

## Focused acceptance test

Exact command:

```text
env FUTON3C_TYPED_BELLS=0 FUTON3C_AGENCY_INBOX_DIR=/tmp/futon3c-agency-inbox-test clojure -M:test -n futon3c.agency.inbox-test
```

Output:

```text
WARNING: Unknown module: org.apache.arrow.memory.core specified to --add-opens

Running tests in #{"test"}

Testing futon3c.agency.inbox-test
[invoke-trace] at=2026-08-26T10:54:04.467392576Z agent=push-seat-a321d8f7-605e-4a7d-ba53-d89187494c2a msg-id= thread=clojure-agent-send-off-pool-1 preview="--- CURRENT TURN ---\nSurface: bell\nFrom: test-caller\nTo: push-seat-a321d8f7-605e-4a7d-ba53-d89187494c2a\nOrigin: agent\nEd"
[federation] announce skipped {:agent-id push-seat-a321d8f7-605e-4a7d-ba53-d89187494c2a, :reason :no-peers, :peers [], :self-url nil}

Ran 1 tests containing 17 assertions.
0 failures, 0 errors.
```

The single test covers the direct registry no-spawn guard, roster visibility,
HTTP bell inbox delivery and JSON parsing, non-terminal `delivered`, the direct
auto-bellback path, and the default push-mode control.

## Other gates and suites

Paren check:

```text
emacs -Q --batch -l ../futon4/dev/check-parens.el --eval '(arxana-check-parens-cli)' -- src/futon3c/agency/registry.clj src/futon3c/agency/inbox.clj src/futon3c/agency/roster_store.clj src/futon3c/transport/http.clj test/futon3c/agency/inbox_test.clj
OK
```

Lint:

```text
clj-kondo --lint src/futon3c/agency/registry.clj src/futon3c/agency/inbox.clj src/futon3c/agency/roster_store.clj src/futon3c/transport/http.clj test/futon3c/agency/inbox_test.clj
src/futon3c/agency/registry.clj:1446:67: info: Redundant boolean coercion: expression already has type boolean
src/futon3c/agency/registry.clj:1448:71: info: Redundant boolean coercion: expression already has type boolean
src/futon3c/agency/registry.clj:1456:58: info: Redundant boolean coercion: expression already has type boolean
src/futon3c/transport/http.clj:6627:18: info: Redundant boolean coercion: expression already has type boolean
linting took 295ms, errors: 0, warnings: 0
```

Existing suites:

- `clojure -M:test -n futon3c.agency.registry-test` — 51 tests, 192 assertions, 0 failures, 0 errors.
- `env FUTON3C_TYPED_BELLS=0 FUTON3C_AGENCY_INBOX_DIR=/tmp/futon3c-agency-inbox-test clojure -M:test -n futon3c.transport.auto-bellback-test` — 30 tests, 93 assertions, 0 failures, 0 errors.
- `clojure -M:test -n futon3c.agency.roster-store-test` — 11 tests, 38 assertions, 0 failures, 0 errors.
- `env FUTON3C_TYPED_BELLS=0 FUTON3C_AGENCY_INBOX_DIR=/tmp/futon3c-agency-inbox-test clojure -M:test -n futon3c.transport.job-timeout-test` — 20 tests, 54 assertions, 8 failures, 0 errors. Failures are in existing overrun/ceiling tests: jobs finish `failed` or remain `running` where tests expect `done`, `overrun`, or `timeout`.
- `env FUTON3C_TYPED_BELLS=0 FUTON3C_AGENCY_INBOX_DIR=/tmp/futon3c-agency-inbox-test clojure -M:test -n futon3c.transport.http-test` — 122 tests, 616 assertions, 27 failures, 3 errors. Existing failures cover public-view redaction, War Machine/live AIF snapshots, health evidence count, minibuffer timestamps, compact response shape, and whistle timeout completion. The bell acceptance exercised during that run did not fail.

The first auto-bellback run inherited `FUTON3C_TYPED_BELLS=true` from the live
shell and failed the suite's default-off assertions (3 failures). Re-running
with `FUTON3C_TYPED_BELLS=0` produced the clean result above.

## Spec findings and decisions

- The design note says `delivery: :inbox`; the handoff explicitly requires the
  non-conflicting name `delivery-mode`, which is what the implementation uses.
- `delivered` was added to the non-terminal checks in job polling and cancel,
  and to `finalize-invoke-job!`, requested-job deduplication, and active-job
  counts. `terminal-invoke-state?` remains unchanged. The two terminal-state
  definitions still disagree, as requested; they were not unified here.
- The inbox writer rejects both agent IDs and job IDs containing `/` or `..`.
  The handoff only required this for agent IDs, but a caller-supplied job ID is
  also a filename segment and needs the same containment rule.
- No ack endpoint and no staleness metric were added.
- No JVM was restarted and no file was loaded into the shared JVM.

No part of the requested behavior was impossible. The only additional judgment
was preserving `delivery-mode` across the existing durable roster and HTTP
registration lifecycle; otherwise restart would silently restore spawn delivery.
