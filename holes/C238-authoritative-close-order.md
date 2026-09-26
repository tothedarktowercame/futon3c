# C238 — authoritative runner-service close order

Date: 2026-08-31

## Finding

The runner service declared its in-process close boundary authoritative, but
`report-phase!`, `close-click!`, and `fail-click!` published the secondary
Agency registry projection before updating `!status`. A slow registry ledger
serialization could therefore leave a completed worker looking live, and a
publication exception could replace a successful runner result with
`:service-failed`.

## Repair

All three paths now cross the in-process lifecycle boundary first. Registry
publication remains synchronous and is not hidden behind another worker. Its
state is exposed as `:registry-publication` with `:pending`, `:published`, or
`:failed`, including stage, exception class, and cause on failure.

The service projection is authoritative. If later registry publication fails,
the registry may temporarily remain busy, but the click stays closed and its
`:last-result` is preserved. The failure is recorded in status and printed to
stderr; it does not roll the lifecycle backward. Making ledger serialization
faster or asynchronous remains a separate performance decision.

## Falsifier and focused verification

The ordering control blocks phase and idle publication deliberately. While
publication is blocked it observes the phase transition, then observes
`:running? false` and the populated successful `:last-result`; actual worker
completion remains blocked until synchronous publication is released. Two
failure controls prove a registry exception cannot rewrite either a successful
terminal result or a genuine `:service-failed` result.

Canonical focused invocation:

```sh
clojure -M:test -n futon3c.wm.runner-service-test
```

Result: exit 0, 7 tests, 63 assertions, 0 failures, 0 errors.
