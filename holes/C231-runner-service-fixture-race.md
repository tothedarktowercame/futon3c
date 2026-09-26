# C231 — runner-service fixture completion boundary

Date: 2026-08-31

The runner fixture used `!status` as both an HTTP projection and a worker join.
Those are not the same fact.  `reset-service!` reset `!status` and the Agency
registry before proving that the prior `wm-runner-click` thread had returned.
The old worker could then execute `report-idle!` against the following test's
fresh registry.  The two failures observed during C214 were therefore facts
about test ordering, not about the test's intended runner state.

The service now owns a completion promise separate from `!status` and exposes
`await-click!`.  Fixture setup and teardown join that exact click before
resetting shared state.  No sleep, retry, reordering, skipped test, or enlarged
timeout was added. The bounded test service owns the suite deadline; the
fixture cannot respond to a local deadline by corrupting the next test.

The ordering control deliberately starts a blocked click, performs the legacy
status reset while the worker is alive, first verifies a bounded observation
reports `:timed-out`, releases it, and joins by click id.
Before the completion carrier this order had no join handle; the following
fixture could begin while the worker still owned registry effects.  With the
carrier the join completes independently of the erased projection.

This is separate from C223: no Python cascade constructor or its 30-second
deadline is involved.

Canonical focused invocation:

```sh
clojure -M:test -n futon3c.wm.runner-service-test
```
