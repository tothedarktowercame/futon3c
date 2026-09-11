# Disabled successor-v2-selection historical admission

This packet binds the independently accepted verification artifact for
`repair-run4-u88-production-successor-20260911-v2--attempt-001-untyped-failure`
to a distinct one-attempt cohort and frozen RUN4 series. The authenticated U88
pin remains requested-but-not-enacted when the open stop-line is selected.
Historical execution may produce only awaiting-validation; it is not U88 task
success and does not resolve the repair.

The queue visibility path is
`/home/joe/run4/successor-v2-selection-admission/queue/visibility.json`.
The packet and queue are disabled. No roots, cohort capacity, handler config,
or queue were installed.

The packet-specific disposable lifecycle gate is:

```sh
clojure -Sdeps '{:aliases {:futon2-test-support {:extra-paths ["../futon2/test"]}}}' -M:test:test-all:futon2-test-support -i :slow -n futon3c.wm.run4-successor-v2-selection-packet-roundtrip-test
```

It recreates the accepted verification through the real verifier using an
isolated checkout at the artifact's source HEAD. Repair, cohort, controller,
recording, binding, projection, visibility, and queue roots are temporary.

Private installation, only after independent review, consists of materializing
`server-config.disabled.edn` with the existing server-owned credential and
mission/admissibility ports, attaching the exact enabled historical action and
four-key execution-cohort binding, then constructing the runtime queue entry
from `queue.disabled.edn` and calling
`futon3c.wm.run4-series-queue/start!`. No request endpoint accepts this config.
