# RUN4 repair-058 disabled admission packet

This directory freezes a disabled, server-owned historical-admission packet
for `repair-attempt-058-untyped-failure`.  It preserves the independently
accepted verification artifact byte-for-byte and pins Futon2 HEAD
`c9c6d6ce341cb8bb5751aeff908435a552e1c25d`.  It neither assigns an execution
identity nor materializes a historical-successor link.

The casting is author `codex-10`, ordinary reviewer `codex-12`, and repair
reviewer `codex-12`.  The new series, controller attempt, and cohort identities
are recorded in `PREPARATION-READINESS.edn`; the cohort target is one.  All
configured production roots are proposals only.  Nothing in this preparation
created or activated those roots.

## Packet-specific disposable gate

Run from `/home/joe/code/futon3c`:

```sh
clojure -Sdeps '{:aliases {:packet-review {:extra-paths ["test" "dev" "../futon2/test"]}}}' -M:packet-review -e "(require 'futon3c.wm.run4-repair058-packet-roundtrip-test) (let [r (clojure.test/run-tests 'futon3c.wm.run4-repair058-packet-roundtrip-test)] (shutdown-agents) (System/exit (+ (:fail r) (:error r))))"
```

The gate relocates the frozen authority tree into disposable roots and hashes
the relocated refs through the real materializer.  It copies the canonical 058
finding, executes the accepted qualification/review through the real
historical-verification producer into a disposable verification store, copies
and activates the frozen cohort only in a disposable data root, and then uses
the actual trusted entry, historical candidate/action, repair store, runner,
cohort, binding/run-record/projection writers, strict evidence readers,
recording, controller, and visibility paths.

Only external availability, substrate, judge, environment, review, and
notification ports come from the established isolated runner fixture.  No
evidence validator or historical selection core is replaced.  The assertions
require repair 058 to be the applicable first open stop-line, one attempt and
one close, an awaiting-validation non-task outcome, requested-not-enacted pin
status, unknown task verdict, pending visibility, no task-terminal artifact,
and an idempotent duplicate observation after cohort remaining capacity is
zero.

The fixture-local verification is deliberately bound to the relocated finding;
it does not alter or requalify the accepted production artifact.  Independent
packet review remains required before any production provisioning, activation,
configuration installation, or admission.  A distinct grounded production
successor remains mandatory before repair resolution.
