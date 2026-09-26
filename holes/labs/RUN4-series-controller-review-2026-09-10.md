# RUN4 ordered controller: independent review

Codex-17, 2026-09-10. Reviewed Codex-10's 4f5e843b, 8e4d5aef,
and 413c917a. Accepted at 413c917a for the bounded controller capability.
No serving scheduler, live invocation, or compliant-run acceptance is claimed.

The initial controller allowed successor dispatch after an interrupted unsafe
stop and trusted incomplete terminal records. The first correction repaired
those but still admitted impossible safe/not-attempted states and completed
terminals without start evidence. The final correction validates the lifecycle:
started events join exact durable admission click identity/time; completed
terminals require starts; busy rejection requires durable busy admission;
orphan predecessor-stop markers cannot authorize advancement.

Independent executions from futon3c, in separate processes and disposable stores:

- `clojure -M:test -n futon3c.wm.run4-series-controller-test`:
  11 tests, 48 assertions, zero failures/errors.
- Interrupted unsafe-stop/remainder write: recovery returns
  `:infrastructure-stopped`, exactly one click (the original trial).
- Schema-and-ID-only terminal: `:invalid-persisted-terminal`, zero clicks.
- Full-identity `:not-attempted/:safe/:reason :invented` terminal:
  `:invalid-persisted-terminal`, zero clicks.
- Full-identity succeeded terminal without start: `:terminal-without-start`,
  zero clicks.

These probes are the original failing cases with exception capture added to
show the expected refusal. Logs are /tmp/series-413-review.log,
/tmp/series-413-original.log and /tmp/series-413-terminal.log. The committed
controller tests retain regression coverage; temporary logs are supplementary.
clj-kondo: zero errors/warnings. check-parens: OK. git diff --check: clean.

The controller uses a normalized-root JVM mutex and an OS file lock around
series transitions. This review does not claim subprocess-concurrency or
physical power-loss testing. The atomic-write/fsync layer's previous tests
bound the durability evidence. Complete preparation-only manifests still refuse;
no task mappings, frozen packets, credentials, or live stores were created.

Next prerequisite: derive terminal evidence from authoritative runner artifacts,
binding exact click/run/attempt/trial identity and independent outcome evidence.
A fabricated callback result or service-idle observation is not completion.
That consumer must be established before serving scheduler integration or the
visibility projection can truthfully report enacted trials.
