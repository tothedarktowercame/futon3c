# Canonical registry environment — 2026-09-19

Implementation commit: c7732bd8. Registry execution and fingerprint probes now impose LC_ALL=C.UTF-8, LANG=C.UTF-8 and TZ=UTC. Other ambient variables retain prior treatment. New registration specs carrying :test-environment, including nil or an empty map, refuse :environment-not-configurable with the canonical trio in details. Old warrant checks still fingerprint canonically and use the existing mismatch semantics; no records migrated or rewritten.

## Single registered execution

Command from futon3c:

```
clojure -M -m futon3c.test-registry.validation register holes/labs/registry-canonical-environment-2026-09-19/register.edn
```

evidence-id test-registry-00542817aa31f1cf284e1fc70659cc76d5eb7413a8dae8cd91ca1a7f392f23ad
warrant? false
results {:assertions 20, :duration-ms 2534, :errors 0, :exit 1, :failures 1, :tests 4}

**No warrant or binding resulted.** Four tests / 20 assertions had one failure: the nested/plain check equivalence assertion compared :checked-at timestamps. Both checks returned warrant? true; their sole difference was the check timestamp. The receipt and durable evidence remain intact.

The follow-up assertion correction excludes only :checked-at when comparing the complete results. It is lint/parens checked but **not executed**, honoring the requested single registered execution. Thus acceptance remains incomplete and a new authorized run is needed to obtain a self-warrant. No production code changed after the execution.

Coverage: real environment fingerprint construction under two ambient locale/timezone maps, retired specs rejected before execution, nested/plain check agreement, and an actual child process printing its canonical trio. Toolchain probes and test execution for fixture warrants are stubbed; the child-environment test really spawns a short shell. The old registry test for configurable locale was updated to expect explicit retirement.

No :6768 reload, migration, or unrelated registration. Gate outputs, spec, CLI stdout/stderr/exit, execution log, closure and durable HTTP evidence are adjacent.

## Corrected-content execution — separately authorized

Claude-12 authorized one execution of the corrected test content from `0a71a836`. The command was `clojure -M -m futon3c.test-registry.validation register holes/labs/registry-canonical-environment-2026-09-19/corrected-register.edn`. The scope includes the registry source, environment test and fixture namespace. No code changed in this follow-up.

```text
evidence-id test-registry-0bd83851e1a42d3179048e558e55423b31988615bbf807ee15d8f3a921d3bf08
warrant? true
results {:assertions 20, :duration-ms 2043, :errors 0, :exit 0, :failures 0, :tests 4}
bound test-registry/canonical-environment -> test-registry-0bd83851e1a42d3179048e558e55423b31988615bbf807ee15d8f3a921d3bf08
```

CLI exit: 0. The corrected suite passed once: 4 tests, 20 assertions, zero failures/errors. The durable warrant is bound to `test-registry/canonical-environment`; its HTTP evidence response is retained as `corrected-run.evidence.json`. Self-warrant acceptance is now complete. Earlier failed receipts remain intact; new runner log and closure are in `corrected-execution/`.
