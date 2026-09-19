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
