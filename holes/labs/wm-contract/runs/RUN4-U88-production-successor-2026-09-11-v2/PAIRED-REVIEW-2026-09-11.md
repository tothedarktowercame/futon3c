# Paired historical and successor review

The actual paired test exposed two replay defects: historical reads filtered out
resolved obligations, and the resolution writer rejected identical repeated writes.
The reader now uses the existing validated obligation-history view. Resolution
replay first validates fresh reader authority and then strictly compares existing
immutable evidence; a conflicting record refuses without overwrite.

`test/futon3c/wm/run4_real_paired_test.clj` copies the fourteen digest-pinned live
historical receipt artifacts and their finding, verifier, preregistration and
admission dependencies into disposable storage. Absolute paths and dependent
hashes are explicitly rebased. These transformed copies are test evidence, not
new production qualification or admission. Neither durable reader is replaced.

The new frozen successor packet is materialized with disposable roots and actual
cohort activation. The ordinary task core and effective environment are explicit
fixtures. Real async wrapper, cohort writers, recording, projection, binding,
controller and both historical/successor readers feed the actual repair resolver.
A fault is injected before resolution after terminal persistence. Recovery resolves,
repeat completion retains one cohort attempt, and conflicting retained resolution
is refused without overwrite. No live stores or capacity were changed.

Validation: paired gate 1 test / 30 assertions; repair obligations 9 / 36;
terminal evidence 10 / 54; all pass. Clojure lint, parentheses and diff checks pass.
The test is tagged slow and requires the retained live receipt sources locally.

Reproduce from futon3c:

```sh
clojure -Sdeps '{:aliases {:pair-review {:extra-paths ["test" "dev"]}}}' -M:pair-review -e "(require 'futon3c.wm.run4-real-paired-test) (let [r (clojure.test/run-tests 'futon3c.wm.run4-real-paired-test)] (shutdown-agents) (System/exit (+ (:fail r) (:error r))))"
```

Independent review and successor source-provenance refresh remain before live
loading and the Joe-authorized successor trial. Historical production evidence
and its original qualification are unchanged.
