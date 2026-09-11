# Repair 058 offline qualification

This directory is a fresh qualification package for
`repair-attempt-058-untyped-failure`.  It is distinct from every repair 057
qualification, verification, admission, cohort, and successor artifact.

The plan pins the canonical 058 finding bytes, current applicable Futon2
runner and repair-store sources/tests, and the retained exact
`HttpTimeoutException` fault probe.  Candidate ancestry begins with the patient
retry ladder at `9ab503bd`, includes typed per-attempt evidence at `3bdc381e`
and later transport classification, and ends at `af82d3b8`.  The captured
Futon2 source HEAD is `c9c6d6ce341cb8bb5751aeff908435a552e1c25d`.

The exact producer invocation was:

```sh
cd /home/joe/code/futon3c
clojure -M -e '(require (quote [futon3c.wm.run4-historical-qualification :as q])) (q/produce! {:source-root "/home/joe" :output-root "/home/joe/code/futon3c/holes/labs/wm-contract/runs/RUN4-repair058-revalidation-2026-09-11/offline-evidence" :manifest-path "/home/joe/code/futon3c/holes/labs/wm-contract/runs/RUN4-repair058-revalidation-2026-09-11/qualification-plan.disabled.edn" :manifest-sha256 "0c55c13a51c34b76f6c79d64fd0f2883e130ff0e13cc8e86cb0fec509c931371"})'
```

The producer captured every source before execution and reread the plan,
sources, and repository HEAD-bound bytes before immutable publication.  All
three preregistered rows exited zero without timeout.  The timeout probe
observed recovery on call two and typed exhaustion with three retained attempt
summaries.  The full runner suite includes its deterministic/non-transient
rejection controls.

The exact commands are `bash -lc` wrappers so their reviewed working directory
is explicit.  The producer timeout terminates the immediate shell process; it
is not claimed to be a descendant-process sandbox.  All commands completed,
so that limitation was not exercised here.

This receipt says `:independent-review :not-performed` and
`:repair-admitted? false`.  It creates no executed review job, verification,
historical admission, repair transition, capacity, or live trial identity.
