# Historical qualification output producer — 2026-09-11

Implemented `futon3c.wm.run4-historical-qualification/produce!`. This server-only
component reads a digest-pinned manifest, validates unique nonempty check and
source populations, executes the exact preregistered commands, retains exit
status, timeout status and bounded stdout/stderr with SHA-256, rechecks source
and manifest bytes, and immutably publishes a verification receipt. Supplied
result rows are not an input. Reuse of a verification ID refuses before command
execution. Output root confinement and symlink refusal precede publication.
A normalized JVM mutex and OS lock serialize producers sharing an output root.

Passing qualification is not review or repair admission. Every output says
`:independent-review :not-performed` and `:repair-admitted? false`. The missing
fresh review consumer, implementation ancestry/finding association validator,
and awaiting-validation admission remain required next components. This code
is not connected to HTTP, the runner, a scheduler, or the repair store.

Manifest schema: `:wm/historical-qualification-plan-v1`; exact fields are
`:schema`, `:verification-id`, `:repair-id`, `:sources` (path/SHA-256 entries),
and `:checks` (unique keyword ID, argv vector, timeout milliseconds). It must
be independently reviewed server configuration. Command exit zero has only
the meaning established by that reviewed test command; it cannot prove its
own adequacy. Commands inherit the subprocess environment and run in the
source root; no sandbox or protection from malicious reviewed commands is
claimed. Source checks detect before/after drift, not transient mutation.
Timeout kills the immediate subprocess; general descendant containment is not
implemented. Commands requiring process-tree cancellation must not be admitted
without that support. Output is capped at 1 MiB per stream at collection.

Validation: 5 tests, 20 assertions, zero failures/errors, including real
subprocess success/rejection, timeout, post-command source drift, duplicate or
missing check population, caller-supplied results, repeated ID and output
symlink escape. Subprocess tests are tagged slow and explicitly run with
`clojure -M:test:test-all -n futon3c.wm.run4-historical-qualification-test`.
Lint 0 errors/0 warnings; check-parens OK; diff check clean. Cross-process
locking is implemented but not separately subprocess-contention tested.

Only disposable directories were used. No live repair/cohort/admission files,
credentials, services, namespaces, capacity or attempts were changed.
