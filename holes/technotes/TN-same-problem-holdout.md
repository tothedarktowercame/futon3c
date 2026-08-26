# Same-problem memory holdout on Student attempt 1

Date: 2026-08-26. Authority: preregistration amendment 5 at futon3c `1b9d880f33d033d336f718eb3387d7353b4d90f7` (`holes/labs/M-apm-demonstration/prereg-capability-transfer-v1.edn:644-701`).

## Implementation

`live-learning-phases/build-request` now applies the holdout only when the dispatch is `:student-attempt` with ordinal 1. It reads provenance from the already verified snapshot, withholds an accessible candidate only when `:provenance` is a map and `[:provenance :problem-id]` is a string exactly equal to the unit's problem id, and sorts the resulting ids deterministically. Missing or malformed provenance fails open and remains accessible. Attempts 2 and 3 keep the original complete accessible-id set.

I chose to leave the shared snapshot untouched. The existing check still compares the promotion receipt's digest with the complete snapshot digest before the request is assembled (`src/futon3c/apm/live_learning_phases.clj:129-134`). Filtering happens afterward and only changes the student-facing `:memory-snapshot :accessible-memory-ids` (`live_learning_phases.clj`, the holdout binding immediately after request validation). This avoids manufacturing a second snapshot identity and preserves the promotion receipt's content-addressed claim. The filtered ids are also the seeds for memory-cascade expansion, so the ordinary student request and cascade do not seed expansion from withheld shelf entries.

Every attempt-1 request records `:shelf/holdout :same-problem`, the exact sorted `:shelf/withheld-ids`, and `:shelf/withheld-count`, including an empty vector/count zero. The Student receipt copies those three controller-derived fields into its content-addressed body (`src/futon3c/apm/live_learning_phases.clj`, Student branch of `receipt`). Attempts 2 and 3 carry none of the keys.

Focused regression coverage is in `test/futon3c/apm/live_learning_phases_test.clj`: mixed same/cross/missing/malformed provenance, unchanged attempts 2 and 3, zero-match audit evidence, exact receipt shape, and an explicit assertion that verified attempt 1 does not produce `:student-snapshot-access-unverified`.

## Validation

Commands and results:

```text
clj-kondo --lint src/futon3c/apm/live_learning_phases.clj test/futon3c/apm/live_learning_phases_test.clj
linting took 50ms, errors: 0, warnings: 0

emacs -Q --batch -l /home/joe/code/futon4/dev/check-parens.el --eval "(arxana-check-parens-cli)" src/futon3c/apm/live_learning_phases.clj test/futon3c/apm/live_learning_phases_test.clj
OK

clojure -M:test -n futon3c.apm.live-learning-phases-test
Ran 36 tests containing 158 assertions.
0 failures, 0 errors.

clojure -M:test -n futon3c.apm.queued-frame-terminal-test
Ran 10 tests containing 69 assertions.
21 failures, 0 errors.
```

The queued-terminal count is unchanged from the stated baseline at `3d91dd0e`: all 21 remain in `five-problem-no-dispatch-terminal-qualification`, with `queue/tick!` returning nil status; this change does not touch that path.

`/home/joe/code/apm-lean/.lake/build/lib/Mathlib.olean` was absent after the gates. No Lean or Lake command was run.

## Installation boundary

No reload or restart was performed. The code is on master but will affect live dispatch only after the futon3c JVM next starts from master or the changed `futon3c.apm.live-learning-phases` namespace is deliberately reloaded from the canonical master checkout. Therefore the mid-flight f44 JVM remains on its morning implementation; the holdout begins with the next campaign cycle after that launch/reload, not merely because this commit exists on disk.
