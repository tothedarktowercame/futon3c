# f193 semantic-stall regression fixture

Captured 2026-09-08 from the halted jit-all-open-v3 campaign:

    data/apm-campaigns/jit-all-open-v3/queue-state.edn
    data/apm-campaigns/jit-all-open-v3/coordinator.edn

verbatim, at the moment f193/m00A02 was stalled with
`:live-job-transport-retry-exhausted` and its guide job stuck at
`:waiting-for-terminal-result`.

These are COPIES on purpose. semantic-progress-watchdog-test originally
slurped the live campaign files by relative path, which made the suite
non-deterministic -- one run in seven errored -- and would have turned it
outright red the moment the campaign advanced past f193, because the test
asserts `frame-id "f193"`. A regression pinned to a live record is right; a
regression that reads a file another process is still writing is not.
