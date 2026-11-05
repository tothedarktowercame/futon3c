# T-fail-worker-lost-on-restart -- worker-lost-on-restart (3x)

**Status (triaged 2026-09-01): PARTIAL, quiescent.** Exactly the ticketed 3 (last 08-23), all stuck in `delivering`. Close as non-reproducing after a delivery-retry cap for restart-orphaned jobs.

<!-- census:begin -->
- **class**: `worker-lost-on-restart`
- **multiplicity**: 3
- **window**: 2026-08-21T08:23 -> 2026-08-22T14:17
- **most often**: `f9045569047055-solver` <- `countdown-control`
- **stage**: BELIEVE
- **source**: `GET /api/alpha/invoke/jobs`, field `terminal-code`
<!-- census:end -->

## Why this file exists

Written by `scripts/failure-tickets.py` from the invoke-job ledger, so a typed
failure lands in the queue without anyone noticing it first. Everything above
the `census:end` marker is regenerated on each run; everything below it is
yours and is never touched.

Editing anything in this section is what moves the class back to BELIEVE: there
is no status field, so a file that nobody has written in is a class that nobody
has responded to.

## What would close this

(unstated -- until it is, this class has no promotion test and cannot reach
SELECT)

## Completion criteria

- A delivery-retry cap for restart-orphaned jobs stuck in `delivering` lands — check: the agency job/delivery path in `futon3c/src/futon3c/agency/` moves such jobs to a terminal state after N retries (commit + file:line) **Not met.**
- No new occurrences of terminal-code `worker-lost-on-restart` after 2026-08-23 — check: `GET /api/alpha/invoke/jobs` filtered on that terminal-code returns no rows after that date **Met.**
- The close-as-non-reproducing decision is recorded — check: a dated decision line below `census:end` in the ticket file **Not met.**
