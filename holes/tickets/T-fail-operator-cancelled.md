# T-fail-operator-cancelled -- operator-cancelled (4x)

**Status (triaged 2026-09-01): RECLASSIFY CANDIDATE.** 19 at review, mostly deliberate watchdog cancels. Propose census-exempting operator cancellation as expected behaviour rather than fixing anything.

<!-- census:begin -->
- **class**: `operator-cancelled`
- **multiplicity**: 4
- **window**: 2026-08-23T10:59 -> 2026-08-24T15:16
- **most often**: `codex-10` <- `countdown-control`
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
