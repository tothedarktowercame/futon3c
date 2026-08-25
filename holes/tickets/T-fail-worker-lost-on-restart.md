# T-fail-worker-lost-on-restart -- worker-lost-on-restart (3x)

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
