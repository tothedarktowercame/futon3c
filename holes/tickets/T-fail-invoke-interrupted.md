# T-fail-invoke-interrupted -- invoke-interrupted (1x)

<!-- census:begin -->
- **class**: `invoke-interrupted`
- **multiplicity**: 1
- **window**: 2026-08-24T14:04 -> 2026-08-24T14:04
- **most often**: `codex-12` <- `codex-15`
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
