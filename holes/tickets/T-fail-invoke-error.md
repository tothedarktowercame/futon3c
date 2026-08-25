# T-fail-invoke-error -- invoke-error (2x)

<!-- census:begin -->
- **class**: `invoke-error`
- **multiplicity**: 2
- **window**: 2026-08-21T08:23 -> 2026-08-24T11:31
- **most often**: `codex-10` <- `claude-cli`
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
