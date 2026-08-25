# T-fail-agent-not-found -- agent-not-found (72x)

<!-- census:begin -->
- **class**: `agent-not-found`
- **multiplicity**: 72
- **window**: 2026-08-22T07:32 -> 2026-08-25T13:34
- **most often**: `claude-cli` <- `codex-10`
- **stage**: PERCEIVE -- fell back: 72 occurrences at or over 10 with nothing written here
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
