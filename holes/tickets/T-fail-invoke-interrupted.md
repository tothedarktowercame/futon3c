# T-fail-invoke-interrupted -- invoke-interrupted (1x)

**Status (triaged 2026-09-01): STILL-OPEN, rare.** 4 occurrences, latest 08-31. Decide whether interruption is a defect or an expected terminal class.

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

## Completion criteria

- The decision 'interruption is a defect' or 'interruption is an expected terminal class' is recorded below `census:end` — check: a dated decision line in the ticket file **Not met.**
- If expected: the class is census-exempted like other expected terminals — check: `scripts/failure-tickets.py`/`failure-census.py` excludes `invoke-interrupted`, or an equivalent exemption note in the ticket **Not met.**
- If defect: a fix lands and occurrences stop — check: `GET /api/alpha/invoke/jobs` filtered on terminal-code `invoke-interrupted` shows no growth after the fix, recounted in the ticket **Not met.**
