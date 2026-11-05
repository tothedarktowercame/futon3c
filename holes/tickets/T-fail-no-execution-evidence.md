# T-fail-no-execution-evidence -- no-execution-evidence (1x)

**Status (triaged 2026-09-01): PARTIAL -- merge with defect C3.** 22 occurrences; TN-apm-defect-register C3 records a job can be functionally successful yet state:failed under this code. The state semantics need the C3 decision; census alone cannot close it.

<!-- census:begin -->
- **class**: `no-execution-evidence`
- **multiplicity**: 1
- **window**: 2026-08-21T08:23 -> 2026-08-21T08:23
- **most often**: `codex-bell-noev` <- `http-caller`
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

- Defect C3 in TN-apm-defect-register reaches a decision (state semantics fixed, or the class reclassified as expected) — check: `/home/joe/code/futon3c/holes/technotes/TN-apm-defect-register.md` C3 row no longer shows repair `none` **Not met.**
- This ticket's disposition follows that decision explicitly — check: a dated line below `census:end` in this ticket saying closed/merged per C3 **Not met.**
- If fixed: no further functionally-successful jobs report terminal-code `no-execution-evidence` — check: `GET /api/alpha/invoke/jobs` filtered on that terminal-code shows no successful-but-failed cases after the fix, recounted in the ticket **Not met.**
