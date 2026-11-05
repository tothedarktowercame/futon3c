# T-fail-invoke-error -- invoke-error (2x)

**Status (triaged 2026-09-01): STILL-OPEN.** 2 -> 111 at review (latest 09-01T03:06); now the second-heaviest class. Needs exception-payload triage before a close criterion can be stated.

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

## Completion criteria

- Exception-payload triage of the `invoke-error` occurrences is written below `census:end` — check: the ticket names the distinct exception payloads/classes with counts and a dominant cause **Not met.**
- A close criterion for the class is stated from that triage (fix, exempt, or split into subclasses) — check: a 'What would close this' answer replacing '(unstated)' in the ticket file **Not met.**
- If a fix is chosen: occurrence count for terminal-code `invoke-error` stops growing after it lands — check: `GET /api/alpha/invoke/jobs` filtered on that terminal-code, recounted in the ticket **Not met.**
