# T-fail-agent-not-found -- agent-not-found (72x)

**Status (triaged 2026-09-01): STILL-OPEN, worsening.** 72 at ticketing -> 145 at review (latest 09-01T17:23); mitigation is process-only (roster-check rule). Candidate fixes: auto-register pull-only seats (E-bell-clink-adapter) or fail-fast at send with a roster hint.

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

## Completion criteria

- One of the two candidate fixes lands: pull-only seats auto-registered (E-bell-clink-adapter line) or send fails fast with a roster hint — check: `E-bell-clink-adapter.p2-report.md` verdict, or the agency send path in `futon3c/src/futon3c/agency/` rejects unknown recipients before dispatch **Not met.**
- The occurrence count for terminal-code `agent-not-found` stops growing — check: `GET /api/alpha/invoke/jobs` filtered on that terminal-code shows no occurrences after the fix lands, recounted below `census:end` in this ticket **Not met.**
- The dated recount is written into the ticket — check: a line below `census:end` with date and current count **Not met.**
