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

## Decision

**2026-11-07 (wm-author): interruption is an expected terminal class, not a defect.**

Evidence, from the code that produces the terminal-code:

- `futon3c.transport.http/classify-terminal` (src/futon3c/transport/http.clj,
  ~line 2124) maps an error message containing "invoke interrupted" to the
  status `cancelled` with terminal-code `invoke-interrupted` — the transport
  itself classifies it as a cancellation, not a failure; the `failed` bucket is
  a different branch of the same cond.
- The message originates from a deliberate interrupt action, not a crash:
  `futon3c.agents.zai-api` reports `:action :interrupt-issued` /
  "zai invoke interrupted; turn stops at the next round boundary", and
  `interrupt-job-worker!` (src/futon3c/transport/http.clj, ~line 2997) is the
  designed worker-interrupt path used by job cancellation.

So each occurrence is an operator- or system-initiated stop at a designed
boundary. No fix is owed; occurrences should recur only when someone actually
interrupts an invoke.

## Census-exemption note (equivalent exemption)

`invoke-interrupted` stays IN the census on purpose: `scripts/failure-tickets.py`
excludes nothing by fiat ("nothing is excluded by fiat" — see its docstring;
even `operator-cancelled` is not filtered). This note is the ticket's
equivalent exemption record: the class is an expected terminal, tracked for
recurrence only. It demotes itself to PERCEIVE if ignored, and if occurrences
appear without a matching interrupt action, that contrast IS a defect and this
decision should be revisited.

## Completion criteria

- The decision 'interruption is a defect' or 'interruption is an expected terminal class' is recorded below `census:end` — check: a dated decision line in the ticket file **Met.**
- If expected: the class is census-exempted like other expected terminals — check: `scripts/failure-tickets.py`/`failure-census.py` excludes `invoke-interrupted`, or an equivalent exemption note in the ticket **Met.**

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

- The decision 'interruption is a defect' or 'interruption is an expected terminal class' is recorded below `census:end` — check: a dated decision line in the ticket file **Met.**
- If expected: the class is census-exempted like other expected terminals — check: `scripts/failure-tickets.py`/`failure-census.py` excludes `invoke-interrupted`, or an equivalent exemption note in the ticket **Met.**
- If defect: a fix lands and occurrences stop — check: `GET /api/alpha/invoke/jobs` filtered on terminal-code `invoke-interrupted` shows no growth after the fix, recounted in the ticket **Not met.**
