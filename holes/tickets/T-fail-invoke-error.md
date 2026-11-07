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

## Exception-payload triage (2026-11-07, wm-author)

Recount from `GET /api/alpha/invoke/jobs?limit=1000`: **38 jobs with
terminal-code `invoke-error`**, created 2026-11-06T17:05 -> 2026-11-07T05:56
(the August census window is stale; the class is still live).

Distinct terminal messages, with counts:

| count | payload class | terminal-message (abbreviated) |
|------:|---------------|--------------------------------|
| 21 | provider empty response | `Claude Platform response contained no text` |
| 14 | per-call budget cap | `Claude Platform request exceeds per-call budget` |
| 1 | seat requisition guard | 象-1: "You can't use a Kimi seat without a requisition…" |
| 1 | provider overload | `HTTP 429 engine_overloaded_error` |
| 1 | seat requisition guard | kimi-4: same requisition/clock-in guard text |

Dominant edge: caller `ma-occurrence` -> agent `馬-sonnet` carries 35 of the 38;
the remaining three are one-off seats hitting their guards.

**Dominant cause: provider-side Claude Platform behaviour** (empty response +
per-call budget = 35/38), not an Agency invocation defect. The requisition-guard
and 429 cases are policy/overload signals that already surface distinctly at
the seat, not silent swallows.

## Why this file exists

Written by `scripts/failure-tickets.py` from the invoke-job ledger, so a typed
failure lands in the queue without anyone noticing it first. Everything above
the `census:end` marker is regenerated on each run; everything below it is
yours and is never touched.

Editing anything in this section is what moves the class back to BELIEVE: there
is no status field, so a file that nobody has written in is a class that nobody
has responded to.

## What would close this

Split into subclasses and exempt the provider tail (stated from the
2026-11-07 triage above): the 35/38 provider empty-response and per-call-budget
payloads get their own terminal codes (e.g. `provider-empty-response`,
`per-call-budget-exceeded`) so `invoke-error` keeps only genuine invocation
failures. The class closes when a recount of `GET /api/alpha/invoke/jobs`
filtered on `terminal-code=invoke-error` after the split shows the residual
count flat at zero-over-provider-noise for a full census window.

## Completion criteria

- Exception-payload triage of the `invoke-error` occurrences is written below `census:end` — check: the ticket names the distinct exception payloads/classes with counts and a dominant cause **Met.**
- A close criterion for the class is stated from that triage (fix, exempt, or split into subclasses) — check: a 'What would close this' answer replacing '(unstated)' in the ticket file **Met.**
- If a fix is chosen: occurrence count for terminal-code `invoke-error` stops growing after it lands — check: `GET /api/alpha/invoke/jobs` filtered on that terminal-code, recounted in the ticket **Not met.**
