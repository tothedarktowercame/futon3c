# P3-2: read-only dispatch graph

Implements `futon3c.agency.dispatch-graph`. No new evidence, grants, dispatches,
harness tags, registrations, server changes or JVM reloads. Local fixture/report
files are the only new records in this packet.

## Source and identity findings

`social/coordination_ledger.clj` builds `:coordination/:mesh-edge` evidence.
HTTP `transport/http.clj:create-invoke-job!` (~1690) supplies **job-id** as
`:edge/id`, and `agency_send.py` (~208) supplies `caller` from `--from`. The
synchronous `invoke-with-edge!` wrapper instead allocates `mesh-edge-UUID`.
Therefore edge/id equals job-id for the HTTP jobs here, not universally. The
normalised `:job-id` preserves that literal key; an unjoinable wrapper edge ends
with typed missing/unattributed evidence, never a fabricated Agency job.

Only `:invoke` creates a dispatch. A matching `:invoke-result` can supply :ok?;
it is not a second dispatch. Today's captured invokes have no result rows, so
:ok? is nil (unknown), not true. Missing/blank from is retained as `"unknown"`.
Names identify the recorded sender, not authenticated authority or a grant.

## Query contract

- `edges` takes `{:evidence rows :jobs jobs :as-of instant :coverage ...}` and
  returns normalised vectors with from/to/at/job-id/surface/ok?/source. The source
  is the mesh evidence id. Input may also be an evidence sequence alone.
- The vector metadata retains the bounded job context for `upstream`. If it is
  removed by JSON serialisation or other processing, attribution is unavailable,
  not guessed. Reconstruct from the raw context to preserve the query basis.
- `upstream` follows exactly one caller job whose `[started-at,finished-at]`
  includes the edge instant; running jobs with no finish are bounded by as-of.
  Terminal jobs lacking a finish do not get invented infinite intervals.
  Zero candidates returns :unattributed; multiple candidates returns :ambiguous
  with candidate IDs. Duplicate dispatch rows, cycles, and depth256 have typed
  stops. Every result says :bounded-reconstruction, not stored causality.
- Operator/parked-resume text is not invented from sender names. Without an
  explicit join to such an act, the walk ends :unattributed; no keyword/prompt
  heuristic manufactures an operator origin.
- `dispatched-by` and `dispatched-to` use actual direction and `[from,to)`.
- `read-records!` performs sequential LIST reads from futon1b, with one
  system-as-of pin and paginated tag/event-time filters. It does NOT use the
  non-temporal by-id route. Agency jobs are a separately observed current,
  bounded snapshot (limit1000), not a historical system-as-of API. Coverage is
  returned explicitly. Refused/incomplete reads throw, never become empty graphs.

## Real answer, 2026-09-27 capture

Evidence pin **2026-09-27T22:18:11.900276Z**, midnight through that pin.
All pages complete, 586 mesh records, all :invoke. **586/586 = 100% have a named
caller** (none blank/unknown/http-caller). Of these, 170 are `turn-capture` and
114 `auto-bellback`: **284/586** are those explicitly named harness senders.
The other 302/586 (51.54%) carry other sender names; this is not a claim they
have resolved parent jobs or grants.

Agency list was capped at1000; earliest retained created-at was
2026-09-26T03:59:21.309706323Z. This covers the displayed day's recent jobs but
cannot prove there were no older still-running jobs outside the retained set.
All attribution conclusions are explicitly bounded by that captured input.

P3-1 upstream:

```
job: invoke-1790546585343-25693-c50aba00
claude-17 -> codex-5
at: 2026-09-27T22:03:06.439281775Z
surface: bell
source: e-1a7e216e-960a-423b-ab6d-16975b3f0efd
status: :unattributed
reason: :no-running-caller-job
basis: :bounded-reconstruction
```

No captured claude-17 running interval contains that dispatch time. We therefore
cannot join it to an operator or parked-resume act. The direct dispatch itself
is evidenced; its preceding cause is not resolved.

## Direction correction to the acceptance wording

The requested packet list is **dispatched-by claude-17**, not dispatched-to.
For 19:00 through the capture pin, the relevant edges are:

| Packet | Job | Direction |
|---|---|---|
| P13b | invoke-1790539946049-25644-91e630d2 | claude-17 → codex-4 |
| P14 | invoke-1790540799674-25653-e4b9b785 | claude-17 → codex-4 |
| P0 first pass | invoke-1790545572299-25685-258cc9ab | claude-17 → codex-5 |
| P0 second pass | invoke-1790546083866-25690-f025814e | claude-17 → codex-5 |
| P3-1 | invoke-1790546585343-25693-c50aba00 | claude-17 → codex-5 |

The full `dispatched-by` result has nine rows (also P6o-3, P5, P13a and this
P3-2 request). `dispatched-to claude-17` returns one row:
`invoke-1790539003417-25635-c9c4175e`, **codex-5 → claude-17**, the prompt-line
provider-seam coordination request. Completion text is not reclassified as a
new reverse dispatch to force the requested direction.

Raw local capture: `/tmp/p3-2-capture/{mesh,jobs}.json`; executed query output:
`/tmp/p3-2-query.edn`. The committed eight-job fixture is a small verbatim mesh
cut with only job timing/identity fields retained (not large prompt/results).

## Test evidence

The positive cause case uses unmodified real `codex-4 → p5-hash-probe` job
`invoke-1790539177774-25638-6b49179b`, contained in caller job
`invoke-1790538588909-25630-83088148`. Shifting just its edge time to 1ns before
that caller's start yields :unattributed and removes its cause-job-id. Tests
also cover overlapping synthetic copies of a real job, absent caller jobs,
blank caller preservation, cycle guard, lost metadata, incomplete terminal
times, and result edges (including false :ok?) not duplicating dispatches.

Validation: clj-kondo 0 errors/warnings; check-parens OK; only
`futon3c.agency.dispatch-graph-test` ran, **7 tests / 22 assertions, pass**.
The implemented `read-records!` was also exercised live on the narrow
22:03:00–22:03:10 window, then `edges`/`upstream`: same P3-1 dispatch and
:unattributed result (pin22:22:25.171283417Z), saved at
`/tmp/p3-2-live-reader.edn`. No temporal by-id reads, store writes or reloads.
