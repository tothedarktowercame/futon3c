# T-wm-turns-are-not-operator-turns — the WM's ticks get none of the treatment operator turns get

**Opened:** 2026-08-27 · claude-13, from Joe: *"my point was to try and align the
WM with operator-facing considerations … WM 'turns' are not stored in quite such
a durable or queriable or annotatable fashion."*

**Status:** open. Diagnosis complete and measured; the repair is specified below
and not started.

## The finding

Operator turns came out of the C1 exercise clean — 20 of 20 stored, and every
reported loss turned out to be my own instrument. The asymmetry is that the War
Machine's ticks were never in that régime at all.

| | operator turn | WM tick |
|---|---|---|
| **where** | Evidence Landscape (futon1b, XTDB, bitemporal) | `futon2/data/wm-trace/*.edn`, 53 flat files |
| **durable** | append-only store; survived a transcript rewrite this afternoon | **gitignored** — `.gitignore:46` is `data/*`; one host, no replication, no history |
| **queryable** | HTTP API filtered by `session-id`, `author`, `tags`, `since`; JSON on `Accept: application/json` | open the file and parse EDN, and **the shape differs per day** — one file opens `{:timestamp …}`, the newest opens `{:habit-prior-state …}` |
| **annotatable** | `:evidence/id` per record and `:evidence/in-reply-to` threading, so a later record can point at an earlier one | **no per-tick id**; the newest file is 829 KB and holds two `:timestamp` occurrences, so there is nothing stable to attach an annotation to |
| **last written** | continuously | **2026-07-21 10:05** |

**Bounds.** The store survey is over the most recent 1000 entries, in which no
`wm-tick`-shaped event appears. The gitignore line, the file dates and the
per-file shapes are direct reads and are not window-limited.

## Why this is a ticket rather than a note

**The WM's analogue of C1 is false by construction.** *"Every WM tick is stored
in the Evidence Landscape"* cannot hold, because ticks are not written there at
all. So the end-to-end criteria in `M-formal-war-machine` §3.1d can be stated for
the operator half of the loop and not for the machine half — the wrong way round
for a paper whose subject is the machine.

And the deeper reason, which is what makes it worth doing rather than worth
noting: **an apparatus that cannot be re-measured cannot be corrected.** Three
extraction bugs in my operator-side measurement were all recoverable, because
operator turns have ids, a query surface and a stable schema — I could re-query
and find each one. The same class of error against `wm-trace` would not be
recoverable; there is nothing to re-query against.

## The repair

**Three of the four rows are one change.** Durability, queryability and
annotatability all follow from writing a tick as an evidence record with an id,
rather than appending to a day file. This is not a new mechanism: it is the one
the operator side already uses, and `family-fired`, `mission-sync-snapshot`,
`memory-pull-use` and `process-alert` show non-conversational events already
living in the store comfortably.

### Acceptance

1. A WM tick is written to the Evidence Landscape as a typed event with its own
   `:evidence/id`.
2. The tick body has a **declared shape** — the same fields every day. Where a
   field is absent it is typed as absent, not omitted, so a schema change is
   visible rather than silent.
3. Ticks are retrievable by the same filtered query surface operator turns use.
4. **The criterion becomes computable**: `every WM tick is stored in the Evidence
   Landscape` can be measured the way `c1_turn_survival.py` measures its operator
   analogue, and reports typed attrition rather than a bare count.
5. Parse the store as JSON (`Accept: application/json`). Do not scrape EDN with
   regexes — that produced three separate false findings in one afternoon, each
   of which read as a defect in the pipeline rather than in the instrument.

## Split off deliberately — do not bundle

**No tick has been recorded since 2026-07-21**, thirty-seven days. Whether the
loop ran without tracing, or did not run, is **not established here**, and the
two have very different consequences for what the paper may claim. That question
should be answered *before* the writer is repaired: the repair is pointless if
nothing is calling it, and misleading if something is.

## Related

- `futon2/holes/missions/M-formal-war-machine.md` §3.1d — end-to-end criteria.
- `p4ng/empirics-futon/NOTE-red-ring-findings-vs-paper.md` — the claims audit.
- `futon2/scripts/c1_turn_survival.py` — the operator-side measurement.
- `futon3c/holes/excursions/E-R14-red-ring-fill.md`, `E-R8-red-ring-fill.md`.
