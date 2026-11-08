# Cascade Live as one pattern cascade: skeleton and holes (2026-11-08)

DRAFT by claude-6 at Joe's direction (2026-11-08): "develop the pattern cascade
that describes the Cascade Live as a whole, so that we can inspect the project
as a whole and reason about it using our familiar design pattern + cascade
formalisms." The War Machine is paused meanwhile (no dispatches).

This file only assembles what already exists and marks what is missing. Every
node below names its source; nothing here is a new claim. Construction follows
`futon3/library/cascade-construction/` (read what exists first) and
`cascades/declared-skeleton` (edges declared in the pattern files, each with
its own because).

## Levels as they exist today

| level | what | count | source | form |
|---|---|---|---|---|
| 0 | apex: the problem | 1 | `futon3c/holes/excursions/apex-thesis.json` (2026-09-06) | JSON, not a pattern |
| 1 | wings: the therefores | 5 | A–D: `futon3/library/workshop/{records-carry-warrant,one-queryable-self-account,feedback-reaches-every-participant,accountable-next-action-choice}.flexiarg` (2026-09-12); V: `notes/commons-map/map.edn` only | 4 flexiargs + 1 map entry |
| 1a | molecules inside wings | 11 | `notes/commons-map/map.edn` `:molecules` | map entries |
| 2 | ways of working: the practice | 13 | `futon3c/holes/excursions/ways-of-working.json` (problem + invariant + probe each) | JSON, pattern-shaped |
| 3 | packets: clusters of open work | 37 (over 105 items) | Giraffe `work-clusters.json`, run 20261106T154554; theses in `DRAFT-cascade-argument-2026-11-06.md` | cluster lists |
| 4 | items: missions, excursions, tickets | 368 live + 202 repair records | Giraffe run 20261108T010609; Plane projects per wing | files |
| — | tests on the whole: Mozilla's questions | 11 | `futon7/holes/argument-graph/apex.edn` | argument graph |

## The ways under the wings

| way | wing | items placed (A1 reading, 109 rows) |
|---|---|---|
| records-carry-warrant | A | 31 |
| inbox-zero | A | 0 |
| operator-turns-logged | A | 0 |
| one-queryable-self-account | B | 11 |
| backlog-understood | B | 1 |
| feedback-reaches-every-participant | C | 9 |
| real-users | C | 1 |
| intents-are-tracked | C | 3 |
| selection-gives-a-reason | D | 14 |
| stays-alive-and-paid | — | 17 |
| eat-your-own-tail | — | 4 |
| status-is-known | — | 0 |
| dispatches-name-an-agreed-item | — | 0 |

## Holes (what stops the cascade being read as one thing)

- **H1. The apex is not a pattern.** It is a question in JSON. As a problem
  (`@flexiarg problems/...`) the wings could name it in `@why`.
- **H2. Wing V has no pattern.** map.edn marks it `:proposed? true` (Joe,
  2026-11-01: "no capability if servers fail or money stops. Sits under A-D").
  Whether V sits beside A–D or under them is open.
- **H3. Four ways have no wing.** stays-alive-and-paid looks like V's way;
  status-is-known and eat-your-own-tail look like they sit across all wings;
  dispatches-name-an-agreed-item could be A or D.
- **H4. Wings A, B and C share their name with a way.** The wing pattern
  states the therefore; the way of the same name states a practice with a
  probe. Two nodes with one name blur which level an item is placed at.
- **H5. No edge is declared in a pattern file.** Wing→apex, way→wing and
  item→way links live in JSON and TSV readings, so none carries a because
  and none can be refused in review.
- **H6. Most items are placed only by wing.** 109 items have a way from the
  A1 re-reading; the rest carry a wing from the earlier sheets or nothing
  (21 "not yet placed").
- **H7. Packets do not line up with wings.** The 37 Giraffe clusters were
  built from dependency readings, not from the wings, and cover 105 items.
- **H8. Nothing answers the Mozilla questions.** No `:answers` edge exists,
  so all 11 tests read failing; 6 are assessed weak.
- **H9. Finished work is outside the cascade.** Section 4 of the argument
  ("what was built first, and why") needs the closed items placed the same
  way; none are.
