# E-apm-halftime-pre-go-live — the locked problem list

**LOCKED 2026-08-13 by Joe. 24 open items, verified against live state at
time of writing. This list is CLOSED.**

## The discipline (read before adding anything)

Joe, 2026-08-13: *"let's LOCK these as a list, so we don't generate more
findings. It is very stressful to get extra lists of problems when we are
trying to solve a current list."*

Therefore:

1. **Do not add top-level items.** The list is A1–F5 and stays A1–F5.
2. If something genuinely new is found while working an item, it becomes a
   **sub-step of that item** (`D2.2`), and only if it blocks that item.
3. If it blocks nothing on this list, it goes in **§Parked** at the bottom,
   unnumbered, and is not worked.
4. A finding that is not blocking is not a task. Write it down and move on.
5. Tick items off in place with a date and how it was verified. Do not
   delete them — the record of what was fixed is what stops it being
   re-discovered.

## Already fixed today — do NOT re-find these

| what | verification |
|---|---|
| `respond!` round-tripped every response through `edn/read-string` | 3 entities + type index back to 200 |
| Boot gate allowed 5 attempts against an indexing backlog | `FUTON1B_PROJECTION_BUILD_ATTEMPTS`; clean boot 6m31s |
| `safe-q` rethrew the retryable pgwire cached-plan conflict | boot passes |
| `retractable-tables` excluded `:relations` | relation retract returns 200 |
| Review script hardcoded reviewer / session / `:verdict :approve` | 2 edges moved to `:reviewed`, reject stayed `:proposed` |
| Flexiargs modelled as code vertices | ingest writes pattern + 7 clauses + 7 relations |
| Watcher never removed patterns on delete/rename | both live orphans now 404 |
| `clj-kondo` not installed anywhere | v2026.08.04, gate runs |
| 62 commits unpushed on a feature branch | fast-forwarded to `origin/master` |
| Probe litter in the store | 8 retracted, 1 superseded by marker |

---

## A. Store contains surplus or wrong-shaped data

- **A1 — 1,213 surplus `pattern/library` hyperedges.** From the abandoned
  re-typing. Nothing produces or reads them. *Owed first: an explanation of
  why there are 1,213, by diffing hyperedge ids against entity ids.* Verified
  1213.
- **A2 — 284 legacy `code/v05/pattern-slot` records.** Patterns as code,
  superseded by the new ingest but not removed. Verified 284.
- **A3 — ~5 entity rows per pattern.** 5,000 rows / 1,194 distinct names;
  811 names duplicated. **Unknown whether real duplicates or bitemporal
  versions — that distinction decides everything.** Gates the re-filing.
- **A4 — `math/*` buckets exist only in the store.** Seven coarse buckets
  (`missing-dependency-protocol`, `measure-integration-api`,
  `holomorphic-disk-api`, …) with no files and no `library/math/` directory.

## B. Retrieval is not wired

- **B1 — patterns and memories are disjoint taxonomies.** 3 of 76 math
  patterns are named by any memory edge. The rest are unreachable through the
  only path recall uses. *Structural, not lexical.*
- **B2 — Tier-0 indexes 45 of 1,359.** Family-prefix filter. No live
  consumer, so low priority.
- **B3 — Tier-0 scorer has no stopword/IDF stage.** Ranks on `the`, `of` at
  whole-index scale. Same caveat as B2.
- **B4 — staging directories are wired into the retrieval pool.**
  `cas_select.load_all_patterns` defaults `extra_library_dirs=DEFAULT_STAGING_DIRS`,
  so unassayed candidates are retrievable. Quarantine is not quarantining.
- **B5 — 24 index rows silently dropped** by qualified-name collisions
  (`f3/p0` is both "Portal Query Layer" and "MUSN Coordination Substrate").

## C. Format and parser

- **C1 — the two flexiarg parsers disagree.** `contrib/flexiarg.el` builds
  the tree with an indent stack; `futon3a/.../projection.clj` discards
  indentation. No conformance test. Mitigated in ingest (canonical seven
  selected by name) but the divergence stands.
- **C2 — `COUNTERFACTUAL` still flattened** to a peer of the five in
  projection output.

## D. Substrate residuals

- **D1 — `respond!`'s JSON path still parses.** A JSON-requested response
  whose EDN does not read back still fails. Real fix is callers passing maps,
  ~30 sites.
- **D2 — the boot gate retries rather than waits.** 500 expensive projection
  builds is a blunt instrument; waiting for indexing quiescence then building
  once is correct.
- **D3 — no pre-restart check exists.** Comparing `du -sb log` against the
  processed offset would have made the 36-minute outage a warning.

## E. Ownership and hygiene

- **E1 — 86 uncommitted files in `futon3/library/`.** No owner. Verified 86.
- **E2 — 198 uncommitted entries in `futon3c`,** including the deleted
  `.clj-kondo/config.edn`. Verified 198.
- **E3 — ground-control README forked.** 1,170 lines in `futon6/` vs 1,147 in
  `apm-evidence/docs/`; contents differ.
- **E4 — `patterns-index.tsv` twin.** Live repo copy (1,359) vs orphaned
  storage copy (1,355). **Restoring the symlink would silently drop every
  pattern authored since 2026-08-10.**
- **E5 — `a94A09/status.json` says `partial-lean-proof`, 1 sorry** for a
  problem closed by `a266157`. Any status-derived corpus percentage is
  understated. Audit needs a comment-aware detector.

## F. The experiment itself

- **F1 — zero assays have ever passed.** The one that ran was invalid: no
  headroom, and the arms shared a session.
- **F2 — no pre-registration written** for the commissioning test.
- **F3 — `glue-census` (6,114 steps) never analysed.** It holds the one
  positive result on record: a hand-derived `closure_ball` rewrite found
  verbatim at two sites in `a95A05` by plain-English query at 0.77 cosine.
  *Closest item to the actual research question.*
- **F4 — no interim report on memory use.** The receipts support one — 610
  rows, 242 surfacing dispatches, 86 with a use, 54 distinct memories used,
  94 problems — but "to what effect" needs a join not yet done.
- **F5 — `:attribution-incomplete` gate reliability unknown.** This is the
  gate that falsely indicted a seat over a backtick in a regex.

---

## Decisions blocking work (not problems)

- `construct-through-a-finite-correspondence` — filed `math-strategy`, is
  construction content
- grade on `close-bijectivity-by-counting-not-inverting` — synthesis said
  snippet, reads as technique
- the ~40-move re-filing manifest (`library/MANIFEST-math-split-proposal.md`)
  — approved in principle, gated on A3

## Parked (found, not blocking, NOT to be worked)

- `math-strategy` is almost entirely meta rather than mathematics
- three `math-strategy` entries are result-type taxonomies, not moves
- the polynomial zero-count bridge was re-derived three times (a92J05,
  a97A08, a94A09) and is absent from the store
- `LEMMA-INDEX.md` is the only place the trapped-lemma recurrence is visible
