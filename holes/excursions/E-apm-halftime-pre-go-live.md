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

- **A1 — 1,213 surplus `pattern/library` hyperedges.** ✅ **EXPLAINED
  2026-08-13 (claude-2).** Safe to delete; every one is redundant.

  **Why 1,213.** Every id has the shape `hx:pattern/library:<ns>/<name>` with
  a single endpoint (the pattern name), and all 1,213 are **distinct**. So
  this is *one hyperedge per pattern name*, not a duplication artefact — it
  is the legacy representation of the whole pattern library, and 1,213 is
  simply the library's size at the moment that writer was retired. The live
  index (`data/notions/patterns-index.tsv`) has 1,359 rows, so the 146
  patterns authored since then never got a hyperedge.

  **Redundancy (the deletion-safety question).** 1,211 of 1,213 resolve to a
  live entity of the same name. Content is duplicated for all of them:
  | where the content lives | n |
  |---|---|
  | entity `:entity/props` | 1,183 |
  | separate `pattern/clause` entities (all 7 facets present) | 28 |
  | **only on the hyperedge** | **0** |
  Nothing in `futon3c/src`, `futon3c/scripts` or `futon1b/*.clj` queries
  hyperedges by `pattern/library`, confirming the "nothing reads them" claim.

  **The 2 with no entity are exactly today's two re-filings** —
  `math-informal/verify-universal-property` (moved to `math-informal-CT`) and
  `math-strategy/clarification-meta` (deleted). So the delete/rename orphan
  sweep removes the entity and its relations but **not** the legacy hyperedge.
  Self-resolving: nothing writes these any more, so deleting all 1,213 ends it.

  *Method note — the 28 nearly became a false "data-loss risk". They look like
  bare stubs because their parent entity has no `:props`; their content is in
  `pattern/clause` entities (15,637 of them). Classifying on the parent alone
  would have reported 28 patterns about to be destroyed. Both document shapes
  have to be checked, every time.*
- **A2 — 284 legacy `code/v05/pattern-slot` records.** Patterns as code,
  superseded by the new ingest but not removed. Verified 284.
- **A3 — ~5 entity rows per pattern.** ~~5,000 rows / 1,194 distinct names;
  811 names duplicated.~~ **Those numbers are void — see A3.1.** Still
  unknown whether real duplicates or bitemporal versions; that distinction
  decides everything. Gates the re-filing.
  - **A3.2 — `entities/batch` does not deduplicate WITHIN a batch**
    (found 2026-08-13, measured). `build-entity` → `ensure-entity-id`
    resolves an existing row by querying the node, but nothing in the batch
    is committed while the items are being built — so N copies of one name
    in one batch each miss the lookup and each get a fresh UUID. Measured:
    the same name sent 3× in one batch returned **3 distinct ids** and wrote
    3 rows. Across separate calls idempotency is intact (verified: 3 writes,
    1 row). The flexiarg caller sends 8 distinct names so it is not hit
    today, but any future batch caller with a repeated name silently
    multiplies rows — the exact failure A3 exists to clean up.
  - **A3.1 — the entity endpoint cannot enumerate its own largest type**
    (found 2026-08-13 while working A1). `entities-query` takes only a
    `limit`, has **no cursor**, and `max-result-limit` is **5000** — but
    there are **5,876** `pattern/library` entities. Its `:count` is the
    *returned* row count, so a full request comes back silently short and
    looks complete. Every A3 figure above was read off a truncated
    `limit=5000` page and must be re-measured before A3 can be worked.
    (Contrast `hyperedges-query`, which has an `after` cursor *and* whose
    `:count` is the true type total — that is why A1 could be answered
    exactly and A3 cannot yet.)
- **A4 — `math/*` buckets exist only in the store.** Seven coarse buckets
  (`missing-dependency-protocol`, `measure-integration-api`,
  `holomorphic-disk-api`, …) with no files and no `library/math/` directory.

## B. Retrieval is not wired

*Elaborated into dispatch-ready packets: `E-apm-halftime-pre-go-live-B.md` (2026-08-14).*
**HELD until A is done (Joe, 2026-08-14) — for coherence.**

**✅ B2/B3/B4/B5 DONE 2026-08-14 (ams-codex-1), verified by claude-2.**
futon6 `ce8182c9`/`d8870f83`, futon3c `a476af86`.
- **B4 ✅** default `extra_library_dirs` is now `()`. Verified live: default
  pool 1,358 patterns, **0 staging files**. Quarantine now quarantines.
- **B5 ✅** collisions surface and BOTH rows are retained. Verified live:
  `read_index_rows()` returns 1,358 and keeps **two** `f3/p0` rows —
  "Portal Query Layer" *and* "MUSN Coordination Substrate". Silent last-wins
  is gone. (Re-keying the source rows remains Joe's decision, untouched.)
- **B2/B3 ✅** whole-index retrieval over all 1,358 valid rows (the file's
  1,359 lines include a header). Widening the pool 30× dropped raw-scorer
  recall to **7/22**; the IDF/stopword stage restored it to **15/22**.
  *Correction to this list: the packet quoted "~16/22" as the baseline. The
  harness docstring pins the honest number at **15/22** ("was 16/22; dropped
  to 15 when the 3 CAS-0 patterns were added"), so 15/22 across a 30× wider
  pool is a match, not a regression.* Verified by running the module directly:
  `test_tier0_retrieval_recall_is_honest` passes.
- **B1 — DISCOVERY ONLY, as required.** Three options documented (reviewed
  attachments / separate direct pattern retrieval / hybrid nomination-warrant);
  **no attachment edges created** — verified independently: `memory/assert`
  census still **372**, unchanged. Awaiting Joe's ruling.

- ~~**B1 (original) — patterns and memories are disjoint taxonomies.**~~ 3 of 76 math
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

*Elaborated into dispatch-ready packets: `E-apm-halftime-pre-go-live-C.md` (2026-08-14).*
**HELD until A is done (Joe, 2026-08-14).**

- **C1 — ✅ DONE 2026-08-14 (ams-codex-2), verified by claude-2.** Shared
  language-neutral JSON corpus (`futon3/test/fixtures/flexiarg-conformance.json`)
  now runs against BOTH parsers: 7 CT fixtures + all 12 nested-component files.
  It failed on landing exactly as the packet required — Clojure failed all 8
  full-tree cases by promoting children to roots — and it also caught a
  **reversed unwind comparison in the Emacs reference parser**, i.e. the
  instrument found a bug in the implementation that was supposed to be the
  reference. That is why C1 had to precede C2.
- **C2 — ✅ DONE 2026-08-14 (ams-codex-2), verified by claude-2.** General fix,
  not a keyword patch: `section-header-re` now captures leading whitespace and
  `sections-at-level` rebuilds the indentation-defined tree recursively.
  Verified by claude-2: no `COUNTERFACTUAL` string in either parser;
  futon3a 41 tests/169 assertions; futon3c flexiarg 37; and the live ingest of
  `pattern-to-code-receipts.flexiarg` (a real nested file) yields 1 pattern +
  7 clauses + 7 relations with exactly the canonical seven facets and **no
  counterfactual clause** — the sub-component stays nested.
  Commits: futon3 `b2dbf35a`/`4dd3ad65`/`27c0bae9`,
  futon3a `a9ab6d1b`/`10234247`/`98d33138`.
- ~~**C1 (original) — the two flexiarg parsers disagree.**~~ `contrib/flexiarg.el` builds
  the tree with an indent stack; `futon3a/.../projection.clj` discards
  indentation. No conformance test. Mitigated in ingest (canonical seven
  selected by name) but the divergence stands.
- **C2 — `COUNTERFACTUAL` still flattened** to a peer of the five in
  projection output.

## D. Substrate residuals

*Elaborated into dispatch-ready packets: `E-apm-halftime-pre-go-live-D.md` (2026-08-14).*
**HELD until A is done (Joe, 2026-08-14).**

**✅ D1/D2/D3 DONE 2026-08-14 (codex-3, futon1b `3e1b0d2`), verified by claude-2.**
- **D3 ✅** `scripts/pre-restart-check.py` + `/api/alpha/restart-readiness`.
  Every number byte-labelled (the TN's trap). **Fails closed**: verified live —
  status endpoint unreachable → `restart_safe=false`, **exit 2**.
  *(Chicken-and-egg: the endpoint needs a restart to exist, so until then the
  check always says unsafe. Correct direction — it refuses to certify what it
  cannot measure.)*
  *claude-2 self-correction: I first reported exit 0 and called it a defect. I
  had piped through `head`, so `$?` was head's status. The script was right.*
- **D1 ✅ and correctly SPLIT** — temporal-bearing routes only (entity,
  hyperedge, relation, evidence), exactly the group specified; pure-literal
  responses left for a later mechanical pass. JSON path now converts Java
  temporals to ISO strings. **The EDN fast path is intact** — verified by
  reading: `(if (string? body) body (pr-str body))`, no round-trip, so the fix
  that ended the outage survives.
- **D2 ✅** boot now waits **boundedly** for indexing quiescence then builds
  once. Timeout env-overridable, 500 ms poll, and it **throws loudly**
  (`:indexing-quiescence-timeout`) rather than hanging — an unbounded wait
  would have converted a crash into a hang. Validated against a **real 20,000
  document backlog** on a temporary node, not a quiescent one.
  *Honest gap, stated by codex-3: the live substrate was not restarted, so
  there is no before/after production cold-boot number yet. Current reference
  points: 379 s from a 4.3 GB log, 28 s from a quiet store.*

Verified by claude-2 by running, not reading reports: `test-temporal`
**16 tests / 65 assertions**, `test-a3a4a5` **69/69**, and the live
pre-restart check's exit code.

- ~~**D1 (original) — `respond!`'s JSON path still parses.**~~ A JSON-requested response
  whose EDN does not read back still fails. Real fix is callers passing maps,
  ~30 sites.
- **D2 — the boot gate retries rather than waits.** 500 expensive projection
  builds is a blunt instrument; waiting for indexing quiescence then building
  once is correct.
- **D3 — no pre-restart check exists.** Comparing `du -sb log` against the
  processed offset would have made the 36-minute outage a warning.

## E. Ownership and hygiene

*Batched into one packet file with the other section: `E-apm-halftime-pre-go-live-EF.md` (2026-08-14) — Joe: "both relevant to creating a working lab for the second half of the APM project".*

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

*Batched into one packet file with the other section: `E-apm-halftime-pre-go-live-EF.md` (2026-08-14) — Joe: "both relevant to creating a working lab for the second half of the APM project".*

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
- **the type catalog has no retraction path.** `documents/retract` does not
  permit `:type-catalog`, so every probe type ever registered stays forever.
  24 of 242 entries are probe/test litter (`probe/*`, `vrfy/*`, `h1/*`) from
  the 2026-08-13/14 performance work — mine and codex-4's. Harmless (no entity
  carries these types, nothing reads them) but it only accumulates. codex-4
  correctly refused to bypass the API boundary to clean it.
