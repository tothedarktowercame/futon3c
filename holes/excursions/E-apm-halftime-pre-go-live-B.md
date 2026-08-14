# E-apm-halftime-pre-go-live-B — "Retrieval is not wired", as handoff packets

**Written 2026-08-13/14 by claude-2 at Joe's request:** *"B, C, D … might need
some elaboration for clarity before they are sent out … elaborate B, C, D into
handoff packets so they are ready to process when we have figured out A."*

Section **B** of the locked list (`E-apm-halftime-pre-go-live.md`). These
decompose B1–B5; they are **not** new items. Every number below was
re-verified against the live store and the live files on 2026-08-14 — where it
differs from the locked list, the corrected figure is marked.

**Dispatch order:** B4 first (it is a one-line correctness fix and it changes
what every other retrieval measurement sees). Then B5, then B2/B3 together,
then B1 last — B1 is a design question, not a bug, and should not be sent as
if it had an obvious answer.

---

## B4 — staging directories are wired into the retrieval pool

**Smallest and most urgent: quarantine is not quarantining.**

`futon6/scripts/cas_select.py`:

```python
DEFAULT_STAGING_DIRS = (                       # line 36
    FUTON3C / "data" / "pattern-staging" / "slice-1",
    FUTON3C / "data" / "pattern-staging" / "slice-3",
)
def load_all_patterns(..., extra_library_dirs = DEFAULT_STAGING_DIRS, ...)   # line 207
    files.extend(sorted(f for d in extra_library_dirs if d.is_dir()
                        for f in d.glob("*.flexiarg")))                       # line 240
```

So **unassayed staging candidates are retrievable by default**. Any retrieval
measurement taken so far may have been scoring against patterns that were
never assayed.

**Goal.** Staging directories must not be in the default retrieval pool.
Callers that genuinely want them must opt in explicitly.

**Acceptance.**
- `load_all_patterns()` with no arguments returns **only** library patterns.
- An explicit `extra_library_dirs=` still works, so the staging workflow is
  not broken — this is a change of default, not a removal.
- A test asserting a staged-only pattern is absent from the default pool.
- Report how many patterns the default pool gained/lost, so we know whether
  earlier retrieval numbers were inflated.

**Gates.** `futon6/tests/test_cas_select.py` must pass; state the count.

---

## B5 — index rows silently dropped by qualified-name collisions

**Corrected figure: 26 collisions, 26 rows lost** (locked list said 24).

`data/notions/patterns-index.tsv` has 1,359 rows keyed on column 0
(`pattern`), and 26 keys appear twice. Real example, both rows present in the
live file:

```
f3/p0  lukin  习  Portal Query Layer -> …
f3/p0  kin    双  MUSN Coordination Substrate -> …
```

Same key, entirely different patterns. Anything that loads this file into a
dict keyed on column 0 keeps one and silently discards the other. This is why
a kernel query once returned "MUSN Coordination Substrate".

**Goal.** No index row may be silently lost.

**Acceptance.**
- Loading the index either preserves all 1,359 rows under a genuinely unique
  key, or **fails loudly** naming the colliding keys. Silent last-wins is the
  defect; do not replace it with silent first-wins.
- A test with a deliberately colliding fixture asserting the loud behaviour.
- Report the 26 colliding keys so Joe can decide whether the *source* rows
  should be re-keyed (that decision is his, not the implementer's).

**Note for the implementer.** Do not "fix" this by editing the TSV — the
collision may be legitimate data that needs a better key. Fix the loader's
silence first; re-keying is a separate decision.

---

## B2 + B3 — Tier-0 indexes 45 of 1,359, and scores on stopwords

These two are one packet because they are the same file and the same function.

**B2 — the family-prefix filter.** `cas_select.py`:

```python
FAMILY_PREFIX = "math-informal"                                    # line 32
def in_family(qualified, family_prefix=FAMILY_PREFIX):             # line 52
    return family_prefix is None or qualified.split("/",1)[0].startswith(family_prefix)
```

Verified live: **45 of 1,359** index rows are `math-informal*`. Tier-0 sees 3%
of the library.

**B3 — the scorer has no stopword or IDF stage.** Line 285:

```python
score = len(hits) + (len(hits) / max(1, len(hot)))
```

Raw hit-count. At whole-index scale this ranks on `the`, `of`, `every`.

**Goal.** Make Tier-0 able to see the whole index without its ranking
collapsing — the two are coupled, which is why they ship together: widening
the pool 30× makes the missing IDF stage bite.

**Acceptance.**
- Tier-0 can index all 1,359 rows; the family filter becomes a caller option,
  not a hardcoded default.
- A stopword/IDF stage such that widening the pool does not degrade ranking.
- **Report both numbers before and after** on the existing recall harness
  (`test_tier0_retrieval_recall_is_honest`, quoted in-file as ~16/22@k4,
  ceiling ~19/22). If recall drops, say so — a widened index that ranks worse
  is a regression, not progress.
- Depends on B4 landing first, otherwise the "after" number is measured
  against a pool contaminated by staging.

**Priority note.** The locked list marks these low priority because there is
**no live consumer**. Do not let this packet grow; it is a correctness
cleanup, not a retrieval project.

---

## B1 — patterns and memories are disjoint taxonomies

**Verified 2026-08-14: 3 of 75 math patterns are named by any memory edge**
(locked list said 3 of 76; it is 75 since `math-strategy/clarification-meta`
was deleted). The three:

```
math-formalization/tactic-algebra-interference
math-informal/monotone-approximation
math-informal/split-into-cases
```

Live store: 372 `memory/assert` hyperedges, 885 distinct endpoints, 23 of them
in any `math*` namespace. Math patterns on disk: 75
(`math-informal` 38, `math-formalization` 18, `math-strategy` 12,
`math-informal-CT` 7).

Recall reaches patterns **only** through `:attachment-status :reviewed` edges
carrying a pattern endpoint. So 72 of 75 math patterns are unreachable by the
only path recall uses. **This is structural, not lexical** — no amount of
scorer tuning reaches them.

**This is a DISCOVERY packet. Do not implement a fix.**

**Goal.** Establish what the relationship between patterns and memories is
*supposed* to be, and what it would cost to wire it — as options with
tradeoffs, for Joe to rule on.

**Deliverable.**
- Whether the two taxonomies are meant to converge, or whether patterns are
  meant to be reached by a *different* path than memory edges (in which case
  B1 is misfiled as a defect and should be re-scoped).
- If they should converge: what creates the edges, who reviews them, and what
  the 72 missing edges would cost to create.
- **Do not mass-create attachment edges.** Generating 72 unreviewed edges to
  make a number go up is precisely the failure this campaign is documenting.

**Context the implementer needs.** `futon3c/docs/pattern-retrieval-architecture.md`
describes the four representations and is current. The actual research line is
**Scribe-extracted rewrite rules mined from Zai self-corrections**, not
patterns — check with Joe before treating pattern recall as the goal.

---

## Gates for every packet in this file

Python: the relevant `futon6/tests/` suite, stating pass counts.
Clojure: `clj-kondo` 0 errors/0 warnings, `futon4/dev/check-parens.el`,
`git diff --check`.
All: **bell `claude-2` back with a summary + commit shas.**

Do **not** restart the futon1b substrate (~6m30s boot) without saying so
first; check `du -sb migration-store-21/log` beforehand per D3.
