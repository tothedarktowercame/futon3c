# E-apm-halftime-pre-go-live-C — "Format and parser", as handoff packets

**Written 2026-08-13/14 by claude-2** alongside the B and D packets, at Joe's
request that B/C/D be elaborated so they are ready to process once A is
settled.

Section **C** of the locked list (`E-apm-halftime-pre-go-live.md`). Decomposes
C1–C2; not new items. Verified against live files on 2026-08-14.

**Read first:** `futon3/README-flexiarg.md` is the spec of record. The rule
that matters here is that flexiarg components are **strictly nested by
indentation**, and that the five required components (context/IF/HOWEVER/
THEN/BECAUSE) plus `! conclusion:` are a closed set, while **sub-components
are an open vocabulary**.

**Dispatch order:** C1 first — it builds the conformance corpus that C2's fix
must then pass. Sending C2 first means fixing a divergence with no test that
can detect the next one.

---

## C1 — the two flexiarg parsers disagree, and nothing detects it

There are two independent implementations and no shared test.

| | builds the tree by | evidence |
|---|---|---|
| `futon3/contrib/flexiarg.el` | an **indent stack** | `flexiarg--parse-node-line` returns `(indent node)` (l.313–325); the parser keeps `(stack '())` of `(indent . node)` and pops while `(>= indent (caar stack))` (l.347–368) |
| `futon3a/src/futon/flexiarg/projection.clj` | **discarding indentation** | only whitespace trimming — `trim-empty-lines` (l.35), `str/trim` (l.49), `str/trimr` (l.92). No indent stack, no depth tracking. |

Per-implementation tests exist (`futon3/test/elisp/flexiarg-test.el`,
`futon3c/test/futon3c/watcher/projections/flexiarg_test.clj`,
`futon3a/test/futon/flexiarg/`) but **none is shared**, so the two can drift
apart silently and did.

Partially mitigated in the current ingest (the canonical seven are selected by
name, so nesting is not consulted) — but the divergence stands, and anything
that *does* consult structure gets a different tree depending on which parser
ran.

**Goal.** A single conformance corpus that both implementations must pass, so
the next divergence fails a test instead of reaching the store.

**Acceptance.**
- A corpus of flexiarg files with **expected parse trees**, in one place, in a
  language-neutral form (EDN or JSON — not an Elisp structure and not a
  Clojure structure, or one implementation becomes the reference by accident).
- Both `flexiarg.el` and `projection.clj` run against it.
- The corpus **must include the currently-divergent cases** — start from the
  12 files carrying a nested sub-component (see C2) and the CT six.
- It is fine, and expected, for this to **fail on landing**: the corpus is
  the instrument, C2 is the repair. Report which cases fail rather than
  adjusting the corpus until it passes.

**Explicitly out of scope.** Do not change either parser in this packet. The
deliverable is the instrument.

**Existing task note.** The locked list's "Make the CT six a golden
conformance corpus for both parsers" is this packet; the CT six are a starting
set, not the whole corpus.

---

## C2 — `COUNTERFACTUAL` is flattened to a peer of the five

**Verified: 12 flexiarg files carry a `COUNTERFACTUAL` sub-component.**

Real nesting, `futon3/library/pattern-discipline/pattern-to-code-receipts.flexiarg`:

```
    + EVIDENCE: …                (4-space — child)
    + MECHANISM: …               (4-space — child)
    + COUNTERFACTUAL: …          (4-space — child)

  + BECAUSE: …                   (2-space — one of the five)
```

`COUNTERFACTUAL` is a **sub-component nested under a parent**, exactly like
`EVIDENCE` and `MECHANISM` beside it. Because `projection.clj` discards
indentation, it is promoted to a peer of the five in projection output — so a
consumer reading projection output sees a component the spec does not define
at that level.

**Goal.** Projection output preserves nesting depth, so a sub-component stays
a sub-component.

**Acceptance.**
- `COUNTERFACTUAL` in the 12 affected files projects as a **child of its
  parent**, not a peer of the five.
- `EVIDENCE`, `MECHANISM` and any other sub-component behave identically —
  **fix the general case, not the one keyword**. Sub-components are an open
  vocabulary (README-flexiarg §3a); special-casing `COUNTERFACTUAL` by name
  reproduces the bug for the next sub-component someone invents.
- The C1 conformance corpus passes for both implementations afterwards.
- The canonical seven facets used by `ingest-flexiarg!`
  (`:conclusion :context :if :however :then :because :next-steps`,
  `futon3c/src/futon3c/watcher/file_ingest.clj:1330`) still resolve exactly
  as they do today — **the flexiarg tests must still pass 37/37**, and a
  re-ingest of the Baldwin file must still produce 1 pattern + 7 clauses +
  7 relations and no counterfactual clause.

**Blocked on C1.** Without the corpus there is no way to show the fix is
general rather than a patch that happens to satisfy 12 files.

**Known adjacent item, do NOT fold in.** The locked list's "BLOCKER B —
flat-to-tree clauses is a cross-repo contract migration" is the *downstream*
consumer change. C2 is the projection fix only. Keep them separate; a packet
whose goal contains "and" is two packets.

---

## Gates for every packet in this file

`clj-kondo` 0 errors/0 warnings; `futon4/dev/check-parens.el` on Lisp and
Clojure; `git diff --check`; the flexiarg tests (37 assertions) and the
deletion/rename tests (33 assertions).

**Bell `claude-2` back with a summary + commit shas.**

Do **not** restart the futon1b substrate (~6m30s boot) — none of this work
needs it.
