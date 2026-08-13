# Pattern / memory retrieval — architecture, and the reduction to one representation

**claude-2, rewritten 2026-08-13 after three of the first version's claims
turned out to be my own measurement errors. Every fact below carries the
check that established it; where a check could not fail, it is not a fact
and is marked as such. Joe's instruction: "reduce to ONE complete and
coherent representation in XTDB."**

## 0. Corrections to the first version of this document

| First version said | Actually |
|---|---|
| "Two worlds that do not talk" — files/TSV vs XTDB, mined patterns absent from the store | **Wrong.** The watcher ingests every flexiarg into XTDB, completely and currently. There are **four** representations, not two worlds, and the bridge is not the problem |
| "The mined patterns are 404 in the store" | True only of the `:pattern/library` **entity** namespace. As `code/v05/var` hyperedges they are all present |
| "The multi_watcher drops payloads" (reported to Joe, who authorised stop-the-line work on it) | **Wrong.** `source-file-vertices` filters by source-file path and is not a census. Direct id lookup: every pattern checked returns 200 |

The three errors share one shape: a count was taken with a query that was
not measuring the claim. That is now the standing rule for this document —
**no fact without a check that could have failed.**

## 1. The as-is: four representations

```
   ┌── (1) FILES ─────────────────────────────────────────────────┐
   │  futon3/library/<family>/<name>.flexiarg   ~1078 files       │
   │  authoring + review format; git-diffable; what claude-4 reads│
   └───────┬──────────────────────────────────┬───────────────────┘
           │ multi_watcher (RUNNING, 5s        │ build_pattern_index
           │ cycles, roots include futon3)     ▼
           ▼                          ┌── (2) TSV ────────────────┐
   ┌── (3) CODE GRAPH ────────────┐   │ patterns-index.tsv  1360  │
   │ hx:code/v05/var:futon3-d/    │   │ + a divergent 1355-row    │
   │   flexiarg.<family>/<name>   │   │   twin in storage/        │
   │ COMPLETE and CURRENT         │   │ 24 rows silently dropped  │
   │ **NOTHING READS IT**         │   │ read ONLY by cas_select   │
   └──────────────────────────────┘   │ Tier-0 — no live consumer │
                                      └───────────────────────────┘
   ┌── (4) THE ONLY PATH RECALL READS ────────────────────────────┐
   │  :pattern/library entities  (uuid-keyed; coarse buckets too, │
   │      e.g. math/holomorphic-disk-api)                         │
   │            ▲                                                 │
   │            │ pattern endpoint                                │
   │  memory/assert hyperedge  ── must be :attachment-status      │
   │            │                  :reviewed, else EXCLUDED       │
   │            ▼                                                 │
   │  memory evidence row (e-*)                                   │
   └──────────────────────────────────────────────────────────────┘
```

**Check for (3):** direct hyperedge lookup, five patterns spanning May 4 →
today, all `200`:
`hx:code/v05/var:futon3-d/flexiarg.math-formalization/transport-across-an-instance-diamond`,
`…flexiarg.math-informal/transport-across-isomorphism`,
`…flexiarg.pattern-mining/probe-the-claimed-property-not-the-acceptance-proxy`
(created 30 minutes before the check),
`…flexiarg.math-informal-CT/chase-the-diagram`,
`…flexiarg.math-strategy/construct-through-a-finite-correspondence`.
The watcher works.

**Check for (4):** `memory_recall/propose-patterns-by-query` — *"Each memory
match must project through a currently reviewed memory/assert edge in
DOMAIN; pattern proposals additionally require a pattern endpoint on that
edge."* The gate is `memory_recall.clj:45`:

```clojure
(not= :reviewed attachment-status)
(-> acc (update :attachment-excluded inc)
        (cond-> (= :proposed attachment-status)
          (update :proposed-excluded inc)))
```

**Check on the population:** of 372 sampled `memory/assert` edges,
**199 are `:reviewed`, 12 are `:proposed`.**

## 2. The two real defects, restated correctly

**D1 — the watcher's complete ingest is on a limb nothing reads.**
Representation (3) is current and complete; the recall path is (4). No
process reconciles them. Every flexiarg in the store is invisible to
recall not because it is missing but because it is the wrong shape.

**D2 — `:proposed` edges are excluded from recall, and the tool that
promotes them is broken.** The three a94A09 memories carry `:approve`
review evidence written 2026-08-12 and their edges are still `:proposed`,
because `review_codex_lane_attachments.clj` hardcodes `claude-10` as
reviewer, hardcodes a foreign session id, and hardcodes `:verdict
:approve` so it cannot express a rejection. **Cleanup item 7 is therefore
not housekeeping — it is the reason reviewed memories never reach
recall.** 12 edges sit behind it.

Note what this means for the mining campaign's whole output: even if a
pattern were the right shape, an approved memory attached to it cannot
project until that script is fixed.

## 3. The reduction: ONE representation

```
   futon3/library/**.flexiarg          ← AUTHORING ONLY (git, review, diff)
              │
              │  multi_watcher   (already running; already complete)
              ▼
   ┌─────────────────────────────────────────────────────────────┐
   │  XTDB — the single pattern representation                   │
   │                                                             │
   │   pattern entity, stably keyed, one per flexiarg:           │
   │     :qualified  "math-formalization/transport-across-…"     │
   │     :family :domain :grade :status                          │
   │     :title :hotwords :conclusion :however :provenance       │
   │     :source-file                                            │
   │                                                             │
   │   memory/assert edges point AT this entity                  │
   │   recall reads it through the existing gate                 │
   └─────────────────────────────────────────────────────────────┘
              │
              ▼
   patterns-index.tsv  ← DERIVED EXPORT, or deleted. Never authored,
                         never hand-edited, never a second source.
```

**The single representation is the XTDB pattern entity.** Everything else
becomes either an input (files) or an output (TSV), never a peer.

Concretely, that means:

1. **The watcher becomes the only writer.** It already runs, already
   covers futon3, already parses every flexiarg correctly (verified: the
   projection returns the right var with `:pattern/id` and qname). What
   changes is its *target shape* — it should maintain the pattern entity
   that recall's edges point at, rather than a code-graph var vertex that
   nothing consumes.
2. **`code/v05/var` for flexiargs is retired or becomes a projection of
   the entity**, not a parallel object. Flexiargs are not code; modelling
   them as vars is what put them on the unread limb.
3. **The coarse buckets get resolved.** `math/holomorphic-disk-api` and
   `math-formalization/transport-across-an-instance-diamond` are currently
   different kinds of thing both called "pattern". Ingest must decide:
   are the buckets patterns, tags, or a third thing? **This is the one
   genuine design question in the reduction** and it is not mine to settle
   alone.
4. **The TSV stops being a source.** Generated on demand or dropped; the
   divergent `storage/` twin archived. This dissolves the 24 dropped rows
   and the symlink class outright rather than fixing them.
5. **`:domain` is added metadata on a working ingest** — the scoping
   predicate that keeps process patterns out of math runs — not a new
   mechanism.

## 4. Proofs — what must pass before this is called done

| # | Proof | Must currently FAIL |
|---|---|---|
| P1 | A known flexiarg round-trips: edit the file → within one watcher cycle the pattern entity reflects the edit | yes — no such entity exists |
| P2 | Every flexiarg on disk has exactly one pattern entity; count reconciles or every difference is reported | yes |
| P3 | The four mined content patterns are retrievable under `:domain :mathematics`; the process pattern is NOT — both asserted in one test | yes |
| P4 | A `:reviewed` memory/assert edge whose pattern endpoint is a mined pattern projects through recall | yes — blocked by D2 |
| P5 | **Canary (Fable's loopback probe):** a pattern deposited for a specific problem surfaces in a dispatch of that problem | yes |
| P6 | No second writable copy of the index exists anywhere under `/home/joe/code` | yes |

**P5 gates everything downstream.** No assay, no tide test, no slice, no
case loop until a canary surfaces. Joe's CT prelim problem is the natural
vehicle: `library/math-informal-CT/` already holds six patterns
(`chase-the-diagram`, `check-it-on-generators`,
`compare-universal-properties`, `factor-and-lift`,
`strictify-via-coherence`, `transpose-across-an-adjunction`), so a
hand-written CT problem can be scoped to hit exactly them, with the right
answer known in advance.

## 5. Ordering

D2 (`review_codex_lane_attachments.clj`) is **independent and blocking**:
12 edges cannot project until it is fixed, and P4 cannot pass. It is small
and it should go first regardless of how the reduction is sequenced.

Then the reduction (§3.1–3.5), then P5.

## 6. Carried forward from the previous version, unchanged

§ **Programme fit.** This is a Phase 0 instrument under the rule *an
instrument is Phase 0 only if the next measurement's DV flows through it*
(Fable). The next measurement's DV **does** flow through the recall path —
so D2 and the reduction qualify. Tier-0/`cas_select` does **not** — which
is why its stopword, IDF and vocabulary defects are explicitly out of
scope here.

§ **P3 of the V3 programme is the live danger.** If waves run while
patterns are unreachable, a flat recall-empty rate reads as "the
attachment-layer conjecture dominates; lexical/scribe work is misdirected"
when the true cause is plumbing. P5 is the countermeasure.

§ **Process vs content is a measurement requirement.** P2 of the programme
turns on the regulative/substitutive ratio, so a math runner surfacing a
process pattern would be a miscounted regulative hit. Hence
`library/pattern-mining/` and `:domain` scoping. The sort was executed
2026-08-13; note it did **not** by itself change retrievability.

§ **Seven candidate process patterns** from this week are recorded with
provenance in the git history of this file and deliberately not authored;
two are at threshold. They should be mined blind by a seat that has not
seen the table.
