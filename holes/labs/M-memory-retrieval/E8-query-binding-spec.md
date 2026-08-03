# E8 — is the retrieval query a binding constraint?

**Frozen data. No runners, no dispatches, no tokens.** Opened 2026-08-03 by
claude-12, on Joe's go-ahead.

## The question

`dispatch_with_recall.clj` builds the retrieval key as a bag of **at most four
frequency-ranked words** (`text-keywords` → stopword removal → frequency sort;
then `(take 4)` at line ~366, introduced `6d9c3c5f`, 2026-07-30).

V2 falsified two *lexical mechanisms* — term rarity and pairwise co-occurrence —
and inferred from their failure that the bottleneck lies past the lexical stage,
at the attachment layer (§5.2, explicitly labelled "Conjecture, not result").
Both falsified mechanisms concern **properties of terms**. Neither concerns the
**cardinality and selection rule of the query**. So a third lexical-stage
explanation survives V2's falsifications untested.

This matters because the V3 cohort dispatches through this builder, and
E-memory-resourcing-and-strategy §4 makes C1 shape the cohort's arms: if the
attachment layer is the bottleneck, the treatment is populated-graph vs
star-forest. **If the query is the binding constraint instead, populating the
graph yields a null for reasons unrelated to attachment — an expensive null,
paid for in fresh backlog problems.**

## Design

Known-item retrieval, holding store / ranking / projection fixed and varying
**only** query construction.

### Labelled cases

These were labelled *contemporaneously by the loop* in
`holes/labs/M-zai-learning-loop/cohort-2-ops-log.md`, not selected post hoc by
the analyst. Each is a documented miss with a named expected target.

| # | problem | expected target | contemporaneous diagnosis |
|---|---|---|---|
| 1 | a93A03 | `e-30e87097-f843-4341-81c0-a49ee7ce0ef4` (direction-scoped liminf) | S4: "NOT surfaced … v1.2 normalization firing but drowned by TeX fragments + packet boilerplate — **pollution not absence**" |
| 2 | a93J02 | that problem's own memories | S5: "recall-empty — own-terrain miss" |
| 3 | a96A03 | `weak-convergence-hilbert` pattern | "did NOT surface; Liouville pair surfaced instead" |
| 4 | lib-young-completion | missing-dependency pair `e-dfea2de9…` / `e-9751e537…` | S1b: "no memories surfaced (no problems/ dir → packet-only terms)" |
| 5 | a96A04 | `e-9751e537-f5b7-4c40-a857-0c0b699b93a2` | S6 (2026-08-03): predicted to surface under Ψ-weighting, did not |

All seven referenced memory ids were confirmed present in the store on
2026-08-03 (`type=memory` pull, 522 entries).

### Arms — query construction only

- **A (baseline)** — the shipped builder, `(take 4)`.
- **B (cardinality sweep)** — identical, with `(take 8)`, `(take 12)`, `(take 16)`.
- **C (structure-aware)** — terms drawn from mathematical identifiers in the
  Lean/TeX statement (declaration names, `Mathlib` module paths, operator names)
  rather than prose word frequency.
- **D (oracle)** — query built from the *expected target memory's own* name and
  body terms.

### Why D is the arm that matters

**D is the refutation arm.** If the expected target does not surface even when
queried with its own vocabulary, the failure is downstream of the query — in
attachment or projection — and V2 §5.2's conjecture is supported against my
hypothesis. Report D honestly whichever way it falls; a design that cannot come
out against the analyst is not an experiment.

### Required control: reachability vs retrievability

Before scoring any case, establish for each expected target whether it *could*
surface at all: does it carry a current `memory/assert` edge, with reviewed
attachment to a pattern the projection can reach? A target that is unreachable
under **any** query is a V2 §5.1 reachability failure, not a query failure, and
must be reported in that column rather than counted as a query miss.

### Measurements

Per (case × arm): whether the expected target appears in the surfaced set, and
at what rank; surfaced-set size; empty-recall (yes/no); Jaccard overlap of the
surfaced set against arm A.

Aggregate: known-item hit rate by arm; the A→B curve (does hit rate rise
monotonically with term count, and where does it saturate?); C vs best-B.

## Acceptance bar

- Deterministic and re-runnable: same inputs → byte-identical output. Freeze the
  result to `holes/labs/M-memory-retrieval/e8-query-binding-<date>.json` with a
  sha256, and record the store's entry count at read time (the store is live;
  say what you read).
- **No writes to the evidence store. No dispatches. No agent invocations.**
  Read-only throughout.
- Reuse the existing recall machinery (`futon3c.peripheral.memory-recall`,
  `futon3c.dispatch-with-recall`) rather than reimplementing retrieval — an
  ablation that reimplements the thing it ablates measures the reimplementation.
  Vary the query by parameterising the existing path.
- Do **not** modify the shipped `(take 4)` default. Add a parameter with the
  current behaviour as its default, or drive the internals from the analysis
  script.
- Report the reachability control separately from the hit rates.
- State every case where the labelled target is ambiguous (case 2 names "that
  problem's own memories" as a set, not an id — resolve it explicitly and say
  how).

## Gates

`clj-kondo` 0 errors 0 warnings on any Clojure touched; `futon4/dev/check-parens.el`;
existing test suites still green; no serving-JVM reload.

## Interpretation, written before the result

- Hit rate rises materially from A to B → **the query is binding**; C1's arm
  design needs revisiting before the cohort registers.
- Hit rate flat across A→B→C but D surfaces the targets → the query is not the
  binding constraint at the *margin* tested, though the vocabulary still is.
- D fails too → the bottleneck is downstream, **V2 §5.2 is supported**, and the
  populated-graph arm is the right treatment after all.
