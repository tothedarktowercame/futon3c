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

## Results — frozen run 2026-08-03

The run read 522 `:memory` entries. The canonical memory snapshot hash was
`b5fed62aecf0a9fa6cf4a149ec8a7581b96b3053f9fb22ebb259d39bda5572d0` and the
ground-control ranking-receipt snapshot hash was
`25860bf6bcc4a110ee892782c29c3fafe983c380230391f2ea8d1f3b0f5d6bbf`.
Both hashes were unchanged at the end of the run. The frozen result is
`e8-query-binding-20260803.json`, SHA-256
`07be2f39ee48aa38100aaf5ace7b70bcf2660de4681b0d565daedf510ca7b3a2`.

### Reachability control and per-case result

All five cases were scoreable. Every singleton or pair member had a current
reviewed mathematics-domain `memory/assert` path. The set-valued cases were
resolved before scoring:

- Case 2 means **any of the five** current reviewed memories directly attached
  to endpoint `a93J02`; the exact ids are frozen in the JSON.
- Case 3 means **any of the five current** memories attached to
  `math/weak-convergence-hilbert`. Two older members visible in raw projection
  rows were excluded because their edge state is superseded/retracted.
- Case 4 requires **both** named dependency memories; surfacing only one is a
  miss for the pair.

| case | reachable? | A | B8 | B12 | B16 | C | D |
|---|---:|---:|---:|---:|---:|---:|---:|
| a93A03 direction-scoped liminf | yes | miss | miss | miss | miss | miss | hit |
| a93J02 own-memory set | yes | hit | hit | hit | hit | hit | hit |
| a96A03 weak-convergence pattern | yes | hit | hit | hit | hit | hit | hit |
| lib-young missing-dependency pair | yes | miss | miss | miss | miss | miss | miss (1/2 surfaced) |
| a96A04 inventory memory | yes | miss | miss | miss | miss | miss | hit |

Known-item hit rates were therefore:

| arm | hits / 5 | rate |
|---|---:|---:|
| A, shipped take-4 | 2 | 40% |
| B8 | 2 | 40% |
| B12 | 2 | 40% |
| B16 | 2 | 40% |
| C, structure-aware | 2 | 40% |
| D, oracle vocabulary | 4 | 80% |

### Interpretation

The A→B curve is flat and saturates at baseline: **query cardinality is not a
binding constraint under the shipped recall path**. More precisely, every A
case hit the first three-term ladder rung, so merely increasing the builder's
four-term cap cannot affect retrieval: the ladder consumes the same first three
terms and stops. The B results mark this exact equivalence rather than pretending
to have made different backend calls.

C does not beat best-B (40% versus 40%) and changes no case verdict. Vocabulary
still matters, however: D recovers the a93A03 and a96A04 targets, taking the
rate to 80%. Thus the broad claim “the lexical stage is not binding” is too
strong; **cardinality is inert, while oracle vocabulary can be decisive**.

D also provides the preregistered refutation. For lib-young it surfaced
`e-9751e537-f5b7-4c40-a857-0c0b699b93a2` but not
`e-dfea2de9-8979-4f8f-9343-caabb48487e6`, although both passed the reachability
control. That member's failure is downstream of simple query vocabulary and
supports V2 §5.2's attachment/projection rival locally. It does not establish
that rival as the universal bottleneck, because D fixed two other misses.

No case was padded or dropped as unscoreable. Per-arm terms, surfaced ids,
ranks, set sizes, empty flags, Jaccard overlap with A, reachability evidence,
and input hashes are in the frozen JSON.

### Post-hoc refinement (claude-12, 2026-08-03) — what the 40% baseline is made of

Noticed while applying claude-10's absent-vs-drowned discriminator; checkable
from the frozen JSON, no re-run required.

| case | target kind | #expected | A | D |
|---|---|---:|---|---|
| 1 a93A03 | singleton | 1 | miss | **hit** |
| 2 a93J02 | set-valued (any of 5) | 5 | **hit** | hit |
| 3 a96A03 | set-valued (any of 5) | 5 | **hit** | hit |
| 4 lib-young | pair (both required) | 2 | miss | miss |
| 5 a96A04 | singleton | 1 | miss | **hit** |

**Both of arm A's hits are the set-valued cases.** Every case with a
specifically-named target — two singletons and one pair — misses under the
shipped query. So the shipped retrieval path did not once retrieve a *named*
target across these five cases; its 40% is carried entirely by cases where
any-of-five sufficed.

That splits something the aggregate rate conflates: retrieving *a relevant
memory* is not retrieving *the needed one*. Under oracle vocabulary both
singletons convert, which sharpens the vocabulary finding rather than softening
it — the terms decide whether a specific memory is reachable at all, and the
shipped term-selection never got there.

The pair case (4) remains the sole D-failure and remains unresolved between the
attachment and pollution residuals, pending the pre-cutoff-rank instrument.
