# TN — executable meaning of retrieval-probe availability

**Date:** 2026-08-15  
**Scope:** discovery only; no producer or invariant is proposed as implemented.

## Finding

The production path does not currently persist the set of memories that passed
retrieval eligibility before the surfaced cutoff. It persists the query and the memories that
survived cutoff. Therefore the most faithful definition of
`:rprobe/available-ids` — the post-eligibility, pre-cutoff set produced by the actual
index and graph path — is **not computable retrospectively from today's
receipts**. It needs one new recorded field at recall time.

This matters because `derive-trace` merely concatenates
`:rprobe/available-ids` and `:rprobe/retrieved-ids`
(`src/futon3c/apm/cycle_harness.clj:80-95,122-123`), while the validator asks
whether the former is a subset of the latter
(`src/futon3c/apm/preregistration.clj:433-435`). The runtime preregistration
already excludes F7 because its earlier definition equated availability with
retrieval and could never fail (`src/futon3c/apm/preregistration.clj:18-23`).

## 1. What recall records today

### The query and the index operation

The query vocabulary is not raw packet text. `query-keywords` normalizes math
text, removes stopwords, ranks terms using the shipped problem-document-frequency
table, and caps the result (`dispatch_with_recall.clj:302-351`). `recall-query`
round-robins terms from the problem statement, proof outline, and packet, applies
the global cap, chooses a required anchor, and returns the terrain, source terms,
anchor, selected terms, and joined query (`dispatch_with_recall.clj:380-409,
470-566`). The document-frequency table influences query construction but is a
shipped resource, not a per-recall observation.

The live retrieval has two bounded arms. It asks full-text search for pattern or
memory rows and may issue a token-disjunction fallback
(`src/futon3c/peripheral/memory_recall.clj:515-563`). It then validates reviewed
attachments and returns proposal candidates, content matches, lexical seed, and
`:index-as-of` (`memory_recall.clj:564-611`). Dispatch combines those content
matches with memories recalled through problem, subject, and selected-pattern
endpoints and deduplicates them into the vector named `candidates`
(`dispatch_with_recall.clj:937-975`). Receipt statistics may reorder that vector;
the required-term check and `take limit` then determine the surfaced `memories`
(`dispatch_with_recall.clj:982-1043`).

### What survives dispatch

The in-memory recall result includes:

- the full query map, trace id, proposal count, lexical seed, index-as-of,
  ladder rung/query, selected pattern ids and endpoints;
- only the final surfaced `:memories` by default
  (`dispatch_with_recall.clj:1044-1058`).

There is already opt-in analysis instrumentation capable of describing the
complete ranked candidate vector: `pre-cutoff-ranking-audit`
(`dispatch_with_recall.clj:848-882`). But `recall-now` adds it only when
`:include-pre-cutoff-ranking?` is explicitly true
(`dispatch_with_recall.clj:895-897,1056-1058`), and `offered-evidence` does not
copy that field into its persisted body. This audit is only a close scaffold for
the needed observation: it runs before the required-term body check at
`dispatch_with_recall.clj:1027-1043`, so it can include memories that the live
path could not surface.

The offered receipt persists surfaced IDs (and initially empty used IDs),
surfacing route/kind, withholding, ranking audit, recall status, the complete
query map, lexical seed, index-as-of and ladder metadata
(`dispatch_with_recall.clj:1279-1374`). `record-offered!` writes that receipt to
the substrate evidence endpoint (`dispatch_with_recall.clj:1401-1428`). This is
the only durable per-dispatch query record I found; there is no separate query
log. It is written only after Agency supplies a job id
(`dispatch_with_recall.clj:1478-1499`). A dry run prints but does not write it
(`dispatch_with_recall.clj:1466-1477`), and a receipt-write failure is warned
after a successful dispatch rather than rolling the dispatch back
(`dispatch_with_recall.clj:1487-1494`). Thus “every dispatch has a surviving
query” is not guaranteed.

## 2. Candidate definitions of `available`

| Definition | Computable from data persisted today? | Assessment |
|---|---|---|
| **(a) Every reviewed memory in the cycle-open snapshot** | **Yes.** `snapshot-reviewed-memories` records the distinct `:memory/assert` entry IDs in `:snap/memory-ids` (`problem.clj:1054-1076`). A live evaluation on 2026-08-15 returned `{:snapshot-count 377}`. | This measures store eligibility, not query availability. With bounded recall it makes nearly every ordinary cycle fail for failing to retrieve hundreds of irrelevant memories. It is falsifiable but degenerate, and should not be called a retrieval miss rate. The snapshot endpoint also has a hard 5000-row cap and no cursor; the producer refuses a full page rather than silently truncating it (`problem.clj:1054-1070`). |
| **(b) Every memory admitted by the actual index/graph path and required-term check before surfaced cutoff** | **No, not retrospectively.** The broader vector exists transiently as `candidates` (`dispatch_with_recall.clj:954-975`); ranking follows at `:996-999`, and the required-term eligibility check immediately precedes `take limit` at `:1027-1043`. Neither the broad candidate IDs nor the post-eligibility IDs are in the normal recall result or offered receipt. Persisted query, lexical seed and `:index-as-of` do not freeze the graph/projection results or receipt-ranking inputs well enough to reproduce the exact vector. | This is the only candidate that directly answers “the executed index, graph, and anchor path could have returned it.” It is the recommended semantics, conditional on recording the post-eligibility vector at recall time. Recording the broader pre-anchor audit would create false misses for memories the anchor rule makes ineligible. |
| **(c) Every snapshotted memory whose tags intersect persisted query keywords** | **Not faithfully from the frozen record.** Query terms are persisted, and snapshot IDs identify the then-member memories, but the snapshot contains IDs rather than the tags/bodies used for matching. Fetching those entries later can compute a present-day proxy, not a frozen per-recall observation. | It is also not the algorithm's candidate predicate: production search is full-text over evidence plus reviewed projections and graph endpoints, not tag intersection (`memory_recall.clj:519-563`; `dispatch_with_recall.clj:942-966`). Recording tag-match IDs would make this falsifiable, but would measure a new tag heuristic rather than the live retriever. |

The command used to check (a) was:

```sh
bash scripts/proof-eval.sh /dev/stdin <<'CLJ'
(do (require '[futon3c.peripheral.problem :as p])
    {:snapshot-count (count (#'p/snapshot-reviewed-memories))
     :snapshot-first (take 3 (#'p/snapshot-reviewed-memories))})
CLJ
```

It returned:

```clojure
{:ok true,
 :value {:snapshot-count 377,
         :snapshot-first ("e-001b61c3-d4fb-4551-8afa-abc108100d00"
                          "e-01becd15-006b-4d93-beb1-1c0b34fff70a"
                          "e-03d4f995-6912-4893-bcfa-f27ad608d31b")}}
```

## 3. Falsifiability

- **Snapshot universe (a):** failure is possible and, under normal small recall
  limits, effectively inevitable. That proves the predicate can fire but does
  not discriminate good retrieval from bad retrieval.
- **Actual eligible vector (b):** failure is genuinely possible whenever an ID
  that the executed retrieval path admitted through its anchor check is absent from the need-probe result.
  It can detect cutoff/ranking misses without defining availability from the
  result it is checking. This is the useful F7-style predicate.
- **Tag overlap (c):** failure is possible once the matched set is frozen, but it
  tests a parallel tag rule. It can disagree with the production retriever in
  either direction and therefore cannot validate that retriever without a new,
  explicit experimental claim.

The tautological definition — `available-ids := retrieved-ids` — is rejected:
the subset check is true by construction, exactly the defect documented at
`preregistration.clj:21-23`.

## 4. Recommendation and rough costs

**Recommendation:** define availability as the deduplicated IDs that pass the
actual recall path's index/graph retrieval and required-term eligibility check,
after ranking but before surfaced cutoff, and do not reinstate F7 until that vector is persisted with
the offered receipt and linked to the cycle's retrieval probe.

Rough costs:

1. **Persist (b): small implementation and storage cost.** The vector already
   exists and an audit formatter for the broader pre-anchor vector already
   exists. Recording the eligible IDs plus cutoff/ranking/index metadata is a
   local instrumentation change; no new store scan or retriever is required.
   Post-eligibility/pre-cutoff is canonical: ranking changes order, while the
   required-term check changes whether a memory can be returned at all.
2. **Use (a): near-zero implementation cost, high validity cost.** Copying the
   real snapshot IDs into a probe is easy, but the resulting always-red check is
   not a retrieval-quality measurement.
3. **Use (c): moderate read/instrumentation cost, high interpretation cost.** It
   requires freezing tag-match IDs (or tags for all 377 snapshot entries) and
   maintaining a second matching rule whose relationship to full-text and graph
   recall must itself be validated.

Until option (b) is recorded, the honest executable state is: retrieved IDs and
query metadata are available, but a non-tautological, retriever-faithful
`:rprobe/available-ids` is not.

---

## Review — claude-2, 2026-08-15

**Accepted.** Document only (`src/` and `test/` untouched, 150 insertions in one
file). 24 file:line citations and one live command with its output.

**Three citations spot-checked by opening the file at the line.** All landed:
`dispatch_with_recall.clj:954-975` really is the deduplicated candidate set;
`:1487-1494` really is the offered-receipt write; `offered-evidence` at `:1279`
really persists `surfaced-memory-ids` and the ranking audit and *not* the
candidate vector.

**One challenge, which the document survives.** `:withheld-memory-ids` IS
persisted in the receipt, and I expected `surfaced ∪ withheld` to reconstruct the
pre-cutoff set with no new instrumentation — which would have contradicted the
recommendation. It does not: `apply-withholding` (`dispatch_with_recall.clj:1120`)
takes a *caller-supplied ablation list*, an experimental knock-out, not the
candidates that lost at ranking. The conclusion stands.

**Falsifiability, the question that decides it.** The document's three-way split
is right and is the reason to accept (b):

- (a) snapshot universe — falsifiable but **degenerate**: a cycle fails for not
  retrieving 377 irrelevant memories, so it cannot discriminate good retrieval
  from bad;
- (b) actual eligible vector — **falsifiable and discriminating**: a cycle fails
  exactly when the executed path admitted an id that the need-probe did not
  return, which is a real cutoff/ranking miss;
- (c) tag intersection — falsifiable but measures a **parallel rule**, not the
  live retriever.

And the tautology `available := retrieved` is refused explicitly, for the reason
F7 was dropped.

### Decision for round 1: leave the gap open

Recommended to the operator, not adopted unilaterally.

1. Building (b) means recording a new field **inside `dispatch_with_recall.clj`**
   — that is the retrieval harness, and the harness is pinned for the round
   (`:reg/harness-revision`). It can be re-pinned pre-launch, but it is a change
   to the instrument in the week we start measuring with it.
2. F7 is already absent from round 1's `:runtime-invariants`
   (`[:F2 :F3 :F4 :F5 :F6 :F8 :F9]`), so nothing in the round is waiting on it.
3. `:need-retrieval` should **stay** in `:required-capabilities`. It is
   unprobeable, F9 reports that as `:f9-capability-probe-missing`, and that
   refusal is the honest record. Removing it to let a cycle close would convert
   "not measured" into "satisfied" — the exact move this peripheral exists to
   prevent.

So round 1 closes refused, by design and on the record, and (b) is the round-2
build with its cost already scoped.
