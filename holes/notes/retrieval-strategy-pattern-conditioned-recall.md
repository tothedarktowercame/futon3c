# Retrieval Strategy: Pattern-Conditioned Memory Recall

Date: 2026-07-23
Author: zai-3 (at Joe's direction)
Status: **Strategy proposal for review**

## The Problem (confirmed live)

Memories are stored but not operationally retrievable. Verified:

1. **9 `:memory/assert` hyperedges exist** — endpoints are `[entry, Mathlib, apm/a94A06, session]`. **None has a pattern endpoint.**
2. `psr_search` queries the futon3a notions-index (a TSV file), not the evidence store. It returns pattern candidates but has no knowledge of attached memories.
3. `psr_select` records discipline state but does not retrieve memories.
4. `memory_search` queries evidence by type/author/tags/subject — but the agent must independently decide to call it with a good query.
5. The boot packet has no memory projection.

**Conclusion: the system has storage, not operational memory.** A memory surfaces only if the agent independently calls `memory_search` with the right query — which it has no instruction or trigger to do at the decision point.

## The P1 Architecture: Pattern-Conditioned Recall

Joe's framing is correct: a memory remains a concrete episode; a pattern becomes its reusable retrieval handle. The key workflow is:

```
current task → psr_search → candidate pattern → memories attached to that pattern
→ psr_select returns pattern + relevant memories → agent applies and cites evidence id
```

Five rungs, in dependency order:

### Rung 1: Pattern endpoints on memories (hyperedge side)

**What**: Each `:memory/assert` hyperedge should include a `:pattern` endpoint in addition to its existing `[entry, subject, session]` endpoints.

**How**: Two paths:
- **Agent-side** (P0): `memory_record` already accepts subjects with `{:ref/type :pattern :ref/id "..."}`. But the agent doesn't know which pattern to cite — it would need to `psr_search` first, then reference the pattern id. This works but places the burden on the agent.
- **Librarian-side** (P2): A post-hoc curation pass that reads each memory body, runs `psr_search` on its hook/body, and attaches the best-scoring pattern as an endpoint via a hyperedge update. This is the scalable path.

**Immediate action**: Backfill the 4 canonical a94A06 memories with pattern endpoints. The mapping is:

| Memory | Best pattern match |
|--------|-------------------|
| `e-6bcbb51e` field_simp poly denom | `math-formalization/tactic-algebra-interference` |
| `e-d9b1739f` HasDerivAt.comp npowRec | `math-formalization/tactic-algebra-interference` |
| `e-3c8277ed` expand composed antiderivative | `math-formalization/tactic-algebra-interference` |
| `e-94028b3f` polyrith unavailable | (no good existing pattern — environment/ops fact) |

All three math memories cluster under `tactic-algebra-interference` — the pattern about "tension between algebraic truth and tactic mechanics." This is the right umbrella: each memory is a concrete instance of that tension.

**Where in code**: The backfill is a one-shot XTDB transaction: for each memory hyperedge, add the pattern endpoint to `:hx/endpoints` and update `:hx/props` to record the pattern role. The `put-verified!` gate stack in `futon1b_graph.clj` is the write primitive.

### Rung 2: Enrich `psr_search` to surface attached memories

**What**: When `psr_search` returns a pattern candidate, include a bounded list (top 3) of memory hooks attached to that pattern via hyperedge endpoints.

**How**: After the existing notions-index scoring produces candidates, for each candidate pattern id, run a bounded hyperedge query:
```
SELECT evidence_id, hook, body_excerpt
FROM memory/assert hyperedges
WHERE pattern_id IN endpoints
LIMIT 3
ORDER BY evidence.at DESC
```

This is a secondary query per candidate — bounded, facet-scoped, riding the existing keyset-cursor read discipline.

**Where in code**: `tool-psr-search` in `real_backend.clj` (~line 486). After building `candidates`, for each candidate, call the futon1b hyperedge query with `{:type :memory/assert :end pattern-id}` and attach `:memory-hooks` to the result map.

**Cost**: N candidates × 1 bounded hyperedge query = O(top_k) queries. At top_k=5, this is 5 bounded queries — well within the permit budget.

### Rung 3: Make `psr_select` the reliable retrieval trigger

**What**: When the agent calls `psr_select` to adopt a pattern, the response should include the **full bodies** of the top 3 memories attached to that pattern.

**How**: In `tool-psr-select` (`real_backend.clj` ~line 530), after recording the PSR, query for memory hyperedges with the selected pattern as endpoint, fetch the evidence entries via their entry-endpoint, and return them in the result map as `:attached-memories`.

**Why this is the right trigger**: The agent is already instructed to call `psr_search` → `psr_select` → `pur_update` as part of pattern discipline. Making memories arrive at `psr_select` means retrieval happens at the decision point where the agent has already committed to applying the pattern — exactly when the concrete experience is most useful.

**Where in code**: `tool-psr-select` in `real_backend.clj`. The futon1b evidence-query API (`memory_backend.clj` `evidence-query`) already supports subject-based filtering. The pattern endpoint becomes a queryable subject ref.

### Rung 4: Record memory use in PSR/PUR

**What**: `pur_update` should carry a `:memory-ids` field recording which memory evidence ids the agent cited when applying the pattern. This makes retrieval-to-application observable.

**How**: Add an optional `:memory_ids` parameter to `pur_update`. Store it in the PUR evidence entry. The acceptance metric (did retrieval lead to application?) becomes: `memory_ids` present in PUR for sessions where pattern-conditioned recall surfaced memories.

**Where in code**: `tool-pur-update` in `real_backend.clj`, the PUR evidence entry shape.

### Rung 5: Boot projection of recent memories (complementary)

**What**: The boot packet should include a bounded projection (top 5-10) of recent memories by author/facet, similar to how `mission_context` composes orientation.

**How**: A projection query over `:memory/assert` hyperedges, ordered by `evidence.at DESC`, limited to the agent's own memories (or all memories for shared ops facts). This is Stage 1 of the M-typed-memories MAP.

**Why complementary, not primary**: Boot projection surfaces recent memories regardless of task relevance. Pattern-conditioned recall (Rungs 2-3) surfaces memories at the decision point. The former helps with broad context; the latter helps with specific decisions.

## P1 Acceptance Test

Joe's criterion, made concrete:

1. A **fresh Lean problem** (e.g., another integral evaluation) is dispatched to a zai agent.
2. The agent calls `psr_search` with a query like "field_simp ring algebraic identity tactic".
3. `psr_search` returns `math-formalization/tactic-algebra-interference` as a candidate, **with `e-6bcbb51e` (field_simp poly denom) as an attached memory hook**.
4. The agent calls `psr_select` for that pattern.
5. `psr_select` returns the **full body** of `e-6bcbb51e` — including the fix (normalize denominator before field_simp).
6. The agent **applies** the fix and **cites** `e-6bcbb51e` in its `pur_update :memory_ids`.

Until this works end-to-end, the system does not have memory in the practical sense.

## What Can Be Done Now (Agent-Accessible Actions)

1. **Backfill pattern endpoints**: Requires XTDB write access or a librarian script. The agent cannot do this directly through available tools — `memory_record` creates new edges but doesn't update existing ones.

2. **Verify the pattern→memory mapping**: Can be done now by checking that `psr_search` returns the right pattern for each memory's hook.

3. **Test the current gap**: Confirm that `psr_search` for "field_simp ring polynomial denominator" does NOT return `e-6bcbb51e` — proving the gap is real.

## Implementation Priority

| Rung | Effort | Impact | Dependency |
|------|--------|--------|------------|
| 1: Pattern endpoints on memories | Small (1 XTDB transaction) | High | None |
| 2: Enrich psr_search with memory hooks | Medium (code change in tool-psr-search) | High | Rung 1 |
| 3: psr_select returns full memory bodies | Small (code change in tool-psr-select) | High | Rung 1 |
| 4: Record memory use in PUR | Small (add :memory_ids param) | Medium | Rungs 2-3 |
| 5: Boot projection | Medium (new projection query) | Medium | None |

Rungs 1+3 are the minimum viable: pattern endpoints + psr_select as trigger. This is ~1 day of work (one XTDB transaction for backfill + one code change in tool-psr-select).

## Live Verification: The Infrastructure Already Exists

After inspecting the code, the key finding is that **the hyperedge query by endpoint
already works**:

```
GET /api/alpha/hyperedges?end=Mathlib&limit=2
→ returns all hyperedges with "Mathlib" in their :hx/endpoints
```

The `hyperedges-query-uncached` function in `futon1b_graph.clj:507` has a dedicated
`:end` branch (line 568+) that unnests endpoints and filters by exact match. This means:

- **Rung 2 is mostly a wiring change**: In `tool-psr-search` (`real_backend.clj:486`),
  after building candidates from the notions-index, for each candidate pattern id,
  call the futon1b API with `{:end pattern-id :type "memory/assert" :limit 3}` and
  attach the results as `:memory-hooks`.

- **Rung 3 is equally small**: In `tool-psr-select` (`real_backend.clj:530`), after
  recording the PSR, call the same endpoint query for the selected pattern and return
  full memory bodies.

The `:type` and `:end` parameters cannot be combined in one query (the function uses
`cond`, with `:type` taking precedence over `:end`). For pattern-conditioned recall,
the `:end` branch is what we need — it finds all hyperedges (of any type) that have
the pattern as an endpoint. If type filtering is also needed, post-filter the results.

### Concrete code change for Rung 2 (psr_search enrichment)

In `tool-psr-search` (`real_backend.clj:486`), after the `candidates` vector is built,
add a memory-hook enrichment pass:

```clojure
candidates-with-memories
(mapv (fn [cand]
        (if-let [pid (:pattern-id cand)]
          (let [mem-result (futon1b-api-call
                            :get "/api/alpha/hyperedges"
                            {:end pid :limit 3})
                hooks (->> (:hyperedges mem-result)
                           (filter #(= :memory/assert (:hx/type %)))
                           (mapv (fn [hx]
                                   {:memory-id (get-in hx [:prop/roles :entry])
                                    :hook (:prop/hook hx)
                                    :kind (get-in hx [:prop/kind])})))]
            (assoc cand :memory-hooks hooks))
          cand))
      candidates)
```

This is ~10 lines of code. The futon1b HTTP call is already available via the
existing backend plumbing.

### Concrete code change for Rung 3 (psr_select returns memory bodies)

In `tool-psr-select` (`real_backend.clj:530`), after the PSR is recorded:

```clojure
;; Fetch full memory bodies for the selected pattern
attached-memories
(->> (futon1b-api-call :get "/api/alpha/hyperedges"
                       {:end pattern-id :limit 3})
     :hyperedges
     (filter #(= :memory/assert (:hx/type %)))
     (mapv (fn [hx]
             (let [entry-id (get-in hx [:prop/roles :entry])]
               {:memory-id entry-id
                :body (evidence-get entry-id)}))))
;; Add to result
{:ok true
 :result {:pattern-id pattern-id
          :psr psr
          :selected? true
          :attached-memories attached-memories}}
```

## Open Questions

1. **Hyperedge update API**: Can existing hyperedges be updated with new endpoints,
   or must they be retracted and rewritten? The `put-verified!` gate stack needs an
   update path. **Answer from code inspection**: hyperedges are XTDB documents; the
   update path is `xt/submit-tx` with a new document version at the same `:xt/id`.
   The `invalidate-hyperedge-query-cache!` call (line 597) must be invoked after any
   mutation. Retraction is also available (`:hyperedges` ∈ retractable-tables).

2. **Pattern id as endpoint**: The notions-index uses ids like
   `math-formalization/tactic-algebra-interference`. These become dangling endpoints
   in the hypergraph (no entity pre-registration needed — XTDB doesn't enforce
   referential integrity). The `?end=` query matches by string equality, so the
   pattern id string IS the endpoint string.

3. **Multiple patterns per memory**: A memory can attach to multiple patterns. The
   backfill for the a94A06 memories would add the same pattern endpoint
   (`tactic-algebra-interference`) to three of the four memories, since all three are
   instances of the same tension (tactic mechanics vs algebraic truth).

4. **Volatile memories and patterns**: `e-94028b3f` (polyrith unavailable) is a
   volatile ops fact (kind: reference). It should NOT need pattern attachment —
   reference memories are retrieved by boot projection or direct search, not by
   pattern-conditioned recall. The `kind: reference` distinction handles this
   naturally.

5. **The `:type` + `:end` cond issue**: `hyperedges-query-uncached` uses `cond`, so
   `:type` takes precedence over `:end`. For pattern-conditioned memory recall, use
   only `:end` (which returns hyperedges of ALL types with that endpoint). If
   type filtering is needed, post-filter in Clojure. Alternatively, extend the
   `:end` branch to also accept an optional `:type` filter.
