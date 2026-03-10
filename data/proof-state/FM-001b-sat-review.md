# FM-001b SAT Encoding Review

**Reviewer**: claude-2 (Mentor)
**Task**: f3-mentor-review
**Date**: 2026-03-10
**Files reviewed**:
- `futon6/scripts/fm001/ramsey_book_sat.py` (SAT encoder)
- `futon6/scripts/fm001/block_circulant_ip.py` (IP encoder)
- `futon3c/scripts/fm001/verify_witness.py` (witness verifier)

## Verdict: SAT encoding is CORRECT

The book-number constraints match the Ramsey definition exactly.

## Detailed Analysis

### 1. SAT Encoder (`ramsey_book_sat.py`)

**Encoding strategy**: For each vertex pair (u,v) and each other vertex w,
introduce helper variables:

- `red_helper(u,v,w)` ⟺ (uv red) ∧ (uw red) ∧ (vw red)
- `blue_helper(u,v,w)` ⟺ (uv blue) ∧ (uw blue) ∧ (vw blue)

Then apply cardinality constraints:
- `atmost(red_helpers, n-2)` — at most n-2 common red neighbors per pair
- `atmost(blue_helpers, n-1)` — at most n-1 common blue neighbors per pair

**Correctness argument**:

- B_k exists in a graph ⟺ some edge has ≥ k common neighbors.
- Red avoids B_{n-1} ⟺ every red edge has < n-1 common red neighbors (≤ n-2).
- Blue avoids B_n ⟺ every blue edge has < n common blue neighbors (≤ n-1).
- When uv is blue, all red_helpers for (u,v) are forced false by the
  conjunction, so the red cardinality constraint is trivially satisfied.
  Vice versa for blue_helpers when uv is red. **Correct.**

**Helper encoding** (`add_helper_equiv`):
- Forward: h → (each literal) via unit implications. **Correct.**
- Backward: (all literals satisfied) → h via single clause. **Correct.**

**Symmetry breaking** (`add_vertex_zero_monotone_edges`):
- Forces edges from vertex 0 to be monotone (red before blue). Valid —
  any satisfying assignment can be permuted to satisfy this. Reduces
  search space without eliminating solutions. **Correct.**

**Verification** (`verify_assignment`):
- Independently checks all C(V,2) pairs, counting red and blue common
  neighbors. Rejects if red_support ≥ n-1 or blue_support ≥ n. **Correct.**

### 2. Witness Verifier (`verify_witness.py`)

- `max_book(A)` = max over edges of common-neighbor count via A²·A. **Correct.**
- B_{n-1}-free check: `bg < n-1`. **Correct.**
- Complement B_n-free check: `bc < n`. **Correct.**
- Complement construction: `1 - A` with diagonal zeroed. **Correct.**

### 3. IP Encoder (`block_circulant_ip.py`)

**Status: Skeleton only.** No book-freeness constraints are encoded.
The docstring and comments acknowledge this ("placeholder objective,"
"initial version only sets up the combinatorial skeleton"). The variable
structure (D11, D22 symmetric; D12 asymmetric with D21 = -D12) is correct
for 2-block-circulant graphs.

The actual IP run (n=11 verbose log) used a different script
(`FM-001b-ip-encoding.py` in futon6) with 2902 binary variables and
8682 constraint rows — this appears to be the complete encoding. The
CBC solver confirmed INFEASIBLE for n=11 circulant, consistent with our
algebraic proof that circulant constructions fail for non-prime-power q.

## Edge Cases

| Case | Status | Notes |
|------|--------|-------|
| n=2 (6 vertices) | Blocked by `n >= 3` guard | Encoding would be correct: bound=max(0,0)=0 means no red triangle, which is correct B_1-freeness |
| n=3 (10 vertices) | Known SAT | Witness exists (n3-witness.json) |
| n=4 (14 vertices) | Known SAT | Witness exists (n4-witness.json) |
| n=5 (18 vertices) | Known SAT | Witness found by kissat 4.0.4 |
| n=6 (22 vertices) | CNF built, unsolved | 432 KB compressed |
| n=7 (26 vertices) | CNF built, unsolved | 836 KB compressed |
| n=11 (40 vertices) | Running (maplesat) | 568K vars, 1.3M clauses |

## Performance Note (not a correctness issue)

The encoding creates helpers for ALL C(V,2) × (V-2) triples, even when
the cardinality constraint is trivially satisfied (e.g., red helpers when
the edge is blue). This produces O(V³) helper variables where O(V² × avg_deg)
would suffice. For n ≤ 11 this is manageable. For n ≥ 23 (88 vertices,
~300K edge pairs × 86 witnesses each) the CNF will be very large. The
status doc already flags n=50 as impractical.

## Recommendations

1. **No changes needed** to the SAT encoding — it is correct.
2. The IP skeleton should either be completed with book-freeness constraints
   or archived, since the full IP encoding lives in a separate script.
3. For n ≥ 23, consider symmetry-aware encodings (e.g., constrain to
   vertex-transitive graphs) to reduce variable count.
