# FM-001b: SAT result for n=8 (q=15, 30 vertices)

**Task**: f3-sat-verify
**Date**: 2026-03-10
**Result**: INCONCLUSIVE (solver timeout)

## Instance

- n=8, V=30, edges=435
- CNF (pysat Tseitin encoding): 146,160 vars, 353,248 clauses
- Z3 (native PbLe): 870 cardinality constraint pairs, 435 edge variables

## Solver Runs

| Solver | Method | Time | Conflicts | Result |
|--------|--------|------|-----------|--------|
| glucose4 (pysat) | Tseitin CNF | >10min | — | timeout |
| cadical153 (pysat) | Tseitin CNF | >10min | — | timeout |
| Z3 4.16 | native PbLe | 300s | — | unknown |
| kissat (compiled from source) | Tseitin CNF | 294s | 2,611,120 | unknown |

## Kissat Details

- Preprocessing eliminated 68% of variables (99,436 of 146,160)
- 782M propagations at 2.66M/sec
- 63,654 restarts, 248 reductions
- Peak memory: 111 MB
- After 5 minutes, ~21% of variables remaining unresolved

## Analysis

The n=8 instance is beyond what CDCL solvers can handle in minutes.
For comparison:
- n=5 (18 vertices, 153 edges): solved by kissat in seconds
- n=6 (22 vertices, 231 edges): unsolved (CNF built, ~432KB compressed)
- n=7 (26 vertices, 325 edges): unsolved (CNF built, ~836KB compressed)

The scaling is roughly exponential in vertex count. n=8 would likely need
hours to days of compute.

## Recommendations

1. **Longer run**: Re-queue with 60-minute or 24-hour kissat timeout
2. **Structural symmetry**: The encoding treats all 30 vertices as
   interchangeable. Adding graph automorphism breaking (e.g., via BreakID
   or Shatter) could dramatically reduce the search space.
3. **Hybrid approach**: Use the Z3 cardinality encoding (435 edge vars, no
   Tseitin helpers) with a longer timeout — Z3's internal reasoning about
   PbLe may be more efficient than the CNF expansion.
4. **Focused search**: Instead of searching all graphs, search within
   structured families (e.g., vertex-transitive, Cayley on Z_15×Z_2).

## Interpretation

The timeout is NOT evidence that R(B_7,B_8) ≠ 29. It only means the
general SAT approach cannot determine the answer at this scale within
5 minutes. The circulant obstruction (F1) already proved no circulant
witness exists for q=15, but non-circulant witnesses remain possible.
