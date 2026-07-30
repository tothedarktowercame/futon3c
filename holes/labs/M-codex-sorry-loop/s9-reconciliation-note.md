# S9 statement reconciliation — census note

Date: 2026-07-30
Auditor: codex-4

## Coverage

All 66 rows whose status was `:untouched` in
`data/codex-sorry-queue.edn` were compared against their Lean bundle,
the original `problem.md`, and (where present) `informal-solution.md`.
The report's row-id set exactly matches the queue-derived untouched set.

Nothing remains uncovered. No Lean file was edited, no proof was attempted,
and no evidence-store request or write was made.

## Verdicts

| Verdict | Count |
|---|---:|
| `:ok` | 57 |
| `:suspect-translation` | 4 |
| `:suspect-informal` | 0 |
| `:suspect-exam` | 1 |
| `:cannot-assess` | 4 |
| **Total** | **66** |

## Highest-risk rows

1. **a01J06 — translation defect, high confidence.** `ProblemData.hEntire`
   is `True`, the zeros are arbitrary, and `zeroCount` is constant zero.
   Those placeholders erase the entire-function and zero-counting content
   required by the exam.
2. **a96A02 — translation defect, high confidence.** The intended
   triangular-spike construction is defined as the zero function. In
   particular, the asserted non-absolute-continuity conclusion cannot hold
   for that definition.
3. **a95J04 — translation defect, high confidence.** The intended bounded
   Taylor coefficients are replaced by the disconnected proposition
   `∃ M, 0 ≤ M`.
4. **a95J03 — translation defect, high confidence.** `windingNumber` is
   defined as zero, so the formalization cannot express the exam's
   winding-number theorem even though the currently queued algebraic helper
   is locally meaningful.
5. **a01A09 — exam defect, high confidence.** The original uses
   `exp(+tz)` while claiming analyticity in the right half-plane for general
   `Lp` data. The informal solution diagnoses the sign error and the Lean
   formalization uses the standard `exp(-tz)` kernel.

The four BPM rows (`bpm-1-1-2`, `bpm-1-3-2`, `bpm-1-7-1`,
`bpm-1-8-1`) are `:cannot-assess`, not suspected defects: each lacks an
`informal-solution.md`, so there is no independent intended-theorem artifact
with which to complete the requested three-way reconciliation.

## Mechanical-check summary

- No additional dropped section-level proposition was found.
- No additional measure `.toReal` hypothesis lacking a finiteness premise
  was found.
- No additional Euclidean-domain theorem was encoded using the Pi sup norm
  on `Fin n → ℝ`. The use in a02J01 is coordinate indexing for product
  integration, and a95A03 uses `EuclideanSpace`.
- No additional pointwise strengthening of an almost-everywhere intended
  statement was found.
- Semantic-strength checks produced the four translation findings above and
  the one exam-sign finding; no informal-solution defect was asserted.

The row-level evidence, confidence, and citations are in
`s9-reconciliation.jsonl`.
