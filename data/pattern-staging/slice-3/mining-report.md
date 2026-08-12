# Slice 3 stage-2 audit of persisted marks

This continuation used only the 93 files in
`/home/joe/apm-evidence/mining/slice-3/marks/`, as instructed. Their terminal
states are 15 done, 4 failed, and 74 cancelled.

## Stage-2 step 0: quote verification

The persisted files contain zero `QUOTE:` fields, zero `MOVE:` fields, and
zero structured `MARK n` blocks. Therefore there are zero quotes available to
verify and a fabrication count of zero. This is a vacuous zero, not evidence
that the student quoted accurately.

Several done files say in prose that the student “marked” a number of moments,
but preserve only a summary of those marks. The missing QUOTE/MOVE records
cannot be reconstructed from summaries without violating the instruction to
work from persisted marks only.

## Coordination-only count

Six files contain `COORDINATION-ONLY` (seven occurrences): four b97A02 reads
and two b98A03 reads. Because only 15 reads completed, the file-level rate
among completed reads is 6/15 = 40%. This does not support the expected strict-
stratum improvement and is evidence that the classifier or extracted chunks
still admit substantial coordination content.

## Clustering and authoring

No semantic clustering is possible from the required MOVE phrases because no
MOVE phrases were persisted. Consequently no candidate can satisfy the
three-distinct-problem threshold, no dedupe survivor exists, and no flexiarg is
authored.

The previously staged `replace-enumeration-with-structural-counting` file was
removed: it was derived from a different `marks-final/` directory, not from
the 93 mark files placed in scope for this continuation, and is unsupported by
this audit.

There is likewise no admissible evidence in these files for math-side
reinforcement of either slice-1 agency candidate. Any such claim would require
reconstructing absent MOVE records.
