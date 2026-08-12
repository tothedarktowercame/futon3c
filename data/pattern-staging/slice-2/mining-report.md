# Slice 2 math-stratum synthesis

## Read accounting

The requested 12 IDs contain 269 `stratum=math` chunks in
`prose/index-strata.tsv` (not 945). They were initially grouped into 72
four-chunk packets. Thirteen completed and 59 were rejected by zai-1 with
HTTP 400 / code 1261 (`Prompt exceeds max length`).

The 59 rejected packets were split into 112 two-chunk retries. All 112 were
also rejected with the same prompt-length error. A concurrent resume after the
first dispatcher outlived its command timeout created 15 duplicate retry jobs;
these were identified from the jobs ledger and persisted as well. Total reads
dispatched: 199, preserving the <=200 cap. Terminal outcomes: 13 completed and
186 failed for prompt length. All 199 terminal results are persisted under
`/home/joe/apm-evidence/mining/slice-2/marks/`.

The completed reads produced 14 `MARK` records and 8 `COORDINATION-ONLY`
results. No `NOTHING-SURPRISED` result occurred. Successful coverage was only
two distinct problems: b97A02 (9 reads) and b98A03 (4 reads). Consequently no
semantic cluster can satisfy the required threshold of three distinct
problems, and no flexiarg is authored from this slice.

This is down-sampling by transport failure, not by conductor preference: 236 of
269 indexed math chunks occurred only in packets rejected by the model's prompt
limit. The retry experiment established that two nominal <=9KB chunks can still
exceed the model's token limit when Lean/code text is token-dense; future packet
construction must budget tokens or bytes, not chunk count.

## Observed clusters (all below threshold)

| Cluster | Problems | Completed read transcripts | Disposition |
|---|---:|---:|---|
| Distinguish mathematically different constructors or map directions before encoding | 1 | 2 | Reject: below 3-problem threshold |
| Partition reported library gaps into absent, near-neighbour, search, and packaging gaps | 1 | 3 | Reject: below threshold |
| Reframe or honestly narrow a false/vacuous formalization | 1 | 1 | Reject: below threshold |
| Expose hidden equality, measure, or closure hypotheses by constructing edge witnesses | 1 | 2 | Reject: below threshold |
| Validate a defect probe against fresh and exception-bearing examples | 1 | 1 | Reinforces slice-1 `probe-the-claimed-property-not-the-acceptance-proxy`, but does not independently clear the math threshold |

## Dedupe and reinforcement

There were no threshold-clearing candidates to dedupe or author. The complete
library, case-1 staging, and slice-1 staging therefore receive no new near-match
decision beyond the one reinforcement above. No math-side mark reinforced
slice-1 `separate-evidence-history-from-verdict-state`.

The optional recognizer was not rerun: slice 1 established that
`cas_select.py` consumes prepared fixtures or `.steps.json`, not prose
directories, and building a conversion pipeline was outside this slice.
