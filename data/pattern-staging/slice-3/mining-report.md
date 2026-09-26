# Slice 3 math-strict synthesis

The index contains exactly 180 `math-strict` chunks for the requested IDs,
forming 93 two-chunk-or-smaller packets. `b71A68` has no strict chunks.

An initial 93-job attempt exposed a queue-semantics error in the conductor:
rotating after enqueue does not bind queued work to that context. Cancelling
ledger jobs also does not remove their durable queued turns. That attempt was
stopped (9 done, 84 cancelled), as was a 22-job correction attempt. To preserve
the 200-read ceiling, the final clean run down-sampled eight b97A02 packets and
ran the remaining 85 packets. Each batch drained completely before the next
verified restore. All 85 final jobs completed; zero failed. Six final rotations
returned `ok:true`.

The clean run produced 312 marks, 5 `COORDINATION-ONLY`, and 16
`NOTHING-SURPRISED`. Coordination-only rate: 5/85 = 5.9%, below slice 2's
8/13. Quote verification found 128/312 QUOTE fields that could not be matched
verbatim to their source packet after whitespace/Markdown normalization and
ellipsis-aware segmentation. Those marks were excluded. The synthesis uses
184 quote-verified marks. This is reported as a fabrication count of 128,
though many are faithful paraphrases mislabeled as verbatim quotes rather than
invented mathematical claims.

## Quote-verified clusters

| Cluster | Problems | Transcripts | Disposition |
|---|---:|---:|---|
| Match a goal to a named theorem by hypothesis/conclusion shape | 7 | 13 | Reinforces `math-informal/reduce-to-known-result` and `math-informal/find-the-right-abstraction` |
| Replace enumeration with structural counting | 3 | 4 | Authored as technique |
| Probe the transitive axiom/trust closure | 5 | 9 | Reinforces slice-1 `probe-the-claimed-property-not-the-acceptance-proxy` |
| Re-express a goal into the library's native API form | 4 | 6 | Reinforces `math-informal/structural-equivalence` and `math-strategy/convention-bridge` |
| Construct an edge witness or pathological model to test semantics | 4 | 4 | Reinforces `math-informal/construct-an-explicit-witness`, `check-the-extreme-cases`, and `failure-mode-characterization` |
| Check hidden instance, equality, direction, and ambient hypotheses | 4 | 4 | Reinforces `math-strategy/hypothesis-category-check` and case-1 `transport-across-an-instance-diamond` |

No quote-verified math cluster reinforced slice-1
`separate-evidence-history-from-verdict-state`; that remains an agency-family
candidate. The axiom-closure cluster materially reinforces slice-1's other
candidate from five distinct math problems.

The authored structural-counting move is a gap after dedupe against the full
futon3 library and case-1/slice-1 staging. Existing
`bijectivity-from-injectivity-plus-count` uses an already-known finite count,
while this move explains how to replace unsafe or infeasible enumeration with
a kernel-checkable decomposition that obtains the count.
