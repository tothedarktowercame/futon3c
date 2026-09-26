# E1 historical arm-attribution back-fill (2026-08-01)

## Verdict

The attribution gap is partially recoverable without re-running a dispatch.
Sixteen of the 17 successful, non-empty, unattributed dispatches occurred
strictly before the content-match arm was commissioned and all 61 surfaced
memories have reviewed pattern attachments visible in a historical projection
at both valid-time and system-time as of the dispatch.  Those 61 surfacings are
therefore recoverable as `:pattern`.

One dispatch is not recoverable from the available artifacts:
`invoke-1785441024938-386-944657a5` at
`2026-07-30T19:50:25.794205436Z`.  It overlaps the implementation job
`invoke-1785440682974-385-e69dc511`, commissioned at
`2026-07-30T19:44:42.974047737Z`, and precedes commit `50916c84` at 20:55:42
BST.  The dispatcher loaded the live working tree, so neither the commit clock
nor current source establishes whether its five memories came from
content-match or pattern retrieval.  The missing artifact is a frozen
dispatch-time source revision together with the selected candidate records
(including their `:via` field), or an equivalent recall trace.  Timestamp
proximity and current graph state are insufficient.

The other 82 of the wider 99 unattributed dispatches contain no recoverable
per-memory question: 79 had no surfaced memories, and three were unusable
recalls.  Thus the 17 records in the back-fill artifact exhaust the
non-empty-attribution gap.

## E1 result

The original attributed tail contained 67 pattern and 82 content-match
surfacings (44.97% pattern).  Adding the 61 recovered pattern surfacings gives:

- pattern: 128
- content-match: 82
- denominator: 210
- pattern share: 60.95%
- classification: `:pattern-arm-substantial`
- attributed span: 376,878 seconds of a 379,093-second corpus span
- `coverageNotTail`: true
- `attributionComplete`: false (the single overlap dispatch remains)

This closes the temporal-tail observable but not the completeness observable.
It also does not establish C1 causally: the recovered period predates the
content-match arm, so its 100% pattern attribution reflects an architectural
regime in which content-match was unavailable.  The result rules out treating
the 44.97% tail share as representative of the whole historical corpus, but a
fresh, fully instrumented window in which both arms are stably live remains the
clean way to test whether attachment coverage, rather than lexical matching,
is the bottleneck.

## Derivation rule

For a dispatch strictly before the commissioning time:

1. The parent of commit `50916c84` did not return
   `(:content-matches proposals)`; that commit introduced the merge and the
   `:memory-use/surfacing-via` receipt field.
2. Query `/api/alpha/memory/projection` read-only with both `:valid-as-of` and
   `:system-as-of` set to the dispatch timestamp.  Historical graph state is
   available from 2026-07-25, and all recovered dispatches are on or after
   2026-07-26.
3. Classify a surfaced memory as pattern only when that historical projection
   contains a reviewed attachment with a non-empty `:roles :patterns` value.

No classification uses a current attachment to infer a historical arm.

## Three evidence chains

1. `invoke-1785096421259-158-9638e22e`, dispatched
   2026-07-26T20:07:01Z, surfaced
   `e-1ac936fb-04e8-460e-a710-37fac474401c`.  Content-match was unavailable;
   historical edge `hx-mem-1ac936fb-04e8-460e-a710-37fac474401c` was reviewed
   at 11:57:12Z and attached the memory to `math/proof-architecture`.
2. `invoke-1785342613132-282-b20c35fc`, dispatched
   2026-07-29T16:30:13Z, surfaced
   `e-0e4e32fe-54af-451b-b302-17aa521891fd`.  Content-match was unavailable;
   historical edge `hx-mem-0e4e32fe-54af-451b-b302-17aa521891fd` was reviewed
   on 2026-07-27 and attached the memory to
   `math/weak-convergence-hilbert`.
3. `invoke-1785440558225-384-2820fe28`, dispatched
   2026-07-30T19:42:38Z, surfaced
   `e-codexpilot-count-polynomial-lemniscate-components-by-roots-plus-one-exterior`.
   Content-match was unavailable; the matching historical edge was reviewed
   at 18:57:08Z and attached it to `math/connectedness-component-api`.

The frozen EDN contains the complete timestamps, review evidence IDs, and
hyperedge IDs for every recovered surfacing.

## Reproduction

From the `futon3c` root, with the store at `127.0.0.1:7073` available read-only:

```sh
clojure -M holes/labs/M-memory-retrieval/arm_attribution_backfill_20260801.clj
sha256sum holes/labs/M-memory-retrieval/arm-attribution-backfill-20260801.edn
```

Input receipt SHA-256:
`0cc527e23c3678a4cc7d8053d6636d0cde556dab15fcc3ce69bedf0b659820b3`.

Derivation script SHA-256:
`0a0cb2bded6abf28fcc462b6e75515fa09efdaab89d312f826cf26f0f4534822`.

Frozen back-fill SHA-256:
`63e4fe8285e71d2ae4479ec4343d18aa583a73da71fbb23367848935cca19a33`.
The script was run twice against the read-only historical projection endpoint;
the second run compared byte-for-byte equal to the first before this hash was
recorded.
