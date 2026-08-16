# Dispatch-time recall miss diagnosis (frames 1–6)

Date: 2026-08-16. This is a diagnosis only; no source was changed.

## Result

The store was reachable and the memories were present. Recall rejected every
edge because the round's deposited `:memory/assert` hyperedges have no
`:attachment-status`; the recall projector treats an absent status as
`:unreviewed` and admits only `:reviewed` attachments. This is the direct cause
of the zero push result. Query wording is a secondary defect (the problem id is
still selected as the required lexical anchor), but changing case or removing
query terms does not cross the attachment-review gate.

## Production path and exact recorded queries

`GroundControlBackend` fixes solver dispatch to the push channel and invokes
`dispatch-with-recall/run-dispatch!` at
`src/futon3c/peripheral/problem.clj:1068-1080` (the production default is wired
at lines 1118-1129). `recall-now` chooses the configured substrate, proposes on
the required term, then recalls the problem/subject/pattern endpoints in domain
`:mathematics` at `src/futon3c/dispatch_with_recall.clj:915-976`. Candidate
memories are then subjected to the required-term body check at lines 978-1056.

The persisted solver steps, rather than a reconstruction from today's files,
record the actual queries:

```text
data/problem-state/t94J02-ad8b70.../v7.edn
  terms         ["t94J02" "coarser" "topologies" "checkout"]
  query         "t94J02 coarser topologies checkout"
  required-term "t94J02"
  endpoints     ["t94J02"]
  status        :recall-empty
  eligible      []

data/problem-state/t00A05-fdeb762.../v26.edn
  terms         ["t00A05" "ellipsoid" "apm_t00a05_isc2" "report"]
  query         "t00A05 ellipsoid apm_t00a05_isc2 report"
  required-term "t00A05"
  endpoints     ["t00A05"]
  status        :recall-empty
  eligible      []
```

There is a real query-construction inconsistency: the comment at
`dispatch_with_recall.clj:507-517` says the problem id must be graph-only, and
the file-source terms remove it at lines 487-493, but `subjects` is interleaved
unchanged at lines 536-545. These dispatches supplied `:subjects [problem-id]`,
so the id re-entered the lexical terms and became the required anchor at lines
546-557.

## Live checks

### Entries and target store

The running process environment names the intended standalone substrate:

```text
$ printf 'FUTON_SUBSTRATE_URL=%s\n' "$FUTON_SUBSTRATE_URL"
FUTON_SUBSTRATE_URL=http://127.0.0.1:7073
```

Direct GETs on 7073 returned HTTP 200 and complete memory entries for all three
rehearsal ledger samples:

```text
e-33cf23e7-a574-487f-ac5f-ad98302b8047  HTTP 200
  :evidence/type :memory
  :evidence/subject {:ref/type :problem, :ref/id "a01A06"}
e-907281cd-b003-418b-8311-73b8eab3d0ba  HTTP 200
e-2d8f82c7-e617-4fef-8230-b7193fec28e8  HTTP 200
```

Frame-2/3 rider entries also exist on 7073 and name `t00A05` as their subject:

```text
e-1f2d3d6d-e809-4867-a153-a7c05659fc33  2026-08-16T09:59:18Z
e-84690e27-6853-402d-bdbb-aa48c5f56825  2026-08-16T09:59:19Z
e-44de0908-82a0-4463-9be4-97f2e1213faa  2026-08-16T10:19:01Z
e-a36bde67-a104-4d04-9044-4c1d4965123a  2026-08-16T10:57:44Z
```

Thus, in particular, the first three existed before the recorded frame-4
solver dispatch at 11:15 UTC.

Port targeting is not the failure: 7071 refused connections, while 7073 was
the configured EDN substrate. Port 7070 also proxied the sample entry (JSON
envelope), but production selected 7073 at
`dispatch_with_recall.clj:930`. No snapshot identifier or snapshot memory-id
set is passed to `recall-by-endpoints` (lines 970-976), so snapshot scoping is
not responsible for excluding these records.

### Search and projection variations

Live text search was case-insensitive and found rows for both the exact and
lower-cased queries. Removing the id or shortening the query increased and
improved lexical hits:

```text
"t94J02 coarser topologies checkout"  -> results (mostly dispatch receipts)
"t94j02 coarser topologies checkout"  -> same leading result
"coarser topologies checkout"         -> results
"coarser"                             -> relevant memory

"t00A05 ellipsoid apm_t00a05_isc2 report" -> results (mostly receipts)
"t00a05 ellipsoid apm_t00a05_isc2 report" -> same leading result
"ellipsoid"                                -> results
```

The graph endpoint itself also works. Direct POSTs to
`/api/alpha/memory/projection` returned four components for `t94J02` and 24 for
`t00A05`, including the rider ids above. The failure occurs in the client-side
projection gate. Running the same production `recall-by-endpoints` call live
gave:

```clojure
(recall-by-endpoints {:domain :mathematics} ["t94J02"] {:limit 10})
;; returned 0
;; audit {:edge-count 4, :returned-count 0,
;;        :attachment-excluded 2, :domain-excluded 2, ...}

(recall-by-endpoints {:domain :mathematics} ["t00A05"] {:limit 10})
;; returned 0
;; audit {:edge-count 24, :returned-count 0,
;;        :attachment-excluded 21, :domain-excluded 3, ...}
```

Re-running the full production `bounded-recall` today with the saved opts and
packet reproduced `:recall-empty`, proposal count 0, and eligible `[]` for both
problems. This rules out a transient frame-time outage or stale projection as
the continuing explanation.

Inspection of the projected `t00A05` edges showed `:domain :mathematics` and
`:state :current`, but no `:attachment-status`. The code makes the consequence
explicit: `memory_recall.clj:38-49` defaults missing status to `:unreviewed`
and excludes everything not exactly `:reviewed`. The writer creates current,
domain-labelled edges but supplies `:attachment-status :proposed` only when a
pattern subject exists (`memory_write.clj:180-197`); ordinary problem-subject
deposits therefore have no status.

## Root cause and ranked alternatives

1. **Certain: the round deposits never crossed the reviewed-attachment
   boundary.** The endpoint returned them, and the recall audit counted them
   under `attachment-excluded`. Missing status is deterministically interpreted
   as `:unreviewed`.
2. **Real but secondary: problem-id leakage into the lexical anchor.** It makes
   proposal search receipt-heavy and often produces zero warranted proposals.
   It does not explain endpoint recall returning zero after the endpoint has
   already found the rider memories; the attachment gate does.
3. **Rejected: case mismatch.** Upper/lower-case searches returned the same
   leading hit.
4. **Rejected: wrong store/port or missing entries.** The configured URL was
   7073 and direct entry reads succeeded.
5. **Rejected: snapshot exclusion.** Dispatch recall does not pass the cycle's
   snapshot ids or temporal cursor into this call.

## Smallest proposed fix (not implemented)

Do not weaken `project-components` and do not special-case round memories as
reviewed. The smallest coherent fix is to add the missing review step to the
round's deposit/promotion workflow: deposit rider memories with explicit
pattern attachments, create separately authored review evidence, and call the
existing `memory-lifecycle/review-attachment!` with `:approve` before a later
frame is allowed to expect them in push recall. That function is the existing
invariant-preserving transition to `:attachment-status :reviewed`
(`memory_lifecycle.clj:192-273`) and verifies visibility after the write.

Separately, remove the exact problem id from the `subjects` contribution to
lexical `selected-terms` while retaining it in graph `endpoints`; that is a
small query fix, but it cannot substitute for attachment review.
