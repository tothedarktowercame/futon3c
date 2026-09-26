# Controller-owned identifier replay audit

Date: 2026-08-25. Trigger: F32 `student-attempt-2` terminal repair.

## Incident

The immutable Student request contained reviewed memory id
`e-1866fc8e-aa5a-426c-aa30-d8d57c224238`. The repair agent manually copied
the 21-element accessible list into `:memory-use/:surfaced-ids` and wrote
`aa5e` at that position. The original turn had omitted its typed submission,
so this serialization repair was the only authorized repair. Semantic terminal
validation rejected the unknown id after that budget was exhausted.

## Repair

Student terminal submissions now report only the semantic observation
`:memory-use {:used-ids [...]}`. The controller derives:

- snapshot receipt, id, digest, and accessible identifiers from dispatch;
- surfaced identifiers from that snapshot plus content-address-valid FTS
  receipts bound to the current job and explicit repair predecessor;
- query strings from those same receipts.

Legacy freehand snapshot, surfaced, query, and search-receipt fields are
ignored. An actually used identifier remains an agent claim and must belong to
the controller-derived surfaced set.

## Audit of adjacent typed paths

| Path | Classification | Action |
|---|---|---|
| Student snapshot/search accounting | Redundant replay of controller facts | Removed in this change. |
| Common role authority (`job-id`, dispatch, agent, frame, problem, phase, role, token) | Controller fact | Already injected by `typed-role-submission`; agent-supplied authority is rejected. |
| Solver/proctor branch, revision, head, Lean result | Observation about workspace | Legitimate role evidence; independently checked against Git/Lean. |
| Student `used-ids` | Semantic attribution by Student | Retained as a foreign-key claim; checked against controller-derived surfacing. A future tool-owned use receipt would be stronger. |
| Scribe/Zai-Scribe/Promotion-Proctor search receipt ids | Redundant replay of job-bound controller receipts | Removed: typed-submission validation now derives the complete content-address-valid job receipt set while preserving pattern-accounting checks. |
| Promotion reviews keyed by candidate/memory id | Semantic judgement mapping | Retain until reviews use controller-issued ordinal handles; IDs are checked against the exact candidate set. |
| Guide store candidates naming newly written memory/hyperedge ids | Effect result replay | Open follow-up: bind memory-write receipts to the Guide job and derive deposit candidates from them. Current independent review prevents publication of nonexistent identities. |
| Close-frame trace/result | Semantic audit conclusion | Retained; terminal receipt identity and inputs are controller-derived. |

The remaining Guide write-result replay and promotion-review selection are
real instances of the same design smell. They require distinct typed receipts
for write effects or controller-issued ordinal handles for candidate review;
they must not be silently ignored or normalized.

## Formal-spec status

The Lean cycle model now distinguishes content, semantic references, and
controller accounting at field granularity. It proves that Student
`:used-ids` is a valid role-authored semantic claim and that role-authored
`:surfaced-ids` is invalid. The emitter owns phase I/O, receipt schemas, and
terminal submission authority schemas; Clojure validates their exact generated
form and the typed submission tool consumes the Student memory-use boundary.
The generated-receipt-schema residual hole is therefore closed.
