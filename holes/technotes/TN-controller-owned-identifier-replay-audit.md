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
| Scribe/promotion-proctor search receipt ids | Redundant replay of job-bound controller receipts | Open follow-up: derive the complete job receipt set at typed-submission validation while preserving pattern-accounting checks. |
| Promotion reviews keyed by candidate/memory id | Semantic judgement mapping | Retain until reviews use controller-issued ordinal handles; IDs are checked against the exact candidate set. |
| Guide store candidates naming newly written memory/hyperedge ids | Effect result replay | Open follow-up: bind memory-write receipts to the Guide job and derive deposit candidates from them. Current independent review prevents publication of nonexistent identities. |
| Close-frame trace/result | Semantic audit conclusion | Retained; terminal receipt identity and inputs are controller-derived. |

The two open follow-ups are real instances of the same design smell, but they
are not changed here because their consumers require a distinct typed receipt
for write effects or candidate-review selection. They must be replaced with
controller-owned receipts, not silently ignored or normalized.

## Formal-spec status

The Clojure frame-cycle contract now states
`:student-terminal-search-accounting-controller-derived`. The checked-in
generated campaign contract already states that self-reported queries are not
search evidence and that submission authority is controller-owned, but its
Lean generator source is not present in this repository. The next canonical
Lean contract generation must add the stronger Student terminal-accounting
property before regenerating the JSON artifact; hand-editing generated JSON
would violate its authority boundary.
