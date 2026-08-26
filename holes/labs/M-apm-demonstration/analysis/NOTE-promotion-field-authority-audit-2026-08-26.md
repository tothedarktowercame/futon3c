# Promotion field authority audit — 2026-08-26

Scope: the live path from a role's deposit submission through candidate
persistence, mechanical review, independent review, attachment projection, and
snapshot publication.  The rule is that the controller computes everything it
can compute.  An agent reports only content or judgement that is not available
from controller state.  A reported copy of controller-owned data is retained
under `:reported-*`, never used as authority.

## Deposit and candidate persistence

| Field | Required by | Authoritative producer |
|---|---|---|
| `:name`, `:hook`, `:body`, `:pattern-ids` | candidate content gate; evidence body; mechanical guard | depositing agent; these are the content it proposes, and all three depositing cards state this schema |
| `:lanes` (`:lane`, `:status`, conditional `:reason`) | Scribe deposit gate | Scribe observation; the role card and dispatch prompt specify the closed vocabulary |
| `:new-pattern-rationales` | coined-pattern publication, when present | depositing agent; the rationale is authored content; witness memory ids are derived from candidates |
| `:depositor` | evidence author, reviewer-independence gate, snapshot | controller from deposit dispatch `:agent-id`; an agent value is `:reported-depositor` |
| `:memory-id` | evidence/edge identity and every later join | controller from dispatch, ordinal, depositor, and content digest; agent value is `:reported-memory-id` |
| `:content-digest` | readback and review-set binding | controller from the exact persisted evidence body; agent value is `:reported-content-digest` |
| `:kind` | proof-text guard and edge projection | controller from body byte size, `:= by` count, and certified-head declaration overlap; agent value is `:reported-kind` |
| `:source-attempts` | provenance and Guide deposit gate | controller from the phase request's source jobs, including repair jobs; agent value is `:reported-source-attempts` |
| evidence subject/type/claim/author/session/time/tags | persisted EvidenceEntry | controller from frame, dispatch, and candidate authority |
| edge id/type/endpoints/roles/domain/state/initial `:attachment-status :proposed` | proposed `:memory/assert` projection | controller from persisted candidate and frame authority |
| problem id and certified Solver source used by mechanical guards | problem-id and proof-text guards | controller from the frozen frame request |

There is no unproduced required candidate field after `af5a6ea2`,
`fa4912b3`, and this audit's depositor normalization.  The live prompt had
continued to demand `:kind` and `:source-attempts` after the cards stopped doing
so; this audit aligns it with the cards.

## Independent review and attachment projection

| Field | Required by | Authoritative producer |
|---|---|---|
| `:candidate-set-digest`, `:base-problem-blob` | review response binding | reviewer repeats controller-supplied authority and the controller compares it exactly; a mismatch rejects the response |
| `:open-residuals` | review gate | reviewer observation from the pinned base problem |
| `:memory-id` | exact review-set accounting | reviewer selects one of the controller-supplied candidate ids; the set must equal the dispatched set |
| `:verdict`, `:reason`, `:residual`, `:pattern-ids` | merit judgement and review evidence | independent reviewer; these are the judgement itself |
| `:reviewer` | independence and evidence author | controller from review dispatch `:agent-id`; agent value is `:reported-reviewer` |
| `:review-evidence-id` | immutable evidence identity and edge join | controller from review job plus exact judgement; agent value is `:reported-review-evidence-id` |
| `:attachment-status` | review validation, lifecycle projection, snapshot | controller from verdict (`approve`/`reassign` -> `reviewed`, `reject` -> `proposed`); agent value is `:reported-attachment-status` |
| `:witness-status` | lifecycle approval invariant | controller derives `:independently-witnessed` only for an approval produced by a reviewer distinct from the controller-authoritative depositor; agent value is `:reported-witness-status` |
| `:memory-use/kind` | optional reviewed-use classification | reviewer when it makes that semantic classification; absent remains unknown; if present it is copied into review evidence and checked against the closed vocabulary |
| review evidence subject/type/claim/author/session/time/tags/provenance | persisted review EvidenceEntry | controller from the review dispatch and canonical judgement |
| review history, reviewed-at, edge pattern reassignment | attachment projection | controller through `memory-lifecycle/review-attachment!` from persisted review evidence |

The F40 failure was the sole field with no producer: lifecycle correctly
required `:review/witness-status` for approval, but review evidence construction
omitted it and no proctor card requested it.  It is now derived from verified
independent-review authority before the attachment is projected.  The review
evidence identity is schema-versioned, so recovery appends corrected evidence
and retains the earlier incomplete record rather than rewriting it.

## Snapshot publication

| Field | Required by | Authoritative producer |
|---|---|---|
| candidate `:depositor`, `:reviewer`, `:review-evidence-id`, `:attachment-status`, `:pattern-ids` | snapshot shape and fresh substrate visibility | canonical candidate plus canonical persisted review |
| review reason/residual and independent authorship | fresh substrate visibility | persisted review evidence and memory evidence |
| snapshot frame/problem ids, policy, memories, provenance summary, id/digest/path | immutable snapshot and Student binding | controller from frame state, reviewed candidates, prior receipt chain, and canonical content digest |

Publication remains fail-closed: the attachment edge and both evidence entries
must read back with the exact review id, verdict, pattern set, status, and
independent authors before an approved candidate enters a snapshot.
