# CL-16 — VERIFY obligation 1 residue after CL-1 through CL-15b

Survey only, 2026-11-17 UTC. I changed no production source and made no store
writes. The live sample is one bounded read of the local turn store: 840
operator records created from 2026-11-10T00:00Z through the survey time. The
counts therefore mix old records with code introduced late on 2026-11-16; a
missing new projection on an old record is a backfill/deployment fact, not by
itself evidence that the current writer is broken. Examples below are shapes,
not copied operator, memory, or mission text.

## Verdict

The declared GCC interoperation path is now present for every animal, and the
four prose reads named as closure blockers in Revision 2 are either gone or
narrowed to two small residues. Obligation 1 is not quite clean: 馬 still
re-parses reply marks from `reply_text`, and 狗 retains a compatibility branch
which JSON-parses a string `evidence/body` even though the memory-sidecar
contract now supplies a map. Failure absence is also not uniformly durable:
驴 and 馬 still collapse or swallow listener/model failures, 象 does not persist
a failed dispatch before a job id exists, 狗 has outer catches that log without
a turn receipt, and Giraffe keeps several service failures only in its JVM
health atom.

The smallest next packet is 馬 reply marks. It needs no new GCC type: put the
already-derived mark on each typed reply act and have 馬 read that field. Then
remove 狗's obsolete string-body compatibility branch. The remaining packets
should each address one durable failure receipt, in the order at the end.

## 1. Re-verification of the closed graph interfaces

| Animal/interface | Writer and durable output | Named reader and decision consumer | Current finding |
|---|---|---|---|
| 猴 focus | `circus/hou_store.clj:149-176` writes the focus header, links and attestation; `turn_service.clj:569-588` retries and writes a terminal attempt on exhaustion. | `hou_store.clj:215-238` reads a complete header by the turn evidence reference; `operator_focus.clj:192-209` uses its `:focus` relations and pins every header. | Closed. The operator window no longer derives named items from evidence text. Missing/incomplete headers become `:pending`, not no-mission. |
| Giraffe turn listener | `turn_mention_writer.clj:104-145` writes typed per-field mentions; 猴 supplies the completion barrier. | `reread_service.clj:39-70` calls `headers-for-turn` and `read-turn-mentions`, then resolves item ids through the entity sidecar. | Closed. No turn-prose id regex remains in this function; absent records return `:pending` at `:49-53`. |
| 驴 proposals | `pattern_proposals.clj:24-65` plans and batch-writes `[:pattern/proposal 1]`; `lu_rerank.clj:343-356` stores its outcome. | `pattern_proposals.clj:68-93` reads rank attestations; 馬 calls it at `ma_occurrence.clj:318-321,388-392`. | Closed as a GCC path; listener failure durability remains open below. |
| 象 interpretations | `xiang_interpretations.clj:47-104` maps operator-act ranges and writes `[:interpretation 1]`; `turn_service.clj:1126-1143` waits for and records the write before `:ported`. | `xiang_interpretations.clj:106-136` rebuilds acts; 馬 calls it at `ma_reading.clj:257-265` and records pending at `:271-279`. | Closed for operator acts. Reply acts are a separate residue below. |
| 馬 occurrences | `pattern_occurrences.clj:14-70` writes `[:pattern/occurrence 1]`; `ma_occurrence.clj:413-434` records the projection outcome before notification. | `pattern_occurrences.clj:73-102` resolves latest stamped attestations; 驴 seeds from it at `lu_rerank.clj:81-110`. | Closed as a GCC path; model failure typing remains open below. |
| 狗 memory outputs | `dog_memory.clj:82-273` writes recall, use, burial and repetition; all four types are declared at `type_registry.clj:179-194`. | `dog_memory.clj:284-358` provides `read-recall`, `read-use`, `read-burials`, and `read-repetitions`; `dog_provider.clj:803-820` uses GCC recall for the use check with no turn-field fallback. | Closed as four GCC paths. Recall search parsing and outer failure receipts have small residues below. |
| Giraffe needs | `giraffe_needs.clj:54-121,241-248` writes one `[:needs 1]` batch per reading/run. | `giraffe_needs.clj:154-239` reads current-run claims and verifies reading pins; 猴 defaults to it at `hou_store.clj:101-108` and `operator_focus.clj:224-230`. | Closed. Empty GCC is typed `:needs-unavailable`, not an `input.edn` fallback. |
| Mission supersession | `item_inventory.clj:93-141,157-201` is the sole status-text parser and writes `[:supersedes 1]`. | `operator_focus.clj:21-70` reverse-reads current-sha claims and `:72-85` follows the chain. | Closed. 猴 no longer slurps mission prose; stale/un-superseded claims are ignored by the source-sha check at `:41-47`. |
| 象 job failure kind | `transport/http.clj:2152-2187,2630-2683` classifies and persists `failure-kind` on job and terminal event. | `turn_record.clj:1251-1310` reads it (legacy projection only when absent); `turn_service.clj:1662-1733` branches on exact kinds. Emacs token predicates are `emacs/session-turn-analysis.el:862-866,1332-1336`. | Closed. Typed kind wins over contradictory prose; legacy prose is isolated at the two reader boundaries. |

## 2. Prose or evidence-body parsing still on animal decision paths

### 狗 recall: one obsolete compatibility parser remains

The production query is now the bounded memory sidecar: `search-memories` adds
`source=memory-sidecar&limit=10` at `agency/dog_provider.clj:307-314`, and the
two branches run concurrently at `:350-411`. The sidecar returns the exact map
fields that `memory-item` consumes. Thus the old broad search, tag filtering,
and per-hit memory fetch are gone.

However `memory-item` still accepts a string body and JSON-parses it at
`dog_provider.clj:268-280`, then interprets `source/project/name/hook` at
`:281-294`. This branch is reachable if a caller violates or emulates the old
response shape, so under the literal “never JSON parsed for meaning” test it
is still present. Smallest replacement: require `evidence/body` to be a map in
the memory-sidecar response, treat any other shape as a typed malformed-hit
count, and remove the `json/parse-string` branch. No new GCC type is needed;
the exposure itself is already `[:memory/recall 1]`.

### 馬 reply marks: still re-derived from reply prose

Operator acts are now typed interpretations: `ma_reading.clj:62-70` consumes
`:operator_interpretation_acts`, populated from the GCC reader at `:257-265`.
Reply acts are typed maps from `:acts` at `:72-78`, but `paragraphs` reparses
`reply_text` with `turn_record/reply-marks` at `:80-91` solely to recover each
reply act's mark; `classical-pass` joins that map at `:99-101`. This is still a
prose-derived fact in 馬's decision path.

Smallest replacement: when 象/turn-record constructs each `<turn>-r-<n>` act,
store its `:mark` on that act. 馬 can then use `(juxt :id :mark)` over the typed
reply acts and delete `paragraphs`. If reply acts are later moved to GCC, the
same field belongs in the interpretation attestation; a new record type is not
required for this packet.

### 猴 named mentions and successors: gone

`operator_focus/read-turn` obtains named items only from the stored 猴 header's
`:hou/relations` (`operator_focus.clj:192-209`). Successors come only through
the GCC reverse read (`:21-70`). The only remaining mission-text regex is at
the inventory ingestion boundary, `item_inventory.clj:83-91`, which is the
declared single parser/writer and is not a 猴 decision-path prose read.

### Giraffe item ids: gone

`reread_service/items-touched-by-turn` explicitly reads typed mentions at
`:39-50`; its only regex-like choice is a `case` on the already-typed id prefix
to select the entity type at `:57-63`. No source/reply/象 prose fields are
searched for ids.

## 3. Failure and absence typing against the Revision 2 rows

### 驴 — still partial

`rerank` now returns a structured fallback with source, reason and timing
(`lu_rerank.clj:229-281`), so “causes merged” is partly fixed: ask exceptions
become `ask-error: …`, empty pools and missing readers are distinct. But the
production HTTP adapter returns nil for every HTTP/timeout/parse failure
(`:285-304`), so those remain merged as `timeout-or-empty-reply` (`:264-270`).
Worse, the outer listener catch still discards every exception at `:327-357`.
The proposal writer outcome is durable only after the listener reaches
`:351-356`.

`lu_memory/handle-event` computes a detailed tally at `lu_memory.clj:260-273`
but neither persists it on the turn nor exposes it in durable health; the
sidecar append catches also return nil (`:247-252`). Small typed fixes: make
the whistle adapter return `{:ok false :kind ...}` instead of nil; always
write `:lu_ranked`/`:lu_proposals` failure outcomes from the listener catch;
and store the memory tally as `:lu_memory` (written/unchanged/failed ids).

### 象 — job-terminal typing fixed; pre-job dispatch absence still open

CL-15a/b fixed terminal failures as shown above. The earlier “failed bell
leaves requested with no job id” finding still holds in code: `dispatch-to!`
records a job id only on success (`turn_service.clj:1466-1472`); the failure
branch updates only in-memory service health and returns a reason
(`:1473-1477`). A later reap sees `:no-job-id` (`:1657-1659`) but does not
persist the dispatch failure. Typed fix: atomically store
`analysis_dispatch.outcome {:state "failed" :failure_kind
"dispatch/failed" :failure_kind_source "dispatch" ...}` and set an explicit
retryable/terminal status before returning.

### 狗 — most old findings fixed; outer-operation receipts remain open

Recall now distinguishes `partial`, `failed`, and `none`, preserving per-branch
errors (`dog_provider.clj:350-415`); a preparation exception is cached as
failed (`:452-491`). Thus a store/search failure is no longer silently “none”.
Prompt rendering carries typed absence in the turn segment (`:1293-1323`), and
the GCC projection records its outcome. Use-check writes `pending`, `failed`,
`no-recall`, or `no-reply` (`:785-827`). Burial writes `off`, `none`, or `ok`,
and each item retains `buried`, `exists`, or `failed` (`:1196-1225,1227-1260`);
so “failed burial never written” is fixed when `bury-one!` returns normally.

Two outer catches still only print: use/bury orchestration failures at
`:828-842`, and a failure outside the inner burial result at `:1261-1262`.
Likewise `schedule-gcc!` logs a thrown projector before its writer can record
the turn outcome (`:431-442`). Small typed fix: one guarded receipt helper that
writes `:dog_use`, `:dog_buried`, or the corresponding `_gcc` field with
`{:status "failed" :reason-kind ...}` unless a positive success already exists.

### 馬 — proposal pending fixed; reader/model failure still collapsed and not retried

Missing proposals are now a durable, retryable `:lu-proposals-pending` record
that cannot overwrite a real verdict (`ma_occurrence.clj:394-405`). A missing
model reply becomes explicit per-pattern void rows (`:300-307,330-364`), and
these are projected too. But `whistle-ask` maps every HTTP status, 429,
timeout, parse error, and exception to nil (`:479-498`); consequently the
record says only reader `none` / “no reader”. There is no retry trigger for
those voids, and the outer `verdict!` catch still returns nil (`:373-436`).
Typed fix: preserve a model outcome kind (`quota`, `timeout`, `http`, `parse`,
`no-reader`) on the verdict, schedule a bounded retry only for the retryable
kinds, and persist an outer failure receipt without replacing a real verdict.

### 猴 — the named old gaps are fixed; one generic hook exception remains log-only

Non-operator turns are intentionally skipped before mention writing
(`turn_service.clj:606-615`), not warned as evidence failures. Every refusal in
`write-focus!` produces an attempt receipt (`hou_store.clj:130-176`), and
retry exhaustion produces a terminal `:readerFailedAfterRetries` receipt
(`turn_service.clj:566-588`). This fixes both old findings.

The generic async wrapper still only prints exceptions thrown outside the
typed 猴 writer path (`turn_service.clj:591-620`). Small hardening fix: append
a turn-subject attempt with `:hookException` in that catch when the operator
turn had reached 猴 readiness. Mention-only errors should receive their own
projector outcome rather than masquerading as 猴 refusal.

### Giraffe — durable reading queue, but service errors and pauses remain memory-only

Document reading failures are durable in the queue and bounded retries expose
exhaustion (`reread_service.clj:356-384`). Typed mention absence is `:pending`
and deliberately leaves the turn unseen so a later event/backstop can retry
(`:551-577`). GCC needs absence is specifically `:needs-unavailable`
(`giraffe_needs.clj:154-172,218-239`), replacing the old single generic focus
absence.

The old operational-residue claim otherwise holds: `failure!` writes only the
process-local `state :errors` atom (`reread_service.clj:160-162`); document
event exceptions end there after retry scheduling (`:462-503`), turn errors at
`:551-577`, mint/backstop errors at `:223-248,434-448`, and usage pauses are
computed only in `health` at `:679-711`. A restart loses these observations.
Typed fix: append a small durable service-attempt/event row (or persist the
existing health outcome alongside the SQLite queue) for mint, turn and usage
pause classes, keyed by operation and target; retain the atom as a cache.

## 4. Bounded live turn-store census

The sample contains 840 operator turns. Current field coverage is:

| Animal/output field | Present | Typed outcomes observed | Missing |
|---|---:|---|---:|
| 驴 `lu_ranked` | 840 | all structured maps | 0 |
| 驴 `lu_proposals` | 6 | 6 writer outcomes | 834 |
| 象 `xiang_interpretations` | 7 | 7 writer outcomes | 833 |
| 象 `analysis_status` | 840 | 836 analyzed, 4 failed; 0 requested | 0 |
| 狗 `dog_recall` | 668 | 566 not-ready, 82 item-bearing/ordinary maps, 13 none, 7 failed | 172 |
| 狗 `dog_recall_gcc` | 86 | 82 written, 1 none, 3 not-ready | 754 |
| 狗 `dog_use` | 287 | 217 no-recall, 68 ok, 1 pending, 1 no-reply | 553 |
| 狗 `dog_use_gcc` | 73 | 68 written, 4 no-recall, 1 pending | 767 |
| 狗 `dog_buried` | 224 | 189 none, 35 ok | 616 |
| 狗 `dog_buried_gcc` | 41 | 37 written, 4 none | 799 |
| 馬 `pattern_verdicts` | 820 | 820 structured verdict maps | 20 |
| 馬 `pattern_occurrences` | 62 | 57 written, 5 other structured outcomes | 778 |

There were no `requested` records in the bounded sample and therefore no live
instance of requested-without-job-id at survey time. GCC marker sparsity is
expected because CL-3 through CL-9 went live near the end of this seven-day
window and their backfills used narrower windows or output-specific
eligibility. These counts answer historical coverage only; the source audit
above decides whether the current path is typed. 猴 headers and Giraffe needs
live in futon1b rather than turn fields, so counting them would require the
prohibited broad graph scan and is intentionally omitted.

## 5. Recommended one-behaviour packets

1. **馬 reply marks:** persist `:mark` on typed reply acts and remove the
   `reply_text` parser from `ma_reading`.
2. **狗 recall shape:** delete string-body JSON compatibility; count malformed
   sidecar hits as a typed search outcome.
3. **象 pre-job dispatch:** persist a typed dispatch failure when bell returns
   no job id.
4. **馬 model outcome:** retain HTTP/quota/timeout/parse kinds and retry only
   typed retryable voids.
5. **驴 listener outcome:** persist outer listener failures and separate the
   whistle failure kinds; then persist the `lu_memory` tally.
6. **狗 outer receipts:** make use, burial and GCC wrapper catches write typed
   failure receipts without replacing success.
7. **Giraffe durable service attempts:** persist mint/turn/usage failures now
   held only in the health atom.
8. **猴 hook hardening:** add a terminal hook-exception attempt for the narrow
   exception path outside `write-focus!`; keep mention failures distinct.

After packets 1 and 2, the known animal decision paths no longer parse prose or
JSON strings for typed cross-component facts. Packets 3 through 8 close the
remaining partial-failure/absence half of Revision 2 rather than adding new
interoperation types.
