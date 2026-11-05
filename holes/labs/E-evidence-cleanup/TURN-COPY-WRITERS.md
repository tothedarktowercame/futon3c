# Writers that still copy operator-turn text

Discovery only, 2026-11-05. Counts below are read-only measurements from
`futon1b/migration-store-21/fts5-evidence.db` over the half-open window
`[2026-10-29T00:00:00Z, 2026-11-06T00:00:00Z)`. “Copied share” is the decoded
character count of the named copied fields divided by the serialized body
character count; it is an estimate because EDN/JSON escaping and field names
remain in the denominator. The queries joined `ev_fts` to `ev_tags`, bounded
on `ev_fts.at`, and summed `length(body)`; no live-store read was made.

## Summary

| Writer family | Writer and copied field | Seven-day rows / body bytes | Copied turn material | Field readers and what they need | Model class / source id available? | Recommendation |
|---|---|---:|---|---|---|---|
| Zai/Kimi transcript `:turn-start` (`zai-1`, `象-1`) | `src/futon3c/agents/zai_api.clj:1312-1327`; full dispatch in body `:prompt` | 562 / 10,594,318 B (10.59 MB) | 10,240,610 decoded prompt chars, about 96.9% of body characters; mean 18,222 chars/row | `scripts/ma_observe.py:287,520,630` needs the full prompt to distinguish continuation packets and recover their packet body; `scripts/turn_frames.py:465-478` needs the prompt to frame a Zai/Kimi turn and its transport header; `scripts/scope_view_data.py:127-132` joins the dispatch through `:dispatch-id` and then uses the turn; `scripts/memory_use_rate.py:77-98,182-186` needs injection metadata, not prompt text. The first two could follow a retained dispatch/operator-turn reference if it resolved to the exact delivered prompt; the latter two do not need a copy. | Dispatch/participant act. `persist-turn-start!` receives `dispatch-id` but no general operator-turn evidence id (`zai_api.clj:1312-1327,2513-2521`). Some specialist prompts embed evidence ids in their text, but that is not a typed source link. | **Replace the copy with references**, retaining a content hash and a typed reference to the immutable dispatch payload (and operator-turn evidence id where applicable). The exact delivered prompt must remain resolvable because Ma and turn framing currently require machine-added instructions as well as Joe's text. |
| Claude/Codex invoke lifecycle `prompt-preview` | `dev/futon3c/dev.clj:3693-3706,3990-4004,4622-4661`; first 200 characters in `"prompt-preview"` | 5,532 / 2,539,558 B (2.54 MB) | 1,103,037 decoded preview chars, about 43.5%; mean 199 chars/row | `src/futon3c/agency/history_constraints.clj:64-78` reads only the leading `CURRENT TURN` header to recover Surface/Caller/From; `scripts/turn_frames.py:439-448` likewise reads the header; `scripts/harvest_turn_origins.py:1-15` reads transport origin from it; `scripts/xiang2000_p0.py:514` contains a one-off exact header check. None needs Joe's body after the header. Runtime HUD readers use the separate in-memory `:agent/invoke-prompt-preview`, not this evidence field (`src/futon3c/blackboard.clj:1297-1306`). | Dispatch/participant act. At these emit sites the code has agent/session/prompt but no operator-turn evidence id; the emitted body is only lifecycle evidence (`dev.clj:3693-3706`). | **Drop the text copy after extracting structured transport fields** (`surface`, `caller`, `from`, and preferably the dispatch/turn reference). A hash plus structured header is sufficient for every production reader found. |
| Context retrieval | `dev/futon3c/dev.clj:965-1045`; the A→B retrieval input is truncated into body `:query` by `emit-context-evidence!` (call at `:1014-1015`) | 5,485 / 4,388,514 B (4.39 MB) | 547,629 decoded query chars, about 12.5%; mean 100 chars/row (the stored field is capped at 100 by the emitter) | `src/futon3c/agency/pattern_card_provider.clj:130-146,179-196` reads `:results`, author, session and time, not `:query`; `src/futon3c/transport/http.clj:9201-9268` derives activation counts from `:results`, not `:query`; selection attestation at `http.clj:10582-10602` reads result ids and the evidence id, not `:query`. The only text users found are display/debug projections: the in-memory context HUD uses its local query at `dev.clj:1028-1042`, and `scripts/scope_view_data.py:255-268` uses the stored query to match a retrieval back to a turn prefix. That script could follow a turn reference instead. | Derived output, not the dispatch itself. `perform-context-retrieval!` receives agent/session/prompt/response/turn-id but no operator-turn evidence id (`dev.clj:980-991`); it generates a new retrieval evidence id (`:1001-1015`). | **Drop `:query` from the evidence row and add a turn reference plus input hash.** Keep result ids/scores/ranks; those are what all semantic readers consume. |
| 馬 observation | `scripts/ma_observe.py:377-403`; copied or derived transcript material is in body `observations` and `current-task`, with `turn-evidence-id` beside it | 245 / 907,496 B (0.91 MB) | 729,990 chars across `observations` + `current-task`, about 81.5%; mean 2,980 chars/row | Ma itself reads prior `observations` to avoid repetition and construct the next observer prompt (`ma_observe.py:330,416,475-487`), and uses them for progress extraction (`:570-579`). `scripts/turn_frames.py:748-763` projects both fields into the turn frame. `scripts/xiangma_port_export.py:103-108,166-219` exports Ma observations and their source linkage. These readers need Ma's derived summary, not verbatim Joe text; they can already follow `turn-evidence-id` for the source turn. | Derived output. The operator/seat source id is explicitly present as `turn-evidence-id` (`ma_observe.py:377-403`), so a reference is available at write time. | **Keep only Ma's genuinely derived observations/current-task and replace any quoted source passage with the existing `turn-evidence-id` plus short quote locators if needed.** Longer-term this operational observer state belongs in a derived store, but removing source-text copying does not require that move. |
| 象 settled turn memory | `src/futon3c/xiang/turn_service.clj:384-415,493-533`; `turn-evidence-entry` embeds the whole turn `:record`, including `:original-text`, `:reply-text`, and `:happened-summary` | 657 / 7,627,151 B (7.63 MB) | 2,293,945 decoded chars in those three fields, about 30.2%; mean 3,491 chars/row | `scripts/turn_frames.py:88-108,250-267,622-656` reads settled records/readings and may fall back to the evidence packet; `scripts/xiangma_port_export.py:214-245` uses the settled row to establish which turn records have evidence; both could follow the turn-record id. Live 象 consumers read the canonical turn store: `turn_service.clj:465-480` builds acts from `ts/read-record`, and `src/futon3c/xiang/ma_reading.clj:219-230` needs full reply text for quote validation but receives the canonical record, not this evidence copy. | Derived `:memory/:assert` packet. The canonical record carries `:evidence_id`, and the entry uses it as `:in-reply-to` when present (`turn_service.clj:384-415`), so the operator-turn evidence id is available; `:turn-record-id` is also stable. | **Replace copied record text with references** to the canonical turn record and operator-turn/reply evidence ids, retaining the analysis/reading and hashes. Readers needing full text can follow those references; the evidence packet should not duplicate `original-text`, `reply-text`, or `happened-summary`. |

## Writer details and classification

### Zai/Kimi transcript

The sample `e-3921bf40-…` is a `zai-1` `:turn-start`; samples
`e-32d75e12-…` and `e-e34f1ad7-…` are the same writer path under author
`象-1`. `transcript-entry` supplies `:coordination/:step` and tags
`:transcript`, the event and profile (`zai_api.clj:1297-1309`), while
`persist-turn-start!` merges the complete `:prompt` (`:1312-1327`). The row
is evidence of an actual dispatch to a seat, but its full prompt is a second
copy of source turns plus machine-added instructions. A plain operator-turn
id alone is insufficient for exact replay; a dispatch-payload document or
content-addressed prompt is the appropriate referenced object.

### Invoke preview

The sample `e-18ff74fe-…` is the warm-pouch branch at
`dev/futon3c/dev.clj:3990-4004`. The stored 200-character prefix includes the
transport header and then begins Joe's words. The production readers only
parse the header (`history_constraints.clj:64-78`), so copying the beginning
of the body is accidental rather than required.

### Context retrieval

The sample `e-286948f9-…` demonstrates that `:query` begins with Joe's turn.
`perform-context-retrieval!` builds `proto-text` from the first 200 characters
of the extracted user message plus the first 200 characters of the reply
(`dev/futon3c/dev.clj:980-991`). The persisted query is therefore a derived
search input containing source text, while all decision readers consume the
ranked `:results` instead.

### 馬

The sample `e-ef15c2e1-…` is not a byte-for-byte transcript copy: Ma has
summarised a Zai turn, but the summary repeats quoted phrases and source facts
that make FTS look like another occurrence of Joe's turn. This is derived
output rather than a dispatch. Its explicit `turn-evidence-id` already
provides the correct source edge, so source quotations should be bounded and
located rather than carrying whole passages.

### 象 turn service

The samples `e-xiang-turn-turn-0TXnm5` and `e-xiang-turn-turn-whDEjs` contain
the complete canonical record nested under `:record`. This duplicates both
Joe's `original-text` and the agent's `reply-text`, and often duplicates the
reply again inside `happened-summary`. The row is a derived settled reading,
not the participant act itself; the participant act remains the original
operator/agent evidence linked by `:evidence_id` / `:in-reply-to`.

## Measurement query shape

For each family, the bounded count/byte query was equivalent to:

```sql
SELECT count(*), sum(length(CAST(f.body AS blob)))
FROM ev_fts AS f JOIN ev_tags AS t ON t.id = f.id
WHERE f.at >= '2026-10-29T00:00:00Z'
  AND f.at <  '2026-11-06T00:00:00Z'
  AND t.tag = :family_tag;
```

Family tags were `:turn-start` (additionally authors `zai-1`,`象-1`),
`:invoke-start` (and body contains `prompt-preview`), `:context-retrieval`,
`:ma-observation`, and `:xiang-turn-analyzed`. Copied-character totals decoded
the named string fields before summing; this avoids counting escape characters
as copied prose.

## Writer 4 discovery

Discovery rerun 2026-11-05, read-only. The short answer is **B: put the
provider-facing payload in a content-addressed payload store outside evidence**.
There is a durable dispatch-prompt authority, but it is not the bytes that the
Zai/Kimi model receives.

### What is already retained

| Candidate authority | What it contains | Exact model input? | Retention |
|---|---|---|---|
| Agency invoke-job ledger / request commission | The raw prompt accepted at dispatch, keyed by job/dispatch id. `normalized-invoke-commission` deliberately retains that exact preimage (`src/futon3c/transport/http.clj:2143-2157`), and `invoke-job-request-commission` reads and verifies either its hot or archived copy (`:2159-2205`). | **No.** The job is created from the caller's raw `prompt` (`http.clj:6534-6558,6653-6687`); the transport header and projections are rendered later (`:5399-5483,5542-5554`). Zai can then prepend a context-compaction note/summary and a retrieved memory block (`src/futon3c/agents/zai_api.clj:2477-2482,2506-2525`). | Full terminal job detail is compacted after one day and its tombstone drops after seven days (`http.clj:507-516,684-700`), but before drop the request commission is copied to an immutable per-job archive (`:557-575,642-682`). No archive expiry/deletion path was found. |
| Bell/whistle/dispatch evidence | Coordination identities and receipts. Peripheral dispatch evidence has ids but no prompt (`src/futon3c/social/dispatch.clj:154-173`); whistle evidence has only a 200-character prompt truncation (`src/futon3c/social/whistles.clj:40-45`). A registered-agent work bell also creates a canonical 象 record containing the raw request prompt (`src/futon3c/transport/http.clj:13384-13445`). | **No.** These are the raw request or a truncation, before the rendered header, dynamic prompt facts, compaction and memory injection. The 象 record also exists only for the registered-agent/work-bell subset. | Evidence and canonical turn records are durable, but neither is a complete provider-payload authority. |
| Operator-turn evidence | Joe's original `chat-turn` text and its evidence id. | **No.** It is the participant act, not the specialist packet, transport header, retrieved context or prior-session material. It is nevertheless the right source reference for Joe's portion. |
| Kangaroo/pouch transcript | Claude's JSONL transcript is located by session and treated as provider-owned history (`src/futon3c/agency/agent_pouch.clj:134-154,210-215`). | **Not applicable to Zai/Kimi.** Their harness keeps `!messages` in the process. No corresponding durable provider transcript was found. | Claude/provider transcript lifetime, not a Zai/Kimi payload store. |
| Zai/Kimi `turn-round` rows | Assistant text (capped at 4096 characters) and tool-call summaries (`src/futon3c/agents/zai_api.clj:1177-1204,1673-1687`). | **No.** They do not contain the user prompt. | Evidence-store lifetime. |
| Current `turn-start` row | `prompt*`, its length, dispatch/turn ids and injection ids (`zai_api.clj:1312-1328,2513-2524`). | **Still not quite.** `prompt*` is the post-compaction prompt, but the model receives `prompt-with-memory`; the latter is what is appended to `!messages` (`:2506-2525`). Thus even today's large evidence copy omits the verbatim injected-memory block. | Evidence-store lifetime. |

The request-commission archive is therefore a useful, already-durable payload
reference for the *raw dispatch*. It cannot replace `turn-start :prompt` without
changing semantics for turns whose header/projection, context-compaction summary,
or memory block matters.

### Ten-row sample

I selected the newest five `zai-1` and newest five `象-1` rows by joining
`ev_fts` to `ev_tags` on tag `:turn-start`, ordered by `at DESC`, limit 5 per
author. “Operator” below is a conservative byte count: complete Joe
`chat-turn :text` strings of at least 20 characters from the preceding seven
days that occur exactly in the prompt, with overlapping spans merged. “Header”
is the leading `--- CURRENT TURN ---` through its terminating `---`; “other” is
the remainder. Thus “other” includes caller-authored task prose as well as
machine instructions and retrieved material; it is not a claim that every
remaining byte was generated by a machine.

| Author / at | Evidence id | Prompt chars | Header | Exact Joe turns | Other payload |
|---|---|---:|---:|---:|---:|
| zai-1 / 18:14:51 | `e-e1b95ed1-…` | 2,827 | 557 | 0 | 2,270 |
| zai-1 / 16:43:10 | `e-f44a6fc3-…` | 20,438 | 337 | 3,038 | 17,063 |
| zai-1 / 16:42:49 | `e-0d2d81f7-…` | 20,438 | 337 | 3,038 | 17,063 |
| zai-1 / 16:41:58 | `e-b78924ed-…` | 25,962 | 0 | 3,061 | 22,901 |
| zai-1 / 16:41:10 | `e-e76f8e25-…` | 19,866 | 337 | 3,061 | 16,468 |
| 象-1 / 21:04:02 | `e-c712c2e0-…` | 13,118 | 152 | 65 | 12,901 |
| 象-1 / 20:51:39 | `e-bd8d828c-…` | 15,863 | 152 | 0 | 15,711 |
| 象-1 / 20:48:23 | `e-9404302c-…` | 23,606 | 152 | 0 | 23,454 |
| 象-1 / 20:40:21 | `e-8df409c6-…` | 13,058 | 152 | 0 | 12,906 |
| 象-1 / 20:35:28 | `e-6bb05d95-…` | 19,924 | 152 | 0 | 19,772 |
| **Total / share** | | **175,100** | **2,328 (1.3%)** | **12,263 (7.0%)** | **160,509 (91.7%)** |

The very low exact-Joe share in recent 象 rows is expected: their packets tell
the seat to read a named canonical turn record rather than embedding that turn.
The repeated large Zai rows contain multiple complete historical Joe turns.
This exact-match method intentionally misses paraphrases and partial quotes.

The header renderer itself is versioned code (`http.clj:5399-5483`), but its
output includes dynamic prompt facts, notices, warrant state and backpack state
(`:5439-5480`). The payload can also include a model-produced compaction summary
(`zai_api.clj:2477-2482`) and retrieved memory (`:2506-2512`). Consequently the
non-operator portion is not reproducible from a code revision plus ids alone;
the resolved inputs or final bytes must be retained.

### What the readers actually require

- **馬:** it currently inserts the whole `turn-start` prompt as the user message
  in its observation history (`scripts/ma_observe.py:545-565`). Separately it
  strips the transport envelope and parses the packet body to distinguish new
  work from a continuation and to mint the next continuation
  (`ma_observe.py:284-299,718-740`). It needs the payload body, not an inline
  evidence copy; header plus operator/dispatch references and a resolvable
  payload hash are sufficient.
- **Turn frames:** `is_harness_turn_start` currently requires the `prompt` key
  (`scripts/turn_frames.py:477-486`), and frame assembly uses prompt as the
  displayed turn text and as a fallback join check (`:785-809`). It can retain
  the header in the row and dereference the payload for display; ids already do
  the primary joins.
- **Scope view:** it uses `dispatch-id` to map the start to round ids
  (`scripts/scope_view_data.py:124-140`), not prompt content.
- **Memory-use report:** it scans prompt only to infer whether the packet asked
  for memory (`scripts/memory_use_rate.py:88-118`). `turn-start` already retains
  typed `:requisition` and `:injected` metadata (`zai_api.clj:1312-1328`), so
  that report can use metadata instead of prose.

### Recommendation and costs

**Choose B.** At the Zai/Kimi boundary, write the exact provider-facing user
payload (`prompt-with-memory`) once to a content-addressed store, named by
SHA-256. Keep on `turn-start`: the hash, character/byte lengths, dispatch id,
turn id, operator-turn evidence id when known, typed injection/requisition
metadata, and the bounded transport header. Do not silently treat the Agency
commission as equivalent: it is the raw request, and its digest should remain a
separate provenance edge.

This costs one immutable blob write per distinct payload and roughly the same
order of storage as today's prompt copies (potentially more when injected
memory is present), but removes those bytes from full-text evidence search,
deduplicates identical payloads, and makes the “what was actually sent” claim
true. 馬 and `turn_frames.py` need a small hash resolver with typed
payload-missing behavior and old-row fallback; scope view needs no change;
`memory_use_rate.py` should switch to requisition/injection metadata.

Option A (only reference `invoke-job-request-commission`) is cheaper and already
durable, and is adequate when a reader wants the caller's original packet. It
is not adequate for exact replay or audit after dynamic header/projection,
compaction, and memory injection. Reconstructing those bytes from code and ids
(option C) is cheaper in storage but is not reliable because several inputs are
time-varying or model-produced.
