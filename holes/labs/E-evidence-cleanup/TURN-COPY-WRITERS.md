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
