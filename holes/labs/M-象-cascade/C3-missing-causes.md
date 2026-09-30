# C3 — causes of the 15 missing C1 triples

Date: 2026-09-30. Discovery only; the stack and C1 script were not changed.

## Method

For every missing row in `corpus-c1.jsonl`, I read its local
`~/.emacs-graph/session-turn-analysis/<turn>.json`, the C1 cached frames, and
the relevant evidence session. The prototype in `/tmp/xiang_c3_probe.py`
called `turn_frames.fetch_evidence(session, page_limit=1000)`, changed the
operator predicate in memory to also admit a legacy row only when it was a
Joe-authored `coordination` `chat-turn` with role `user` and no origin, then
required exact equality with the local record's `source_text`. It wrote
`/tmp/xiang-c3-probe.json`; no repository script or live state changed.

The phrase queries below were:

```text
GET :7073/api/alpha/evidence/text-search?q=<quoted phrase>&session-id=<sid>&limit=10
Accept: application/json
```

No query returned 504. “Legacy exact” below means the scratch session LIST
contained exactly one Joe/user chat-turn whose text equalled the entire local
`source_text`; its evidence id and time are shown.

## Findings, one row per missing turn

| Turn | Family | C1 reason | Actual cause | Evidence (exact read and output) | Join change recovers? |
|---|---|---|---|---|---|
| `turn-3ecbrN` | go-ahead | turn-not-found | Operator chat-turn absent. A downstream context-retrieval row retained the words, but that is not the operator row. | Local file: no `evidence_id`, `created_at=2026-09-28T19:33:02Z`. Exact-text scan of 113 legacy-inclusive frames: 0 matches. Text-search phrase `OK, all of that sounds good...`: count 1, only `e-5da9f979...`, context-retrieval; exact chat-turn count 0. | no |
| `turn-DIIjU8` | go-ahead | turn-not-found | Legacy operator row has no `evidence/origin`; `turn_frames.is_operator_turn` filtered it out. | Legacy exact: `emacs-c019eaf90f11ef12257a2ca2f2f0efae`, `2026-09-27T18:33:27.697114024Z`; local `created_at=18:33:28Z`. | yes |
| `turn-HfZ1oZ` | go-ahead | turn-not-found | Legacy origin missing. The modern-only cache starts three days after this turn. | Legacy exact: `emacs-69992988018475bd3d762904ea566bf1`, `2026-09-24T19:17:02.282112438Z`; local `19:17:02Z`. | yes |
| `turn-MZx4U8` | go-ahead | turn-not-found | Legacy origin missing. The modern-only cache begins 5 h 13 min later. | Legacy exact: `emacs-cec3fc08f3844ed89c2b78c02a377590`, `2026-09-27T16:06:18.113445988Z`; local `16:06:18Z`. | yes |
| `turn-VjI6w9` | go-ahead | turn-not-found | Legacy origin missing. | Legacy exact: `emacs-a8eab385cf9846d9dae1b786e56aa557`, `2026-09-24T20:15:35.545234637Z`; local `20:15:35Z`. | yes |
| `turn-gFfHfw` | go-ahead | session-no-frames | Session has evidence, but every relevant operator row is legacy and lacks origin; the cached frame list is `[]`. | Text-search phrase `Yes, this makes sense to me`: count 2; exact chat-turn `emacs-01e42fc162240c2f1daa9196e3703bb5`, `2026-09-26T11:43:35.340117839Z`, role user, origin nil. Legacy rebuild: 67 frames, exact index 47. | yes |
| `turn-gtZeub` | go-ahead | turn-not-found | Operator chat-turn absent; context retrieval holds the query text only. | Local: no evidence id, `2026-09-28T12:31:52Z`. Exact scan of 174 legacy-inclusive frames: 0. Text-search phrase `In that case let's continue with the mission`: count 1, only `e-855f2c11...`, context-retrieval; exact chat-turn count 0. | no |
| `turn-lPDozk` | go-ahead | turn-not-found | Legacy origin missing. | Legacy exact: `emacs-83f7f3429b8970492e5813664228ed0d`, `2026-09-24T22:02:02.228956443Z`; local `22:02:02Z`. | yes |
| `turn-wYIXMd` | go-ahead | turn-not-found | Operator chat-turn absent. | Local: no evidence id, `2026-09-27T18:41:53Z`. Exact scan of 174 legacy-inclusive frames: 0. Text-search phrase `Right, so your P13A is interesting...`: count 0. | no |
| `turn-0anLV2` | correction | turn-not-found | The row is already a modern operator frame, but local record time precedes evidence time by 91.702 s, outside C1's 60 s bound; local record has no evidence id. | Cached frame exact text at index 105: `emacs-93c0678df5b51981e1d3cbcdfc1eb32d`, `2026-09-30T01:56:19.701846410Z`; local `created_at=01:54:48Z`. | yes, by unique exact text |
| `turn-2GmY4p` | correction | turn-not-found | Legacy origin missing. The modern-only cache begins 30 h 54 min later. | Legacy exact: `emacs-414ebf4e21842621b30a51ff5f6d37af`, `2026-09-26T14:24:58.899217871Z`; local `14:24:59Z`. | yes |
| `turn-QLrMMq` | correction | session-no-frames | Evidence exists, but the operator row is legacy with origin nil. | Text-search phrase `Can you check if futon1b has the desired features`: count 2; exact chat-turn `emacs-cdccb952f1370b55aeb52cec9e12af16`, `2026-09-26T10:59:41.628209792Z`, role user, origin nil. Legacy rebuild: 153 frames, exact index 132. | yes |
| `turn-Xy8ygM` | correction | pre-frame-missing | It is only the first **modern-origin** frame, not the session's first operator turn. Two legacy operator frames precede it and provide its pre-frame. | Original cache: exact index 0 of 3. Legacy rebuild: 5 frames, same evidence id `emacs-6aa2374e245e9d00909e0bce0e59074f` at index 2. | yes |
| `turn-ZUtRsq` | correction | session-no-frames | Evidence exists, but operator origin is nil. | Text-search phrase `I am seeing the most recent turns annotated`: count 5; exact chat-turn `emacs-de7ef917a6c4abd9c8501ac274243903`, `2026-09-26T03:36:23.683768344Z`, role user, origin nil. Legacy rebuild: 67 frames, exact index 33. | yes |
| `turn-qiO3lB` | correction | session-no-frames | Older Codex REPL chat-turn is stored without origin. | Text-search phrase `Do we have a flexarg that would capture`: count 1; exact chat-turn `emacs-1346e5405747c1245942d35c4263dc7d`, `2026-09-22T17:31:52.803989184Z`, role user, origin nil. Legacy rebuild: 21 frames, exact index 16. | yes |

## Proposed C1 join change and computed effect

I would change frame construction, then the C1 join, in this order:

1. In `turn_frames.build_frames`, retain the current origin-stamped operator
   rule. Also admit an origin-nil Joe/user chat-turn **only when its complete
   text exactly matches one local operator analysis record in that session**.
   This avoids treating an old harness user row as Joe merely because origin
   had not yet been added.
2. In `xiang_cascade_corpus.find_frame`, keep evidence id first and bounded
   time second, then add a unique exact-`source_text` fallback. This recovers
   the modern `turn-0anLV2` skew without widening the time window.

The scratch prototype computed the result, rather than extrapolating:

| Family | C1 full | Recovered | Full after change | Still missing |
|---|---:|---:|---:|---:|
| go-ahead | 26 | 6 | 32 | 3 |
| done-is-observed-running correction | 7 | 6 | **13** | 0 |
| total | 33 | 12 | 45 | 3 |

Thus the correction family crosses the declared C2 minimum of ten. The three
remaining go-ahead rows require historical recovery of missing operator
chat-turn evidence; changing a join cannot recover them.
