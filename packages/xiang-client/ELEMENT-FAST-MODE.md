# 象 fast mode in the FUTON Element fork

2026-10-02, claude-17, for the Codex agent working in `element-web-futon`
(branch `futon/main`). Two changes to the fork, items 1 and 4 of the plan Joe
approved. Items 2 and 3 are futon3c changes and are noted at the end because
item 1 reads what they produce.

Line numbers are for `element-web-futon` at fd64e618af.

## Item 1: show 小象's draft before 象's analysis arrives

**Today.** `apps/web/src/customisations/FutonIntentMarks.ts:79`
`validatedIntentMarks` underlines two things only:
- `analysis.sentences[].fragments[].display_cues`, when `analysis.status` is
  `"analyzed"`;
- otherwise the lexical cues in `record.sentences[].cues`.

`GET /chat-api/api/xiang/turns/:id` (futon3c `turn-view`) also returns
`draft`: 小象's naive-Bayes reading, stored when the turn is recorded (about
0.06 s). Element never reads it. The analysis itself waits for the agent's
reply and then for a 象 job (the first reap is 180 s after dispatch), so for
minutes a turn shows lexical cues or nothing.

**Draft shape** (futon3c `turn_record.clj` `validate-draft`):

```json
{"status": "drafted", "labeller": "小象", "source_text": "...",
 "fragments": [{"start": 0, "end": 20, "text": "...", "intent": "ask-action" | null,
                "sure": true, "guesses": ["ask-action", "approve"],
                "precision": 0.52, "sentence": "s1",
                "basis": "declared" | "model", "mark": "🈸"}]}
```

Offsets are codepoints into `source_text`, as for analysis cues. `basis` and
`mark` arrive with item 2. Treat them as optional, and treat a missing `basis`
as `"model"`.

**Change.**
- Order of preference: analysis (as now), then the draft, then lexical cues.
- Draft fragments are whole fragments, not short keyword cues, so do not
  underline the full span. Underline the leading mark when `basis` is
  `"declared"`. Otherwise underline the first one to three words of the
  fragment. Use `exactMark` for validation, so `MAX_CUE_WORDS`,
  `MAX_CUE_CODEPOINTS` and the exact-text check still apply.
- Sure fragments (`intent` non-null) show their intent. An unsure fragment
  shows `?` plus its two `guesses` in the hover help.
- Give draft marks a separate style: for example, `data-futon-intent-basis`
  set to `draft` or `declared`, a dotted underline, and help text ending in
  `(小象)`. That way a draft is never mistaken for 象's reading. Declared marks
  are the author's own act, so they may use the solid style.
- When the analysis arrives, it replaces the draft marks (this already
  happens, because `applyFutonIntentMarks` clears the old marks first).

**Tests** (`FutonIntentMarks.test.ts`):
- a detail with a draft and no analysis yields draft marks;
- a detail with both yields only the analysis marks;
- an unsure fragment yields `?` help with both guesses;
- a draft whose text does not match `source_text` yields no mark.

## Item 4: poll once per room, not once per message

**Today.**
- `TextualBodyFactory.tsx:283-300`: every rendered message body starts its
  own `setInterval(..., 10_000)`. Each tick runs
  `decorateWithFutonIntentMarks`, which fetches that turn's detail and clears
  and rebuilds the underlines. With 50 bodies on screen that is 50 detail
  fetches and 50 DOM rewrites every 10 s, including for turns whose analysis
  is final.
- `FutonIntentMarks.ts:161`: the shared list is
  `/api/xiang/turns?limit=300`. It covers all sessions and carries every
  turn's full `source-text`, which is 167 KB per fetch. A Matrix event older
  than the newest 300 turns system-wide gets no marks at all.
- `TextualBodyFactory.tsx:340-347`: each render of each body maps the whole
  live timeline to collect authors for `requestsPostsPerAuthorCell`. This is
  quadratic in the number of messages, and it runs for every message, even
  though only a Marimo request uses it.

**Change.**
- One poller per room, in `FutonIntentMarks.ts`, started by the first body
  that mounts in the room and stopped when the last one unmounts. It keeps a
  map from evidence-id to `{turnId, analysisStatus}` and calls registered
  bodies back only when their turn appears or its `analysis-status` changes.
  A body registers its event id and element; it starts no timer of its own.
- Fetch a turn's detail only on first sight and when its status changes.
  Stop polling a turn once its status is final. Final statuses are
  `analyzed`, `refused` and `failed`, plus whatever status item 3 introduces
  for recorded agent replies; read it from that commit.
- Narrow the list query. The route already accepts `session=` and `agent=`
  (futon3c `http.clj` `handle-xiang`). If the room-to-session mapping is not
  available to the client, ask claude-17 for a `room=` filter rather than
  raising `limit`.
- Compute `authors` only when `requestsPostsPerAuthorCell(content.body)` is
  true, and compute it inside that branch.

**Tests.** With a fake fetch and fake timers:
- ten bodies in one room produce one list fetch per interval, not ten;
- a body whose turn is already `analyzed` triggers no further detail fetch;
- a status change triggers exactly one detail fetch and one redecorate.

## The futon3c side (not for this fork)

- Item 2 (kimi-1): the draft takes a leading proforma mark as the declared
  intent (`basis: "declared"`, `mark`). Before this change, `🈸 yes please do
  that` was drafted as `approve`.
- Item 3 (kimi-3): the bridges stop asking 象 to analyse agent replies. A
  reply is still recorded, with the author's `proforma_marks`, so item 1 can
  underline the marks an agent wrote without any LLM reading.

Neither change alters the `turn-view` response shape beyond the optional
`basis`/`mark` fields above.

## Not covered

Correcting a mark from Element (click an underline and choose the right
intent, which 小象 then trains on) has no route yet. The chat proxy forwards
GETs only, and futon3c has no correction action. It is a later step.
