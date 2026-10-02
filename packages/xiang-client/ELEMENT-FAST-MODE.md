# 象 fast mode in the FUTON Element fork

2026-10-02, claude-17, for the Codex agent working in `element-web-futon`
(branch `futon/main`). Two changes to the fork, items 1 and 4 of the plan Joe
approved. Items 2 and 3 are futon3c changes and are noted at the end because
item 1 reads what they produce.

Line numbers are for `element-web-futon` at fd64e618af.

## Handoff

Implement items 1 and 4 below in `element-web-futon` on `futon/main`. Make
one commit per item, item 4 first: it changes the call sites that item 1
then extends. Each commit carries the tests listed for its item. Gates:

- the fork's lint (`pnpm lint:types`, plus eslint on the touched files);
- the unit tests for `src/customisations/` (`pnpm test:unit`, vitest, filtered to those files);
- a build with `apps/web/scripts/build-futon.sh`.

Then check by hand in a room where the 象 widget is mounted:

- a new operator message shows dotted 小象 marks within a few seconds;
- they are replaced by 象's marks once the analysis lands;
- the network panel shows one turn-list request per 10 s per room.

Report the commit shas, the test output, and what the manual check showed.

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
  `/api/xiang/turns?limit=300`. Since futon3c 67758c73 the proxy returns
  only turns recorded in the verified room, without `source-text`, so this
  response is now small. Its `limit` counts this room's turns only.
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
- Leave the list query as it is: the proxy already scopes it to the room.
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
