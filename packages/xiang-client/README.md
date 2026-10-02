# @futon/xiang-client

The 象 (M-象-2000) frontend as a TypeScript library: what `emacs/agent-chat.el`
and `emacs/session-turn-analysis.el` did in a buffer, for a client that has no
filesystem and no python3 (a browser page, an Element widget, the Matrix
bridge).

The port has two halves, and only the second one is in this package:

1. **The server half** (`futon3c.xiang.turn-record`, `turn-store`,
   `turn-service`; routes under `/api/alpha/xiang/`). Recording a turn,
   dispatching it to a 象 seat, reaping the job and publishing the reading used
   to happen in Emacs with files under `~/.emacs-graph/session-turn-analysis/`
   and three python scripts. They now run in the futon3c JVM against the same
   directory and the same file shapes, so every existing reader
   (`turn_frames.py`, `xlate.py census`, feed.html, the stepper) is unchanged.
2. **This client**: a pure consumer of those routes plus the three agent-chat
   routes (`invoke-stream`, `agreement`, `withdrawal/undo`, `prompt-line`).

## Modules

| file | what it ports | from |
|---|---|---|
| `types.ts` | the turn record (seam schema), the analysis, the route payloads | `packages/turn-seam/turn-record.schema.json` |
| `offsets.ts` | codepoint ↔ UTF-16 conversion, exact-span check | the seam's `offset_unit` |
| `recognisers.ts` | `yes …`, `undo …`, the 🗣 surface marker | `agent-chat--acceptance-command`, `--undo-command`, `agent-chat-split-surface-marker` |
| `marks.ts` | which cues get drawn, and the HTML for them | `session-mode--paint-analysis-cues`, `session-mode-repaint-past-turns` |
| `stream.ts` | `POST /api/alpha/invoke-stream` as an async iterator of NDJSON events | `claude-repl.el`'s stream reader |
| `client.ts` | `XiangClient`: the routes, never throwing on an HTTP status | `agent-chat--check-acceptance`, `--maybe-handle-undo`, the python dispatch/reap scripts |
| `lines.ts` | the ✓/✗/? one-liners the buffer inserted | `agent-chat--report-acceptance`, `--maybe-handle-undo` |
| `matrix.ts` | a turn with its marks as an `m.room.message` with `formatted_body` | new |
| `conformance.ts` | does a record conform to the seam | `packages/turn-seam/conformance.py` |

## The turn, start to finish

```ts
import { XiangClient, invokeStream, parseAcceptance, undoCommand, undoOutcome,
         acceptanceLine, ACCEPTANCE_NO_EVIDENCE, marksFor, marksHtml } from "@futon/xiang-client";

const api = new XiangClient({ base: "http://localhost:7070" });
const agent = "claude-17", session = "564c8e50-…", turnId = `${agent}-turn-${n}`;

// 1. Before anything leaves: the two local recognisers.
const undo = undoCommand(text);
if (undo) {
  const r = undoOutcome(await api.undo({ agent, session, effect: undo === "undo" ? undefined : undo }));
  if (r.consumed) { show(r.line); return; }       // else the text goes to the agent unchanged
}

// 2. Record the turn (secrets redacted server-side; the response says which kinds).
const rec = await api.recordTurn({ text, "agent-id": agent, "session-id": session, "turn-id": turnId,
                                   "evidence-id": lastOperatorEvidenceId });
show(marksHtml(rec.json!.record.source_text, marksFor(rec.json!.record, null)));   // lexical cues now

// 3. A classical acceptance is also checked, in the background, never consuming the text.
if (parseAcceptance(text)) {
  if (!lastOperatorEvidenceId) show(ACCEPTANCE_NO_EVIDENCE);
  else api.agreement({ agent, session, text, evidenceId: lastOperatorEvidenceId }).then(r => show(acceptanceLine(r)));
}

// 4. Send it and stream the reply.
let reply = "";
for await (const ev of invokeStream("http://localhost:7070", { "agent-id": agent, prompt: text, surface: "web", "turn-id": turnId })) {
  if (ev.type === "text") reply += ev.text;
  if (ev.type === "done" && ev["prompt-line"]) drawPrompt(ev["prompt-line"]);
}

// 5. At reply end, tell 象 what happened; the server dispatches and reaps.
await api.happened(rec.json!.id, { reply, commits: [] });

// 6. Later: the reading, and the notices a REPL would have shown.
const view = await api.getTurn(rec.json!.id);
if (view.json?.analysis) show(marksHtml(view.json.record.source_text, marksFor(view.json.record, view.json.analysis)));
for (const n of view.json?.notices ?? []) show(`象: ${n.text}`);
```

## Element

Two routes, not exclusive:

- **Bridge route, no TypeScript in Element.** `scripts/matrix_bridge.py` already
  posts agent replies into a room. `formattedTurn(record, analysis)` gives it
  an `m.room.message` whose `formatted_body` carries the marks as `<u>` with
  the reading in `data-mx-spoiler`, which Element renders; `noticesMessage`
  gives the withdrawal notices as one `m.notice`. The bridge calls the
  `/api/alpha/xiang/` routes like any client (`recordTurn` when a room
  message is routed to an agent, `happened` when the reply lands).
- **Widget route.** An Element widget is an iframe app; this package is what
  it runs. The widget reads the agent and session from its URL parameters and
  talks to futon3c directly over `fetch`, so it needs CORS (the JSON routes
  already send `Access-Control-Allow-Origin: *`) and a reachable base URL.
  The stepper and per-fragment hover help exist only on this route.

## Offsets

Every offset in a record or an analysis is a Unicode codepoint, zero-based,
end-exclusive. A JavaScript string indexes UTF-16 code units, so a turn with
an emoji or CJK (`🗣`, `象`) has different numbers in the two systems and the
difference is silent. `marks.ts` returns both (`start`/`end` and
`utf16Start`/`utf16End`), and never draws a cue whose text at its codepoint
offsets is not the cue's text. `conformance.ts` is the same check over a whole
record; run it on anything you did not write yourself.

## Build and test

```sh
cd packages/xiang-client
npm install
npm test        # tsc, then node --test over dist/test
```

Node 22 and TypeScript 5.5; no runtime dependencies.
