/**
 * Rendering a turn for Matrix (Element), where there is no buffer to paint.
 *
 * Two routes exist and are not exclusive:
 *
 * 1. The bridge route: `scripts/matrix_bridge.py` posts the agent's reply as
 *    an `m.room.message`; with `formattedTurn` it can post the operator turn
 *    (or a reaction to it) with the marks in `formatted_body`, which Element
 *    renders as HTML. No TypeScript runs in Element for this.
 *
 * 2. The widget route: an Element widget is an iframe app; the rest of this
 *    package (client.ts, stream.ts, marks.ts) is what it runs. The widget
 *    reads the room and session from its URL parameters and talks to
 *    futon3c directly.
 *
 * Element's HTML allowlist admits `<span>`, `<u>`, `<em>`, `<strong>`,
 * `<code>`, `<br>` and `data-mx-*` attributes; `<mark>` and `title=` are
 * stripped, so cues are rendered as `<u>` with the intent in `data-mx-spoiler`
 * (shown on hover/click) rather than as the `<mark>` of marks.ts.
 */

import { escapeHtml, marksFor, segments, type Mark } from "./marks.js";
import type { Analysis, Notice, TurnRecord } from "./types.js";

export interface MatrixMessage {
  msgtype: "m.notice" | "m.text";
  body: string;
  format: "org.matrix.custom.html";
  formatted_body: string;
}

function markHtml(text: string, mark: Mark): string {
  const html = escapeHtml(text).replace(/\n/g, "<br>");
  const label = escapeHtml(mark.kind === "cue" ? mark.help : mark.intent);
  // data-mx-spoiler on a <span> hides its content behind the reason; here
  // the reason is the reading and the content stays visible under <u>.
  return `<u><span data-mx-spoiler="${label}">${html}</span></u>`;
}

/**
 * The operator turn with its marks as a Matrix message. `body` is the plain
 * fallback with marks shown as ⟦text⟧ followed by the intent in brackets.
 */
export function formattedTurn(record: TurnRecord, analysis: Analysis | null | undefined, msgtype: "m.notice" | "m.text" = "m.notice"): MatrixMessage {
  const marks = marksFor(record, analysis);
  const parts = segments(record.source_text, marks);
  const formatted = parts.map(({ text, mark }) => (mark ? markHtml(text, mark) : escapeHtml(text).replace(/\n/g, "<br>"))).join("");
  const plain = parts.map(({ text, mark }) => (mark ? `⟦${text}⟧[${mark.intent}]` : text)).join("");
  return { msgtype, body: plain, format: "org.matrix.custom.html", formatted_body: formatted };
}

/** The withdrawal notices a REPL would have inserted, as one m.notice. */
export function noticesMessage(notices: Notice[]): MatrixMessage | null {
  if (notices.length === 0) return null;
  const lines = notices.map((n) => `象: ${n.text}`);
  return {
    msgtype: "m.notice",
    body: lines.join("\n"),
    format: "org.matrix.custom.html",
    formatted_body: lines.map((l) => `<em>${escapeHtml(l)}</em>`).join("<br>"),
  };
}
