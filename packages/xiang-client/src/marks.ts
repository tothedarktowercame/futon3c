/**
 * Marks: what the Emacs painter underlined, as data a renderer can use.
 *
 * `session-mode--paint-analysis-cues` underlined a display cue only when it
 * was short (at most 80 characters and 8 words), its text equalled the source
 * at its offsets, and it lay inside the source. `session-mode-repaint-past-turns`
 * showed the record's own lexical cues the same way. Both checks are kept:
 * an offset that is wrong is dropped, never drawn somewhere else.
 *
 * Every returned span carries both codepoint and UTF-16 offsets, so a DOM
 * renderer slices the string it has without re-deriving anything.
 */

import { codepointLength, cpSubstring, cpToUtf16, exactSpan, wordCount } from "./offsets.js";
import type { Analysis, Fragment, TurnRecord } from "./types.js";

export type MarkKind = "cue" | "lexical";

export interface Mark {
  kind: MarkKind;
  /** Codepoint offsets into source_text. */
  start: number;
  end: number;
  /** UTF-16 offsets into source_text. */
  utf16Start: number;
  utf16End: number;
  text: string;
  intent: string;
  /** Hover text: intent, target, rationale, labeller (the Emacs help-echo). */
  help: string;
  sentenceId: string;
}

export const MAX_CUE_CHARS = 80;
export const MAX_CUE_WORDS = 8;

export function shortPhrase(text: string): boolean {
  return codepointLength(text) <= MAX_CUE_CHARS && wordCount(text) <= MAX_CUE_WORDS && !text.includes("\n");
}

function fragmentHelp(fragment: Fragment, labeller: string | undefined): string {
  const parts = [fragment.intent];
  if (fragment.target) parts.push(`→ ${fragment.target}`);
  if (fragment.rationale) parts.push(fragment.rationale);
  if (fragment.pattern_refs && fragment.pattern_refs.length > 0) {
    parts.push("patterns: " + fragment.pattern_refs.map((r) => r.id).join(", "));
  }
  if (labeller) parts.push(`(${labeller})`);
  return parts.join(" · ");
}

/** The analysis's display cues that pass the painter's checks against `source`. */
export function analysisMarks(analysis: Analysis, source: string): Mark[] {
  const marks: Mark[] = [];
  if (analysis.status !== "analyzed" || analysis.source_text !== source) return marks;
  for (const sentence of analysis.sentences) {
    for (const fragment of sentence.fragments) {
      for (const cue of fragment.display_cues ?? []) {
        if (!exactSpan(source, cue.start, cue.end, cue.text)) continue;
        if (!shortPhrase(cue.text)) continue;
        marks.push({
          kind: "cue",
          start: cue.start,
          end: cue.end,
          utf16Start: cpToUtf16(source, cue.start),
          utf16End: cpToUtf16(source, cue.end),
          text: cue.text,
          intent: fragment.intent,
          help: fragmentHelp(fragment, analysis.labeller),
          sentenceId: sentence.id,
        });
      }
    }
  }
  return marks.sort((a, b) => a.start - b.start || a.end - b.end);
}

/** The record's own literal-phrase cues, drawn before any analysis lands. */
export function lexicalMarks(record: TurnRecord): Mark[] {
  const source = record.source_text;
  const marks: Mark[] = [];
  for (const sentence of record.sentences) {
    for (const cue of sentence.cues) {
      if (!exactSpan(source, cue.start, cue.end, cue.text)) continue;
      marks.push({
        kind: "lexical",
        start: cue.start,
        end: cue.end,
        utf16Start: cpToUtf16(source, cue.start),
        utf16End: cpToUtf16(source, cue.end),
        text: cue.text,
        intent: cue.label,
        help: `${cue.label} (${cue.method})`,
        sentenceId: sentence.id,
      });
    }
  }
  return marks.sort((a, b) => a.start - b.start || a.end - b.end);
}

/**
 * The marks to show for a turn: the analysis's cues when it has landed, else
 * the record's lexical cues, as the Emacs buffer did.
 */
export function marksFor(record: TurnRecord, analysis: Analysis | null | undefined): Mark[] {
  if (analysis) {
    const cues = analysisMarks(analysis, record.source_text);
    if (cues.length > 0) return cues;
  }
  return lexicalMarks(record);
}

export interface Segment {
  text: string;
  mark: Mark | null;
}

/**
 * `source` cut into plain and marked segments, in order. Overlapping marks
 * are resolved by taking the earlier one and skipping any that start inside
 * it, so the segments always tile the whole string.
 */
export function segments(source: string, marks: Mark[]): Segment[] {
  const out: Segment[] = [];
  let pos = 0;
  const total = codepointLength(source);
  for (const mark of [...marks].sort((a, b) => a.start - b.start || a.end - b.end)) {
    if (mark.start < pos) continue;
    if (mark.start > pos) out.push({ text: cpSubstring(source, pos, mark.start), mark: null });
    out.push({ text: cpSubstring(source, mark.start, mark.end), mark });
    pos = mark.end;
  }
  if (pos < total) out.push({ text: cpSubstring(source, pos, total), mark: null });
  return out;
}

export function escapeHtml(s: string): string {
  return s
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;")
    .replace(/'/g, "&#39;");
}

/**
 * HTML for the turn with its marks: `<mark class="xiang-cue" data-intent=…
 * title=…>` around cues, `<span class="xiang-lexical">` around lexical cues,
 * newlines as `<br>`. Safe to insert: all text is escaped.
 */
export function marksHtml(source: string, marks: Mark[]): string {
  return segments(source, marks)
    .map(({ text, mark }) => {
      const html = escapeHtml(text).replace(/\n/g, "<br>");
      if (!mark) return html;
      const tag = mark.kind === "cue" ? "mark" : "span";
      const cls = mark.kind === "cue" ? "xiang-cue" : "xiang-lexical";
      return `<${tag} class="${cls}" data-intent="${escapeHtml(mark.intent)}" title="${escapeHtml(mark.help)}">${html}</${tag}>`;
    })
    .join("");
}
