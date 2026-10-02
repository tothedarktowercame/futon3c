import type { TurnSummary, TurnView } from "../src/types.js";

export const GLYPHS: Record<string, string> = {
  gist: "㊥", constrain: "🈲", propose: "㊭", "ask-action": "🈸", approve: "㊣",
  delegate: "㊯", disagree: "🈚", prioritize: "㊝", qualify: "㊟", collect: "㊮",
  explain: "🈖", extend: "🈕", clarify: "🈯", continue: "🈰", report: "㊢",
  defer: "🈝", "report-problem": "㊩", redirect: "🈘", verify: "㊬", explore: "㊫",
  retract: "🈹", withdraw: "🈡", unresolved: "🈳",
};

export type IntentStage = "perceive" | "believe" | "evaluate" | "select" | "act" | "annotator";

const STAGES: Record<string, IntentStage> = {
  "report-problem": "perceive", explain: "perceive", report: "perceive",
  clarify: "believe", qualify: "believe", approve: "believe", disagree: "believe", collect: "believe", retract: "believe",
  constrain: "evaluate", extend: "evaluate", explore: "evaluate",
  propose: "select", prioritize: "select", redirect: "select", defer: "select", delegate: "select", withdraw: "select",
  "ask-action": "act", continue: "act", verify: "act",
  gist: "annotator", unresolved: "annotator",
};

export function intentStage(intent: string): IntentStage {
  return STAGES[intent] ?? "annotator";
}

export function pageBounds(total: number, pageSize: number, offset: number): { start: number; end: number; offset: number } {
  const size = Math.max(1, Math.floor(pageSize));
  const boundedOffset = Math.max(0, Math.min(Math.floor(offset), Math.max(0, total - 1)));
  const end = Math.max(0, total - boundedOffset);
  return { start: Math.max(0, end - size), end, offset: boundedOffset };
}

/** A requested turn is mutable: fetch it again once the summary says analysis landed. */
export function needsViewRefresh(summary: TurnSummary, cached: TurnView | undefined): boolean {
  return !cached || (cached.record.analysis_status ?? "not-requested") !== (summary["analysis-status"] ?? "not-requested");
}

export function countByAuthor(events: Array<{ sender: string }>): Array<{ author: string; count: number }> {
  const counts = new Map<string, number>();
  for (const event of events) counts.set(event.sender, (counts.get(event.sender) ?? 0) + 1);
  return [...counts].map(([author, count]) => ({ author, count })).sort((a, b) => b.count - a.count || a.author.localeCompare(b.author));
}

export function postsPerAuthorPython(events: Array<{ sender: string }>): string {
  const authors = JSON.stringify(events.map((event) => event.sender));
  return `from collections import Counter\nimport marimo as mo\n\nauthors = ${authors}\ncounts = Counter(authors)\nmo.ui.altair_chart({\n    "mark": "bar",\n    "data": {"values": [{"author": author, "posts": posts} for author, posts in counts.items()]},\n    "encoding": {\n        "x": {"field": "author", "type": "nominal", "sort": "-y"},\n        "y": {"field": "posts", "type": "quantitative"},\n    },\n})`;
}

export function anchorsAuthorChart(body: string): boolean {
  const text = body.toLowerCase();
  return (text.includes("bar chart") && text.includes("posts per author"))
    || text.includes("first python-cell chart");
}

export function intents(view: TurnView): Array<{ intent: string; glyph: string; declared: boolean }> {
  const record = view.record as typeof view.record & { proforma_marks?: Array<{ intent: string; mark: string }> };
  const declared = (record.proforma_marks ?? []).map((m) => ({ intent: m.intent, glyph: m.mark, declared: true }));
  if (declared.length) return unique(declared);
  const inferred = (view.analysis?.sentences ?? []).flatMap((s) => s.fragments.map((f) => ({
    intent: f.intent, glyph: GLYPHS[f.intent] ?? "·", declared: false,
  })));
  return unique(inferred);
}

function unique<T extends { intent: string }>(rows: T[]): T[] {
  return [...new Map(rows.map((r) => [r.intent, r])).values()];
}

export function addressedBody(agent: string, body: string): string {
  const text = body.trim();
  return agent ? `@${agent} ${text}` : text;
}

/** Polling must never move the conversation while the operator is composing. */
export function shouldAutoScroll(distanceFromBottom: number, composing: boolean): boolean {
  return !composing && distanceFromBottom <= 80;
}
