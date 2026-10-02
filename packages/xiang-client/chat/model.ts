import type { TurnView } from "../src/types.js";

export const GLYPHS: Record<string, string> = {
  gist: "㊥", constrain: "🈲", propose: "㊭", "ask-action": "🈸", approve: "㊣",
  delegate: "㊯", disagree: "🈚", prioritize: "㊝", qualify: "㊟", collect: "㊮",
  explain: "🈖", extend: "🈕", clarify: "🈯", continue: "🈰", report: "㊢",
  defer: "🈝", "report-problem": "㊩", redirect: "🈘", verify: "㊬", explore: "㊫",
  retract: "🈹", withdraw: "🈡", unresolved: "🈳",
};

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
