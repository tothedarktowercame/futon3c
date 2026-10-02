import { test } from "node:test";
import assert from "node:assert/strict";
import { analysisMarks, lexicalMarks, marksFor, marksHtml, segments } from "../src/marks.js";
import type { Analysis, TurnRecord } from "../src/types.js";

const source = "I agree 🗣 with 象 here.  Let's ask 象-2 next?";

const record: TurnRecord = {
  version: 1,
  source_text: source,
  offset_unit: "unicode-codepoints-zero-based-end-exclusive",
  sentences: [
    { id: "s1", start: 0, end: 22, text: "I agree 🗣 with 象 here.", status: "cue-only", cues: [{ start: 0, end: 7, label: "approve", text: "I agree", method: "literal-phrase" }] },
    { id: "s2", start: 24, end: 43, text: "Let's ask 象-2 next?", status: "cue-only", cues: [{ start: 24, end: 33, label: "delegate", text: "Let's ask", method: "literal-phrase" }] },
  ],
  unmatched: [],
  created_at: "2026-10-02T00:00:00Z",
  agent_id: "claude-17",
  session_id: "s",
  turn_id: "t",
};

const analysis: Analysis = {
  version: 2,
  status: "analyzed",
  labeller: "象-1",
  source_text: source,
  offset_unit: "unicode-codepoints-zero-based-end-exclusive",
  sentences: [
    {
      id: "s1",
      fragments: [{ start: 0, end: 22, text: "I agree 🗣 with 象 here.", intent: "approve", target: "the plan", rationale: "plain agreement", relations: ["action"], display_cues: [{ start: 0, end: 7, text: "I agree" }] }],
    },
    {
      id: "s2",
      fragments: [
        {
          start: 24,
          end: 43,
          text: "Let's ask 象-2 next?",
          intent: "delegate",
          target: "象-2",
          rationale: "r",
          relations: ["action"],
          display_cues: [
            { start: 34, end: 37, text: "象-2" },
            { start: 24, end: 33, text: "WRONG TEXT" },
            { start: 24, end: 25, text: "L" },
          ],
          pattern_refs: [{ id: "social/ask-the-seat", rationale: "fits" }],
        },
      ],
    },
  ],
};

test("analysis cues are painted only when they pass the painter's checks", () => {
  const marks = analysisMarks(analysis, source);
  assert.deepEqual(
    marks.map((m) => [m.start, m.end, m.text, m.intent, m.utf16Start, m.utf16End]),
    [
      [0, 7, "I agree", "approve", 0, 7],
      [24, 25, "L", "delegate", 25, 26],
      [34, 37, "象-2", "delegate", 35, 38],
    ],
  );
  assert.equal(marks[2]!.help, "delegate · → 象-2 · r · patterns: social/ask-the-seat · (象-1)");
});

test("an analysis for other text paints nothing", () => {
  assert.deepEqual(analysisMarks({ ...analysis, source_text: "something else" }, source), []);
});

test("lexical cues are the fallback", () => {
  assert.deepEqual(
    lexicalMarks(record).map((m) => [m.start, m.end, m.intent, m.kind]),
    [
      [0, 7, "approve", "lexical"],
      [24, 33, "delegate", "lexical"],
    ],
  );
  assert.equal(marksFor(record, null)[0]!.kind, "lexical");
  assert.equal(marksFor(record, analysis)[0]!.kind, "cue");
});

test("segments tile the string and overlapping marks are dropped", () => {
  const marks = analysisMarks(analysis, source);
  const parts = segments(source, marks);
  assert.equal(parts.map((p) => p.text).join(""), source);
  assert.deepEqual(parts.filter((p) => p.mark).map((p) => p.text), ["I agree", "L", "象-2"]);
});

test("HTML is escaped and marked", () => {
  // "a <b> & " is 8 codepoints, so "I agree" is [8, 15).
  const html = marksHtml("a <b> & I agree\nok", [
    { kind: "cue", start: 8, end: 15, utf16Start: 8, utf16End: 15, text: "I agree", intent: "approve", help: 'say "yes"', sentenceId: "s1" },
  ]);
  assert.equal(html, 'a &lt;b&gt; &amp; <mark class="xiang-cue" data-intent="approve" title="say &quot;yes&quot;">I agree</mark><br>ok');
});

test("the draft tier sits between the reading and the lexical cues", async () => {
  const { draftMarks } = await import("../src/marks.js");
  const draft = {
    version: 1 as const,
    status: "drafted" as const,
    labeller: "小象",
    source_text: source,
    offset_unit: "unicode-codepoints-zero-based-end-exclusive" as const,
    fragments: [
      { start: 0, end: 22, text: "I agree 🗣 with 象 here.", intent: "approve", sure: true, guesses: ["approve", "report"], precision: 0.84, sentence: "s1" },
      { start: 24, end: 43, text: "Let's ask 象-2 next?", intent: null, sure: false, guesses: ["delegate", "clarify"], sentence: "s2" },
    ],
  };
  const d = draftMarks(draft, source);
  assert.deepEqual(d.map((m) => [m.kind, m.start, m.end, m.intent, m.help]), [["draft", 0, 22, "approve", "approve (小象, p 0.84)"]]);
  assert.equal(marksFor(record, null, draft)[0]!.kind, "draft");
  assert.equal(marksFor(record, analysis, draft)[0]!.kind, "cue");
  assert.equal(marksFor(record, null, { ...draft, source_text: "other" })[0]!.kind, "lexical");
  assert.ok(marksHtml(source, d).startsWith('<span class="xiang-draft" data-intent="approve"'));
});
