import { test } from "node:test";
import assert from "node:assert/strict";
import { changedTurns, configFromUrl, healthPane, obligationsPane, turnDetail, turnRows } from "../widget/model.js";
import type { TurnSummary, TurnView } from "../src/types.js";

test("configuration comes from the URL, query or fragment", () => {
  const c = configFromUrl("https://xiang.example.org/?agent=claude-17&session=abc&every=5000");
  assert.deepEqual(c, { base: "https://xiang.example.org", agent: "claude-17", session: "abc", every: 5000 });
  const f = configFromUrl("https://x.org/widget/#?agent=codex-1&base=http://h:7070/");
  assert.equal(f.agent, "codex-1");
  assert.equal(f.base, "http://h:7070");
  assert.equal(configFromUrl("https://x.org/?every=10").every, 15000, "too fast a poll falls back");
});

const summaries: TurnSummary[] = [
  { id: "turn-b", "turn-id": "matrix:job-1:reply", "agent-id": "claude-17", "session-id": "s", "created-at": "2026-10-02T10:00:05Z", "analysis-status": "requested", "source-text": "㊥ Done.\nmore" },
  { id: "turn-a", "turn-id": "matrix:job-1", "agent-id": "claude-17", "session-id": "s", "created-at": "2026-10-02T10:00:00Z", "analysis-status": "analyzed", "source-text": "please do the thing" },
];

test("turn rows read downward, with origin from the turn id", () => {
  const rows = turnRows(summaries);
  assert.deepEqual(
    rows.map((r) => [r.id, r.origin, r.author, r.status, r.preview]),
    [
      ["turn-a", "operator", "operator", "analyzed", "please do the thing"],
      ["turn-b", "agent", "claude-17", "requested", "㊥ Done."],
    ],
  );
});

test("changed turns are those whose status moved or that are new", () => {
  const later = [{ ...summaries[0]!, "analysis-status": "analyzed" as const }, summaries[1]!, { ...summaries[1]!, id: "turn-c" }];
  assert.deepEqual(changedTurns(summaries, later), ["turn-b", "turn-c"]);
});

test("the detail panel: marks, fragments, notices, author", () => {
  const view: TurnView = {
    ok: true,
    id: "turn-b",
    record: {
      version: 1,
      source_text: "㊥ Done.\n\n🈸 Shall I go on?",
      offset_unit: "unicode-codepoints-zero-based-end-exclusive",
      sentences: [{ id: "s1", start: 0, end: 7, text: "㊥ Done.", status: "unresolved", cues: [] }],
      unmatched: ["s1"],
      created_at: "2026-10-02T10:00:05Z",
      agent_id: "claude-17",
      session_id: "s",
      turn_id: "matrix:job-1:reply",
      origin: "agent",
      analysis_status: "analyzed",
      ...({ author: "claude-17", proforma_marks: [{ mark: "㊥", intent: "gist", stage: "annotator", text: "Done." }, { mark: "🈸", intent: "ask-action", stage: "act", text: "Shall I go on?" }] } as object),
    },
    analysis: {
      version: 2,
      status: "analyzed",
      labeller: "象-2",
      source_text: "㊥ Done.\n\n🈸 Shall I go on?",
      offset_unit: "unicode-codepoints-zero-based-end-exclusive",
      sentences: [{ id: "s1", fragments: [{ start: 0, end: 7, text: "㊥ Done.", intent: "gist", target: "the fix", rationale: "declared", relations: ["action"], display_cues: [{ start: 2, end: 6, text: "Done" }], pattern_refs: [{ id: "social/report-done", rationale: "fits" }] }] }],
    },
    candidates: null,
    notices: [{ kind: "unresolved", text: "withdraw inferred: unresolved (no target)" }],
  };
  const d = turnDetail(view);
  assert.equal(d.origin, "agent");
  assert.equal(d.author, "claude-17");
  assert.equal(d.labeller, "象-2");
  assert.deepEqual(d.marks.map((m) => m.intent), ["gist", "ask-action"]);
  assert.deepEqual(d.fragments, [{ sentence: "s1", intent: "gist", target: "the fix", rationale: "declared", patterns: ["social/report-done"] }]);
  assert.ok(d.html.includes('<mark class="xiang-cue" data-intent="gist"'));
  assert.equal(d.notices.length, 1);
});

test("the obligations pane from the route's shape, and its failures", () => {
  const pane = obligationsPane(200, {
    ok: true,
    "as-of": "2026-10-02T10:00:00Z",
    owes: [{ "obligation/id": "act:p1", "source/kind": "promise", creditor: "joe", "due-at": "2026-10-03T00:00:00Z", status: "open", deliverable: "the sha" }],
    owed: [{ "obligation/id": "act:a1", agreement: {}, debtor: "codex-10", status: "open" }],
    unchecked: [{}],
    incomplete: [],
  });
  assert.equal(pane.error, null);
  assert.deepEqual(pane.owes, [{ id: "act:p1", kind: "promise", counterparty: "joe", due: "2026-10-03T00:00:00Z", status: "open", deliverable: "the sha" }]);
  assert.deepEqual(pane.owed[0], { id: "act:a1", kind: "agreement", counterparty: "codex-10", due: null, status: "open", deliverable: "" });
  assert.equal(pane.unchecked, 1);
  assert.equal(obligationsPane(504, { ok: false, reason: "store-timeout" }).error, "store-timeout");
  assert.equal(obligationsPane(0, null).error, "unreachable");
});

test("the health pane", () => {
  assert.deepEqual(healthPane({ seat: "象-sonnet", health: { state: "ok", detail: "turn-x: analysed" }, benched: { "象-1": 1, "象-2": 1 }, outstanding: 3 }), {
    seat: "象-sonnet",
    state: "ok",
    detail: "turn-x: analysed",
    benched: ["象-1", "象-2"],
    outstanding: 3,
  });
  assert.equal(healthPane(null).seat, "?");
});
