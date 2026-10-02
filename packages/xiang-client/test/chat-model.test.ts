import assert from "node:assert/strict";
import test from "node:test";
import { addressedBody, anchorsAuthorChart, countByAuthor, intentStage, intents, needsViewRefresh, pageBounds, postsPerAuthorPython, shouldAutoScroll } from "../chat/model.js";
import type { TurnSummary, TurnView } from "../src/types.js";

test("an addressed message uses the Matrix bot identity", () => {
  assert.equal(addressedBody("fucodex", " hello "), "@fucodex hello");
  assert.equal(addressedBody("", " room note "), "room note");
});

test("authored proforma marks remain distinct from inferred intentions", () => {
  const base = { ok: true, id: "t", candidates: null, notices: [], record: { source_text: "x", proforma_marks: [{ mark: "㊥", intent: "gist" }] }, analysis: null } as unknown as TurnView;
  assert.deepEqual(intents(base), [{ intent: "gist", glyph: "㊥", declared: true }]);
  const inferred = { ...base, record: { ...base.record, proforma_marks: [] }, analysis: { status: "analyzed", sentences: [{ id: "s1", fragments: [{ intent: "redirect" }] }] } } as unknown as TurnView;
  assert.deepEqual(intents(inferred), [{ intent: "redirect", glyph: "🈘", declared: false }]);
});

test("polling never auto-scrolls while the operator is composing", () => {
  assert.equal(shouldAutoScroll(0, true), false);
  assert.equal(shouldAutoScroll(40, false), true);
  assert.equal(shouldAutoScroll(200, false), false);
});

test("history pages backwards from the latest three turns", () => {
  assert.deepEqual(pageBounds(10, 3, 0), { start: 7, end: 10, offset: 0 });
  assert.deepEqual(pageBounds(10, 3, 3), { start: 4, end: 7, offset: 3 });
  assert.deepEqual(pageBounds(2, 3, 0), { start: 0, end: 2, offset: 0 });
});

test("intent stages mirror the Emacs proforma palette", () => {
  assert.equal(intentStage("report"), "perceive");
  assert.equal(intentStage("redirect"), "select");
  assert.equal(intentStage("verify"), "act");
});

test("a cached requested turn is refreshed when asynchronous analysis lands", () => {
  const summary = { "analysis-status": "analyzed" } as TurnSummary;
  const cached = { record: { analysis_status: "requested" } } as TurnView;
  assert.equal(needsViewRefresh(summary, cached), true);
  assert.equal(needsViewRefresh(summary, { record: { analysis_status: "analyzed" } } as TurnView), false);
});

test("room posts become a deterministic Marimo-ready author count cell", () => {
  const events = [{ sender: "@joe:example" }, { sender: "@bot:example" }, { sender: "@joe:example" }];
  assert.deepEqual(countByAuthor(events), [{ author: "@joe:example", count: 2 }, { author: "@bot:example", count: 1 }]);
  const code = postsPerAuthorPython(events);
  assert.match(code, /import marimo as mo/);
  assert.match(code, /mo\.ui\.altair_chart/);
  assert.match(code, /@joe:example/);
  assert.equal(anchorsAuthorChart("㊥ (first Python-cell chart) Implemented"), true);
});
