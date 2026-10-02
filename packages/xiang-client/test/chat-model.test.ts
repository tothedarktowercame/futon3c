import assert from "node:assert/strict";
import test from "node:test";
import { addressedBody, intents, shouldAutoScroll } from "../chat/model.js";
import type { TurnView } from "../src/types.js";

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
