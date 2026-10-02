import { test } from "node:test";
import assert from "node:assert/strict";
import { codepointLength, cpSubstring, cpToUtf16, exactSpan, toUtf16Span, utf16ToCp, wordCount } from "../src/offsets.js";

const text = "I agree 🗣 with 象 here.  Let's ask 象-2 next?";

test("codepoints and UTF-16 units part at the emoji", () => {
  assert.equal(codepointLength(text), text.length - 1);
  assert.equal(cpToUtf16(text, 8), 8);
  assert.equal(cpToUtf16(text, 9), 10, "after 🗣 the UTF-16 index is one ahead");
  assert.equal(utf16ToCp(text, 10), 9);
  assert.equal(cpSubstring(text, 24, 43), "Let's ask 象-2 next?");
  assert.deepEqual(toUtf16Span(text, 24, 43), { start: 25, end: 44 });
});

test("clamping at the ends", () => {
  assert.equal(cpToUtf16(text, 1000), text.length);
  assert.equal(cpToUtf16(text, -1), 0);
  assert.equal(utf16ToCp(text, 1000), codepointLength(text));
});

test("exactSpan compares text, not just numbers", () => {
  assert.ok(exactSpan(text, 0, 7, "I agree"));
  assert.ok(!exactSpan(text, 0, 7, "I agreE"));
  assert.ok(!exactSpan(text, 7, 7, ""));
  assert.ok(!exactSpan(text, 0, 999, "x"));
  assert.ok(!exactSpan(text, 0.5, 7, "I agree"));
});

test("wordCount is Python's str.split()", () => {
  assert.equal(wordCount("  one two\tthree\n"), 3);
  assert.equal(wordCount(""), 0);
});
