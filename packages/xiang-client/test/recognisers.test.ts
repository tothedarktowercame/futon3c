import { test } from "node:test";
import assert from "node:assert/strict";
import { parseAcceptance, splitSurfaceMarker, undoCommand } from "../src/recognisers.js";

test("the classical acceptance grammar (agreement-record/parse-acceptance)", () => {
  assert.deepEqual(parseAcceptance("yes"), { offerId: null, optionId: null });
  assert.deepEqual(parseAcceptance("Yes."), { offerId: null, optionId: null });
  assert.deepEqual(parseAcceptance("YES!"), { offerId: null, optionId: null });
  assert.deepEqual(parseAcceptance("  yes 2 "), { offerId: null, optionId: "2" });
  assert.deepEqual(parseAcceptance("yes act:104d037c"), { offerId: "act:104d037c", optionId: null });
  assert.deepEqual(parseAcceptance("yes act:104d037c 3"), { offerId: "act:104d037c", optionId: "3" });
  assert.deepEqual(parseAcceptance("🈸: yes"), { offerId: null, optionId: null }, "answers a 🈸 paragraph");
  assert.deepEqual(parseAcceptance("🈸:yes act:1 2."), { offerId: "act:1", optionId: "2" });
});

test("what is not an acceptance", () => {
  assert.equal(parseAcceptance("yes please"), null);
  assert.equal(parseAcceptance("yes, do that"), null);
  assert.equal(parseAcceptance("yes.."), null, "only one trailing mark is dropped");
  assert.equal(parseAcceptance("yes ACT:1"), null, "ids keep their spelling");
  assert.equal(parseAcceptance("yes?"), null);
  assert.equal(parseAcceptance(""), null);
});

test("undo is exact, case-folded, punctuation-tolerant", () => {
  assert.equal(undoCommand("undo"), "undo");
  assert.equal(undoCommand("UNDO."), "undo");
  assert.equal(undoCommand("undo act:abc"), "act:abc");
  assert.equal(undoCommand("undo act:ABC!"), "act:abc", "the whole line is case-folded, as in Emacs");
  assert.equal(undoCommand("undo that"), null);
  assert.equal(undoCommand("please undo"), null);
});

test("the surface marker counts only when leading", () => {
  assert.deepEqual(splitSurfaceMarker("🗣 hello"), { surface: "dictated", text: "hello" });
  assert.deepEqual(splitSurfaceMarker("🗣hello"), { surface: "dictated", text: "hello" });
  assert.deepEqual(splitSurfaceMarker("say 🗣 hello"), { surface: null, text: "say 🗣 hello" });
});
