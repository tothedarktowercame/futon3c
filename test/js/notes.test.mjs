import assert from "node:assert/strict";
import test from "node:test";

import { applyOp, emptyDoc, nextCounter, spanExtent } from "../../resources/arxana/together/web/together.js";
import { allNoteExtents, decodeNotes, notesAtIndex, underlineRuns } from "../../resources/arxana/together/web/notes.js";

const insert = (id, after, text, attrs = {}) => ({ op: "insert", id, after, text, attrs });

test("decodeNotes accepts schema v1 and ignores malformed and other-document nodes", () => {
  const nodes = {
    good: { kind: "span", attrs: { doc: "d", start: ["a", 1], end: ["a", 2],
      by: "joe", colour: "magenta", title: "T", text: "N", quote: "Q", extra: 3 } },
    highlight: { kind: "span", attrs: { doc: "d", start: ["a", 2], end: ["a", 2], colour: "grey" } },
    other: { kind: "span", attrs: { doc: "else", start: ["a", 1], end: ["a", 2], colour: "red" } },
    malformed: { kind: "span", attrs: { doc: "d", start: ["a"], end: ["a", 2], colour: "red" } },
    badColour: { kind: "span", attrs: { doc: "d", start: ["a", 1], end: ["a", 2], colour: "cyan" } },
    edge: { kind: "edge", attrs: { doc: "d", start: ["a", 1], end: ["a", 2], colour: "red" } },
  };
  assert.deepEqual(decodeNotes(nodes, "d").map(({ id }) => id), ["good", "highlight"]);
  assert.equal(decodeNotes(nodes, "d")[0].extra, undefined);
});

test("allNoteExtents matches spanExtent on 200 random documents", () => {
  let seed = 0x220022;
  const random = (n) => { seed = (Math.imul(seed, 1664525) + 1013904223) >>> 0; return seed % n; };
  for (let trial = 0; trial < 200; trial += 1) {
    let doc = applyOp(emptyDoc(), insert(["seed", 1], null, "A😀BC𝕏Z", { author: "seed" }));
    for (let step = 0; step < 12; step += 1) {
      const anchor = doc.characters[random(doc.characters.length)].id;
      if (random(4) === 0) doc = applyOp(doc, { op: "delete", ids: [anchor] });
      else doc = applyOp(doc, insert([`s${random(3)}`, nextCounter(doc)], anchor,
        ["x", "😀", "𝕏"][random(3)], { author: "fixture" }));
    }
    const notes = Array.from({ length: 8 }, (_, index) => {
      const a = random(doc.characters.length); const b = random(doc.characters.length);
      const lo = Math.min(a, b); const hi = Math.max(a, b);
      return { id: `n${index}`, colour: "blue", start: doc.characters[lo].id, end: doc.characters[hi].id };
    });
    const before = structuredClone(doc);
    const actual = allNoteExtents(doc, notes);
    assert.equal(actual.unresolved.length, 0);
    for (const note of actual.resolved) {
      assert.deepEqual(note.extent, spanExtent(doc, note.start, note.end));
    }
    assert.deepEqual(doc, before);
  }
});

test("innermost notes win and underline runs split without changing text", () => {
  const resolved = [
    { id: "outer", colour: "magenta", extent: { from: 1, to: 6 } },
    { id: "inner", colour: "yellow", extent: { from: 2, to: 4 } },
  ];
  assert.deepEqual(notesAtIndex(resolved, 2).map(({ id }) => id), ["inner", "outer"]);
  const runs = underlineRuns([
    { author: "a", text: "😀abc", face: null, mark: null },
    { author: "b", text: "def", face: null, mark: null },
  ], resolved);
  assert.equal(runs.map(({ text }) => text).join(""), "😀abcdef");
  assert.deepEqual(runs.map(({ text, underline }) => [text, underline]), [
    ["😀", null], ["a", "#c026d3"], ["bc", "#ca8a04"],
    ["de", "#c026d3"], ["f", null],
  ]);
});

test("non-BMP characters before a note count as one visible index", () => {
  const doc = applyOp(emptyDoc(), insert(["a", 1], null, "😀abc", { author: "a" }));
  const note = { id: "n", colour: "green", start: ["a", 2], end: ["a", 3] };
  assert.deepEqual(allNoteExtents(doc, [note]).resolved[0].extent,
    { from: 1, to: 3, text: "ab", live: 2, deleted: 0 });
});

test("a superseded note is not rendered; the note that replaced it is", async () => {
  const { currentNodes } = await import("../../resources/arxana/together/web/notes.js");
  const span = (start, end) => ({ kind: "span", attrs: { doc: "d", start: ["a", start], end: ["a", end], colour: "magenta", title: "t" } });
  const state = { nodes: { old: span(1, 9), new: span(3, 4), other: span(5, 6) }, derived: { new: "old" } };
  assert.deepEqual(decodeNotes(currentNodes(state), "d").map((note) => note.id), ["new", "other"]);
  assert.deepEqual(Object.keys(state.nodes), ["old", "new", "other"]);
  assert.deepEqual(Object.keys(currentNodes({ nodes: { a: span(1, 2) } })), ["a"]);
  assert.deepEqual(currentNodes(undefined), {});
});

import { findWords } from "../../resources/arxana/together/web/notes.js";

test("findWords matches exact words across whitespace with codepoint offsets", () => {
  assert.deepEqual(findWords("before one two after", "one two"), { from: 7, to: 14 });
  assert.deepEqual(findWords("one\n   two", "one two"), { from: 0, to: 10 });
  assert.deepEqual(findWords("one two / one two", "one two"), { from: 0, to: 7 });
  assert.equal(findWords("One two", "one two"), null);
  assert.deepEqual(findWords("😀 one two", "one two"), { from: 2, to: 9 });
  assert.deepEqual(findWords("scale-at-all-costs (model)?", "scale-at-all-costs (model)?"), { from: 0, to: 27 });
  assert.equal(findWords("anything", "  "), null);
});

test("decodeNotes retains valid sources and drops malformed sources only", () => {
  const note = (source) => ({ kind: "span", attrs: { doc: "d", start: ["a", 1], end: ["a", 2], colour: "blue", source } });
  for (const source of [{ doc: "source-1", words: "some words" },
    { url: "https://example.test/source", words: "quoted" }, { url: "http://example.test/" }]) {
    assert.deepEqual(decodeNotes({ n: note(source) }, "d")[0].source, source);
  }
  for (const source of [null, [], "bad", { doc: "bad/key", words: "x" }, { doc: "d", words: " " },
    { doc: "d" }, { url: "javascript:alert(1)" }, { url: "file:///tmp/a" }, { url: "https://" },
    { url: "https://example.test", words: 3 }, { doc: "d", words: "x", url: "https://example.test" }]) {
    const notes = decodeNotes({ n: note(source) }, "d");
    assert.equal(notes.length, 1); assert.equal(notes[0].source, undefined);
  }
});
