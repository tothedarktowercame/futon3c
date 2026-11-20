import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import test from "node:test";

import {
  applyOp,
  emptyDoc,
  fromRuns,
  idAtVisibleIndex,
  ids,
  indexToUtf16Offset,
  leftIdForInsert,
  nextCounter,
  runs,
  text,
  utf16OffsetToIndex,
  visibleIndexOf,
} from "../../resources/arxana/together/web/together.js";

const fixtureUrl = new URL("../../resources/arxana/together/conformance.json", import.meta.url);
const fixture = JSON.parse(await readFile(fixtureUrl, "utf8"));

function runOps(ops, order = ops.map((_, index) => index)) {
  return order.reduce((doc, index) => applyOp(doc, ops[index]), emptyDoc());
}

for (const fixtureCase of fixture) {
  test(`conformance: ${fixtureCase.name}, listed orders`, () => {
    const orders = [fixtureCase.ops.map((_, index) => index), ...(fixtureCase.orders ?? [])];
    for (const order of orders) {
      const doc = runOps(fixtureCase.ops, order);
      assert.equal(text(doc), fixtureCase.text, `text for order ${order}`);
      assert.deepEqual(ids(doc), fixtureCase.ids, `ids for order ${order}`);
    }
  });
}

test("runs and fromRuns round-trip Unicode and split maximal runs", () => {
  const unicodeRuns = [{
    id: ["joe", 7], text: "a😀𝕏", attrs: { tone: "warm", nested: [1, { x: true }] }, deleted: false,
  }];
  assert.deepEqual(runs(fromRuns(unicodeRuns)), unicodeRuns);

  let deleted = fromRuns([{ id: ["joe", 1], text: "abc", attrs: {}, deleted: false }]);
  deleted = applyOp(deleted, { type: "delete", ids: [["joe", 2]] });
  const deletedRuns = runs(deleted);
  assert.deepEqual(deletedRuns.map((run) => [run.text, run.deleted]), [
    ["a", false], ["b", true], ["c", false],
  ]);
  assert.deepEqual(fromRuns(deletedRuns), deleted);

  let split = applyOp(emptyDoc(), {
    type: "insert", id: ["joe", 1], after: null, text: "abc", attrs: { source: "joe" },
  });
  split = applyOp(split, {
    type: "insert", id: ["amy", 4], after: ["joe", 1], text: "X", attrs: { source: "amy" },
  });
  assert.deepEqual(runs(split).map((run) => [run.id, run.text]), [
    [["joe", 1], "a"], [["amy", 4], "X"], [["joe", 2], "bc"],
  ]);
  assert.deepEqual(fromRuns(runs(split)), split);
});

test("visible index helpers account for tombstones and code points", () => {
  let doc = applyOp(emptyDoc(), {
    type: "insert", id: ["joe", 1], after: null, text: "a😀b𝕏c", attrs: {},
  });
  doc = applyOp(doc, { type: "delete", ids: [["joe", 1], ["joe", 3], ["joe", 5]] });

  assert.equal(text(doc), "😀𝕏");
  assert.deepEqual(
    [1, 2, 3, 4, 5].map((counter) => visibleIndexOf(doc, ["joe", counter])),
    [0, 0, 1, 1, 2],
  );
  assert.equal(visibleIndexOf(doc, ["joe", 99]), -1);
  assert.deepEqual(idAtVisibleIndex(doc, 0), ["joe", 2]);
  assert.deepEqual(idAtVisibleIndex(doc, 1), ["joe", 4]);
  assert.equal(idAtVisibleIndex(doc, 2), null);
  assert.equal(leftIdForInsert(doc, 0), null);
  assert.deepEqual(leftIdForInsert(doc, 1), ["joe", 2]);
  assert.deepEqual(leftIdForInsert(doc, 2), ["joe", 4]);
  assert.equal(nextCounter(doc), 6, "tombstones remain part of the counter space");
});

test("UTF-16 and code-point offsets convert at surrogate boundaries", () => {
  const value = "A😀B𝕏C";
  assert.deepEqual(
    [0, 1, 2, 3, 4, 5, 6, 7].map((offset) => utf16OffsetToIndex(value, offset)),
    [0, 1, 1, 2, 3, 3, 4, 5],
  );
  assert.deepEqual(
    [0, 1, 2, 3, 4, 5].map((index) => indexToUtf16Offset(value, index)),
    [0, 1, 3, 4, 6, 7],
  );
});

test("applyOp reports every protocol error code", () => {
  const expectCode = (doc, op, code) => assert.throws(
    () => applyOp(doc, op),
    (error) => error instanceof Error && error.code === code,
  );
  const one = applyOp(emptyDoc(), {
    type: "insert", id: ["joe", 1], after: null, text: "a", attrs: {},
  });

  expectCode(emptyDoc(), {
    type: "insert", id: ["joe", 2], after: ["joe", 1], text: "x", attrs: {},
  }, "unknown-after");
  expectCode(one, { type: "delete", ids: [["joe", 2]] }, "unknown-id");
  expectCode(one, {
    type: "insert", id: ["joe", 1], after: null, text: "x", attrs: {},
  }, "id-in-use");
  expectCode(emptyDoc(), {
    type: "insert", id: ["joe", 1], after: null, text: "", attrs: {},
  }, "empty-text");
  expectCode(one, {
    type: "insert", id: ["amy", 1], after: ["joe", 1], text: "x", attrs: {},
  }, "counter-not-after");
});

function wrongCompare(a, b) {
  if (a[0] !== b[0]) return a[0] < b[0] ? -1 : 1;
  return a[1] - b[1];
}

function runWithWrongOrdering(ops, order) {
  const characters = [];
  for (const opIndex of order) {
    const op = ops[opIndex];
    if ((op.op ?? op.type) === "delete") {
      for (const id of op.ids) {
        const character = characters.find((item) => item.id[0] === id[0] && item.id[1] === id[1]);
        character.deleted = true;
      }
      continue;
    }
    let left = op.after === null
      ? -1
      : characters.findIndex((item) => item.id[0] === op.after[0] && item.id[1] === op.after[1]);
    [...op.text].forEach((point, index) => {
      const id = [op.id[0], op.id[1] + index];
      let at = left + 1;
      while (at < characters.length && wrongCompare(characters[at].id, id) > 0) at += 1;
      characters.splice(at, 0, { id, point, deleted: false });
      left = at;
    });
  }
  return {
    text: characters.filter((item) => !item.deleted).map((item) => item.point).join(""),
    ids: characters.filter((item) => !item.deleted).map((item) => item.id),
  };
}

test("negative control catches editor-first id ordering", () => {
  let mismatches = 0;
  for (const fixtureCase of fixture) {
    const orders = [fixtureCase.ops.map((_, index) => index), ...(fixtureCase.orders ?? [])];
    for (const order of orders) {
      const actual = runWithWrongOrdering(fixtureCase.ops, order);
      if (actual.text !== fixtureCase.text
          || JSON.stringify(actual.ids) !== JSON.stringify(fixtureCase.ids)) {
        mismatches += 1;
      }
    }
  }
  assert.ok(mismatches > 0, "the fixtures must reject editor-then-counter ordering");
});
