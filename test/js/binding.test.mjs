import assert from "node:assert/strict";
import test from "node:test";

import {
  authorColour,
  createClient,
  FACE_STYLES,
  MARK_COLOURS,
  decorations,
  faceOf,
  load,
  localChange,
  receive,
} from "../../resources/arxana/together/web/binding.js";
import {
  applyOp,
  emptyDoc,
  runs,
  text,
} from "../../resources/arxana/together/web/together.js";

class Hub {
  constructor() {
    this.doc = emptyDoc();
    this.entries = [];
  }

  submit(prepared, author) {
    this.doc = applyOp(this.doc, prepared.op);
    const entry = {
      rev: this.entries.length + 1,
      op: prepared.op,
      author,
      parents: prepared.parents,
      at: `fixture-${this.entries.length + 1}`,
    };
    this.entries.push(entry);
    return entry;
  }

  snapshot() {
    return { rev: this.entries.length, runs: runs(this.doc) };
  }
}

function change(client, oldValue, newValue, caret = newValue.length) {
  return localChange(client, oldValue, newValue, caret);
}

function deliver(client, entry, selection = { start: 0, end: 0 }) {
  return receive(client, entry, selection).state;
}

test("two concurrent clients converge without interleaving their runs", () => {
  const hub = new Hub();
  let amy = createClient({ site: "amy.web", author: "amy" });
  let bob = createClient({ site: "bob.web", author: "bob" });

  const amyChange = change(amy, "", "AMY");
  amy = amyChange.state;
  const amyEntry = hub.submit(amyChange.ops[0], "amy");
  const bobChange = change(bob, "", "BOB");
  bob = bobChange.state;
  const bobEntry = hub.submit(bobChange.ops[0], "bob");

  amy = deliver(amy, amyEntry);
  amy = deliver(amy, bobEntry);
  bob = deliver(bob, amyEntry);
  bob = deliver(bob, bobEntry);

  assert.equal(text(amy.doc), text(hub.doc));
  assert.equal(text(bob.doc), text(hub.doc));
  assert.deepEqual(decorations(amy).map((run) => run.author).sort(), ["amy", "bob"]);
  assert.equal(decorations(amy).length, 2, "each concurrent insertion remains one run");
});

test("an own insert echo advances only the revision", () => {
  const hub = new Hub();
  let client = createClient({ site: "joe.web", author: "joe" });
  const local = change(client, "", "hello");
  client = local.state;
  const before = client.doc;
  const result = receive(client, hub.submit(local.ops[0], "joe"), { start: 5, end: 5 });
  assert.equal(result.state.doc, before);
  assert.equal(result.state.rev, 1);
  assert.equal(result.value, "hello");
  assert.deepEqual(result.selection, { start: 5, end: 5 });
});

function repeatedState(value) {
  return load(createClient({ site: "joe.web", author: "joe" }), {
    rev: 4,
    runs: [{ id: ["seed", 1], text: value, attrs: { author: "seed" }, deleted: false }],
  });
}

test("ambiguous insertion splits follow the post-input caret", () => {
  const expectedAfter = [null, ["seed", 1], ["seed", 2]];
  [1, 2, 3].forEach((caret, index) => {
    const result = localChange(repeatedState("aa"), "aa", "aaa", caret);
    assert.equal(result.ops.length, 1);
    assert.deepEqual(result.ops[0].op.after, expectedAfter[index]);
    assert.equal(text(result.state.doc), "aaa");
  });
});

test("ambiguous deletion and replacement select the intended span", () => {
  const deletion = localChange(repeatedState("aaa"), "aaa", "aa", 1);
  assert.deepEqual(deletion.ops[0].op.ids, [["seed", 2]]);
  assert.equal(text(deletion.state.doc), "aa");

  const replacement = localChange(repeatedState("abcd"), "abcd", "aXd", 2);
  assert.deepEqual(replacement.ops.map(({ op }) => op.op), ["delete", "insert"]);
  assert.deepEqual(replacement.ops[0].op.ids, [["seed", 2], ["seed", 3]]);
  assert.deepEqual(replacement.ops[1].op.after, ["seed", 1]);
  assert.equal(replacement.ops[1].op.text, "X");
  assert.equal(text(replacement.state.doc), "aXd");
});

test("local non-BMP insertion and deletion use code-point ids", () => {
  let client = createClient({ site: "joe.web", author: "joe" });
  const inserted = localChange(client, "", "😀", 2);
  client = inserted.state;
  assert.equal(inserted.ops[0].op.text, "😀");
  assert.deepEqual(inserted.ops[0].op.id, ["joe.web", 1]);

  const deleted = localChange(client, "😀", "", 0);
  assert.deepEqual(deleted.ops[0].op.ids, [["joe.web", 1]]);
  assert.equal(text(deleted.state.doc), "");
});

test("remote emoji before a caret moves UTF-16 offsets by two", () => {
  const state = repeatedState("ab");
  const entry = {
    rev: 5,
    op: { op: "insert", id: ["amy.web", 5], after: null, text: "😀", attrs: { author: "amy" } },
  };
  const result = receive(state, entry, { start: 1, end: 1 });
  assert.equal(result.value, "😀ab");
  assert.deepEqual(result.selection, { start: 3, end: 3 });
});

test("selection endpoints independently track insertions and deletions", () => {
  const state = repeatedState("abcd");
  const atCaret = receive(state, {
    rev: 5,
    op: { op: "insert", id: ["amy.web", 5], after: ["seed", 1], text: "X", attrs: { author: "amy" } },
  }, { start: 1, end: 1 });
  assert.deepEqual(atCaret.selection, { start: 2, end: 2 });

  const insertion = receive(state, {
    rev: 5,
    op: { op: "insert", id: ["amy.web", 5], after: ["seed", 2], text: "XY", attrs: { author: "amy" } },
  }, { start: 1, end: 4 });
  assert.equal(insertion.value, "abXYcd");
  assert.deepEqual(insertion.selection, { start: 1, end: 6 });

  const deletion = receive(state, {
    rev: 5,
    op: { op: "delete", ids: [["seed", 2], ["seed", 3]] },
  }, { start: 2, end: 4 });
  assert.equal(deletion.value, "ad");
  assert.deepEqual(deletion.selection, { start: 1, end: 2 });

  const before = receive(state, {
    rev: 5,
    op: { op: "delete", ids: [["seed", 1], ["seed", 2]] },
  }, { start: 3, end: 3 });
  assert.deepEqual(before.selection, { start: 1, end: 1 });
});

test("a revision gap requests resync without changing state", () => {
  const state = repeatedState("abc");
  const selection = { start: 1, end: 2 };
  const result = receive(state, {
    rev: 7,
    op: { op: "delete", ids: [["seed", 1]] },
  }, selection);
  assert.equal(result.resync, true);
  assert.equal(result.state, state);
  assert.equal(result.value, "abc");
  assert.deepEqual(result.selection, selection);
});

test("localChange refuses a stale textarea value", () => {
  assert.throws(
    () => localChange(repeatedState("abc"), "wrong", "new", 3),
    (error) => error.code === "stale-value",
  );
});

test("a display mark splits runs and unknown marks are ignored", () => {
  const state = load(createClient({ site: "joe.web", author: "joe" }), {
    rev: 4,
    runs: [
      { id: ["fable-1.seed", 1], text: "ok", attrs: { author: "fable-1", mark: "ok" }, deleted: false },
      { id: ["fable-1.seed", 3], text: "!!", attrs: { author: "fable-1", mark: "debt" }, deleted: false },
      { id: ["fable-1.seed", 5], text: "x", attrs: { author: "fable-1", mark: "constructor" }, deleted: false },
      { id: ["fable-1.seed", 6], text: "y", attrs: { author: "fable-1" }, deleted: false },
    ],
  });
  assert.deepEqual(decorations(state), [
    { author: "fable-1", mark: "ok", text: "ok" },
    { author: "fable-1", mark: "debt", text: "!!" },
    { author: "fable-1", text: "xy" },
  ]);
  for (const colour of Object.values(MARK_COLOURS)) assert.match(colour, /^#[0-9a-f]{6}$/);
});

test("a face fixed at insert splits runs; Emacs faces from a shared answer map to it", () => {
  const sym = (value) => ({ tag: "symbol", value });
  const state = load(createClient({ site: "joe.web", author: "joe" }), {
    rev: 5,
    runs: [
      { id: ["fable-1.seed", 1], text: "Q", attrs: { author: "fable-1", face: "heading" }, deleted: false },
      { id: ["fable-1.seed", 2], text: "ab", attrs: { author: "fable-1", face: "typed" }, deleted: false },
      { id: ["joe.emacs", 4], text: "cd", attrs: { author: "joe", prov: { face: sym("futon4a-variants-diff-added") } }, deleted: false },
      { id: ["joe.emacs", 6], text: "m", attrs: { author: "joe", prov: { face: { tag: "list", value: [sym("unknown-face"), sym("futon4a-incubator-teammate")] } } }, deleted: false },
      { id: ["joe.emacs", 7], text: "z", attrs: { author: "joe", face: "__proto__", prov: { face: sym("toString") } }, deleted: false },
    ],
  });
  assert.deepEqual(decorations(state), [
    { author: "fable-1", face: "heading", text: "Q" },
    { author: "fable-1", face: "typed", text: "ab" },
    { author: "joe", face: "typed", text: "cd" },
    { author: "joe", face: "teammate", text: "m" },
    { author: "joe", text: "z" },
  ]);
  assert.equal(faceOf(undefined), undefined);
  assert.equal(faceOf({ face: 7 }), undefined);
  for (const style of Object.values(FACE_STYLES)) assert.match(style.color, /^#[0-9a-f]{6}$/);
});

test("decorations cover text and merge adjacent sites of one author", () => {
  const state = load(createClient({ site: "murch.web", author: "murch" }), {
    rev: 2,
    runs: [
      { id: ["murch.web", 1], text: "hi", attrs: { author: "murch" }, deleted: false },
      { id: ["murch.emacs", 3], text: "!", attrs: { author: "murch" }, deleted: false },
      { id: ["rob.web", 4], text: "R", attrs: {}, deleted: false },
    ],
  });
  assert.deepEqual(decorations(state), [
    { author: "murch", text: "hi!" },
    { author: "rob", text: "R" },
  ]);
  assert.equal(decorations(state).map((run) => run.text).join(""), text(state.doc));
  assert.equal(authorColour("murch"), authorColour("murch"));
  assert.match(authorColour("murch"), /^#[0-9a-f]{6}$/);
  assert.ok(new Set(Array.from({ length: 30 }, (_, n) => authorColour(`author-${n}`))).size >= 8);
});

function randomGenerator(seed) {
  let value = seed >>> 0;
  return () => {
    value = (Math.imul(value, 1664525) + 1013904223) >>> 0;
    return value / 0x100000000;
  };
}

test("fixed-seed randomized clients converge through delayed delivery", () => {
  const random = randomGenerator(0x51a7c0de);
  const alphabet = ["a", "b", "c", "😀", "𝕏"];

  for (let trial = 0; trial < 150; trial += 1) {
    const hub = new Hub();
    const clients = ["amy", "bob", "cy"].map((author) => ({
      state: createClient({ site: `${author}.web`, author }),
      selection: { start: 0, end: 0 },
    }));

    for (let step = 0; step < 40; step += 1) {
      const target = clients[Math.floor(random() * clients.length)];
      const pending = hub.entries[target.state.rev];
      if (pending && random() < 0.45) {
        const received = receive(target.state, pending, target.selection);
        assert.equal(received.resync, false);
        target.state = received.state;
        target.selection = received.selection;
        continue;
      }

      const oldValue = text(target.state.doc);
      const points = [...oldValue];
      const insert = points.length === 0 || random() < 0.62;
      let newValue;
      let caretIndex;
      if (insert) {
        const at = Math.floor(random() * (points.length + 1));
        const point = alphabet[Math.floor(random() * alphabet.length)];
        points.splice(at, 0, point);
        newValue = points.join("");
        caretIndex = at + 1;
      } else {
        const at = Math.floor(random() * points.length);
        points.splice(at, 1);
        newValue = points.join("");
        caretIndex = at;
      }
      const caret = [...newValue].slice(0, caretIndex).join("").length;
      const local = localChange(target.state, oldValue, newValue, caret);
      target.state = local.state;
      target.selection = { start: caret, end: caret };
      for (const prepared of local.ops) hub.submit(prepared, target.state.author);
    }

    for (const client of clients) {
      while (client.state.rev < hub.entries.length) {
        const result = receive(client.state, hub.entries[client.state.rev], client.selection);
        assert.equal(result.resync, false);
        client.state = result.state;
        client.selection = result.selection;
      }
      assert.equal(text(client.state.doc), text(hub.doc), `trial ${trial}`);
      assert.equal(
        decorations(client.state).map((run) => run.text).join(""),
        text(client.state.doc),
        `decorations trial ${trial}`,
      );
    }
  }
});

test("negative control catches a disabled own-echo check", () => {
  const hub = new Hub();
  let client = createClient({ site: "joe.web", author: "joe" });
  const local = localChange(client, "", "x", 1);
  client = local.state;
  const echo = hub.submit(local.ops[0], "joe");

  assert.throws(
    () => applyOp(client.doc, echo.op),
    (error) => error.code === "id-in-use",
    "blindly applying the own echo must break the two-client delivery path",
  );
});
