import { test } from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";
import { checkRecord, isTurnRecord } from "../src/conformance.js";
import { formattedTurn, noticesMessage } from "../src/matrix.js";

// dist/test/ when compiled, test/ when run from source: find the seam's
// example by walking up to the packages/ directory.
const here = dirname(fileURLToPath(import.meta.url));
const packagesDir = here.includes(`${"/"}dist${"/"}`) ? join(here, "..", "..", "..") : join(here, "..", "..");
const example = JSON.parse(readFileSync(join(packagesDir, "turn-seam", "example", "turn-example.json"), "utf8"));

test("the seam's example record conforms", () => {
  assert.deepEqual(checkRecord(example), []);
  assert.ok(isTurnRecord(example));
});

test("a record with an off-by-one span is refused with the reason", () => {
  const bad = structuredClone(example);
  bad.sentences[0].end = 22;
  assert.match(checkRecord(bad)[0]!, /s1: source_text\[0:22\] is "This is a total muddle"/);
  const missing = { version: 1 };
  assert.ok(checkRecord(missing).some((m) => m.includes("missing required field source_text")));
});

test("a record in codepoints is checked in codepoints", () => {
  const rec = {
    version: 1,
    source_text: "I agree 🗣 with 象 here.",
    offset_unit: "unicode-codepoints-zero-based-end-exclusive",
    sentences: [{ id: "s1", start: 0, end: 22, text: "I agree 🗣 with 象 here.", status: "cue-only", cues: [{ start: 0, end: 7, label: "approve", text: "I agree", method: "literal-phrase" }] }],
    unmatched: [],
    created_at: "x",
    agent_id: "a",
    session_id: "s",
    turn_id: "t",
  };
  assert.deepEqual(checkRecord(rec), []);
  assert.match(checkRecord({ ...rec, sentences: [{ ...rec.sentences[0], end: 23 }] })[0]!, /outside source_text \(len 22\)/);
});

test("a Matrix message carries the marks in formatted_body and a plain fallback", () => {
  const msg = formattedTurn(example, null);
  assert.equal(msg.msgtype, "m.notice");
  assert.equal(msg.body, example.source_text, "no cues: the plain body is the turn");
  assert.ok(msg.formatted_body.includes("This is a total muddle.<br>") === false);
  assert.ok(msg.formatted_body.endsWith("<br>QUOTE"));
  const withCue = formattedTurn(
    { ...example, sentences: [{ ...example.sentences[0], cues: [{ start: 10, end: 22, label: "report-problem", text: "total muddle", method: "literal-phrase" }] }] },
    null,
  );
  assert.ok(withCue.body.startsWith("This is a ⟦total muddle⟧[report-problem]."));
  assert.ok(withCue.formatted_body.startsWith('This is a <u><span data-mx-spoiler="report-problem">total muddle</span></u>.'));
  assert.equal(noticesMessage([]), null);
  assert.equal(noticesMessage([{ kind: "unresolved", text: "withdraw inferred: unresolved (no target)" }])!.body, "象: withdraw inferred: unresolved (no target)");
});
