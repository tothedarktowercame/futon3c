import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import { auditScopeData, mixInk, splitPieces } from "./scopes.js";

const fixture = JSON.parse(await readFile(
  new URL("./fixtures/claude-16-turn-162.json", import.meta.url), "utf8"));
const audit = auditScopeData(fixture);
assert.deepEqual(audit.invalidSpans, [], "all span ranges are valid code-point ranges");
assert.deepEqual(audit.stubs, [], "fixture edge destinations resolve");
assert.equal(audit.legend.filter((entry) => !entry.defined).length, 15,
  "numeric similarity labels are explicitly reported undefined");
assert(audit.legend.filter((entry) => !entry.defined)
  .every((entry) => entry.text === "undefined"));

const pieces = splitPieces("A😀BC", [
  { id: "emoji", seg: "s", start: 1, end: 2, layer: "c", kind: "k", weight: 1 },
]);
assert.deepEqual(pieces.map(({ text, spanIds }) => [text, spanIds]),
  [["A", []], ["😀", ["emoji"]], ["BC", []]]);
assert.equal(mixInk([
  { id: "c", seg: "s", start: 0, end: 1, layer: "c", kind: "k", weight: 1 },
  { id: "m", seg: "s", start: 0, end: 1, layer: "m", kind: "k", weight: 1 },
], [{ id: "c", name: "c", ink: "c" }, { id: "m", name: "m", ink: "m" }]),
"rgb(0, 0, 255)");

console.log(`ok: ${fixture.spans.length} spans, ${fixture.edges.length} edges, ` +
  `${audit.legend.length} legend values (${audit.legend.filter((entry) => !entry.defined).length} undefined)`);
