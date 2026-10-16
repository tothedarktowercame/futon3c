import assert from "node:assert/strict";
import { readFile, readdir } from "node:fs/promises";
import { auditScopeData, mixInk, splitPieces } from "./scopes.js";

const fixture = JSON.parse(await readFile(
  new URL("./fixtures/claude-16-turn-162.json", import.meta.url), "utf8"));
const audit = auditScopeData(fixture);

// Every fixture the page can serve resolves its edges and legend.
for (const name of (await readdir(new URL("./fixtures/", import.meta.url))).filter((n) => n.endsWith(".json"))) {
  const data = JSON.parse(await readFile(new URL(`./fixtures/${name}`, import.meta.url), "utf8"));
  const a = auditScopeData(data);
  assert.deepEqual(a.invalidSpans, [], `${name}: spans lie inside their segments`);
  assert.deepEqual(a.stubs, [], `${name}: edge destinations resolve`);
  const undefinedKinds = a.legend
    .filter((entry) => !entry.defined && entry.categories.some((c) => c !== "label"))
    .map((entry) => entry.value);
  assert.deepEqual(undefinedKinds, [], `${name}: legend defines every span kind and edge relation`);
}
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
