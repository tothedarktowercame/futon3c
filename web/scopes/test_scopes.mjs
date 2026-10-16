import assert from "node:assert/strict";
import { readFile, readdir } from "node:fs/promises";
import { auditScopeData, mixInk, splitPieces } from "./scopes.js";
import { auditGridData, tallyGrid } from "./grid.js";

const fixture = JSON.parse(await readFile(
  new URL("./fixtures/claude-16-turn-162.json", import.meta.url), "utf8"));
const audit = auditScopeData(fixture);

// Every fixture the page can serve resolves its edges and legend. A per-turn
// fixture has segments; an over-time grid (fixtures/over-time-*.json) has cells.
const fixtureNames = (await readdir(new URL("./fixtures/", import.meta.url))).filter((n) => n.endsWith(".json"));
let grids = 0;
let analyses = 0;
for (const name of fixtureNames) {
  const data = JSON.parse(await readFile(new URL(`./fixtures/${name}`, import.meta.url), "utf8"));
  if (data.cells) {
    grids += 1;
    const g = auditGridData(data);
    assert.deepEqual(g.orphanCells, [], `${name}: every cell names a row and a column`);
    assert.deepEqual(g.undefinedValues, [], `${name}: legend defines every level and status`);
    assert.deepEqual(g.emptyColumns, [], `${name}: every column has a cell`);
    const t = tallyGrid(data);
    for (const row of data.rows) {
      const tally = t.perRow.get(row.turn);
      assert.equal(tally.echoed + tally.reached + tally["retrieved-only"], row.memories,
        `${name} ${row.turn}: the row's memory count is the sum of its cells`);
      if (row.calls.length === 0) assert.equal(row.memories, 0, `${name} ${row.turn}: no call, no memory`);
    }
    continue;
  }
  const a = auditScopeData(data);
  assert.deepEqual(a.invalidSpans, [], `${name}: spans lie inside their segments`);
  assert.deepEqual(a.stubs, [], `${name}: edge destinations resolve`);
  const undefinedKinds = a.legend
    .filter((entry) => !entry.defined && entry.categories.some((c) => c !== "label"))
    .map((entry) => entry.value);
  assert.deepEqual(undefinedKinds, [], `${name}: legend defines every span kind and edge relation`);
  if (name.startsWith("analysis-")) {
    analyses += 1;
    assert.equal(data.segments.length, 1, `${name}: analysis fixture owns one operator segment`);
    assert.equal(data.layers.length, 1, `${name}: analysis fixture has one interpretation layer`);
    assert.equal(data.layers[0].id, "analysis");
  }
}
assert(grids >= 2, "both over-time grids are audited");
assert(analyses >= 1, "at least one turn-analysis fixture is audited");
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
