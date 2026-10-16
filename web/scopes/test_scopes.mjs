import assert from "node:assert/strict";
import { readFile, readdir } from "node:fs/promises";
import { auditScopeData, inkUnits, mixInk, smallestSpan, splitPieces } from "./scopes.js";
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
    // Only display cues are drawn. A fragment covers a whole sentence, and a
    // span for it would assert every word is operative; so the span count is
    // the cue count the adapter reported, nothing more.
    assert.equal(data.spans.length, data.spot_check.cues_drawn,
      `${name}: every span is a drawn display cue, no whole-fragment span`);
    assert(data.spans.every((span) => span.layer === "analysis"),
      `${name}: every span sits in the interpretation layer`);
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

// Hovering a word lights only the smallest span around it. A term inside a
// clause inside a section lights the term, not the section whose outline the
// page already shows. Ties go to the earliest start; no spans, no hover.
const nested = [
  { id: "section", seg: "s", start: 0, end: 3000, layer: "s4", kind: "k", weight: 1 },
  { id: "clause", seg: "s", start: 100, end: 140, layer: "s2", kind: "k", weight: 1 },
  { id: "term", seg: "s", start: 120, end: 125, layer: "s1", kind: "k", weight: 1 },
];
assert.equal(smallestSpan(nested)?.id, "term");
assert.equal(smallestSpan([nested[0], nested[1]])?.id, "clause");
assert.equal(smallestSpan([
  { id: "late", seg: "s", start: 10, end: 15, layer: "s1", kind: "k", weight: 1 },
  { id: "early", seg: "s", start: 5, end: 10, layer: "s1", kind: "k", weight: 1 },
])?.id, "early");
assert.equal(smallestSpan([]), undefined);
// On the layered mark7 page, the smallest span over a piece is a real
// narrowing: a term sits inside a clause inside a section, and the section is
// at least ten times the term. Without that, hover narrowing would change nothing.
const mark7 = JSON.parse(await readFile(
  new URL("./fixtures/mark7-0708.2185.json", import.meta.url), "utf8"));
const mark7SpanById = new Map(mark7.spans.map((span) => [span.id, span]));
const nestedPieces = mark7.segments.flatMap((segment) => splitPieces(segment.text,
  mark7.spans.filter((span) => span.seg === segment.id)))
  .map((piece) => piece.spanIds.map((id) => mark7SpanById.get(id)))
  .filter((spans) => spans.length > 1);
assert(nestedPieces.some((spans) => {
  const sizes = spans.map((span) => span.end - span.start);
  return Math.max(...sizes) >= 10 * Math.min(...sizes);
}), "mark7 has a piece whose spans differ tenfold in extent, so hover narrowing changes what lights");

const unitLayers = [
  { id: "cyan", name: "cyan", ink: "c" },
  { id: "magenta", name: "magenta", ink: "m" },
];
const unitSpans = [
  { id: "cyan-span", seg: "s", start: 1, end: 4, layer: "cyan", kind: "k", weight: 1 },
  { id: "magenta-span", seg: "s", start: 2, end: 5, layer: "magenta", kind: "k", weight: 1 },
];
const mixedUnit = inkUnits([{ a: 2, b: 3 }], unitSpans, unitLayers,
  new Set(["cyan", "magenta"]))[0];
assert.equal(mixedUnit.color, mixInk(unitSpans, unitLayers),
  "a unit under cyan and magenta uses the shared mixer");
assert.deepEqual(mixedUnit.spanIds, ["cyan-span", "magenta-span"]);
assert.deepEqual(inkUnits([{ a: 8, b: 9 }], unitSpans, unitLayers,
  new Set(["cyan", "magenta"])), [{ color: "", spanIds: [] }],
"a unit outside every span keeps the host background");
assert.deepEqual(inkUnits([{ a: 3, b: 5 }], [unitSpans[0]], unitLayers,
  new Set(["cyan"]))[0].spanIds, ["cyan-span"],
"a unit straddling a span end is covered");
const half = inkUnits([{ a: 1, b: 2 }],
  [{ ...unitSpans[0], weight: 0.5 }], unitLayers, new Set(["cyan"]))[0];
const redChannel = Number(half.color.match(/^rgb\((\d+),/)?.[1]);
assert.equal(redChannel, Math.round(255 * (1 - 0.5)),
  "half-strength cyan subtracts half the red channel");
const inactive = inkUnits([{ a: 2, b: 3 }], unitSpans, unitLayers,
  new Set(["cyan"]))[0];
assert.deepEqual(inactive.spanIds, ["cyan-span"],
  "an inactive layer contributes no span id");
assert.equal(inactive.color, mixInk([unitSpans[0]], unitLayers),
  "an inactive layer contributes no ink");
// The boundary of [a, b) against [start, end): a unit beginning exactly at a
// span's end, or ending exactly at its start, touches it without overlapping.
assert.deepEqual(inkUnits([{ a: 5, b: 6 }], unitSpans, unitLayers,
  new Set(["cyan", "magenta"])), [{ color: "", spanIds: [] }],
"a unit starting one code point past a span end is uncovered");
assert.deepEqual(inkUnits([{ a: 0, b: 1 }], unitSpans, unitLayers,
  new Set(["cyan", "magenta"])), [{ color: "", spanIds: [] }],
"a unit ending where a span starts is uncovered");

console.log(`ok: ${fixture.spans.length} spans, ${fixture.edges.length} edges, ` +
  `${audit.legend.length} legend values (${audit.legend.filter((entry) => !entry.defined).length} undefined)`);
