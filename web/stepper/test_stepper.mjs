import assert from "node:assert/strict";
import {readFile} from "node:fs/promises";
import {
  ALIGNMENT_UNREAD, PARSE_PENDING, PATTERNS_NONE_READ, PATTERNS_PENDING, REPLY_ABSENT, alignmentLines, answeredLines, cardLines, cascadeLines, zoomLines, cuePieces, observedLines,
  frameModel, happenedDetail, parseStreamEvent, portsAge, readyFrames, receiveFrames,
} from "./stepper.js";

const fixture = JSON.parse(await readFile(new URL("./fixtures/frames-claude-16-00e653e3.json", import.meta.url)));

const provisional = structuredClone(fixture.at(-1));
provisional.parse.status = "drafted-provisional";
assert.equal(readyFrames([...fixture, provisional]).length, fixture.length + 1);
const missing = structuredClone(fixture.at(-1));
missing.parse.status = "missing";
assert.equal(readyFrames([...fixture, missing]).length, fixture.length);
// live mode keeps the current (unread) turn and names the pending reading
assert.equal(readyFrames([...fixture, missing], true).length, fixture.length + 1);
assert.equal(frameModel(missing).parseNote, PARSE_PENDING);
// Cascade slice: agent lineage, engaged node, patterns by source with their missions
{
  const graph = {lineage: [{agent: "agent:claude-3", target: "futon3c-d/excursion/prove-memory-useful", at: 1}],
                 clusters: [], held: [], arrows: [],
                 patterns: {edges: [{mission: "futon3c-d/mission/futon-seams", pattern: "p/x", state: "linked"}]}};
  const lines = cascadeLines({card: {settled: {engaged: {"mission-id": "M-futon-seams"}}},
                              retrieved: [{id: "p/x", method: "embeddings"}], patterns: {matched: ["p/y"]}}, graph, "claude-3");
  assert.equal(lines[0], "agent claude-3: 1 clock edge(s)");
  assert.ok(lines.includes("engaged futon3c-d/mission/futon-seams"));
  assert.ok(lines.includes("  linked patterns: p/x"));
  assert.ok(lines.includes("  p/x [embedding]: futon-seams"));
  assert.ok(lines.includes("  p/y [象]: no mission links it"));
  assert.deepEqual(cascadeLines({}, null), []);
  const g2 = {lineage: [{agent: "agent:claude-3", target: "x-d/mission/m", at: 2}, {agent: "agent:zai-5", target: "x-d/mission/m", at: 5, session: "abcdefgh123"}],
              arrows: [{have: "x-d/mission/m", want: "x-d/mission/m2", "move-class": ":close-hole"}], held: [{held: "h1", mission: "x-d/mission/m", reason: "needs  a  thing"}],
              clusters: [], patterns: {edges: [{mission: "x-d/mission/m", pattern: "p/z", state: "linked"}]}};
  const l2 = cascadeLines({card: {settled: {engaged: {"mission-id": "M-m"}}}}, g2, "claude-3");
  assert.ok(l2.some(x => x.startsWith("  also here: zai-5") && x.includes("session abcdefgh")));
  assert.ok(l2.includes("  wants → x-d/mission/m2 :close-hole"));
  assert.ok(l2.includes("    - needs a thing"));
}
// FUTON's view: three zoom levels, capped, pointers name the pattern only
{
  const graph = {lineage: [], clusters: [], held: [], arrows: [],
                 patterns: {edges: [{mission: "x-d/mission/far", pattern: "p/a", state: "linked"}]}};
  const view = {agents: {"claude-3": {target: "x-d/excursion/here", at: 1},
                         "claude-16": {target: "x-d/excursion/here", at: 2, purpose: "measure delivery"},
                         "zai-9": {target: "x-d/mission/elsewhere", at: 3, purpose: "unrelated"}},
                mission_patterns: {"x-d/mission/near": [["p/b", 2, 5, ["embedding"]]], "x-d/excursion/here": [["p/a", 1, 9, ["象"]]]}};
  const lines = zoomLines({retrieved: [{id: "p/a"}, {id: "p/b"}], memory: {injected: ["cc:-home-joe/m1"]}}, graph, view, "claude-3");
  assert.ok(lines.includes("    injected: m1"));
  assert.ok(lines.includes("    claude-16: measure delivery"));
  assert.ok(!lines.some(l => l.includes("zai-9")));
  assert.ok(lines.includes("    far also uses p/a  (substrate-2)"));
  assert.ok(lines.includes("    near also uses p/b  (derived, 2 session(s) 5 turn(s))"));
  assert.ok(!lines.some(l => l.includes("here also uses")));
  assert.deepEqual(zoomLines({}, null, null), []);
  // a current job leads the line; the 馬 purpose follows only when futon_view kept it
  view.agents["claude-16"].job = {by: "claude-3", at: "2026-10-19T02:08:11.5Z", state: "done", line: "one small job"};
  const withJob = zoomLines({retrieved: [{id: "p/a"}]}, graph, view, "claude-3");
  assert.ok(withJob.includes("    claude-16: job from claude-3 02:08Z (done): one small job · 馬: measure delivery"));
  view.agents["claude-16"].purpose = null;
  assert.ok(zoomLines({}, graph, view, "claude-3").includes("    claude-16: job from claude-3 02:08Z (done): one small job"));
}
// 馬's card: settled first, held counts shown, absent when there is none
{
  const lines = cardLines({card: {settled: {engaged: {"excursion-id": "E-x"}, done: ["✅ a"], commits: {count: 2, last: ["abc x"]}},
                                  standing: {purpose: {text: "p", held: 3}, open_threads: [{text: "t", held: 1}]},
                                  fresh: {problems: ["🔴 q"]}}});
  assert.equal(lines[0], "SETTLED");
  assert.ok(lines.includes("  engaged: excursion-id E-x"));
  assert.ok(lines.includes("  purpose: p  (held 3)"));
  assert.ok(lines.includes("    - t"));
  assert.ok(lines.indexOf("SETTLED") < lines.indexOf("STANDING") && lines.indexOf("STANDING") < lines.indexOf("FRESH (this turn)"));
  assert.deepEqual(cardLines({}), []);
}
// 馬's observation is its own section, absent when there is none
assert.deepEqual(observedLines({observed: {at: "t", model: "m", observations: "* 🔴 a\n* ✅ b", current_task: "- x"}}),
  ["馬 at t · m", "* 🔴 a", "* ✅ b", "", "current task:", "- x"]);
assert.deepEqual(observedLines({}), []);
// a harness seat's unmarked reply is shown whole
assert.deepEqual(answeredLines({reply: {at: "t", paragraphs: [], text: "line one\nline two"}}).slice(1),
  ["the reply carries no marks, so nothing can be lined up", "line one", "line two"]);
assert.equal(frameModel(fixture.at(-1)).parseNote, "");

const unfound = [];
for (const [fi, frame] of fixture.entries()) {
  for (const [gi, fragment] of (frame.parse.fragments || []).entries()) {
    const pieces = cuePieces(fragment.text || "", fragment.cues || []);
    for (const cue of fragment.cues || []) {
      if (!pieces.some(piece => piece.cue && piece.text.includes(cue))) unfound.push(`${fi}:${gi}:${cue}`);
    }
  }
}
assert.deepEqual(unfound, []);

const commitRow = {summary: {event: "turn-commits", commits: [
  {repo: "a", sha: "123456789", author: "A", subject: "one"},
  {repo: "b", sha: "abcdefghi", author: "B", subject: "two"},
]}};
assert.match(happenedDetail(commitRow), /^2 commit\(s\) in window:/);
assert.equal(portsAge("2026-10-11T04:00:00Z", "2026-10-11T04:17:30Z"), "17m");
assert.equal(portsAge("2026-10-11T04:00:00Z", "2026-10-11T04:00:30Z"), "<1m");

const noPorts = structuredClone(fixture[0]);
delete noPorts.ports;
delete noPorts.memory;
assert.equal(frameModel(noPorts).ports, "");
assert.deepEqual(frameModel(noPorts).memory,
  ["no recorded memory call for this turn (nothing reached it that the record can show)"]);
for (const frame of fixture) assert.doesNotThrow(() => frameModel(frame));
const withMemory = structuredClone(fixture[0]);
withMemory.memory = {calls: [{tool: "m40_find", args: {terms: ["park"]}, reached: [
  {name: "bellback-park", project: "cc:-home-joe"},
  {name: "reader-run-path", project: null},
]}]};
assert.deepEqual(frameModel(withMemory).memory, [
  'm40_find  {"terms":["park"]}',
  "  reached: bellback-park (cc:-home-joe), reader-run-path",
]);

const plusOne = [...fixture, structuredClone(fixture.at(-1))];
assert.equal(receiveFrames({frames: fixture, current: fixture.length - 1}, plusOne).current, fixture.length);
assert.equal(receiveFrames({frames: fixture, current: 2}, plusOne).current, 2);
assert.equal(receiveFrames({frames: fixture.slice(0, 7), current: 5}, fixture.slice(0, 3)).current, 2);
const held = {frames: fixture, current: 4};
assert.strictEqual(receiveFrames(held, []), held);
const liveMissing = structuredClone(fixture.at(-1));
liveMissing.parse.status = "missing";
const liveProvisional = structuredClone(fixture.at(-1));
liveProvisional.parse.status = "drafted-provisional";
assert.equal(receiveFrames({frames: fixture, current: 0}, [...fixture, liveMissing]).frames.at(-1).parse.status, "missing");
assert.deepEqual(
  receiveFrames({frames: fixture, current: 0}, [...fixture, liveMissing, liveProvisional]).frames.slice(-2).map(f => f.parse.status),
  ["missing", "drafted-provisional"],
);

const parsedFrames = parseStreamEvent("frames", JSON.stringify({ok: true, frames: fixture, version: 29}));
assert.equal(parsedFrames.kind, "frames");
assert.equal(parsedFrames.frames.length, fixture.length);
assert.equal(parsedFrames.version, 29);
assert.deepEqual(
  parseStreamEvent("status", '{"state":"rebuild-failed","last-error":{"reason":"script-failed","at":"now"}}'),
  {kind: "status", status: {state: "rebuild-failed", detail: "script-failed"}},
);
assert.deepEqual(parseStreamEvent("hello", '{"session":"x"}'), {kind: "ignore"});
assert.equal(parseStreamEvent("frames", "{not json").kind, "status");

// ANSWERED: the reply's quoted brackets lined up with the sentences they quote
const withReply = structuredClone(fixture[0]);
withReply.parse.fragments = [
  {sentence: "s1", text: "please fix the spinner", combined: "ask-action"},
  {sentence: "s2", text: "and tell me what to expect", combined: "clarify"},
];
withReply.reply = {at: "2026-10-16T22:37:50Z", paragraphs: [
  {mark: "㊥", intent: "gist", target: "gist", quoted: false, answers: []},
  {mark: "㊬", intent: "verify", target: "fix the spinner", quoted: true, answers: ["s1"]},
], unanswered: ["s2"], unanchored: [0]};
assert.deepEqual(answeredLines(withReply), [
  "reply at 2026-10-16T22:37:50Z · 2 marked paragraph(s)",
  's1 ask-action  ← ㊬ verify ("fix the spinner")',
  "s2 clarify  ← (no paragraph quotes this sentence)",
  "not lined up with any sentence: ㊥ gist (gist)",
]);
const noReply = structuredClone(withReply);
delete noReply.reply;
assert.deepEqual(frameModel(noReply).answered, [REPLY_ABSENT]);
const unmarked = structuredClone(withReply);
unmarked.reply = {at: "t", paragraphs: []};
assert.equal(answeredLines(unmarked)[1], "the reply carries no marks, so nothing can be lined up");
// PATTERNS: an empty match after 象's reading is not the same line as a reading in flight
const noneRead = structuredClone(fixture[0]);
noneRead.parse.status = "analyzed"; noneRead.patterns = {matched: [], rejected: [{}, {}, {}]};
assert.equal(frameModel(noneRead).patterns[0], PATTERNS_NONE_READ);
assert.equal(frameModel(noneRead).patterns.at(-1), "rejected: 3");
const stillReading = structuredClone(noneRead);
stillReading.parse.status = "missing"; stillReading.patterns = {matched: [], rejected: []};
assert.equal(frameModel(stillReading).patterns[0], PATTERNS_PENDING);

// ALIGNMENT: the 馬 reading of the operator turn — one line per act plus a
// summary; absent reads "not read yet", never nothing.
{
  const withAlignment = structuredClone(fixture[0]);
  withAlignment.alignment = {
    version: 1, at: "2026-10-19T09:00:00Z", seat: "馬-sonnet", unread: 2,
    acts: [
      {act: "t9-f-s1-0", kind: "ask-action", text: "Please fix the stepper display for the alignment row",
       verdict: "answered", by: "t9-r-0", mark: "㊢", quote: "done; the alignment row renders one line per act for the turn", method: "quote"},
      {act: "t9-f-s2-0", kind: "ask-action", text: "and add a summary line at the top",
       verdict: "answered", by: "t9-r-1", mark: "㊢", quote: "summary counts asked, answered and by which method", method: "model"},
      {act: "t9-f-s3-0", kind: "ask-action", text: "keep the fixtures honest",
       verdict: "not-answered", by: null, mark: null, quote: null, method: null},
      {act: "t9-f-s4-0", kind: "offer", text: "Reply yes 1 or yes 2 to pick the glyph set",
       verdict: "unread", by: null, mark: null, quote: null, method: null},
    ],
  };
  const model = frameModel(withAlignment);
  assert.equal(model.alignmentTitle, "ALIGNMENT (馬-sonnet)");
  assert.equal(model.alignment[0], "4 asked, 2 answered (1 by quote, 1 by model), 1 not answered, 1 unread");
  assert.equal(model.alignment[1],
    '✓ ask-action "Please fix the stepper display for the…" → ㊢ "done; the alignment row renders one line…"  [quote]');
  assert.equal(model.alignment[2],
    '✓ ask-action "and add a summary line at the…" → ㊢ "summary counts asked, answered and by which…"  [model]');
  assert.equal(model.alignment[3], '? ask-action "keep the fixtures honest"');
  assert.equal(model.alignment[4], '• offer "Reply yes 1 or yes 2 to…"');
  const notRead = structuredClone(fixture[0]);
  assert.deepEqual(alignmentLines(notRead), [ALIGNMENT_UNREAD]);
  assert.equal(frameModel(notRead).alignmentTitle, "ALIGNMENT (馬)");
  assert.equal(frameModel(notRead).alignment[0], ALIGNMENT_UNREAD);
}

const compiled = await readFile(new URL("./stepper.js", import.meta.url), "utf8");
const page = await readFile(new URL("./index.html", import.meta.url), "utf8");
assert.doesNotMatch(compiled, /setInterval/);
assert.doesNotMatch(page, /setInterval/);
assert.doesNotMatch(page, /poll/);

console.log(`ok: ${fixture.length} frames; all cues found; pure render models complete`);
