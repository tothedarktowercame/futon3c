import assert from "node:assert/strict";
import {readFile} from "node:fs/promises";
import {cuePieces, frameModel, happenedDetail, portsAge, readyFrames} from "./stepper.js";

const fixture = JSON.parse(await readFile(new URL("./fixtures/frames-claude-16-00e653e3.json", import.meta.url)));

const provisional = structuredClone(fixture.at(-1));
provisional.parse.status = "drafted-provisional";
assert.equal(readyFrames([...fixture, provisional]).length, fixture.length + 1);
const missing = structuredClone(fixture.at(-1));
missing.parse.status = "missing";
assert.equal(readyFrames([...fixture, missing]).length, fixture.length);

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

console.log(`ok: ${fixture.length} frames; all cues found; pure render models complete`);
