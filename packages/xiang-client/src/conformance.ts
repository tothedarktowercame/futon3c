/**
 * Does a turn record conform to the seam? A port of
 * `packages/turn-seam/conformance.py`, so a client can refuse a record whose
 * spans are not the text they claim to be before drawing anything from it.
 */

import { codepointLength, cpSubstring } from "./offsets.js";
import { OFFSET_UNIT, type TurnRecord } from "./types.js";

const REQUIRED = ["version", "source_text", "offset_unit", "sentences", "unmatched", "created_at", "agent_id", "session_id", "turn_id"] as const;
const STATUSES = new Set(["unresolved", "cue-only", "analyzed"]);
const ANALYSIS_STATUSES = new Set(["requested", "not-requested", "analyzed", "refused", "failed"]);

/** The problems with `rec`, empty when it conforms. */
export function checkRecord(rec: unknown): string[] {
  const bad: string[] = [];
  if (!rec || typeof rec !== "object") return ["not an object"];
  const r = rec as Record<string, unknown>;
  for (const k of REQUIRED) if (!(k in r)) bad.push(`missing required field ${k}`);
  if (bad.length > 0) return bad;

  if (r.version !== 1) bad.push(`version is ${JSON.stringify(r.version)}, this checker knows version 1`);
  if (r.offset_unit !== OFFSET_UNIT) bad.push(`offset_unit is ${JSON.stringify(r.offset_unit)}; a client counting UTF-16 units will disagree silently on any emoji or CJK`);
  const src = String(r.source_text);
  const len = codepointLength(src);
  const sentences = Array.isArray(r.sentences) ? r.sentences : [];
  if (!Array.isArray(r.sentences)) bad.push("sentences is not an array");

  for (const s of sentences as Array<Record<string, unknown>>) {
    const missing = ["id", "start", "end", "text", "status", "cues"].find((k) => !(k in s));
    if (missing) {
      bad.push(`sentence missing ${missing}`);
      continue;
    }
    const a = s.start as number;
    const b = s.end as number;
    if (!(Number.isInteger(a) && Number.isInteger(b) && 0 <= a && a < b && b <= len)) {
      bad.push(`${s.id}: span [${a} ${b}] is outside source_text (len ${len})`);
    } else if (cpSubstring(src, a, b) !== s.text) {
      bad.push(`${s.id}: source_text[${a}:${b}] is ${JSON.stringify(cpSubstring(src, a, b).slice(0, 40))}, the record says ${JSON.stringify(String(s.text).slice(0, 40))}`);
    }
    if (!STATUSES.has(String(s.status))) bad.push(`${s.id}: status ${JSON.stringify(s.status)} is not one of ${[...STATUSES].sort().join(", ")}`);
    for (const c of (Array.isArray(s.cues) ? s.cues : []) as Array<Record<string, unknown>>) {
      const ca = c.start as number;
      const cb = c.end as number;
      if (!(Number.isInteger(ca) && Number.isInteger(cb) && a <= ca && ca < cb && cb <= b)) {
        bad.push(`${s.id}: cue [${ca} ${cb}] is outside its sentence [${a} ${b}]`);
      } else if (cpSubstring(src, ca, cb) !== c.text) {
        bad.push(`${s.id}: cue text ${JSON.stringify(c.text)} is not source_text[${ca}:${cb}]`);
      }
    }
  }
  if (!Array.isArray(r.unmatched)) bad.push("unmatched is not an array");
  if ("analysis_status" in r && !ANALYSIS_STATUSES.has(String(r.analysis_status))) {
    bad.push(`analysis_status ${JSON.stringify(r.analysis_status)} is not one of ${[...ANALYSIS_STATUSES].sort().join(", ")}`);
  }
  return bad;
}

export function isTurnRecord(rec: unknown): rec is TurnRecord {
  return checkRecord(rec).length === 0;
}
