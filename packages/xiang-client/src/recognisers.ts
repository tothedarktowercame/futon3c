/**
 * The three things the Emacs REPL recognised locally before a turn left the
 * buffer, ported from agent-chat.el so a web client makes the same calls.
 *
 * - `parseAcceptance`: `agent-chat--acceptance-command`, which mirrors
 *   `futon3c.agency.agreement-record/parse-acceptance`.
 * - `undoCommand`: `agent-chat--undo-command`.
 * - `splitSurfaceMarker`: `agent-chat-split-surface-marker`.
 *
 * These are duplicated on the server; a drift between them is a bug on
 * whichever side moved last. The grammar tests in test/recognisers.test.ts
 * carry the cases from both.
 */

export interface Acceptance {
  offerId: string | null;
  optionId: string | null;
}

/**
 * `yes`, `yes N`, `yes act:ID`, `yes act:ID N`, optionally prefixed by
 * `🈸:` and ending in one `.` or `!`. Only `yes` is case-insensitive.
 */
export function parseAcceptance(text: string): Acceptance | null {
  if (typeof text !== "string") return null;
  const trimmed = text.trim();
  const normalized = trimmed
    .replace(/[.!]$/, "")
    .replace(/^🈸:\s*/, "")
    .trim();
  const m = /^[Yy][Ee][Ss](?:\s+(?:(act:[^\s]+)(?:\s+([0-9]+))?|([0-9]+)))?$/.exec(normalized);
  if (!m) return null;
  return { offerId: m[1] ?? null, optionId: m[2] ?? m[3] ?? null };
}

/**
 * `undo` → "undo"; `undo act:ID` → the effect id; anything else → null.
 * Case-folded, trailing punctuation dropped.
 */
export function undoCommand(text: string): "undo" | string | null {
  if (typeof text !== "string") return null;
  const normalized = text.trim().toLowerCase().replace(/[.!?,;]+$/, "");
  const m = /^undo(?:\s+(act:[^\s]+))?$/.exec(normalized);
  if (!m) return null;
  return m[1] ?? "undo";
}

/** Leading markers that say how a turn was produced. */
export const SURFACE_MARKERS: ReadonlyArray<readonly [string, string]> = [["🗣", "dictated"]];

export interface SurfaceSplit {
  surface: string | null;
  text: string;
}

/** Only a leading marker counts; the same character mid-sentence is text. */
export function splitSurfaceMarker(text: string): SurfaceSplit {
  for (const [marker, surface] of SURFACE_MARKERS) {
    if (text.startsWith(marker)) {
      return { surface, text: text.slice(marker.length).replace(/^\s+/, "") };
    }
  }
  return { surface: null, text };
}
