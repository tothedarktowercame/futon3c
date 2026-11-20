import { FACE_STYLES, faceOf } from "./binding.js";
import { utf16OffsetToIndex } from "./together.js";

export const NOTE_COLOURS = Object.freeze({
  magenta: "#c026d3", yellow: "#ca8a04", green: "#16803c", blue: "#2563eb",
  orange: "#ea580c", purple: "#7c3aed", red: "#dc2626", grey: "#6b7280",
});

function validId(id) {
  return Array.isArray(id) && id.length === 2 && typeof id[0] === "string"
    && Number.isInteger(id[1]) && id[1] >= 0;
}

export function decodeNotes(nodes, documentKey) {
  if (!nodes || typeof nodes !== "object") return [];
  const result = [];
  for (const [id, node] of Object.entries(nodes)) {
    const attrs = node?.attrs;
    if (node?.kind !== "span" || !attrs || attrs.doc !== documentKey
        || !validId(attrs.start) || !validId(attrs.end)
        || !Object.hasOwn(NOTE_COLOURS, attrs.colour)) continue;
    const note = { id, doc: attrs.doc, start: [...attrs.start], end: [...attrs.end], colour: attrs.colour };
    for (const key of ["by", "title", "text", "quote_title", "quote", "where"]) {
      if (typeof attrs[key] === "string") note[key] = attrs[key];
    }
    const source = decodeSource(attrs.source);
    if (source) note.source = source;
    result.push(note);
  }
  return result.sort((a, b) => a.id.localeCompare(b.id));
}

const idKey = (id) => `${id[0]}\0${id[1]}`;

// One pass over DOC resolves all notes together; neither argument is mutated.
export function allNoteExtents(doc, notes) {
  const starts = new Map();
  const ends = new Map();
  for (const note of notes) {
    const start = idKey(note.start);
    const end = idKey(note.end);
    if (!starts.has(start)) starts.set(start, []);
    if (!ends.has(end)) ends.set(end, []);
    starts.get(start).push(note);
    ends.get(end).push(note);
  }
  const active = new Map();
  const seenStarts = new Set();
  const seenEnds = new Set();
  const reversed = new Set();
  const resolved = [];
  let visible = 0;
  for (const character of doc.characters) {
    const key = idKey(character.id);
    for (const note of starts.get(key) ?? []) {
      seenStarts.add(note.id);
      active.set(note.id, { note, from: visible, text: "", live: 0, deleted: 0 });
    }
    for (const extent of active.values()) {
      if (character.deleted) extent.deleted += 1;
      else {
        extent.live += 1;
        extent.text += String.fromCodePoint(character.cp);
      }
    }
    for (const note of ends.get(key) ?? []) {
      seenEnds.add(note.id);
      if (!seenStarts.has(note.id)) reversed.add(note.id);
      const extent = active.get(note.id);
      if (extent) {
        resolved.push({ ...extent.note, extent: { from: extent.from,
          to: extent.from + extent.live, text: extent.text,
          live: extent.live, deleted: extent.deleted } });
        active.delete(note.id);
      }
    }
    if (!character.deleted) visible += 1;
  }
  const unresolved = notes.filter((note) => reversed.has(note.id)
    || !seenStarts.has(note.id) || !seenEnds.has(note.id))
    .map((note) => ({ ...note, error: reversed.has(note.id) ? "invalid-span" : "unknown-id" }));
  return { resolved, unresolved };
}

export function notesAtIndex(resolved, index) {
  return resolved.filter(({ extent }) => extent.from <= index && index < extent.to)
    .sort((a, b) => (a.extent.to - a.extent.from) - (b.extent.to - b.extent.from)
      || a.id.localeCompare(b.id));
}

export function underlineRuns(authorRuns, resolved) {
  const result = [];
  // The innermost note can change only at a note's start or end, so it is
  // looked up once per stretch between boundaries, not once per character.
  const boundaries = [...new Set(resolved.flatMap(({ extent }) => [extent.from, extent.to]))]
    .sort((a, b) => a - b);
  let next = 0;
  let underline = null;
  let index = 0;
  for (const run of authorRuns) {
    for (const point of run.text) {
      if (index === 0 || boundaries[next] === index) {
        while (boundaries[next] <= index) next += 1;
        const note = notesAtIndex(resolved, index)[0];
        underline = note ? NOTE_COLOURS[note.colour] : null;
      }
      const previous = result.at(-1);
      const same = previous && previous.author === run.author && previous.face === run.face
        && previous.mark === run.mark && previous.underline === underline;
      if (same) previous.text += point;
      else result.push({ ...run, text: point, underline });
      index += 1;
    }
  }
  return result;
}

export function textAtCaret(doc, textareaValue, caretUtf16) {
  const index = utf16OffsetToIndex(textareaValue, caretUtf16);
  let visible = 0;
  for (const character of doc.characters) {
    if (character.deleted) continue;
    if (visible === index) {
      const author = character.attrs?.author ?? character.id[0].split(".", 1)[0];
      return { index, author, label: FACE_STYLES[faceOf(character.attrs)]?.label ?? null };
    }
    visible += 1;
  }
  return { index, author: null, label: null };
}

// Does not mutate. The workspace's nodes without those a later `supersede`
// act replaced: STATE.derived maps each replacing id to the id it replaced.
export function currentNodes(state) {
  const replaced = new Set(Object.values(state?.derived ?? {}));
  return Object.fromEntries(Object.entries(state?.nodes ?? {})
    .filter(([id]) => !replaced.has(id)));
}

function decodeSource(source) {
  if (!source || typeof source !== "object" || Array.isArray(source)) return null;
  const keys = Object.keys(source);
  if (keys.every((key) => ["doc", "words"].includes(key))
      && typeof source.doc === "string" && /^[A-Za-z0-9._-]{1,120}$/.test(source.doc)
      && typeof source.words === "string" && source.words.trim()) return { ...source };
  if (keys.every((key) => ["url", "words"].includes(key))
      && typeof source.url === "string" && /^https?:\/\//i.test(source.url)
      && (!Object.hasOwn(source, "words") || typeof source.words === "string")) {
    try {
      const url = new URL(source.url);
      if (["http:", "https:"].includes(url.protocol)) return { ...source };
    } catch { /* Malformed source leaves the note intact. */ }
  }
  return null;
}

export function findWords(text, words) {
  if (typeof text !== "string" || typeof words !== "string" || !words.trim()) return null;
  const pattern = words.trim().split(/\s+/u)
    .map((word) => word.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")).join("\\s+");
  const match = new RegExp(pattern, "u").exec(text);
  if (!match) return null;
  const from = Array.from(text.slice(0, match.index)).length;
  return { from, to: from + Array.from(match[0]).length };
}
