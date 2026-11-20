// Pure textarea binding for Together. Every exported function leaves its
// arguments unchanged and returns new state/data where applicable.

import {
  applyOp,
  emptyDoc,
  fromRuns,
  ids,
  indexToUtf16Offset,
  leftIdForInsert,
  nextCounter,
  text,
  utf16OffsetToIndex,
  visibleIndexOf,
} from "./together.js";

const SITE_RE = /^[A-Za-z0-9._-]+$/;
const PALETTE = [
  "#1f5aa6", "#a33b20", "#34702c", "#7446a8",
  "#9a5b00", "#00716b", "#a02f68", "#4f5b66",
  "#684f17", "#175f7a", "#7a3e70", "#496b1f",
];

function codedError(code, message) {
  const error = new Error(message);
  error.code = code;
  return error;
}

function idKey(id) {
  return `${id[0]}\u0000${id[1]}`;
}

function hasId(doc, id) {
  const wanted = idKey(id);
  return doc.characters.some((character) => idKey(character.id) === wanted);
}

function visibleIndexIfVisible(doc, id) {
  const wanted = idKey(id);
  const character = doc.characters.find((item) => idKey(item.id) === wanted);
  return character && !character.deleted ? visibleIndexOf(doc, id) : -1;
}

function commonPrefix(oldPoints, newPoints) {
  let length = 0;
  while (length < oldPoints.length
         && length < newPoints.length
         && oldPoints[length] === newPoints[length]) length += 1;
  return length;
}

function commonSuffix(oldPoints, newPoints, prefix) {
  let length = 0;
  while (length < oldPoints.length - prefix
         && length < newPoints.length - prefix
         && oldPoints[oldPoints.length - 1 - length] === newPoints[newPoints.length - 1 - length]) {
    length += 1;
  }
  return length;
}

// Finds every maximally retained one-span edit. Repeated characters can make
// several placements equivalent; the post-input caret chooses among them.
function changedSpan(oldValue, newValue, caretUtf16) {
  const oldPoints = [...oldValue];
  const newPoints = [...newValue];
  const candidates = [];

  // The common suffix does not depend on the prefix except through the cap
  // that the two may not overlap, so it is computed once (one pass, not one
  // pass per candidate prefix: that was quadratic in the document length).
  const shorter = Math.min(oldPoints.length, newPoints.length);
  const fullSuffix = commonSuffix(oldPoints, newPoints, 0);
  for (let prefix = 0; prefix <= shorter; prefix += 1) {
    if (prefix > 0 && oldPoints[prefix - 1] !== newPoints[prefix - 1]) break;
    const suffix = Math.min(fullSuffix, shorter - prefix);
    candidates.push({ prefix, suffix, retained: prefix + suffix });
  }

  const bestRetained = Math.max(...candidates.map((candidate) => candidate.retained));
  const best = candidates.filter((candidate) => candidate.retained === bestRetained);
  const atCaret = best.filter((candidate) => (
    indexToUtf16Offset(newValue, newPoints.length - candidate.suffix) === caretUtf16
  ));
  const chosen = (atCaret.length > 0 ? atCaret : best)
    .reduce((left, right) => (right.prefix > left.prefix ? right : left));

  return {
    index: chosen.prefix,
    removed: oldPoints.slice(chosen.prefix, oldPoints.length - chosen.suffix),
    inserted: newPoints.slice(chosen.prefix, newPoints.length - chosen.suffix),
  };
}

function wrap(op, rev) {
  return { op, parents: { rev } };
}

function insertionSelection(oldValue, newValue, selection, insertionIndex) {
  const insertionOffset = indexToUtf16Offset(oldValue, insertionIndex);
  const insertedLength = newValue.length - oldValue.length;
  const move = (offset) => (insertionOffset <= offset ? offset + insertedLength : offset);
  return { start: move(selection.start), end: move(selection.end) };
}

function deletionSelection(oldValue, newValue, selection, deletedIndices) {
  const deleted = new Set(deletedIndices);
  const move = (offset) => {
    const oldIndex = utf16OffsetToIndex(oldValue, offset);
    let newIndex = 0;
    for (let index = 0; index < oldIndex; index += 1) {
      if (!deleted.has(index)) newIndex += 1;
    }
    return indexToUtf16Offset(newValue, newIndex);
  };
  return { start: move(selection.start), end: move(selection.end) };
}

// Does not mutate: constructs a client at revision zero.
export function createClient({ site, author }) {
  if (typeof site !== "string" || !SITE_RE.test(site)) throw new TypeError("invalid site");
  if (typeof author !== "string" || author.length === 0) throw new TypeError("invalid author");
  return { site, author, doc: emptyDoc(), rev: 0 };
}

// Does not mutate: replaces the document and revision from a hub snapshot.
export function load(state, snapshot) {
  if (!snapshot || !Number.isInteger(snapshot.rev) || !Array.isArray(snapshot.runs)) {
    throw new TypeError("invalid Together snapshot");
  }
  return { ...state, doc: fromRuns(snapshot.runs), rev: snapshot.rev };
}

// Does not mutate: converts one textarea value transition into protocol ops.
export function localChange(state, oldValue, newValue, caretUtf16) {
  if (oldValue !== text(state.doc)) {
    throw codedError("stale-value", "textarea value does not match the client document");
  }
  if (typeof newValue !== "string") throw new TypeError("new value must be a string");

  const change = changedSpan(oldValue, newValue, caretUtf16);
  const operations = [];
  let doc = state.doc;

  if (change.removed.length > 0) {
    const deletedIds = ids(doc).slice(change.index, change.index + change.removed.length);
    const op = { op: "delete", ids: deletedIds };
    operations.push(wrap(op, state.rev));
    doc = applyOp(doc, op);
  }

  if (change.inserted.length > 0) {
    const op = {
      op: "insert",
      id: [state.site, nextCounter(doc)],
      after: leftIdForInsert(doc, change.index),
      text: change.inserted.join(""),
      attrs: { author: state.author },
    };
    operations.push(wrap(op, state.rev));
    doc = applyOp(doc, op);
  }

  return { state: { ...state, doc }, ops: operations };
}

// Does not mutate: integrates the next hub entry and maps the UTF-16 selection.
export function receive(state, entry, selection) {
  const oldValue = text(state.doc);
  const oldSelection = { start: selection.start, end: selection.end };
  if (!entry || entry.rev !== state.rev + 1) {
    return { state, value: oldValue, selection: oldSelection, resync: true };
  }

  const op = entry.op;
  const operation = op?.op ?? op?.type;
  if (operation === "insert" && hasId(state.doc, op.id)) {
    return {
      state: { ...state, rev: entry.rev },
      value: oldValue,
      selection: oldSelection,
      resync: false,
    };
  }

  let deletedIndices = [];
  if (operation === "delete") {
    deletedIndices = op.ids
      .map((id) => visibleIndexIfVisible(state.doc, id))
      .filter((index) => index >= 0);
  }

  const doc = applyOp(state.doc, op);
  const value = text(doc);
  let adjusted = oldSelection;
  if (operation === "insert") {
    adjusted = insertionSelection(
      oldValue,
      value,
      oldSelection,
      visibleIndexOf(doc, op.id),
    );
  } else if (operation === "delete") {
    adjusted = deletionSelection(oldValue, value, oldSelection, deletedIndices);
  }

  return {
    state: { ...state, doc, rev: entry.rev },
    value,
    selection: adjusted,
    resync: false,
  };
}

// Does not mutate: returns maximal visible runs sharing a displayed author.
export function decorations(state) {
  const result = [];
  for (const character of state.doc.characters) {
    if (character.deleted) continue;
    const author = character.attrs?.author ?? character.id[0].split(".", 1)[0];
    const point = String.fromCodePoint(character.cp);
    const previous = result[result.length - 1];
    if (previous?.author === author) previous.text += point;
    else result.push({ author, text: point });
  }
  return result;
}

// Does not mutate: deterministically selects a white-background-safe colour.
export function authorColour(author) {
  let hash = 2166136261;
  for (const point of String(author)) {
    hash ^= point.codePointAt(0);
    hash = Math.imul(hash, 16777619);
  }
  return PALETTE[(hash >>> 0) % PALETTE.length];
}
