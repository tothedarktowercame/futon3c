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

// A character may carry a display mark, fixed at insert like its other
// attributes: "ok", "debt" or "bad". The page shows marked text in that mark's
// colour instead of its author's. Any other value is ignored.
export const MARK_COLOURS = { ok: "#1a7f37", debt: "#9a6700", bad: "#b42318" };

// A character may also carry a face, fixed at insert: a name from FACE_STYLES.
// It is how a document copied or shared from the incubator form keeps the form's
// colours (typed, from the call, an agent, a teammate, ...). An answer shared
// from Emacs carries the Emacs face under attrs.prov.face instead; EMACS_FACES
// maps those names here. Unknown names are ignored.
export const FACE_STYLES = {
  plain: { color: "#20242a" },
  heading: { color: "#20242a", fontWeight: "700" },
  muted: { color: "#6b7280" },
  typed: { color: "#1d7a1d", label: "typed" },
  call: { color: "#1f5fbf", label: "from the call" },
  agent: { color: "#7a2fb0", label: "agent" },
  teammate: { color: "#a35200", label: "teammate" },
  peeragogy: { color: "#8b2a7a", label: "Peeragogy call" },
  struck: { color: "#b42318", textDecoration: "line-through", label: "struck" },
};

export const EMACS_FACES = {
  "futon4a-variants-diff-added": "typed",
  "futon4a-incubator-clip": "call",
  "futon4a-incubator-agent": "agent",
  "futon4a-incubator-teammate": "teammate",
  "futon4a-incubator-peeragogy": "peeragogy",
  "futon4a-variants-diff-dropped": "struck",
  "futon4a-incubator-struck": "struck",
  bold: "heading",
  shadow: "muted",
};

// Does not mutate. The face name for ATTRS, or undefined.
export function faceOf(attrs) {
  const direct = attrs?.face;
  if (typeof direct === "string" && Object.hasOwn(FACE_STYLES, direct)) return direct;
  const emacs = attrs?.prov?.face;
  const names = emacs?.tag === "symbol" ? [emacs.value]
    : emacs?.tag === "list" && Array.isArray(emacs.value)
      ? emacs.value.filter((item) => item?.tag === "symbol").map((item) => item.value)
      : [];
  for (const name of names) {
    if (typeof name === "string" && Object.hasOwn(EMACS_FACES, name)) return EMACS_FACES[name];
  }
  return undefined;
}

// Does not mutate: returns maximal visible runs sharing a displayed author, mark
// and face. A run has a `mark` or `face` key only when its characters have one.
export function decorations(state) {
  const result = [];
  for (const character of state.doc.characters) {
    if (character.deleted) continue;
    const author = character.attrs?.author ?? character.id[0].split(".", 1)[0];
    const raw = character.attrs?.mark;
    const mark = typeof raw === "string" && Object.hasOwn(MARK_COLOURS, raw) ? raw : undefined;
    const face = faceOf(character.attrs);
    const point = String.fromCodePoint(character.cp);
    const previous = result[result.length - 1];
    if (previous?.author === author && previous.mark === mark && previous.face === face) {
      previous.text += point;
    } else {
      const run = { author };
      if (mark) run.mark = mark;
      if (face) run.face = face;
      run.text = point;
      result.push(run);
    }
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
