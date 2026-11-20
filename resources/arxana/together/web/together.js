// Pure document model for Arxana Together. None of the exported functions
// mutates its arguments; constructors and transformations return fresh data.

const EDITOR_RE = /^[A-Za-z0-9._-]+$/;

function clone(value) {
  if (Array.isArray(value)) return value.map(clone);
  if (value && typeof value === "object") {
    return Object.fromEntries(Object.entries(value).map(([key, item]) => [key, clone(item)]));
  }
  return value;
}

function deepEqual(a, b) {
  if (Object.is(a, b)) return true;
  if (Array.isArray(a) || Array.isArray(b)) {
    return Array.isArray(a) && Array.isArray(b)
      && a.length === b.length
      && a.every((item, index) => deepEqual(item, b[index]));
  }
  if (a && b && typeof a === "object" && typeof b === "object") {
    const aKeys = Object.keys(a).sort();
    const bKeys = Object.keys(b).sort();
    return aKeys.length === bKeys.length
      && aKeys.every((key, index) => key === bKeys[index] && deepEqual(a[key], b[key]));
  }
  return false;
}

function copyId(id) {
  return [id[0], id[1]];
}

function idKey(id) {
  return `${id[0]}\u0000${id[1]}`;
}

function validId(id) {
  return Array.isArray(id)
    && id.length === 2
    && typeof id[0] === "string"
    && EDITOR_RE.test(id[0])
    && Number.isInteger(id[1])
    && id[1] > 0;
}

function assertId(id) {
  if (!validId(id)) throw new TypeError(`invalid character id: ${JSON.stringify(id)}`);
}

function sameId(a, b) {
  return a[0] === b[0] && a[1] === b[1];
}

// The protocol order is counter first, then ASCII editor name.
function compareIds(a, b) {
  if (a[1] !== b[1]) return a[1] - b[1];
  return a[0] < b[0] ? -1 : a[0] > b[0] ? 1 : 0;
}

function codedError(code, message) {
  const error = new Error(message);
  error.code = code;
  return error;
}

function characters(doc) {
  if (!doc || !Array.isArray(doc.characters)) throw new TypeError("invalid Together document");
  return doc.characters;
}

// Does not mutate: returns a new empty document.
export function emptyDoc() {
  return { characters: [] };
}

// Does not mutate: applies one insert or delete and returns a new document.
export function applyOp(doc, op) {
  const current = characters(doc);
  if (!op || typeof op !== "object") throw new TypeError("invalid operation");
  const operation = op.op ?? op.type;

  if (operation === "insert") {
    assertId(op.id);
    if (op.after !== null) assertId(op.after);
    if (typeof op.text !== "string" || [...op.text].length === 0) {
      throw codedError("empty-text", "insert text must contain at least one code point");
    }

    const anchorIndex = op.after === null
      ? -1
      : current.findIndex((character) => sameId(character.id, op.after));
    if (op.after !== null && anchorIndex < 0) {
      throw codedError("unknown-after", `unknown insertion anchor ${JSON.stringify(op.after)}`);
    }
    if (op.after !== null && op.id[1] <= op.after[1]) {
      throw codedError("counter-not-after", "insert counter must be greater than anchor counter");
    }

    const points = [...op.text];
    const newIds = points.map((_, index) => [op.id[0], op.id[1] + index]);
    const occupied = new Set(current.map((character) => idKey(character.id)));
    if (newIds.some((id) => occupied.has(idKey(id)))) {
      throw codedError("id-in-use", "insert would reuse an existing character id");
    }

    const result = current.slice();
    let leftIndex = anchorIndex;
    for (let index = 0; index < points.length; index += 1) {
      const id = newIds[index];
      let insertionIndex = leftIndex + 1;
      while (insertionIndex < result.length
             && compareIds(result[insertionIndex].id, id) > 0) {
        insertionIndex += 1;
      }
      result.splice(insertionIndex, 0, {
        id,
        cp: points[index].codePointAt(0),
        attrs: clone(op.attrs ?? {}),
        deleted: false,
      });
      leftIndex = insertionIndex;
    }
    return { characters: result };
  }

  if (operation === "delete") {
    if (!Array.isArray(op.ids)) throw new TypeError("delete ids must be an array");
    op.ids.forEach(assertId);
    const known = new Set(current.map((character) => idKey(character.id)));
    if (op.ids.some((id) => !known.has(idKey(id)))) {
      throw codedError("unknown-id", "delete names an unknown character id");
    }
    const deleted = new Set(op.ids.map(idKey));
    return {
      characters: current.map((character) => deleted.has(idKey(character.id))
        ? { ...character, deleted: true }
        : character),
    };
  }

  throw new TypeError(`unknown operation type: ${String(operation)}`);
}

// Does not mutate: reads the visible text.
export function text(doc) {
  return characters(doc)
    .filter((character) => !character.deleted)
    .map((character) => String.fromCodePoint(character.cp))
    .join("");
}

// Does not mutate: returns fresh ids for visible characters, in document order.
export function ids(doc) {
  return characters(doc)
    .filter((character) => !character.deleted)
    .map((character) => copyId(character.id));
}

// Does not mutate: counts deleted ids too, as required by the protocol.
export function nextCounter(doc) {
  return characters(doc).reduce((largest, character) => Math.max(largest, character.id[1]), 0) + 1;
}

// Does not mutate: expands a hub snapshot without reordering its characters.
export function fromRuns(snapshotRuns) {
  if (!Array.isArray(snapshotRuns)) throw new TypeError("runs must be an array");
  const expanded = [];
  for (const run of snapshotRuns) {
    assertId(run.id);
    if (typeof run.text !== "string") throw new TypeError("run text must be a string");
    [...run.text].forEach((point, index) => {
      expanded.push({
        id: [run.id[0], run.id[1] + index],
        cp: point.codePointAt(0),
        attrs: clone(run.attrs ?? {}),
        deleted: Boolean(run.deleted),
      });
    });
  }
  return { characters: expanded };
}

// Does not mutate: returns maximal adjacent runs with fresh ids and attrs.
export function runs(doc) {
  const result = [];
  for (const character of characters(doc)) {
    const previous = result[result.length - 1];
    const canExtend = previous
      && previous.id[0] === character.id[0]
      && previous.id[1] + [...previous.text].length === character.id[1]
      && previous.deleted === Boolean(character.deleted)
      && deepEqual(previous.attrs, character.attrs);
    const point = String.fromCodePoint(character.cp);
    if (canExtend) {
      previous.text += point;
    } else {
      result.push({
        id: copyId(character.id),
        text: point,
        attrs: clone(character.attrs),
        deleted: Boolean(character.deleted),
      });
    }
  }
  return result;
}

// Does not mutate. Returns -1 for an unknown id. Tombstones map to the number
// of visible characters before their document position.
export function visibleIndexOf(doc, id) {
  assertId(id);
  let visible = 0;
  for (const character of characters(doc)) {
    if (sameId(character.id, id)) return visible;
    if (!character.deleted) visible += 1;
  }
  return -1;
}

// Does not mutate. Resolves inclusive document ids in one pass, counting
// visible positions in Unicode code points rather than UTF-16 code units.
export function spanExtent(doc, start, end) {
  assertId(start);
  assertId(end);
  let visible = 0;
  let from;
  let live = 0;
  let deleted = 0;
  let started = false;
  let ended = false;
  let reversed = false;
  let surviving = "";
  for (const character of characters(doc)) {
    if (sameId(character.id, start)) {
      started = true;
      from = visible;
    }
    if (started && !ended) {
      if (character.deleted) deleted += 1;
      else {
        live += 1;
        surviving += String.fromCodePoint(character.cp);
      }
    }
    if (sameId(character.id, end)) {
      if (!started) reversed = true;
      ended = true;
    }
    if (!character.deleted) visible += 1;
  }
  if (!started || !ended) {
    throw codedError("unknown-id", "span names an unknown character id");
  }
  if (reversed) throw codedError("invalid-span", "span end comes before its start");
  return { from, to: from + live, text: surviving, live, deleted };
}

// Does not mutate. Returns a fresh id, or null when i is outside visible text.
export function idAtVisibleIndex(doc, i) {
  if (!Number.isInteger(i) || i < 0) return null;
  let visible = 0;
  for (const character of characters(doc)) {
    if (!character.deleted) {
      if (visible === i) return copyId(character.id);
      visible += 1;
    }
  }
  return null;
}

// Does not mutate. i may range from zero through the visible length.
export function leftIdForInsert(doc, i) {
  const length = ids(doc).length;
  if (!Number.isInteger(i) || i < 0 || i > length) {
    throw new RangeError("visible insertion index is outside the document");
  }
  return i === 0 ? null : idAtVisibleIndex(doc, i - 1);
}

// Does not mutate. Offsets are clamped; a position inside a surrogate pair
// maps to the code-point index at the pair's start.
export function utf16OffsetToIndex(str, offset) {
  if (typeof str !== "string") throw new TypeError("expected a string");
  if (!Number.isFinite(offset) || offset <= 0) return 0;
  const target = Math.min(Math.trunc(offset), str.length);
  let utf16 = 0;
  let index = 0;
  for (const point of str) {
    if (target < utf16 + point.length) return index;
    utf16 += point.length;
    index += 1;
    if (target === utf16) return index;
  }
  return index;
}

// Does not mutate. Indices are clamped to the string's code-point bounds.
export function indexToUtf16Offset(str, index) {
  if (typeof str !== "string") throw new TypeError("expected a string");
  if (!Number.isFinite(index) || index <= 0) return 0;
  const target = Math.trunc(index);
  let utf16 = 0;
  let current = 0;
  for (const point of str) {
    if (current === target) break;
    utf16 += point.length;
    current += 1;
  }
  return utf16;
}
