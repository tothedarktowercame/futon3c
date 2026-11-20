import {
  MARK_COLOURS,
  authorColour,
  createClient,
  decorations,
  load,
  localChange,
  receive,
} from "./binding.js";

const NAME_RE = /^[A-Za-z0-9._-]{1,120}$/;
const SURFACE_RE = /^[A-Za-z0-9_-]{1,60}$/;
const params = new URLSearchParams(location.search);
const documentKey = params.get("doc");
const surface = params.get("surface") ?? "web";
const app = document.querySelector("#app");

let state;
let identity;
let textarea;
let layer;
let statusMain;
let noticeNode;
let legend;
let previousValue = "";
let connection = "connecting";
let notice = "";
let stream;
let composing = false;
let bufferedEntries = [];
let queue = [];
let posting = false;
let postController;
let generation = 0;

// The hub serves this page at .../together/v1/web/index.html with its routes one
// level up. A proxy may instead serve the page and the routes in one directory
// (zone's Caddy does, at /edit/), so that the browser sends its saved basic-auth
// credentials with every request: it only does so at or below the page's directory.
const API_BASE = /\/web\/[^/]*$/.test(location.pathname) ? "../" : "./";

function endpoint(path) {
  return new URL(`${API_BASE}${path}`, location.href).href;
}

function showPlainMessage(message) {
  app.replaceChildren();
  const paragraph = document.createElement("p");
  paragraph.id = "message";
  paragraph.textContent = message;
  app.append(paragraph);
}

function documentURL(key) {
  const url = new URL(location.href);
  url.searchParams.set("doc", key);
  return url.href;
}

async function showDocuments() {
  app.replaceChildren();
  const heading = document.createElement("h1");
  heading.textContent = "Together documents";
  const who = document.createElement("p");
  who.id = "list-identity";
  who.textContent = `Signed in as ${identity}`;
  const form = document.createElement("form");
  const label = document.createElement("label");
  label.textContent = "Document key ";
  const input = document.createElement("input");
  input.name = "document-key";
  input.type = "text";
  label.append(input);
  const button = document.createElement("button");
  button.type = "submit";
  button.textContent = "Open";
  const validation = document.createElement("p");
  validation.id = "key-validation";
  validation.setAttribute("role", "alert");
  form.append(label, button, validation);
  form.addEventListener("submit", (event) => {
    event.preventDefault();
    if (!NAME_RE.test(input.value)) {
      validation.textContent = "Invalid document key: use 1–120 letters, digits, dots, underscores or hyphens.";
      return;
    }
    location.assign(documentURL(input.value));
  });
  const list = document.createElement("ul");
  list.id = "document-list";
  const message = document.createElement("p");
  message.id = "list-status";
  message.textContent = "Reading documents…";
  app.append(heading, who, form, message, list);
  try {
    const response = await fetch(endpoint("docs"));
    if (!response.ok) throw new Error(`documents returned ${response.status}`);
    const { docs } = await response.json();
    if (!Array.isArray(docs)) throw new Error("invalid document list");
    for (const doc of docs) {
      const row = document.createElement("li");
      const link = document.createElement("a");
      link.textContent = doc.key;
      link.href = documentURL(doc.key);
      const detail = document.createElement("span");
      detail.textContent = doc.error
        ? ` — unavailable: ${doc.error}`
        : ` — rev ${doc.rev} · ${doc.chars} chars · ${doc.authors.join(", ") || "no authors"} · ${doc.at ?? "no changes yet"}`;
      row.append(link, detail);
      list.append(row);
    }
    message.textContent = docs.length ? "" : "No shared documents yet.";
  } catch (error) {
    message.textContent = `Unable to list documents: ${error.message}`;
  }
}

function syncScroll() {
  layer.scrollTop = textarea.scrollTop;
  layer.scrollLeft = textarea.scrollLeft;
}

function repaint() {
  if (!state || !layer) return;
  layer.replaceChildren();
  const authorRuns = decorations(state);
  for (const run of authorRuns) {
    const span = document.createElement("span");
    span.dataset.author = run.author;
    if (run.mark) span.dataset.mark = run.mark;
    span.style.color = run.mark ? MARK_COLOURS[run.mark] : authorColour(run.author);
    span.textContent = run.text;
    layer.append(span);
  }
  if (textarea.value.endsWith("\n") || textarea.value === "") {
    const finalLine = document.createElement("span");
    finalLine.className = "final-line";
    finalLine.textContent = "\u200b";
    layer.append(finalLine);
  }
  syncScroll();
  renderStatus(authorRuns);
}

function renderStatus(authorRuns = decorations(state)) {
  if (!statusMain) return;
  statusMain.textContent = `${identity} · ${documentKey} · rev ${state.rev} · ${connection} · ${queue.length} unsent`;
  noticeNode.textContent = notice;
  legend.replaceChildren();
  const authors = [...new Set(authorRuns.map((run) => run.author))];
  for (const author of authors) {
    const item = document.createElement("span");
    item.className = "legend-item";
    const swatch = document.createElement("span");
    swatch.className = "legend-swatch";
    swatch.style.backgroundColor = authorColour(author);
    const label = document.createElement("span");
    label.textContent = author;
    item.append(swatch, label);
    legend.append(item);
  }
  // Marked text is shown in its mark's colour, so the legend names the marks in use.
  for (const mark of Object.keys(MARK_COLOURS)) {
    if (!authorRuns.some((run) => run.mark === mark)) continue;
    const item = document.createElement("span");
    item.className = "legend-item";
    item.dataset.mark = mark;
    const swatch = document.createElement("span");
    swatch.className = "legend-swatch";
    swatch.style.backgroundColor = MARK_COLOURS[mark];
    const label = document.createElement("span");
    label.textContent = { ok: "enforced", debt: "not enforced", bad: "violated" }[mark];
    item.append(swatch, label);
    legend.append(item);
  }
}

function buildEditor() {
  app.replaceChildren();
  const stack = document.createElement("div");
  stack.className = "editor-stack";
  layer = document.createElement("div");
  layer.className = "text-layer";
  layer.setAttribute("aria-hidden", "true");
  textarea = document.createElement("textarea");
  textarea.className = "text-editor";
  textarea.setAttribute("aria-label", `Together document ${documentKey}`);
  textarea.spellcheck = false;
  stack.append(layer, textarea);

  const status = document.createElement("div");
  status.className = "status";
  statusMain = document.createElement("span");
  statusMain.className = "status-main";
  noticeNode = document.createElement("span");
  noticeNode.className = "notice";
  legend = document.createElement("span");
  legend.className = "legend";
  status.append(statusMain, noticeNode, legend);
  app.append(stack, status);

  textarea.addEventListener("scroll", syncScroll);
  textarea.addEventListener("input", (event) => {
    if (composing || event.isComposing) return;
    processLocalInput();
  });
  textarea.addEventListener("compositionstart", () => { composing = true; });
  textarea.addEventListener("compositionend", () => {
    composing = false;
    processLocalInput();
    const waiting = bufferedEntries;
    bufferedEntries = [];
    for (const entry of waiting) {
      if (!applyEntry(entry)) break;
    }
  });
}

function processLocalInput() {
  try {
    const changed = localChange(state, previousValue, textarea.value, textarea.selectionEnd);
    state = changed.state;
    previousValue = textarea.value;
    queue.push(...changed.ops.map((prepared) => ({ prepared, retry: 500 })));
    repaint();
    void drainQueue();
  } catch (error) {
    notice = error.message;
    void resync(true);
  }
}

function applyEntry(entry) {
  const direction = textarea.selectionDirection;
  const scrollTop = textarea.scrollTop;
  const scrollLeft = textarea.scrollLeft;
  const result = receive(state, entry, {
    start: textarea.selectionStart,
    end: textarea.selectionEnd,
  });
  if (result.resync) {
    void resync(true);
    return false;
  }
  state = result.state;
  if (result.value !== textarea.value) {
    textarea.value = result.value;
    textarea.setSelectionRange(result.selection.start, result.selection.end, direction);
  }
  textarea.scrollTop = scrollTop;
  textarea.scrollLeft = scrollLeft;
  previousValue = result.value;
  repaint();
  return true;
}

function openStream() {
  stream?.close();
  connection = "connecting";
  renderStatus();
  stream = new EventSource(endpoint(`doc/${encodeURIComponent(documentKey)}/events?after=${state.rev}`));
  stream.addEventListener("open", () => {
    connection = "live";
    notice = "";
    renderStatus();
  });
  stream.addEventListener("error", () => {
    connection = "reconnecting";
    renderStatus();
  });
  stream.addEventListener("message", (event) => {
    try {
      const entry = JSON.parse(event.data);
      // A reconnect before any event arrived replays from the rev in the URL,
      // which may now be behind us: entries we already hold are not a gap.
      if (Number.isInteger(entry.rev) && entry.rev <= state.rev
          && bufferedEntries.length === 0) return;
      if (composing) bufferedEntries.push(entry);
      else applyEntry(entry);
    } catch (error) {
      notice = `Invalid event: ${error.message}`;
      void resync(true);
    }
  });
}

function sleep(milliseconds) {
  return new Promise((resolve) => setTimeout(resolve, milliseconds));
}

async function drainQueue() {
  if (posting || queue.length === 0) return;
  posting = true;
  const item = queue[0];
  const thisGeneration = generation;
  postController = new AbortController();
  try {
    const response = await fetch(endpoint(`doc/${encodeURIComponent(documentKey)}/ops`), {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(item.prepared),
      signal: postController.signal,
    });
    if (thisGeneration !== generation) return;
    if (response.status === 400 || response.status === 409) {
      const body = await response.json().catch(() => ({}));
      notice = `Hub refused edit: ${body.error ?? response.status}`;
      await resync(true);
      return;
    }
    if (!response.ok) throw new Error(`hub returned ${response.status}`);
    if (queue[0] === item) queue.shift();
    item.retry = 500;
  } catch (error) {
    if (thisGeneration !== generation || error.name === "AbortError") return;
    connection = "reconnecting";
    notice = `Posting failed; retrying in ${item.retry / 1000}s`;
    renderStatus();
    await sleep(item.retry);
    item.retry = Math.min(item.retry * 2, 10000);
  } finally {
    if (thisGeneration === generation) {
      posting = false;
      postController = undefined;
      renderStatus();
      if (queue.length > 0) void drainQueue();
    }
  }
}

async function resync(reportDropped) {
  const dropped = queue.length;
  generation += 1;
  postController?.abort();
  posting = false;
  queue = [];
  bufferedEntries = [];
  stream?.close();
  connection = "resyncing";
  if (reportDropped && dropped > 0) notice = `${dropped} unsent edit${dropped === 1 ? " was" : "s were"} dropped during resync.`;
  renderStatus();

  try {
    const response = await fetch(endpoint(`doc/${encodeURIComponent(documentKey)}/state`));
    if (!response.ok) throw new Error(`state returned ${response.status}`);
    const snapshot = await response.json();
    state = load(state, snapshot);
    textarea.value = state.doc.characters
      .filter((character) => !character.deleted)
      .map((character) => String.fromCodePoint(character.cp))
      .join("");
    previousValue = textarea.value;
    const end = textarea.value.length;
    textarea.setSelectionRange(end, end);
    repaint();
    openStream();
  } catch (error) {
    connection = "reconnecting";
    notice = `Resync failed: ${error.message}`;
    renderStatus();
    setTimeout(() => void resync(false), 1000);
  }
}

window.addEventListener("beforeunload", (event) => {
  if (queue.length > 0) {
    event.preventDefault();
    event.returnValue = "";
  }
});

// Closing explicitly on an offline transition makes the connection state
// truthful even when a browser keeps an already-open SSE socket pending.
window.addEventListener("offline", () => {
  stream?.close();
  connection = "reconnecting";
  renderStatus();
});
window.addEventListener("online", () => {
  if (state) openStream();
});

async function start() {
  if (documentKey !== null && !NAME_RE.test(documentKey)) {
    showPlainMessage("Missing or invalid document key.");
    return;
  }
  if (!SURFACE_RE.test(surface)) {
    showPlainMessage("Invalid surface name.");
    return;
  }
  try {
    const response = await fetch(endpoint("whoami"));
    if (!response.ok) throw new Error(`identity returned ${response.status}`);
    ({ identity } = await response.json());
    if (documentKey === null) {
      await showDocuments();
      return;
    }
    state = createClient({ site: `${identity}.${surface}`, author: identity });
    buildEditor();
    await resync(false);
  } catch (error) {
    showPlainMessage(`Unable to start Together: ${error.message}`);
  }
}

void start();
