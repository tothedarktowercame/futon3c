import {
  FACE_STYLES,
  MARK_COLOURS,
  authorColour,
  createClient,
  decorations,
  load,
  localChange,
  receive,
} from "./binding.js";
import { text } from "./together.js";

export const NAME_RE = /^[A-Za-z0-9._-]{1,120}$/;
export const SURFACE_RE = /^[A-Za-z0-9_-]{1,60}$/;

// The hub normally serves pages below /web/. A proxy may instead mount the
// pages beside the API (zone uses /edit/), where ./ keeps credentials scoped.
const API_BASE = /\/web\/[^/]*$/.test(location.pathname) ? "../" : "./";

export function endpoint(path) {
  return new URL(`${API_BASE}${path}`, location.href).href;
}

export function renderLegend(legend, authorRuns) {
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
  for (const [face, style] of Object.entries(FACE_STYLES)) {
    if (!style.label || !authorRuns.some((run) => run.face === face)) continue;
    const item = document.createElement("span");
    item.className = "legend-item";
    item.dataset.face = face;
    const swatch = document.createElement("span");
    swatch.className = "legend-swatch";
    swatch.style.backgroundColor = style.color;
    const label = document.createElement("span");
    label.textContent = style.label;
    item.append(swatch, label);
    legend.append(item);
  }
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

export function createEditor({
  container,
  documentKey,
  identity,
  surface,
  endpoint: endpointFor,
  grow,
}) {
  let state = createClient({ site: `${identity}.${surface}`, author: identity });
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
  let destroyed = false;
  const timers = new Set();

  container.replaceChildren();
  const stack = document.createElement("div");
  stack.className = `editor-stack${grow ? " editor-grow" : ""}`;
  const layer = document.createElement("div");
  layer.className = "text-layer";
  layer.setAttribute("aria-hidden", "true");
  const textarea = document.createElement("textarea");
  textarea.className = "text-editor";
  textarea.setAttribute("aria-label", `Together document ${documentKey}`);
  textarea.spellcheck = false;
  stack.append(layer, textarea);
  container.append(stack);

  let statusMain;
  let noticeNode;
  let legend;
  if (!grow) {
    const status = document.createElement("div");
    status.className = "status";
    statusMain = document.createElement("span");
    statusMain.className = "status-main";
    noticeNode = document.createElement("span");
    noticeNode.className = "notice";
    legend = document.createElement("span");
    legend.className = "legend";
    status.append(statusMain, noticeNode, legend);
    container.append(status);
  }

  function later(callback, milliseconds) {
    const timer = setTimeout(() => {
      timers.delete(timer);
      if (!destroyed) callback();
    }, milliseconds);
    timers.add(timer);
  }

  function syncScroll() {
    layer.scrollTop = textarea.scrollTop;
    layer.scrollLeft = textarea.scrollLeft;
  }

  function measure() {
    if (!grow) return;
    stack.style.height = "1px";
    stack.style.height = `${Math.max(128, textarea.scrollHeight + 2)}px`;
  }

  function emit(authorRuns = decorations(state)) {
    container.dispatchEvent(new CustomEvent("together-editor-state", {
      bubbles: true,
      detail: {
        documentKey,
        connection,
        unsent: queue.length,
        runs: authorRuns,
        rev: state.rev,
      },
    }));
  }

  function renderStatus(authorRuns = decorations(state)) {
    if (statusMain) {
      statusMain.textContent = `${identity} · ${documentKey} · rev ${state.rev} · ${connection} · ${queue.length} unsent`;
      noticeNode.textContent = notice;
      renderLegend(legend, authorRuns);
    }
    emit(authorRuns);
  }

  function repaint() {
    if (destroyed) return;
    layer.replaceChildren();
    const authorRuns = decorations(state);
    for (const run of authorRuns) {
      const span = document.createElement("span");
      span.dataset.author = run.author;
      if (run.mark) span.dataset.mark = run.mark;
      if (run.face) span.dataset.face = run.face;
      const style = run.face ? FACE_STYLES[run.face] : null;
      span.style.color = run.mark ? MARK_COLOURS[run.mark]
        : style ? style.color : authorColour(run.author);
      if (style?.fontWeight) span.style.fontWeight = style.fontWeight;
      if (style?.textDecoration) span.style.textDecoration = style.textDecoration;
      span.textContent = run.text;
      layer.append(span);
    }
    if (textarea.value.endsWith("\n") || textarea.value === "") {
      const finalLine = document.createElement("span");
      finalLine.className = "final-line";
      finalLine.textContent = "\u200b";
      layer.append(finalLine);
    }
    measure();
    syncScroll();
    renderStatus(authorRuns);
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
    stream = new EventSource(endpointFor(`doc/${encodeURIComponent(documentKey)}/events?after=${state.rev}`));
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
    return new Promise((resolve) => later(resolve, milliseconds));
  }

  async function drainQueue() {
    if (destroyed || posting || queue.length === 0) return;
    posting = true;
    const item = queue[0];
    const thisGeneration = generation;
    postController = new AbortController();
    try {
      const response = await fetch(endpointFor(`doc/${encodeURIComponent(documentKey)}/ops`), {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(item.prepared),
        signal: postController.signal,
      });
      if (thisGeneration !== generation || destroyed) return;
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
      if (thisGeneration !== generation || destroyed || error.name === "AbortError") return;
      connection = "reconnecting";
      notice = `Posting failed; retrying in ${item.retry / 1000}s`;
      renderStatus();
      await sleep(item.retry);
      item.retry = Math.min(item.retry * 2, 10000);
    } finally {
      if (thisGeneration === generation && !destroyed) {
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
    if (reportDropped && dropped > 0) {
      notice = `${dropped} unsent edit${dropped === 1 ? " was" : "s were"} dropped during resync.`;
    }
    renderStatus();
    try {
      const response = await fetch(endpointFor(`doc/${encodeURIComponent(documentKey)}/state`));
      if (!response.ok) throw new Error(`state returned ${response.status}`);
      const snapshot = await response.json();
      if (destroyed) return;
      state = load(state, snapshot);
      textarea.value = text(state.doc);
      previousValue = textarea.value;
      const end = textarea.value.length;
      textarea.setSelectionRange(end, end);
      repaint();
      openStream();
    } catch (error) {
      if (destroyed) return;
      connection = "reconnecting";
      notice = `Resync failed: ${error.message}`;
      renderStatus();
      later(() => void resync(false), 1000);
    }
  }

  function onBeforeUnload(event) {
    if (queue.length > 0) {
      event.preventDefault();
      event.returnValue = "";
    }
  }
  function onOffline() {
    stream?.close();
    connection = "reconnecting";
    renderStatus();
  }
  function onOnline() {
    if (!destroyed) openStream();
  }
  function onInput(event) {
    if (!composing && !event.isComposing) processLocalInput();
  }
  function onCompositionStart() { composing = true; }
  function onCompositionEnd() {
    composing = false;
    processLocalInput();
    const waiting = bufferedEntries;
    bufferedEntries = [];
    for (const entry of waiting) {
      if (!applyEntry(entry)) break;
    }
  }

  textarea.addEventListener("scroll", syncScroll);
  textarea.addEventListener("input", onInput);
  textarea.addEventListener("compositionstart", onCompositionStart);
  textarea.addEventListener("compositionend", onCompositionEnd);
  window.addEventListener("beforeunload", onBeforeUnload);
  window.addEventListener("offline", onOffline);
  window.addEventListener("online", onOnline);
  repaint();
  void resync(false);

  return {
    destroy() {
      if (destroyed) return;
      destroyed = true;
      generation += 1;
      postController?.abort();
      stream?.close();
      timers.forEach(clearTimeout);
      timers.clear();
      window.removeEventListener("beforeunload", onBeforeUnload);
      window.removeEventListener("offline", onOffline);
      window.removeEventListener("online", onOnline);
      textarea.removeEventListener("scroll", syncScroll);
      textarea.removeEventListener("input", onInput);
      textarea.removeEventListener("compositionstart", onCompositionStart);
      textarea.removeEventListener("compositionend", onCompositionEnd);
    },
  };
}
