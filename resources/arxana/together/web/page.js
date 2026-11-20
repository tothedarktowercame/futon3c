import { NAME_RE, SURFACE_RE, createEditor, endpoint } from "./editor.js";
import { NOTE_COLOURS, allNoteExtents, currentNodes, decodeNotes, notesAtIndex,
  findWords, textAtCaret, underlineRuns } from "./notes.js";

const params = new URLSearchParams(location.search);
const documentKey = params.get("doc");
const surface = params.get("surface") ?? "web";
const app = document.querySelector("#app");

function addText(parent, tag, value, className) {
  if (!value) return;
  const node = document.createElement(tag);
  if (className) node.className = className;
  node.textContent = value;
  parent.append(node);
}

function notesController(documentKey, workspace, layout, editorHost) {
  let notes = [];
  let doc = { characters: [] };
  let value = "";
  let extents = { resolved: [], unresolved: [] };
  let failure = "";
  let pane = null;
  let frame = null;
  let repaint = () => {};
  let strip = null; let sourcePane = null; let mobileNav = null;
  let position = 0; let frozen = null; let current = [];
  let request = 0; let sourceAbort = null;
  const narrow = matchMedia("(max-width: 799px)");
  function button(parent, label, title, action, disabled = false) {
    const b = document.createElement("button"); b.type = "button";
    b.textContent = label; b.title = title; b.disabled = disabled;
    b.addEventListener("click", action); parent.append(b); return b;
  }
  function geometry() {
    if (!strip) return;
    const width = layout.clientWidth; const gap = 12;
    const noteWidth = narrow.matches ? width : Math.max(256, (width - gap) / 3);
    const mainWidth = narrow.matches ? width : width - gap - noteWidth;
    strip.style.width = `${2 * mainWidth + noteWidth + 2 * gap}px`;
    strip.style.gridTemplateColumns = `${mainWidth}px ${noteWidth}px ${mainWidth}px`;
    strip.style.transform = `translateX(-${position * (mainWidth + gap)}px)`;
    // Focus moving into a pane can scroll a clipped box sideways; the strip's
    // place is set by the transform alone.
    layout.scrollLeft = 0;
    layout.dataset.position = String(position);
    editorHost.inert = position !== 0;
    sourcePane.inert = position !== (narrow.matches ? 2 : 1);
    pane.inert = narrow.matches && position !== 1;
  }
  const resize = new ResizeObserver(geometry);
  function move(next) {
    if (!strip) return;
    next = Math.max(0, Math.min(narrow.matches ? 2 : 1, next));
    if (position === 0 && next > 0) frozen = current;
    position = next;
    if (position === 0) frozen = null;
    geometry(); render();
  }
  async function showSource(note = current.find((n) => n.source)) {
    if (!note?.source) return;
    move(narrow.matches ? 2 : 1);
    const ticket = ++request;
    sourceAbort?.abort(); sourceAbort = new AbortController();
    sourcePane.replaceChildren(); sourcePane.scrollTop = 0;
    addText(sourcePane, "h2", note.source.doc ?? "Source");
    addText(sourcePane, "p", note.where, "muted");
    if (note.source.url) {
      addText(sourcePane, "blockquote", note.source.words);
      const link = document.createElement("a"); link.textContent = note.source.url;
      link.href = note.source.url; link.rel = "noopener noreferrer"; link.target = "_blank";
      sourcePane.append(link); return;
    }
    const loading = document.createElement("p"); loading.textContent = "Reading the source…"; sourcePane.append(loading);
    try {
      const response = await fetch(endpoint(`doc/${encodeURIComponent(note.source.doc)}/state`),
        { cache: "no-store", signal: sourceAbort.signal });
      if (!response.ok) throw new Error("unreadable");
      const state = await response.json();
      if (!Array.isArray(state.runs) || (state.rev === 0 && !state.runs.length)
          || state.runs.some((run) => typeof run.text !== "string" || typeof run.deleted !== "boolean")) throw new Error("missing");
      if (ticket !== request) return;
      loading.remove();
      const text = state.runs.filter((run) => !run.deleted).map((run) => run.text).join("");
      const found = findWords(text, note.source.words);
      if (!found) addText(sourcePane, "p", "Quoted words not found in the source.", "source-message");
      const pre = document.createElement("pre"); pre.className = "source-text";
      if (found) {
        const points = Array.from(text);
        const before = document.createElement("span"); before.textContent = points.slice(0, found.from).join("");
        const mark = document.createElement("mark"); mark.textContent = points.slice(found.from, found.to).join("");
        mark.style.backgroundColor = `${NOTE_COLOURS[note.colour]}33`;
        const after = document.createElement("span"); after.textContent = points.slice(found.to).join("");
        pre.append(before, mark, after); sourcePane.append(pre);
        requestAnimationFrame(() => {
          if (ticket !== request) return;
          sourcePane.scrollTop += mark.getBoundingClientRect().top - sourcePane.getBoundingClientRect().top
            - sourcePane.clientHeight / 2 + mark.getBoundingClientRect().height / 2;
        });
      } else { pre.textContent = text; sourcePane.append(pre); sourcePane.scrollTop = 0; }
    } catch (error) {
      if (ticket !== request || error.name === "AbortError") return;
      loading.textContent = "Source document missing or unreadable.";
    }
  }
  function keys(event) {
    if (!strip || event.target.closest("textarea, input, [contenteditable='true']")) return;
    if (event.key === ">" && current.some((n) => n.source)) { event.preventDefault(); showSource(); }
    if (event.key === "<") { event.preventDefault(); move(position - 1); }
  }
  function screenChanged() {
    position = position === 0 ? 0 : (narrow.matches ? 2 : 1);
    geometry(); render();
  }
  document.addEventListener("keydown", keys);
  narrow.addEventListener("change", screenChanged);

  function ensurePane() {
    if (!notes.length) {
      if (strip) {
        ++request; sourceAbort?.abort(); resize.disconnect();
        layout.append(editorHost); strip.remove(); mobileNav.remove();
        strip = null; sourcePane = null; mobileNav = null; editorHost.inert = false;
      }
      pane = null; position = 0; frozen = null; layout.classList.remove("has-notes"); return;
    }
    if (!pane) {
      pane = document.createElement("aside");
      pane.id = "notes-pane";
      pane.className = "notes-pane";
      strip = document.createElement("div"); strip.className = "notes-strip";
      sourcePane = document.createElement("aside"); sourcePane.id = "source-pane";
      sourcePane.className = "source-pane"; sourcePane.setAttribute("aria-label", "Source context");
      strip.append(editorHost, pane, sourcePane); layout.append(strip);
      mobileNav = document.createElement("nav"); mobileNav.className = "lineup-mobile-nav";
      button(mobileNav, "‹ Document", "Back to the document", () => move(0));
      button(mobileNav, "Notes ›", "Show notes", () => move(1));
      layout.prepend(mobileNav); layout.classList.add("has-notes");
      resize.observe(layout); geometry();
    }
  }

  function render(caret = editorHost.querySelector("textarea")?.selectionStart ?? 0) {
    ensurePane();
    if (!pane) return;
    pane.replaceChildren();
    const controls = document.createElement("nav"); controls.className = "note-navigation";
    button(controls, "‹", "Back to the document", () => move(position - 1), position === 0);
    button(controls, "›", "Show the source in context", () => showSource(),
      !(frozen ?? current).some((n) => n.source));
    pane.append(controls);
    // Until the document's text has arrived nothing can resolve; saying
    // "no longer resolve" about every note then would be false.
    if (!doc.characters.length) { addText(pane, "p", "Reading the document…", "muted"); return; }
    const at = textAtCaret(doc, editorHost.querySelector("textarea")?.value ?? value, caret);
    addText(pane, "p", at.author
      ? `This text: written by ${at.author}${at.label ? ` (${at.label})` : ""}`
      : "This text: no character at the caret", "notes-this-text");
    const active = frozen ?? notesAtIndex(extents.resolved, at.index);
    current = active;
    controls.lastElementChild.disabled = !active.some((n) => n.source);
    if (active.length) {
      for (const note of active) {
        const card = document.createElement("article");
        card.className = "note-card";
        card.dataset.noteId = note.id;
        card.style.borderColor = NOTE_COLOURS[note.colour];
        addText(card, "strong", note.title);
        addText(card, "p", note.text);
        addText(card, "strong", note.quote_title);
        addText(card, "blockquote", note.quote ? `“${note.quote}”` : "");
        addText(card, "p", note.where, "muted");
        addText(card, "p", `note by ${note.by || "unknown"}`, "muted");
        if (note.extent.deleted) addText(card, "p",
          `${note.extent.deleted} of ${note.extent.live + note.extent.deleted} characters deleted`, "muted");
        if (note.source) button(card, "Source ›", "Show the source in context", () => showSource(note));
        pane.append(card);
      }
    } else {
      addText(pane, "h2", "Notes");
      const counts = new Map();
      for (const note of notes) counts.set(note.colour, (counts.get(note.colour) ?? 0) + 1);
      for (const [colour, count] of [...counts].sort()) {
        const row = document.createElement("p");
        row.className = "note-colour-count";
        const swatch = document.createElement("span");
        swatch.className = "note-swatch";
        swatch.style.backgroundColor = NOTE_COLOURS[colour];
        addText(row, "span", `${colour}: ${count} note${count === 1 ? "" : "s"}`);
        row.prepend(swatch);
        pane.append(row);
      }
    }
    if (extents.unresolved.length) {
      addText(pane, "h2", "Notes that no longer resolve");
      for (const note of extents.unresolved) addText(pane, "p", note.id, "unresolved-note");
    }
    addText(pane, "p", failure, "muted notes-error");
  }

  function schedule(caret) {
    if (frame !== null) return;
    frame = requestAnimationFrame(() => { frame = null; render(caret); });
  }

  async function refresh() {
    try {
      const response = await fetch(endpoint(`structure/${encodeURIComponent(workspace)}/state`), { cache: "no-store" });
      if (!response.ok) throw new Error(`notes returned ${response.status}`);
      const state = await response.json();
      notes = decodeNotes(currentNodes(state), documentKey);
      extents = allNoteExtents(doc, notes);
      failure = "";
      ensurePane();
      repaint();
      schedule();
    } catch (error) {
      failure = `Notes could not be refreshed: ${error.message}`;
      schedule();
    }
  }

  return {
    decorate(nextDoc, runs) { doc = nextDoc; extents = allNoteExtents(doc, notes); return underlineRuns(runs, extents.resolved); },
    update(detail) { doc = detail.doc; value = detail.value; extents = allNoteExtents(doc, notes); schedule(detail.selectionStart); },
    schedule, refresh,
    setRepaint(callback) { repaint = callback; },
    destroy() {
      if (frame !== null) cancelAnimationFrame(frame);
      ++request; sourceAbort?.abort(); resize.disconnect();
      document.removeEventListener("keydown", keys); narrow.removeEventListener("change", screenChanged);
    },
  };
}

function message(text) {
  app.replaceChildren();
  const node = document.createElement("p");
  node.id = "message";
  node.textContent = text;
  app.append(node);
}

function documentURL(key) {
  const url = new URL(location.href);
  url.searchParams.set("doc", key);
  return url.href;
}

function formURL(workspace) {
  const url = new URL("./form.html", location.href);
  url.searchParams.set("ws", workspace);
  url.searchParams.set("surface", surface);
  return url.href;
}

async function showDocuments(identity) {
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
  const button = document.createElement("button");
  button.type = "submit";
  button.textContent = "Open";
  const validation = document.createElement("p");
  validation.id = "key-validation";
  validation.setAttribute("role", "alert");
  label.append(input);
  form.append(label, button, validation);
  form.addEventListener("submit", (event) => {
    event.preventDefault();
    if (!NAME_RE.test(input.value)) {
      validation.textContent = "Invalid document key: use 1–120 letters, digits, dots, underscores or hyphens.";
    } else location.assign(documentURL(input.value));
  });
  const formsHeading = document.createElement("h2");
  formsHeading.textContent = "Shared forms";
  const formList = document.createElement("ul");
  formList.id = "form-list";
  const docsHeading = document.createElement("h2");
  docsHeading.textContent = "Documents";
  const status = document.createElement("p");
  status.id = "list-status";
  status.textContent = "Reading documents…";
  const list = document.createElement("ul");
  list.id = "document-list";
  app.append(heading, who, form, formsHeading, formList, docsHeading, status, list);
  try {
    const response = await fetch(endpoint("docs"));
    if (!response.ok) throw new Error(`documents returned ${response.status}`);
    const { docs } = await response.json();
    if (!Array.isArray(docs)) throw new Error("invalid document list");
    const workspaces = [...new Set(docs.map(({ key }) => {
      const match = /^incubator\.([^.]+)\.(.+)$/.exec(key);
      return match ? `incubator.${match[2]}` : null;
    }).filter(Boolean))].sort();
    for (const workspace of workspaces) {
      const row = document.createElement("li");
      const link = document.createElement("a");
      link.textContent = workspace;
      link.href = formURL(workspace);
      row.append(link);
      formList.append(row);
    }
    if (!workspaces.length) {
      const row = document.createElement("li");
      row.textContent = "No shared forms yet.";
      formList.append(row);
    }
    for (const doc of docs) {
      const row = document.createElement("li");
      const link = document.createElement("a");
      link.textContent = doc.key;
      link.href = documentURL(doc.key);
      const detail = document.createElement("span");
      detail.textContent = doc.error ? ` — unavailable: ${doc.error}`
        : ` — rev ${doc.rev} · ${doc.chars} chars · ${doc.authors.join(", ") || "no authors"} · ${doc.at ?? "no changes yet"}`;
      row.append(link, detail);
      list.append(row);
    }
    status.textContent = docs.length ? "" : "No shared documents yet.";
  } catch (error) {
    status.textContent = `Unable to list documents: ${error.message}`;
  }
}

async function start() {
  if (!SURFACE_RE.test(surface)) return message("Invalid surface name.");
  try {
    const response = await fetch(endpoint("whoami"));
    if (!response.ok) throw new Error(`identity returned ${response.status}`);
    const { identity } = await response.json();
    if (!identity) throw new Error("identity is missing");
    if (documentKey === null) await showDocuments(identity);
    else if (!NAME_RE.test(documentKey)) message("Invalid document key.");
    else {
      const workspace = params.get("ws") ?? `${documentKey}.notes`;
      if (!NAME_RE.test(workspace)) return message("Invalid notes workspace.");
      app.replaceChildren();
      const layout = document.createElement("div");
      layout.className = "document-layout";
      const editorHost = document.createElement("div");
      editorHost.className = "document-editor";
      layout.append(editorHost);
      app.append(layout);
      const notes = notesController(documentKey, workspace, layout, editorHost);
      const editor = createEditor({ container: editorHost, documentKey, identity, surface,
        endpoint, grow: false, decorateRuns: notes.decorate });
      notes.setRepaint(editor.repaint);
      editorHost.addEventListener("together-editor-state", (event) => notes.update(event.detail));
      const selectionChanged = () => notes.schedule(editorHost.querySelector("textarea")?.selectionStart ?? 0);
      editorHost.addEventListener("click", selectionChanged);
      editorHost.addEventListener("keyup", selectionChanged);
      window.addEventListener("focus", notes.refresh);
      const interval = setInterval(notes.refresh, 15000);
      window.addEventListener("pagehide", () => {
        clearInterval(interval); notes.destroy(); editor.destroy();
        window.removeEventListener("focus", notes.refresh);
      }, { once: true });
      await notes.refresh();
    }
  } catch (error) {
    message(`Unable to start: ${error.message}`);
  }
}

void start();
