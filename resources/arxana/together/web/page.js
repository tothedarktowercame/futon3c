import { NAME_RE, SURFACE_RE, createEditor, endpoint } from "./editor.js";
import { NOTE_COLOURS, allNoteExtents, currentNodes, decodeNotes, notesAtIndex,
  textAtCaret, underlineRuns } from "./notes.js";

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

  function ensurePane() {
    if (!notes.length) {
      pane?.remove(); pane = null; layout.classList.remove("has-notes"); return;
    }
    if (!pane) {
      pane = document.createElement("aside");
      pane.id = "notes-pane";
      pane.className = "notes-pane";
      layout.append(pane);
      layout.classList.add("has-notes");
    }
  }

  function render(caret = editorHost.querySelector("textarea")?.selectionStart ?? 0) {
    ensurePane();
    if (!pane) return;
    pane.replaceChildren();
    // Until the document's text has arrived nothing can resolve; saying
    // "no longer resolve" about every note then would be false.
    if (!doc.characters.length) { addText(pane, "p", "Reading the document…", "muted"); return; }
    const at = textAtCaret(doc, editorHost.querySelector("textarea")?.value ?? value, caret);
    addText(pane, "p", at.author
      ? `This text: written by ${at.author}${at.label ? ` (${at.label})` : ""}`
      : "This text: no character at the caret", "notes-this-text");
    const active = notesAtIndex(extents.resolved, at.index);
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
    destroy() { if (frame !== null) cancelAnimationFrame(frame); },
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
