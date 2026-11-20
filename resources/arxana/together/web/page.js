import { NAME_RE, SURFACE_RE, createEditor, endpoint } from "./editor.js";

const params = new URLSearchParams(location.search);
const documentKey = params.get("doc");
const surface = params.get("surface") ?? "web";
const app = document.querySelector("#app");

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
    else createEditor({ container: app, documentKey, identity, surface, endpoint, grow: false });
  } catch (error) {
    message(`Unable to start: ${error.message}`);
  }
}

void start();
