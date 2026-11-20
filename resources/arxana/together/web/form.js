import { NAME_RE, SURFACE_RE, createEditor, endpoint, renderLegend } from "./editor.js";

const params = new URLSearchParams(location.search);
const workspace = params.get("ws");
const surface = params.get("surface") ?? "web";
const app = document.querySelector("#app");
const editors = [];
const editorStates = new Map();

function message(text) {
  app.replaceChildren();
  const node = document.createElement("p");
  node.id = "message";
  node.textContent = text;
  app.append(node);
}

function orderedQuestions(nodes) {
  return Object.entries(nodes).filter(([, node]) => node?.kind === "question")
    .sort(([leftId, left], [rightId, right]) => {
      const a = Number.isInteger(left.attrs?.order) ? left.attrs.order : Number.MAX_SAFE_INTEGER;
      const b = Number.isInteger(right.attrs?.order) ? right.attrs.order : Number.MAX_SAFE_INTEGER;
      return a - b || leftId.localeCompare(rightId);
    });
}

function renderSummary(identity, count, status, legend) {
  const summaries = [...editorStates.values()];
  const live = summaries.filter(({ connection }) => connection === "live").length;
  const unsent = summaries.reduce((sum, entry) => sum + entry.unsent, 0);
  status.textContent = `${identity} · ${count} answers · ${live} live · ${unsent} unsent`;
  renderLegend(legend, summaries.flatMap(({ runs }) => runs));
}

async function start() {
  if (!workspace || !NAME_RE.test(workspace)) return message("Missing or invalid form workspace.");
  if (!SURFACE_RE.test(surface)) return message("Invalid surface name.");
  try {
    const whoResponse = await fetch(endpoint("whoami"));
    if (!whoResponse.ok) throw new Error(`identity returned ${whoResponse.status}`);
    const { identity } = await whoResponse.json();
    const stateResponse = await fetch(endpoint(`structure/${encodeURIComponent(workspace)}/state`));
    if (!stateResponse.ok) throw new Error(`form returned ${stateResponse.status}`);
    const data = await stateResponse.json();
    const nodes = data.nodes && typeof data.nodes === "object" ? data.nodes : {};
    const formNode = Object.values(nodes).find((node) => node?.kind === "form");
    const questions = orderedQuestions(nodes);
    app.replaceChildren();
    const header = document.createElement("header");
    header.className = "form-header";
    const title = document.createElement("h1");
    title.id = "form-title";
    title.textContent = formNode?.attrs?.title || workspace;
    const layer = document.createElement("p");
    layer.id = "form-layer";
    layer.textContent = formNode?.attrs?.layer ? `Layer: ${formNode.attrs.layer}` : workspace;
    const status = document.createElement("p");
    status.id = "form-status";
    status.className = "status-main";
    const legend = document.createElement("span");
    legend.id = "form-legend";
    legend.className = "legend";
    header.append(title, layer, status, legend);
    const answers = document.createElement("section");
    answers.id = "form-questions";
    answers.className = "form-questions";
    app.append(header, answers);
    if (!questions.length) {
      const empty = document.createElement("p");
      empty.id = "empty-form";
      empty.textContent = "This form has not been shared from Emacs yet.";
      answers.append(empty);
      return renderSummary(identity, 0, status, legend);
    }
    for (const [id, node] of questions) {
      const article = document.createElement("article");
      article.className = "form-question";
      article.dataset.nodeId = id;
      const heading = document.createElement("h2");
      heading.textContent = node.attrs?.question || id;
      article.append(heading);
      const documentKey = node.attrs?.doc;
      if (typeof documentKey !== "string" || !NAME_RE.test(documentKey)) {
        const error = document.createElement("p");
        error.className = "editor-error";
        error.textContent = "This question has no valid shared document.";
        article.append(error);
      } else {
        const container = document.createElement("div");
        container.className = "answer-editor";
        container.addEventListener("together-editor-state", (event) => {
          editorStates.set(id, event.detail);
          renderSummary(identity, questions.length, status, legend);
        });
        article.append(container);
        editors.push(createEditor({ container, documentKey, identity, surface, endpoint, grow: true }));
      }
      answers.append(article);
    }
    renderSummary(identity, questions.length, status, legend);
  } catch (error) {
    message(`Unable to open form: ${error.message}`);
  }
}

window.addEventListener("pagehide", () => editors.forEach((editor) => editor.destroy()), { once: true });
void start();
