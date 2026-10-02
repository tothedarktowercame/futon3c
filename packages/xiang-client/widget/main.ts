/**
 * The 象 widget: a page that shows a session's turns with their marks, one
 * turn's reading in detail, and a side pane with the agent's obligations and
 * the 象 seats' health. It runs as static files behind Caddy (see
 * deploy/Caddyfile) or inside Element as a widget; it only ever talks to
 * futon3c over fetch, same origin.
 */

import { XiangClient } from "../src/client.js";
import { escapeHtml } from "../src/marks.js";
import type { TurnSummary } from "../src/types.js";
import { agreementLine, changedTurns, configFromUrl, healthPane, obligationsPane, turnDetail, turnRows, type TurnDetail } from "./model.js";

const config = configFromUrl(window.location.href);
const api = new XiangClient({ base: config.base });

const el = {
  turns: document.getElementById("turns")!,
  detail: document.getElementById("detail")!,
  obligations: document.getElementById("obligations")!,
  health: document.getElementById("health")!,
  title: document.getElementById("title")!,
  status: document.getElementById("status")!,
};

let turns: TurnSummary[] = [];
let selected: string | null = null;

function setStatus(text: string): void {
  el.status.textContent = text;
}

async function refreshTurns(): Promise<void> {
  const r = await api.listTurns({ agent: config.agent || undefined, session: config.session ?? undefined, limit: 200 });
  if (r.status !== 200 || !r.json) {
    setStatus(`turns: ${r.status ? `http ${r.status}` : r.error ?? "unreachable"}`);
    return;
  }
  const changed = changedTurns(turns, r.json.turns);
  turns = r.json.turns;
  renderTurns();
  if (selected && changed.includes(selected)) await showTurn(selected);
  if (!selected && turns.length > 0) await showTurn(turnRows(turns).at(-1)!.id);
  setStatus(`${turns.length} turns · ${new Date().toLocaleTimeString()}`);
}

function renderTurns(): void {
  const rows = turnRows(turns);
  el.turns.innerHTML = rows
    .map(
      (t) => `<li class="turn ${t.origin} ${t.id === selected ? "selected" : ""}" data-id="${t.id}">
        <span class="who">${escapeHtml(t.author)}</span>
        <span class="when">${escapeHtml(t.when.slice(11, 19))}</span>
        <span class="st st-${escapeHtml(t.status)}">${escapeHtml(t.status)}</span>
        <div class="preview">${escapeHtml(t.preview)}</div>
      </li>`,
    )
    .join("");
  el.turns.querySelectorAll<HTMLElement>("li.turn").forEach((li) => {
    li.addEventListener("click", () => void showTurn(li.dataset.id!));
  });
}

async function showTurn(id: string): Promise<void> {
  selected = id;
  renderTurns();
  const r = await api.getTurn(id);
  if (r.status !== 200 || !r.json) {
    el.detail.innerHTML = `<p class="err">turn ${escapeHtml(id)}: ${r.status ? `http ${r.status}` : "unreachable"}</p>`;
    return;
  }
  renderDetail(turnDetail(r.json));
}

function renderDetail(d: TurnDetail): void {
  const marks = d.marks.length
    ? `<ul class="marks">${d.marks.map((m) => `<li><span class="glyph">${escapeHtml(m.mark)}</span> <b>${escapeHtml(m.intent)}</b> <i>${escapeHtml(m.stage)}</i> ${escapeHtml(m.text.slice(0, 160))}</li>`).join("")}</ul>`
    : "";
  const fragments = d.fragments.length
    ? `<table class="fragments"><tr><th>s</th><th>intent</th><th>basis</th><th>target</th><th>rationale</th><th>patterns</th></tr>${d.fragments
        .map(
          (f) =>
            `<tr><td>${escapeHtml(f.sentence)}</td><td>${escapeHtml(f.intent)}</td><td class="basis-${escapeHtml(f.basis)}">${escapeHtml(f.basis)}</td><td>${escapeHtml(f.target ?? "∅")}</td><td>${escapeHtml(f.rationale)}</td><td>${f.patterns.map(escapeHtml).join("<br>")}</td></tr>`,
        )
        .join("")}</table>${
        d.agreement ? `<p class="muted">draft: ${d.agreement.agreed} agreed · ${d.agreement.relabelled} relabelled · ${d.agreement.resegmented} resegmented · ${d.agreement.new} new · ${d.agreement.dropped} dropped</p>` : ""
      }`
    : d.draft.length
      ? `<table class="fragments draft"><tr><th>小象</th><th>text</th><th>candidates</th></tr>${d.draft
          .map((f) => `<tr><td>${f.intent ? escapeHtml(f.intent) + (f.precision != null ? ` <span class="muted">p ${f.precision.toFixed(2)}</span>` : "") : `? ${f.guesses.map(escapeHtml).join(" / ")}`}</td><td>${escapeHtml(f.text.slice(0, 120))}</td><td>${f.candidates.map(escapeHtml).join("<br>")}</td></tr>`)
          .join("")}</table><p class="muted">${d.status === "drafted" ? "settled by the draft; 象 did not read it" : `draft only; 象's reading is ${escapeHtml(d.status)}`}</p>`
      : `<p class="muted">no reading yet (${escapeHtml(d.status)})</p>`;
  const notices = d.notices.map((n) => `<p class="notice">象: ${escapeHtml(n.text)}</p>`).join("");
  el.detail.innerHTML = `
    <header><span class="who ${d.origin}">${escapeHtml(d.author)}</span> <span class="when">${escapeHtml(d.when)}</span>
      ${d.labeller ? `<span class="labeller">read by ${escapeHtml(d.labeller)}</span>` : ""}</header>
    <div class="text">${d.html}</div>
    ${marks}${notices}${fragments}`;
}

async function refreshSide(): Promise<void> {
  if (config.agent) {
    const r = await fetch(`${config.base}/api/alpha/obligations?agent=${encodeURIComponent(config.agent)}`).then(
      async (resp) => ({ status: resp.status, json: await resp.json().catch(() => null) }),
      (e: Error) => ({ status: 0, json: { reason: e.message } }),
    );
    const pane = obligationsPane(r.status, r.json);
    const list = (title: string, rows: typeof pane.owes) =>
      `<h3>${title} <span class="count">${rows.length}</span></h3>` +
      (rows.length
        ? `<ul>${rows.map((o) => `<li><b>${escapeHtml(o.kind)}</b> ${escapeHtml(o.counterparty)}${o.due ? ` · due ${escapeHtml(o.due)}` : ""}<div class="muted">${escapeHtml(o.deliverable)}</div></li>`).join("")}</ul>`
        : `<p class="muted">none</p>`);
    el.obligations.innerHTML = pane.error
      ? `<p class="err">obligations: ${escapeHtml(pane.error)}</p>`
      : list("owes", pane.owes) + list("owed", pane.owed) + `<p class="muted">as of ${escapeHtml(pane.asOf ?? "?")} · ${pane.unchecked} unchecked · ${pane.incomplete} incomplete</p>`;
  }
  const a = await fetch(`${config.base}/api/alpha/xiang/agreement${config.agent ? `?agent=${encodeURIComponent(config.agent)}` : ""}`).then(
    async (resp) => (resp.ok ? await resp.json().catch(() => null) : null),
    () => null,
  );
  const h = await api.health();
  const pane = healthPane(h.json as Parameters<typeof healthPane>[0]);
  el.health.innerHTML = `<h3>象</h3><p>seat <b>${escapeHtml(pane.seat)}</b> · ${escapeHtml(pane.state)} · ${pane.outstanding} outstanding${
    pane.benched.length ? ` · benched: ${pane.benched.map(escapeHtml).join(", ")}` : ""
  }</p><p class="muted">${escapeHtml(pane.detail)}</p><p class="muted">${escapeHtml(agreementLine(a))}</p>`;
}

el.title.textContent = config.agent ? `象 · ${config.agent}${config.session ? ` · ${config.session.slice(0, 8)}` : ""}` : "象";
void refreshTurns();
void refreshSide();
setInterval(() => void refreshTurns(), config.every);
setInterval(() => void refreshSide(), config.every * 4);
