import { escapeHtml } from "../src/marks.js";
import type { TurnSummary, TurnView } from "../src/types.js";
import { turnDetail } from "../widget/model.js";
import { addressedBody, intentStage, intents, pageBounds, shouldAutoScroll } from "./model.js";

const HS = "https://matrix.paragogy.net";
const ROOM = "!_qvu9Pec8-hw1-nsN18SA8uIChKlJPmS4f4ji3zajRw";
const ELEMENT = `https://app.element.io/#/room/${ROOM}?via=matrix.paragogy.net`;
type Event = { event_id: string; sender: string; origin_server_ts: number; type: string; content: { body?: string; msgtype?: string } };

const $ = <T extends HTMLElement>(id: string) => document.getElementById(id) as T;
const login = $("login"), app = $("app"), messages = $("messages"), inspector = $("inspector");
const status = $("login-status"), identity = $("identity");
const pageSizeSelect = $<HTMLSelectElement>("page-size"), loadSizeSelect = $<HTMLSelectElement>("load-size");
const older = $<HTMLButtonElement>("older"), newer = $<HTMLButtonElement>("newer"), range = $("range");
($("element-link") as HTMLAnchorElement).href = ELEMENT;
let token = sessionStorage.getItem("futon.matrix.token") ?? "";
let userId = sessionStorage.getItem("futon.matrix.user") ?? "";
let views = new Map<string, TurnView>();
let events: Event[] = [];
let pageSize = 3, loadSize = 30, pageOffset = 0;

async function matrix(path: string, init: RequestInit = {}): Promise<Response> {
  const headers = new Headers(init.headers);
  if (token) headers.set("Authorization", `Bearer ${token}`);
  headers.set("Content-Type", "application/json");
  return fetch(`${HS}/_matrix/client/v3${path}`, { ...init, headers });
}

async function signIn(username: string, password: string): Promise<void> {
  const response = await matrix("/login", { method: "POST", body: JSON.stringify({
    type: "m.login.password", identifier: { type: "m.id.user", user: username }, password,
    initial_device_display_name: "zone FUTON chat",
  }) });
  const body = await response.json();
  if (!response.ok) throw new Error(body.error ?? `http ${response.status}`);
  token = body.access_token; userId = body.user_id;
  sessionStorage.setItem("futon.matrix.token", token);
  sessionStorage.setItem("futon.matrix.user", userId);
}

async function checkMembership(): Promise<void> {
  const response = await matrix("/joined_rooms");
  const body = await response.json();
  if (!response.ok || !body.joined_rooms?.includes(ROOM)) throw new Error("This account is not a member of the private room.");
}

async function loadEvents(): Promise<void> {
  const previousCount = events.length;
  const response = await matrix(`/rooms/${encodeURIComponent(ROOM)}/messages?dir=b&limit=${loadSize}`);
  if (!response.ok) throw new Error(`Matrix history: http ${response.status}`);
  const body = await response.json();
  events = (body.chunk as Event[]).filter((e) => e.type === "m.room.message" && typeof e.content?.body === "string").reverse();
  if (pageOffset > 0 && events.length > previousCount) pageOffset += events.length - previousCount;
}

async function xiang(path: string): Promise<Response> {
  return fetch(`/chat-api${path}`, { headers: { Authorization: `Bearer ${token}`, Accept: "application/json" } });
}

function visibleEvents(): Event[] {
  const bounds = pageBounds(events.length, pageSize, pageOffset);
  pageOffset = bounds.offset;
  return events.slice(bounds.start, bounds.end);
}

async function loadViews(): Promise<void> {
  const response = await xiang("/api/xiang/turns?limit=300");
  if (!response.ok) return;
  const body = await response.json() as { turns: TurnSummary[] };
  const ids = new Set(visibleEvents().map((e) => e.event_id));
  const relevant = body.turns.filter((t) => t["evidence-id"] && ids.has(t["evidence-id"]!) && !views.has(t["evidence-id"]!));
  await Promise.all(relevant.map(async (summary) => {
    const detailResponse = await xiang(`/api/xiang/turns/${encodeURIComponent(summary.id)}`);
    if (detailResponse.ok) views.set(summary["evidence-id"]!, await detailResponse.json());
  }));
}

function decorateRNodes(root: HTMLElement): void {
  const walker = document.createTreeWalker(root, NodeFilter.SHOW_TEXT);
  const nodes: Text[] = [];
  while (walker.nextNode()) nodes.push(walker.currentNode as Text);
  for (const node of nodes) {
    const parts = node.data.split(/(\bR\d+(?:[.-]\d+)*\b)/g);
    if (parts.length === 1) continue;
    const fragment = document.createDocumentFragment();
    for (const part of parts) {
      const child = /^R\d/.test(part) ? document.createElement("span") : document.createTextNode(part);
      if (child instanceof HTMLElement) { child.className = "r-node"; child.textContent = part; }
      fragment.append(child);
    }
    node.replaceWith(fragment);
  }
}

function annotatedBody(event: Event, view: TurnView | undefined): string {
  const raw = event.content.body!;
  if (!view) return escapeHtml(raw).replace(/\n/g, "<br>");
  const detail = turnDetail(view), source = view.record.source_text, index = raw.indexOf(source);
  if (index < 0) return detail.html;
  return escapeHtml(raw.slice(0, index)) + detail.html + escapeHtml(raw.slice(index + source.length)).replace(/\n/g, "<br>");
}

function sidenotes(view: TurnView | undefined): string {
  if (!view) return "";
  return turnDetail(view).fragments.map((f) => `<aside class="turn-note stage-${intentStage(f.intent)}"><b>${escapeHtml(f.intent)}</b>${f.target ? ` → ${escapeHtml(f.target)}` : ""}<br>${escapeHtml(f.rationale)}${f.patterns.length ? `<small>${f.patterns.map(escapeHtml).join(" · ")}</small>` : ""}</aside>`).join("");
}

function render(): void {
  const distanceFromBottom = messages.scrollHeight - messages.scrollTop - messages.clientHeight;
  const composing = document.activeElement === $<HTMLTextAreaElement>("message");
  const autoScroll = shouldAutoScroll(distanceFromBottom, composing);
  const visible = visibleEvents(), bounds = pageBounds(events.length, pageSize, pageOffset);
  range.textContent = events.length ? `${bounds.start + 1}–${bounds.end} of ${events.length} loaded` : "no turns";
  older.disabled = bounds.start === 0; newer.disabled = pageOffset === 0;
  older.textContent = `← back ${pageSize}`; newer.textContent = `newer ${pageSize} →`;
  messages.innerHTML = visible.map((event) => {
    const view = views.get(event.event_id);
    const badges = view ? intents(view).map((m) => `<button class="mark ${m.declared ? "declared" : "inferred"} stage-${intentStage(m.intent)}" data-event="${escapeHtml(event.event_id)}" title="${m.declared ? "authored declaration" : "象 interpretation"}">${escapeHtml(m.glyph)} ${escapeHtml(m.intent)}</button>`).join("") : "";
    return `<li class="${event.sender === userId ? "mine" : "theirs"}"><article><div class="meta"><b>${escapeHtml(event.sender)}</b><time>${new Date(event.origin_server_ts).toLocaleTimeString()}</time></div><div class="body">${annotatedBody(event, view)}</div><div class="badges">${badges}</div></article><div class="notes">${sidenotes(view)}</div></li>`;
  }).join("");
  messages.querySelectorAll<HTMLElement>(".body").forEach(decorateRNodes);
  messages.querySelectorAll<HTMLButtonElement>("button.mark").forEach((button) => button.onclick = () => showReading(button.dataset.event!));
  if (autoScroll) messages.scrollTop = messages.scrollHeight;
}

function showReading(eventId: string): void {
  const view = views.get(eventId); if (!view) return;
  const d = turnDetail(view); inspector.hidden = false;
  inspector.innerHTML = `<button id="close-reading" aria-label="Close">×</button><h2>象 reading</h2><p>${intents(view).map((m) => `<span class="mark ${m.declared ? "declared" : "inferred"} stage-${intentStage(m.intent)}">${escapeHtml(m.glyph)} ${escapeHtml(m.intent)}</span>`).join(" ")}</p><div class="source">${d.html}</div>${d.fragments.map((f) => `<section><h3>${escapeHtml(f.intent)} ${f.target ? `→ ${escapeHtml(f.target)}` : ""}</h3><p>${escapeHtml(f.rationale)}</p>${f.patterns.length ? `<p class="small">${f.patterns.map(escapeHtml).join(" · ")}</p>` : ""}</section>`).join("")}<p class="small">${d.labeller ? `read by ${escapeHtml(d.labeller)}` : "declared by author"} · ${escapeHtml(d.status)}</p>`;
  $("close-reading").onclick = () => { inspector.hidden = true; };
}

async function changePage(offset: number): Promise<void> { pageOffset = offset; await loadViews(); render(); }
async function refresh(): Promise<void> { await loadEvents(); await loadViews(); render(); }
older.onclick = () => void changePage(pageOffset + pageSize);
newer.onclick = () => void changePage(Math.max(0, pageOffset - pageSize));
pageSizeSelect.onchange = () => { pageSize = Number(pageSizeSelect.value); pageOffset = 0; void changePage(0); };
loadSizeSelect.onchange = () => { loadSize = Number(loadSizeSelect.value); pageOffset = 0; void refresh(); };

async function enter(): Promise<void> {
  await checkMembership(); login.hidden = true; app.hidden = false;
  identity.innerHTML = `${escapeHtml(userId)} <button id="logout">sign out</button>`;
  $("logout").onclick = () => { void matrix("/logout", { method: "POST", body: "{}" }).finally(() => { sessionStorage.clear(); location.reload(); }); };
  await refresh(); setInterval(() => void refresh(), 5000);
}

$("login-form").addEventListener("submit", (event) => { event.preventDefault(); status.textContent = "signing in…"; void signIn(($<HTMLInputElement>("username")).value, ($<HTMLInputElement>("password")).value).then(enter).catch((e: Error) => status.textContent = e.message); });
$("composer").addEventListener("submit", (event) => { event.preventDefault(); const textarea = $<HTMLTextAreaElement>("message"); const body = addressedBody($<HTMLSelectElement>("agent").value, textarea.value); if (!body) return; textarea.value = ""; const txn = crypto.randomUUID(); void matrix(`/rooms/${encodeURIComponent(ROOM)}/send/m.room.message/${txn}`, { method: "PUT", body: JSON.stringify({ msgtype: "m.text", body }) }).then(refresh); });
if (token) void enter().catch(() => { sessionStorage.clear(); token = ""; login.hidden = false; });
