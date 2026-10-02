import { escapeHtml } from "../src/marks.js";
import type { TurnSummary, TurnView } from "../src/types.js";
import { turnDetail } from "../widget/model.js";
import { addressedBody, intents } from "./model.js";

const HS = "https://matrix.paragogy.net";
const ROOM = "!_qvu9Pec8-hw1-nsN18SA8uIChKlJPmS4f4ji3zajRw";
const ELEMENT = `https://app.element.io/#/room/${ROOM}?via=matrix.paragogy.net`;
type Event = { event_id: string; sender: string; origin_server_ts: number; type: string; content: { body?: string; msgtype?: string } };

const $ = <T extends HTMLElement>(id: string) => document.getElementById(id) as T;
const login = $("login"), app = $("app"), messages = $("messages"), inspector = $("inspector");
const status = $("login-status"), identity = $("identity");
($("element-link") as HTMLAnchorElement).href = ELEMENT;
let token = sessionStorage.getItem("futon.matrix.token") ?? "";
let userId = sessionStorage.getItem("futon.matrix.user") ?? "";
let views = new Map<string, TurnView>();
let events: Event[] = [];

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
  const path = `/rooms/${encodeURIComponent(ROOM)}/messages?dir=b&limit=100`;
  const response = await matrix(path);
  if (!response.ok) throw new Error(`Matrix history: http ${response.status}`);
  const body = await response.json();
  events = (body.chunk as Event[]).filter((e) => e.type === "m.room.message" && typeof e.content?.body === "string").reverse();
}

async function xiang(path: string): Promise<Response> {
  return fetch(`/chat-api${path}`, { headers: { Authorization: `Bearer ${token}`, Accept: "application/json" } });
}

async function loadViews(): Promise<void> {
  const response = await xiang("/api/xiang/turns?limit=300");
  if (!response.ok) return;
  const body = await response.json() as { turns: TurnSummary[] };
  const ids = new Set(events.map((e) => e.event_id));
  const relevant = body.turns.filter((t) => t["evidence-id"] && ids.has(t["evidence-id"]!));
  await Promise.all(relevant.map(async (summary) => {
    const detailResponse = await xiang(`/api/xiang/turns/${encodeURIComponent(summary.id)}`);
    if (detailResponse.ok) views.set(summary["evidence-id"]!, await detailResponse.json());
  }));
}

function render(): void {
  messages.innerHTML = events.map((event) => {
    const view = views.get(event.event_id);
    const badges = view ? intents(view).map((m) => `<button class="mark ${m.declared ? "declared" : "inferred"}" data-event="${escapeHtml(event.event_id)}" title="${m.declared ? "authored declaration" : "象 interpretation"}">${escapeHtml(m.glyph)} ${escapeHtml(m.intent)}</button>`).join("") : "";
    return `<li class="${event.sender === userId ? "mine" : "theirs"}"><div class="meta"><b>${escapeHtml(event.sender)}</b><time>${new Date(event.origin_server_ts).toLocaleTimeString()}</time></div><div class="body">${escapeHtml(event.content.body!).replace(/\n/g, "<br>")}</div><div class="badges">${badges}</div></li>`;
  }).join("");
  messages.querySelectorAll<HTMLButtonElement>("button.mark").forEach((button) => button.onclick = () => showReading(button.dataset.event!));
  messages.lastElementChild?.scrollIntoView({ block: "end" });
}

function showReading(eventId: string): void {
  const view = views.get(eventId); if (!view) return;
  const d = turnDetail(view);
  inspector.innerHTML = `<h2>象 reading</h2><p>${intents(view).map((m) => `<span class="mark ${m.declared ? "declared" : "inferred"}">${escapeHtml(m.glyph)} ${escapeHtml(m.intent)}</span>`).join(" ")}</p><div class="source">${d.html}</div>${d.fragments.map((f) => `<section><h3>${escapeHtml(f.intent)} ${f.target ? `→ ${escapeHtml(f.target)}` : ""}</h3><p>${escapeHtml(f.rationale)}</p>${f.patterns.length ? `<p class="small">${f.patterns.map(escapeHtml).join(" · ")}</p>` : ""}</section>`).join("")}<p class="small">${d.labeller ? `read by ${escapeHtml(d.labeller)}` : "declared by author"} · ${escapeHtml(d.status)}</p>`;
}

async function refresh(): Promise<void> { await loadEvents(); await loadViews(); render(); }

async function enter(): Promise<void> {
  await checkMembership(); login.hidden = true; app.hidden = false;
  identity.innerHTML = `${escapeHtml(userId)} <button id="logout">sign out</button>`;
  $("logout").onclick = () => { void matrix("/logout", { method: "POST", body: "{}" }).finally(() => { sessionStorage.clear(); location.reload(); }); };
  await refresh(); setInterval(() => void refresh(), 5000);
}

$("login-form").addEventListener("submit", (event) => { event.preventDefault(); status.textContent = "signing in…"; void signIn(($<HTMLInputElement>("username")).value, ($<HTMLInputElement>("password")).value).then(enter).catch((e: Error) => status.textContent = e.message); });
$("composer").addEventListener("submit", (event) => { event.preventDefault(); const textarea = $<HTMLTextAreaElement>("message"); const body = addressedBody($<HTMLSelectElement>("agent").value, textarea.value); if (!body) return; textarea.value = ""; const txn = crypto.randomUUID(); void matrix(`/rooms/${encodeURIComponent(ROOM)}/send/m.room.message/${txn}`, { method: "PUT", body: JSON.stringify({ msgtype: "m.text", body }) }).then(refresh); });

if (token) void enter().catch(() => { sessionStorage.clear(); token = ""; login.hidden = false; });
