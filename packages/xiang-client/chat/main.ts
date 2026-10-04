import { escapeHtml } from "../src/marks.js";
import type { TurnSummary, TurnView } from "../src/types.js";
import { turnDetail } from "../widget/model.js";
import { addressedBody, anchorsAuthorChart, annotationTheme, countByAuthor, highlightedIntent, intentStage, intents, needsViewRefresh, pageBounds, postsPerAuthorPython, selectedMatrixEvent, shouldAutoScroll } from "./model.js";

const HS = "https://matrix.paragogy.net";
const DEFAULT_ROOM = "!_qvu9Pec8-hw1-nsN18SA8uIChKlJPmS4f4ji3zajRw";
const params = new URLSearchParams(location.search);
const requestedRoom = params.get("room") ?? "";
const ROOM = /^![^\s/]+(?::[^\s/]+)?$/.test(requestedRoom) ? requestedRoom : DEFAULT_ROOM;
const annotationMode = params.get("mode") === "annotations";
const ELEMENT = `https://app.element.io/#/room/${ROOM}?via=matrix.paragogy.net`;
type Event = { event_id: string; sender: string; origin_server_ts: number; type: string; content: { body?: string; msgtype?: string } };

const $ = <T extends HTMLElement>(id: string) => document.getElementById(id) as T;
const login = $("login"), app = $("app"), messages = $("messages"), inspector = $("inspector");
const status = $("login-status"), identity = $("identity");
const pageSizeSelect = $<HTMLSelectElement>("page-size"), loadSizeSelect = $<HTMLSelectElement>("load-size");
const markStyleSelect = $<HTMLSelectElement>("mark-style");
const older = $<HTMLButtonElement>("older"), newer = $<HTMLButtonElement>("newer"), range = $("range");
($("element-link") as HTMLAnchorElement).href = ELEMENT;
let token = sessionStorage.getItem("futon.matrix.token") ?? "";
let userId = sessionStorage.getItem("futon.matrix.user") ?? "";
let views = new Map<string, TurnView>();
let events: Event[] = [];
let chartEvents: Event[] = [];
let selectedEventId = "";
let focusedIntent: string | null = null;
let pageSize = annotationMode ? 3 : 12, loadSize = 30, pageOffset = 0;
type MarkStyle = "css" | "text" | "png" | "gif";
let markStyle = (localStorage.getItem("futon.mark.style") as MarkStyle | null) ?? "css";
if (!["css", "text", "png", "gif"].includes(markStyle)) markStyle = "css";
markStyleSelect.value = markStyle;
pageSizeSelect.value = String(pageSize);
document.body.classList.toggle("annotations", annotationMode);
if (annotationMode) {
  document.querySelector("h1")!.innerHTML = "FUTON room <span>· annotations</span>";
  $("composer").hidden = true;
}
const rasterMarks = new Set(["approve", "ask-action", "clarify", "collect", "constrain", "continue", "defer", "delegate", "disagree", "explain", "explore", "extend", "prioritize", "propose", "qualify", "redirect", "report-problem", "report", "retract", "verify", "withdraw"]);

function markGlyph(intent: string, glyph: string): string {
  if (markStyle === "css") return `<span class="glyph-css" aria-hidden="true">${escapeHtml(glyph)}&#xfe0e;</span>`;
  if (markStyle === "text" || !rasterMarks.has(intent)) return `<span class="glyph-text">${escapeHtml(glyph)}</span>`;
  const format = markStyle === "gif" && !matchMedia("(prefers-reduced-motion: reduce)").matches ? "gif" : "png";
  return `<img class="glyph-image" src="marks/${format}/${encodeURIComponent(intent)}.${format}" alt="${escapeHtml(glyph)}">`;
}

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
  chartEvents = events;
  if (pageOffset > 0 && events.length > previousCount) pageOffset += events.length - previousCount;
}

async function loadCompleteChartHistory(): Promise<void> {
  const collected: Event[] = [];
  let from = "";
  while (collected.length < 2000) {
    const suffix = from ? `&from=${encodeURIComponent(from)}` : "";
    const response = await matrix(`/rooms/${encodeURIComponent(ROOM)}/messages?dir=b&limit=100${suffix}`);
    if (!response.ok) return;
    const body = await response.json();
    const chunk = (body.chunk as Event[]).filter((e) => e.type === "m.room.message" && typeof e.content?.body === "string");
    collected.push(...chunk);
    if (chunk.length < 100 || !body.end || body.end === from) break;
    from = body.end;
  }
  chartEvents = collected.reverse();
}

async function xiang(path: string): Promise<Response> {
  return fetch(`/chat-api${path}`, { headers: { Authorization: `Bearer ${token}`, Accept: "application/json", "X-Matrix-Room": ROOM } });
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
  const relevant = body.turns.filter((t) => {
    const evidenceId = t["evidence-id"];
    return evidenceId && ids.has(evidenceId) && needsViewRefresh(t, views.get(evidenceId));
  });
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

function wantsAuthorChart(event: Event): boolean {
  return anchorsAuthorChart(event.content.body!);
}

function authorChart(): string {
  const rows = countByAuthor(chartEvents), max = Math.max(1, ...rows.map((row) => row.count));
  const bars = rows.map((row) => `<div class="chart-row"><span>${escapeHtml(row.author)}</span><i style="width:${Math.round(100 * row.count / max)}%"></i><b>${row.count}</b></div>`).join("");
  const code = escapeHtml(postsPerAuthorPython(chartEvents));
  return `<aside class="turn-note code-cell"><h3>Posts per author</h3><div class="bar-chart" role="img" aria-label="Bar chart of posts per author">${bars}</div><details><summary>Python cell · Marimo-ready</summary><pre><code>${code}</code></pre></details><small>${chartEvents.length} Matrix message events across room history · exact sender IDs</small></aside>`;
}

function render(): void {
  const distanceFromBottom = messages.scrollHeight - messages.scrollTop - messages.clientHeight;
  const composing = document.activeElement === $<HTMLTextAreaElement>("message");
  const autoScroll = shouldAutoScroll(distanceFromBottom, composing);
  const visible = visibleEvents(), bounds = pageBounds(events.length, pageSize, pageOffset);
  range.textContent = events.length ? `${bounds.start + 1}–${bounds.end} of ${events.length} loaded` : "no turns";
  older.disabled = bounds.start === 0; newer.disabled = pageOffset === 0;
  older.textContent = `← back ${pageSize}`; newer.textContent = `newer ${pageSize} →`;
  if (annotationMode) {
    if (!visible.some((event) => event.event_id === selectedEventId)) {
      selectedEventId = [...visible].reverse().find((event) => views.has(event.event_id))?.event_id ?? visible.at(-1)?.event_id ?? "";
    }
    messages.innerHTML = visible.map((event) => {
      const view = views.get(event.event_id);
      const badges = view ? intents(view).map((m) => `<span class="mark ${m.declared ? "declared" : "inferred"} stage-${intentStage(m.intent)}">${markGlyph(m.intent, m.glyph)}<span>${escapeHtml(m.intent)}</span></span>`).join("") : `<span class="pending">analysis pending</span>`;
      return `<li class="annotation-turn ${event.event_id === selectedEventId ? "selected" : ""}"><button class="turn-picker" data-event="${escapeHtml(event.event_id)}"><span class="meta"><b>${escapeHtml(event.sender)}</b><time>${new Date(event.origin_server_ts).toLocaleTimeString()}</time></span><span class="turn-preview">${escapeHtml(event.content.body!.replace(/\s+/g, " ").slice(0, 120))}</span><span class="badges">${badges}</span></button></li>`;
    }).join("");
    messages.querySelectorAll<HTMLButtonElement>(".turn-picker").forEach((button) => button.onclick = () => { selectedEventId = button.dataset.event!; render(); });
    if (views.has(selectedEventId)) showReading(selectedEventId);
    else { inspector.hidden = false; inspector.innerHTML = `<h2>Turn annotations</h2><p class="muted">No 象 reading is attached to this Matrix event.</p>`; }
    return;
  }
  messages.innerHTML = visible.map((event) => {
    const view = views.get(event.event_id);
    const badges = view ? intents(view).map((m) => `<button class="mark ${m.declared ? "declared" : "inferred"} stage-${intentStage(m.intent)}" data-event="${escapeHtml(event.event_id)}" title="${m.declared ? "authored declaration" : "象 interpretation"}">${markGlyph(m.intent, m.glyph)}<span>${escapeHtml(m.intent)}</span></button>`).join("") : "";
    return `<li class="${event.sender === userId ? "mine" : "theirs"}"><article><div class="meta"><b>${escapeHtml(event.sender)}</b><time>${new Date(event.origin_server_ts).toLocaleTimeString()}</time></div><div class="body">${annotatedBody(event, view)}</div><div class="badges">${badges}</div></article><div class="notes">${sidenotes(view)}${wantsAuthorChart(event) ? authorChart() : ""}</div></li>`;
  }).join("");
  messages.querySelectorAll<HTMLElement>(".body").forEach(decorateRNodes);
  messages.querySelectorAll<HTMLButtonElement>("button.mark").forEach((button) => button.onclick = () => showReading(button.dataset.event!));
  if (autoScroll) messages.scrollTop = messages.scrollHeight;
}

function showReading(eventId: string): void {
  const view = views.get(eventId); if (!view) return;
  const d = turnDetail(view); inspector.hidden = false;
  const close = annotationMode ? "" : `<button id="close-reading" aria-label="Close">×</button>`;
  const heading = annotationMode ? "" : `<h2>象 reading</h2>`;
  const source = annotationMode ? "" : `<div class="source">${d.html}</div>`;
  inspector.innerHTML = `${close}${heading}<p>${intents(view).map((m) => `<span data-annotation-intent="${escapeHtml(m.intent)}" class="mark ${m.declared ? "declared" : "inferred"} stage-${intentStage(m.intent)} ${focusedIntent === m.intent ? "intent-focused" : ""}">${markGlyph(m.intent, m.glyph)}<span>${escapeHtml(m.intent)}</span></span>`).join(" ")}</p>${source}${d.fragments.map((f) => `<section data-annotation-intent="${escapeHtml(f.intent)}" class="stage-${intentStage(f.intent)} ${focusedIntent === f.intent ? "intent-focused" : ""}"><h3>${escapeHtml(f.intent)} ${f.target ? `→ ${escapeHtml(f.target)}` : ""}</h3><p>${escapeHtml(f.rationale)}</p>${f.patterns.length ? `<p class="small">${f.patterns.map(escapeHtml).join(" · ")}</p>` : ""}</section>`).join("")}<p class="small">${d.labeller ? `read by ${escapeHtml(d.labeller)}` : "declared by author"} · ${escapeHtml(d.status)}</p>`;
  if (!annotationMode) $("close-reading").onclick = () => { inspector.hidden = true; };
}

async function changePage(offset: number): Promise<void> { pageOffset = offset; await loadViews(); render(); }
async function refresh(): Promise<void> {
  await loadEvents();
  if (events.some(wantsAuthorChart)) await loadCompleteChartHistory();
  await loadViews(); render();
}
older.onclick = () => void changePage(pageOffset + pageSize);
newer.onclick = () => void changePage(Math.max(0, pageOffset - pageSize));
pageSizeSelect.onchange = () => { pageSize = Number(pageSizeSelect.value); pageOffset = 0; void changePage(0); };
loadSizeSelect.onchange = () => { loadSize = Number(loadSizeSelect.value); pageOffset = 0; void refresh(); };
markStyleSelect.onchange = () => { markStyle = markStyleSelect.value as MarkStyle; localStorage.setItem("futon.mark.style", markStyle); render(); };
window.addEventListener("message", (event) => {
  if (event.origin !== location.origin) return;
  const theme = annotationTheme(event.data);
  if (theme) {
    document.body.dataset.elementTheme = theme.theme;
    document.body.style.setProperty("--element-font", theme.fontFamily);
    document.body.style.setProperty("--element-fg", theme.foreground);
    document.body.style.setProperty("--element-bg", theme.background);
    return;
  }
  const focus = highlightedIntent(event.data);
  if (focus && events.some((item) => item.event_id === focus.eventId)) {
    selectedEventId = focus.eventId;
    focusedIntent = focus.intent;
    render();
    return;
  }
  const eventId = selectedMatrixEvent(event.data);
  if (!eventId || !events.some((item) => item.event_id === eventId)) return;
  selectedEventId = eventId;
  focusedIntent = null;
  render();
});

async function enter(): Promise<void> {
  await checkMembership(); login.hidden = true; app.hidden = false;
  identity.innerHTML = `${escapeHtml(userId)} <button id="logout">sign out</button>`;
  $("logout").onclick = () => { void matrix("/logout", { method: "POST", body: "{}" }).finally(() => { sessionStorage.clear(); location.reload(); }); };
  await refresh(); setInterval(() => void refresh(), 5000);
}

$("login-form").addEventListener("submit", (event) => { event.preventDefault(); status.textContent = "signing in…"; void signIn(($<HTMLInputElement>("username")).value, ($<HTMLInputElement>("password")).value).then(enter).catch((e: Error) => status.textContent = e.message); });
$("composer").addEventListener("submit", (event) => {
  event.preventDefault();
  const textarea = $<HTMLTextAreaElement>("message"), button = $<HTMLButtonElement>("send"), sendStatus = $("send-status");
  const draft = textarea.value, body = addressedBody($<HTMLSelectElement>("agent").value, draft);
  if (!body) { textarea.reportValidity(); return; }
  button.disabled = true; sendStatus.textContent = "sending…";
  const txn = crypto.randomUUID();
  void matrix(`/rooms/${encodeURIComponent(ROOM)}/send/m.room.message/${txn}`, { method: "PUT", body: JSON.stringify({ msgtype: "m.text", body }) })
    .then(async (response) => {
      if (!response.ok) {
        const error = await response.json().catch(() => ({})) as { error?: string };
        throw new Error(error.error ?? `Matrix send: http ${response.status}`);
      }
      if (textarea.value === draft) textarea.value = "";
      sendStatus.textContent = "sent";
      await refresh();
    })
    .catch((error: Error) => { sendStatus.textContent = `not sent: ${error.message}`; textarea.focus(); })
    .finally(() => { button.disabled = false; });
});
if (token) void enter().catch(() => { sessionStorage.clear(); token = ""; login.hidden = false; });
