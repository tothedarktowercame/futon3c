export interface Label { source?: string; intent?: string; weak?: boolean }
export interface Fragment {
  sentence?: string; text?: string; cues?: string[]; labels?: Label[];
  combined?: string | null; disagree?: boolean;
}
export interface Commit { repo?: string; sha?: string; author?: string; subject?: string }
export interface HappenedRow { at?: string; type?: string; summary?: Record<string, unknown> }
export interface PortAct { kind?: string; text?: string; since?: string }
export interface MemoryReached { name?: string; project?: string | null }
export interface MemoryCall {
  round?: number | string; tool?: string; args?: unknown;
  at?: string; error?: boolean; "evidence-id"?: string;
  reached?: (string | MemoryReached)[];
}
export interface ReplyParagraph {
  mark?: string; intent?: string; target?: string | null; quoted?: boolean; body?: string; answers?: string[];
}
export interface Reply {
  at?: string; evidence_id?: string; turn_id?: string;
  paragraphs?: ReplyParagraph[]; unanswered?: string[]; unanchored?: number[];
  /** the reply's text, carried for harness seats (Zai, Kimi), whose replies are unmarked */
  text?: string;
}
export interface Frame {
  turn: { evidence_id?: string; at?: string; text?: string };
  parse: { status?: string; fragments?: Fragment[] };
  operators?: { hits?: Record<string, unknown>[]; cues_without_operator?: Record<string, unknown>[] };
  patterns?: {
    matched?: (string | Record<string, unknown>)[];
    rejected?: unknown[];
    proposed_by_parent?: Record<string, Record<string, unknown>[]>;
  };
  happened?: HappenedRow[];
  ports?: { closed_this_turn?: PortAct[]; still_open?: PortAct[] };
  /** 馬's alignment reading of the operator turn (the M1 packet), as the
   * turn record carries it; absent when the turn has not been read. */
  alignment?: Alignment;
  record?: { id?: string; turn_id?: string; agent_id?: string; session_id?: string };
  memory?: { calls?: MemoryCall[]; injected?: string[] };
  reply?: Reply;
  /** patterns retrieved for this turn: the prompt header and the context-retrieval row */
  retrieved?: RetrievedPattern[];
  /** 馬's agent card as it stood after this turn: SETTLED, STANDING, FRESH */
  card?: AgentCard;
  /** 馬's observation of the exchange after this turn (ma_observe.py) */
  observed?: { at?: string; observations?: string; current_task?: string; model?: string; seconds?: number };
}

/** The part of Cascade Live's /api/alpha/cascade-real/graph the slice reads. */
export interface CascadeGraph {
  lineage?: { agent: string; target: string; session?: string; at?: number }[];
  clusters?: { cluster: string; mission: string }[];
  held?: { held: string; mission: string; reason?: string }[];
  arrows?: { have: string; want: string; "move-class"?: string }[];
  patterns?: { edges?: { mission: string; pattern: string; state?: string; relation?: string }[] };
}
/** futon_view.py's output: derived mission -> pattern links and each agent's latest 馬 purpose. */
export interface FutonView {
  built_at?: string;
  mission_patterns?: Record<string, [string, number, number, string[]][]>;
  agents?: Record<string, { session?: string; target?: string; at?: number; purpose?: string | null;
                            job?: { by?: string; at?: string; state?: string; line?: string } }>;
}

export interface RetrievedPattern { id?: string; score?: number | null; method?: string }

export interface HeldItem { text: string; held: number }
export interface AgentCard {
  at?: string;
  settled?: { engaged?: Record<string, string>; done?: string[];
              commits?: { count?: number; last?: string[] }; mission_patterns?: string[] };
  standing?: { purpose?: HeldItem; packet_vs_purpose?: string | null; xiang_patterns?: [string, number][];
               open_threads?: HeldItem[]; repair?: HeldItem[]; ramifications?: HeldItem[] };
  fresh?: { done?: string[]; problems?: string[] };
}

export interface CuePiece { text: string; cue: boolean }
export interface ParseLine { head: string; pieces: CuePiece[]; sources: string; provisional: boolean }
/** One operator act of the alignment reading: the act, its verdict, and —
 * when answered — the reply paragraph (mark, quoted words) that answered it. */
export interface AlignmentAct {
  act?: string; kind?: string; text?: string;
  verdict?: "answered" | "deferred" | "declined" | "not-answered" | "unread" | string;
  by?: string | null; mark?: string | null; quote?: string | null;
  method?: "quote" | "model" | string;
}
export interface Alignment {
  version?: number; at?: string; seat?: string | null;
  acts?: AlignmentAct[]; unread?: number;
}
export interface FrameModel {
  status: string; at: string; said: string; saidHref?: string;
  operators: string[]; parseNote: string; parse: ParseLine[];
  patterns: string[]; happened: string[]; ports: string; memory: string[];
  answered: string[];
  alignmentTitle: string; alignment: string[];
  observed: string[];
  card: string[];
  session: string;
}
export interface StreamStatus {
  state: "building" | "rebuild-failed" | "reconnecting" | "live";
  detail?: string;
}
export type ParsedStreamEvent =
  | {kind: "frames"; frames: Frame[]; version: number | null}
  | {kind: "status"; status: StreamStatus}
  | {kind: "ignore"};

const MEMORY_ABSENT = "no recorded memory call for this turn (nothing reached it that the record can show)";
export const PARSE_PENDING = "no reading yet — 象 has not read this turn; what has happened so far is under HAPPENED";
export const REPLY_ABSENT = "no reply recorded for this turn yet";
export const PATTERNS_NONE_READ = "matched: (none — 象 read this turn and matched no library pattern)";
export const PATTERNS_PENDING = "matched: (象's reading has not landed; nothing is matched or rejected yet)";

export const READ_STATUSES = ["analyzed", "drafted-provisional"];

/** Frames to show. By default the newest turns that 象 has not read yet are
 * left out, as the Emacs stepper does. In live mode (keepPending) they stay:
 * the current turn's SAID and HAPPENED are what is going on right now, and
 * the reading arrives on the next refresh. An older unread turn always stays,
 * shown as missing. */
export function readyFrames(frames: Frame[], keepPending = false): Frame[] {
  if (keepPending) return frames;
  let end = frames.length;
  while (end > 0 && !READ_STATUSES.includes(frames[end - 1].parse?.status || "")) end--;
  return frames.slice(0, end);
}

export function parseStreamEvent(name: string, data: string): ParsedStreamEvent {
  if (name !== "frames" && name !== "status") return {kind: "ignore"};
  try {
    const value = JSON.parse(data) as Record<string, unknown>;
    if (name === "frames") return {
      kind: "frames",
      frames: Array.isArray(value.frames) ? value.frames as Frame[] : [],
      version: typeof value.version === "number" ? value.version : null,
    };
    const allowed = ["building", "rebuild-failed", "reconnecting", "live"];
    const lastError = value["last-error"] as Record<string, unknown> | undefined;
    const detail = lastError?.reason ?? lastError?.err;
    return {kind: "status", status: {
      state: allowed.includes(String(value.state)) ? value.state as StreamStatus["state"] : "rebuild-failed",
      ...(detail === undefined ? {} : {detail: String(detail)}),
    }};
  } catch (_error) {
    return {kind: "status", status: {state: "rebuild-failed", detail: `bad event: ${data.slice(0, 80)}`}};
  }
}

export function receiveFrames(
  state: {frames: Frame[]; current: number}, incoming: Frame[],
): {frames: Frame[]; current: number} {
  if (!incoming.length && state.frames.length) return state;
  const frames = readyFrames(incoming, true);
  const newest = Math.max(0, frames.length - 1);
  const wasNewest = state.current >= state.frames.length - 1;
  return {frames, current: wasNewest ? newest : Math.max(0, Math.min(state.current, newest))};
}

const short = (value: unknown, n = 8): string => typeof value === "string" ? value.slice(0, n) : "?";
const shown = (value: unknown, fallback = "?"): string =>
  value === undefined || value === null ? fallback : String(value);

export function cuePieces(text: string, cues: string[]): CuePiece[] {
  const chars = Array.from(text);
  const covered = new Array(chars.length).fill(false) as boolean[];
  for (const cue of cues || []) {
    const needle = Array.from(cue);
    if (!needle.length) continue;
    outer: for (let i = 0; i + needle.length <= chars.length; i++) {
      for (let j = 0; j < needle.length; j++) if (chars[i + j] !== needle[j]) continue outer;
      for (let j = 0; j < needle.length; j++) covered[i + j] = true;
      break;
    }
  }
  if (!chars.length) return [{text: "", cue: false}];
  const pieces: CuePiece[] = [];
  let start = 0;
  for (let i = 1; i <= chars.length; i++) {
    if (i === chars.length || covered[i] !== covered[start]) {
      pieces.push({text: chars.slice(start, i).join(""), cue: covered[start]});
      start = i;
    }
  }
  return pieces;
}

export function happenedDetail(row: HappenedRow): string {
  const summary = row.summary;
  if (!summary || typeof summary !== "object") return "";
  if (summary.event === "turn-commits") {
    const commits = Array.isArray(summary.commits) ? summary.commits as Commit[] : [];
    return `${commits.length} commit(s) in window: ${commits.map(c =>
      `${shown(c.repo)} ${short(c.sha, 7)} ${shown(c.author)}: ${shown(c.subject)}`).join(" | ")}`;
  }
  if (summary.intent) return `intent=${shown(summary.intent)} fragment=${shown(summary["fragment-id"])} target=${shown(summary.target)}`;
  if (typeof summary.text === "string") {
    const chars = Array.from(summary.text);
    return chars.length > 80 ? chars.slice(0, 79).join("") + "…" : summary.text;
  }
  return shown(summary.event, "");
}

export function portsAge(since: string | undefined, at: string | undefined): string | undefined {
  if (!since || !at) return undefined;
  const delta = Date.parse(at) - Date.parse(since);
  if (!Number.isFinite(delta)) return undefined;
  const minutes = Math.floor(delta / 60000);
  return minutes < 1 ? "<1m" : `${minutes}m`;
}

function operatorLines(frame: Frame): string[] {
  const hits = frame.operators?.hits || [];
  if (!hits.length) return ["(no operator words)"];
  return hits.map(h => {
    const cue = h.cue_intent;
    let meeting = "outside 象's marks";
    if (cue && h.agree === true) meeting = `in 象 ${cue} "${shown(h.cue_text, "")}" (agrees)`;
    else if (cue) meeting = `in 象 ${cue} "${shown(h.cue_text, "")}"; chip says ${shown(h.chip_intent)}`;
    return `${shown(h.ibol).padEnd(12)} "${shown(h.text, "")}" ${meeting}`;
  });
}

function patternLines(frame: Frame): string[] {
  const p = frame.patterns || {};
  const matched = (p.matched || []).map(x => typeof x === "string" ? x : shown(x.id));
  // An empty match must not look like a reading still in flight (Joe,
  // 2026-10-16): say which it is.
  const out = [matched.length ? `matched: ${matched.join(", ")}`
    : frame.parse?.status === "analyzed" ? PATTERNS_NONE_READ : PATTERNS_PENDING];
  const proposed = Object.entries(p.proposed_by_parent || {});
  if (!proposed.length) out.push("proposed: (none)");
  for (const [parent, children] of proposed) {
    out.push(`proposed under ${parent}:`);
    for (const child of children) out.push(`  - ${shown(child.id)} — ${shown(child.title)} (fragment ${shown(child.fragment)})`);
  }
  out.push(`rejected: ${(p.rejected || []).length}`);
  return out;
}

export const ALIGNMENT_UNREAD = "not read yet";

const VERDICT_GLYPHS: Record<string, string> = {
  "answered": "✓", "noted": "·", "deferred": "→", "declined": "✗", "not-answered": "?", "unread": "•",
};

function firstWords(text: string | null | undefined, words = 7): string {
  const list = String(text ?? "").trim().split(/\s+/).filter(Boolean);
  return list.length ? list.slice(0, words).join(" ") + (list.length > words ? "…" : "") : "…";
}

/** The ALIGNMENT section: a summary line, then one line per operator act —
 * verdict glyph, kind and first words, and for answered acts the reply
 * paragraph's mark and quoted words, with the method in brackets. A frame
 * without the field reads "not read yet", never nothing. */
export function alignmentLines(frame: Frame): string[] {
  const alignment = frame.alignment;
  const acts = alignment?.acts || [];
  if (!alignment || !acts.length) return [ALIGNMENT_UNREAD];
  const answered = acts.filter(a => a.verdict === "answered");
  const byQuote = answered.filter(a => a.method === "quote").length;
  const byModel = answered.filter(a => a.method === "model").length;
  // Each verdict counted on its own: a deferred or unread act is not "not
  // answered", and the per-act verdicts, not the top-level field, are counted.
  const count = (v: string) => acts.filter(a => a.verdict === v).length;
  const rest = (["noted", "deferred", "declined", "not-answered", "unread"] as const)
    .map(v => [count(v), v === "not-answered" ? "not answered" : v] as const)
    .filter(([n]) => n > 0).map(([n, label]) => `, ${n} ${label}`).join("");
  const lines = [`${acts.length} asked, ${answered.length} answered `
    + `(${byQuote} by quote, ${byModel} by model)${rest}`];
  for (const act of acts) {
    let line = `${VERDICT_GLYPHS[act.verdict || ""] || shown(act.verdict, "?")} `
      + `${shown(act.kind)} "${firstWords(act.text)}"`;
    if (act.verdict === "answered")
      line += ` → ${act.mark ? act.mark : "—"} "${firstWords(act.quote)}"`;
    if (act.method) line += `  [${act.method}]`;
    lines.push(line);
  }
  return lines;
}

function alignmentTitle(frame: Frame): string {
  const seat = frame.alignment?.seat;
  return `ALIGNMENT (${seat ? seat : "馬"})`;
}

function portsText(frame: Frame): string {  if (!frame.ports) return "";
  const lines: string[] = [];
  const closed = frame.ports.closed_this_turn || [];
  const open = frame.ports.still_open || [];
  if (closed.length) {
    lines.push("closed:");
    closed.forEach(a => lines.push(`  - ${shown(a.kind)}: ${shown(a.text, "")}`));
  }
  if (open.length) {
    lines.push("still open:");
    open.forEach(a => lines.push(`  - ${shown(a.kind)} (${portsAge(a.since, frame.turn.at) || "?"}): ${shown(a.text, "")}`));
  }
  return lines.join("\n");
}

function memoryLines(frame: Frame): string[] {
  if (!frame.memory) return [MEMORY_ABSENT];
  const out: string[] = [];
  for (const call of frame.memory.calls || []) {
    const args = typeof call.args === "string" ? call.args : JSON.stringify(call.args ?? {});
    const prefix = call.round === undefined ? "" : `${shown(call.round)}  `;
    out.push(`${prefix}${shown(call.tool)}  ${args}${call.error ? "  [error]" : ""}`);
    const reached = (call.reached || []).map(item => typeof item === "string" ? item
      : `${shown(item.name)}${item.project ? ` (${item.project})` : ""}`);
    out.push(`  reached: ${reached.length ? reached.join(", ") : "(none)"}`);
  }
  if ((frame.memory.injected || []).length) out.push(`injected before round 1: ${frame.memory.injected!.join(", ")}`);
  return out.length ? out : ["(no memory events recorded)"];
}

/** The reply's marked paragraphs lined up with the turn's sentences: one
 * line per sentence naming the paragraph whose bracketed target quotes it,
 * then the paragraphs that quote no sentence (a gist, a label, a paraphrase). */
export function answeredLines(frame: Frame): string[] {
  const reply = frame.reply;
  if (!reply) return [REPLY_ABSENT];
  const paras = reply.paragraphs || [];
  const out = [`reply at ${shown(reply.at)} · ${paras.length} marked paragraph(s)`];
  if (!paras.length) {
    out.push("the reply carries no marks, so nothing can be lined up");
    if (reply.text) out.push(...reply.text.split("\n"));
    return out;
  }
  const label = (p: ReplyParagraph): string =>
    `${shown(p.mark)} ${shown(p.intent)} (${p.quoted ? `"${shown(p.target, "")}"` : shown(p.target, "")})`;
  const seen = new Set<string>();
  for (const f of frame.parse?.fragments || []) {
    const sid = f.sentence || "?";
    if (seen.has(sid)) continue;
    seen.add(sid);
    const intent = f.combined || (f.disagree ? "disagree" : "(unlabelled)");
    const hits = paras.filter(p => (p.answers || []).includes(sid));
    if (!hits.length) out.push(`${sid} ${intent}  ← (no paragraph quotes this sentence)`);
    for (const p of hits) out.push(`${sid} ${intent}  ← ${label(p)}`);
  }
  const loose = paras.filter(p => !(p.answers || []).length);
  if (loose.length) out.push(`not lined up with any sentence: ${loose.map(label).join(", ")}`);
  return out;
}

const slug = (id: string): string => id.toLowerCase().replace(/^[cmet]-/, "");
const tail = (node: string): string => node.split("/").pop() || node;

/** The slice of Cascade Live this frame touches, matched by type: the agent
 * (its lineage), the work it is engaged on (clusters, arrows, held items,
 * linked patterns) and the turn's patterns (retrieved by embedding, matched
 * by 象, linked to the mission), each with the missions that link it. The
 * session-only layers (ports, observations) stay in the stepper. */
export function cascadeLines(frame: Frame, graph: CascadeGraph | null | undefined, agentId?: string): string[] {
  if (!graph) return [];
  const out: string[] = [];
  const agent = agentId || frame.record?.agent_id;
  const edges = graph.patterns?.edges || [];
  if (agent) {
    const rows = (graph.lineage || []).filter(r => r.agent === `agent:${agent}`);
    const latest = new Map<string, number>();
    for (const r of rows) latest.set(r.target, Math.max(latest.get(r.target) || 0, r.at || 0));
    const targets = [...latest].sort((a, b) => b[1] - a[1]).slice(0, 6);
    out.push(`agent ${agent}: ${rows.length} clock edge(s)`);
    for (const [t, at] of targets) out.push(`  → ${t}${at ? `  (${new Date(at).toISOString().slice(0, 16)}Z)` : ""}`);
  }
  const engaged = Object.values(frame.card?.settled?.engaged || {}).map(v => slug(String(v)));
  for (const e of engaged) {
    const nodes = new Set<string>();
    for (const x of edges) if (tail(x.mission) === e) nodes.add(x.mission);
    for (const x of graph.clusters || []) if (tail(x.mission) === e) nodes.add(x.mission);
    for (const x of graph.lineage || []) if (tail(x.target) === e) nodes.add(x.target);
    for (const node of nodes) {
      out.push(`engaged ${node}`);
      const clusters = (graph.clusters || []).filter(x => x.mission === node).map(x => x.cluster);
      if (clusters.length) out.push(`  clusters: ${clusters.join(", ")}`);
      // Related sessions: everyone else clocked on this node, newest first.
      const others = new Map<string, { at: number; session?: string }>();
      for (const r of graph.lineage || []) if (r.target === node && r.agent !== `agent:${agent}`) {
        const prev = others.get(r.agent);
        if (!prev || (r.at || 0) > prev.at) others.set(r.agent, { at: r.at || 0, session: r.session });
      }
      for (const [a, v] of [...others].sort((x, y) => y[1].at - x[1].at).slice(0, 6))
        out.push(`  also here: ${a.replace(/^agent:/, "")}${v.at ? `  (${new Date(v.at).toISOString().slice(0, 16)}Z)` : ""}${v.session ? `  session ${v.session.slice(0, 8)}` : ""}`);
      // Wants: what this node is an arrow towards, and what points at it.
      for (const a of graph.arrows || []) if (a.have === node) out.push(`  wants → ${a.want} ${a["move-class"] || ""}`.trimEnd());
      for (const a of graph.arrows || []) if (a.want === node) out.push(`  wanted by ← ${a.have} ${a["move-class"] || ""}`.trimEnd());
      const heldItems = (graph.held || []).filter(x => x.mission === node);
      if (heldItems.length) out.push(`  held items: ${heldItems.length}`);
      for (const h of heldItems.slice(0, 3)) out.push(`    - ${(h.reason || h.held).replace(/\s+/g, " ").slice(0, 140)}`);
      const linked = edges.filter(x => x.mission === node && x.state === "linked").map(x => x.pattern);
      out.push(`  linked patterns: ${linked.length ? linked.slice(0, 8).join(", ") + (linked.length > 8 ? ` … +${linked.length - 8}` : "") : "none"}`);
    }
    if (!nodes.size) out.push(`engaged ${e}: not in Cascade Live`);
  }
  const sources = new Map<string, string[]>();
  const note = (id: string | undefined, how: string): void => {
    if (!id) return;
    const list = sources.get(id) || []; if (!list.includes(how)) list.push(how); sources.set(id, list);
  };
  for (const r of frame.retrieved || []) note(r.id, r.method?.includes("header") ? "header" : "embedding");
  for (const m of frame.patterns?.matched || []) note(typeof m === "string" ? m : String((m as Record<string, unknown>).id || ""), "象");
  if (sources.size) out.push("this turn's patterns, and the missions that link them:");
  for (const [id, how] of sources) {
    const missions = [...new Set(edges.filter(x => x.pattern === id).map(x => x.mission))];
    out.push(`  ${id} [${how.join(", ")}]: ${missions.length ? missions.slice(0, 4).map(tail).join(", ") + (missions.length > 4 ? ` … +${missions.length - 4}` : "") : "no mission links it"}`);
  }
  return out;
}

/** The zoomed injection FUTON (馬 speaking for it) would give this turn, as a
 * preview: nothing is sent. Same work: content (what was injected). Same
 * mission, other agents: one card line each, at most 2. Related mission
 * sharing a pattern this turn retrieved: one pointer line each, at most 2,
 * naming the pattern, never the mission's material. Else nothing. */
export function zoomLines(frame: Frame, graph: CascadeGraph | null | undefined, view: FutonView | null | undefined, agentId?: string): string[] {
  if (!graph && !view) return [];
  const agent = agentId || frame.record?.agent_id;
  const out: string[] = [];
  const injected = frame.memory?.injected || [];
  out.push("1 · same work → content");
  out.push(injected.length ? `    injected: ${injected.map(tail).join(", ")}` : "    (nothing injected for this turn)");
  // the engaged node(s): from the card, else from the agent's clock edge
  const slugs = new Set(Object.values(frame.card?.settled?.engaged || {}).map(v => slug(String(v))));
  const mine = agent ? view?.agents?.[agent]?.target : undefined;
  if (mine) slugs.add(tail(mine));
  const nodeOf = (t: string): boolean => slugs.has(tail(t));
  out.push("2 · same mission, other agents → one line each");
  const others = Object.entries(view?.agents || {})
    .filter(([a, v]) => a !== agent && v.target && nodeOf(v.target))
    .sort((x, y) => (y[1].at || 0) - (x[1].at || 0)).slice(0, 2);
  if (!others.length) out.push("    (no one else on this work)");
  // the agent's current job first: a seat keeps its session across jobs, so a
  // 馬 purpose is shown only when futon_view.py found it newer than the job
  for (const [a, v] of others) {
    const j = v.job;
    out.push(j ? `    ${a}: job from ${j.by || "?"}${j.at ? ` ${j.at.slice(11, 16)}Z` : ""} (${j.state || "?"}): ${j.line || "(no packet text)"}`
                 + (v.purpose ? ` · 馬: ${v.purpose}` : "")
               : `    ${a}: ${v.purpose || "(no 馬 card yet)"}`);
  }
  out.push("3 · related missions sharing a pattern this turn retrieved → pointer only");
  const mineP = new Set<string>([
    ...(frame.retrieved || []).map(r => r.id || ""),
    ...(frame.patterns?.matched || []).map(m => typeof m === "string" ? m : String((m as Record<string, unknown>).id || "")),
  ].filter(Boolean));
  const related = new Map<string, { p: string; how: string; weight: number }>();
  for (const e of graph?.patterns?.edges || [])
    if (mineP.has(e.pattern) && !nodeOf(e.mission) && e.state === "linked")
      if (!related.has(e.mission)) related.set(e.mission, { p: e.pattern, how: "substrate-2", weight: 100 });
  for (const [m, rows] of Object.entries(view?.mission_patterns || {})) {
    if (nodeOf(m)) continue;
    for (const [p, sessions, turns] of rows)
      if (mineP.has(p) && turns >= 2) {
        const w = sessions * 10 + turns;
        const prev = related.get(m);
        if (!prev || (prev.how !== "substrate-2" && w > prev.weight))
          related.set(m, { p, how: `derived, ${sessions} session(s) ${turns} turn(s)`, weight: w });
      }
  }
  const top = [...related].sort((x, y) => y[1].weight - x[1].weight).slice(0, 2);
  if (!top.length) out.push("    (no related mission shares this turn's patterns)");
  for (const [m, r] of top) out.push(`    ${tail(m)} also uses ${r.p}  (${r.how})`);
  return out;
}

/** 馬's card, most settled first; empty when the session has none. */
export function cardLines(frame: Frame): string[] {
  const c = frame.card;
  if (!c) return [];
  const out: string[] = [];
  const held = (i: HeldItem): string => `${i.text}${i.held > 1 ? `  (held ${i.held})` : ""}`;
  const list = (label: string, items: string[] | undefined): void => {
    if (items && items.length) { out.push(`  ${label}:`); for (const x of items) out.push(`    - ${x}`); }
  };
  const st = c.settled || {}, sd = c.standing || {}, fr = c.fresh || {};
  out.push("SETTLED");
  const engaged = Object.entries(st.engaged || {}).map(([k, v]) => `${k} ${v}`).join(" · ");
  out.push(`  engaged: ${engaged || "(not clocked on anything)"}`);
  list(`done (${(st.done || []).length})`, st.done);
  out.push(`  commits: ${st.commits?.count ?? 0}`);
  for (const x of st.commits?.last || []) out.push(`    ${x}`);
  list("mission patterns (substrate-2)", st.mission_patterns);
  out.push("STANDING");
  if (sd.purpose?.text) out.push(`  purpose: ${held(sd.purpose)}`);
  if (sd.packet_vs_purpose) out.push(`  packet vs purpose: ${sd.packet_vs_purpose}`);
  list("open threads", (sd.open_threads || []).map(held));
  list("repair", (sd.repair || []).map(held));
  list("ramifications", (sd.ramifications || []).map(held));
  list("patterns 象 matched more than once", (sd.xiang_patterns || []).map(([p, n]) => `${p} ×${n}`));
  out.push("FRESH (this turn)");
  list("done", fr.done);
  list("problems", fr.problems);
  out.push("institutions: not modelled yet");
  return out;
}

/** 馬's observation, or nothing (the section is shown only for observed sessions). */
export function observedLines(frame: Frame): string[] {
  const o = frame.observed;
  if (!o || !o.observations) return [];
  const out = [`馬 at ${shown(o.at)} · ${shown(o.model)}${o.seconds ? ` · ${o.seconds} s` : ""}`];
  out.push(...o.observations.split("\n"));
  if (o.current_task) out.push("", "current task:", ...o.current_task.split("\n"));
  return out;
}

export function frameModel(frame: Frame): FrameModel {
  const status = frame.parse?.status || "missing";
  const provisional = status === "drafted-provisional";
  const parse = (frame.parse?.fragments || []).map(f => {
    const labels = f.labels || [];
    const intents = labels.map(l => l.intent).filter((x): x is string => !!x);
    const head = f.disagree && intents.length > 1 ? `disagree: ${intents.join(" / ")}` : (f.combined || "(unlabelled)");
    const sources = labels.map(l => `${shown(l.source)}→${shown(l.intent)}${l.weak ? "?" : ""}`).join(", ");
    return {head, pieces: cuePieces(f.text || "", f.cues || []), sources, provisional};
  });
  const happened = (frame.happened || []).map(row =>
    `${shown(row.at)}  ${shown(row.type).padEnd(28)} ${happenedDetail(row)}`);
  return {
    status, at: frame.turn?.at || "?",
    said: frame.turn?.text || "(no text)",
    // No link: the scopes page reads only its static fixtures (five turns),
    // so for a live turn it showed "Could not load scope data: 404" -- and in
    // voxterm's Frames tab a tap while scrolling replaced the stepper with it
    // (Joe, 2026-10-19). The per-turn view is the Diagram tab's Turn view.
    saidHref: undefined,
    operators: operatorLines(frame),
    parseNote: provisional ? "小象, provisional — 象's reading pending" : READ_STATUSES.includes(status) ? "" : PARSE_PENDING,
    parse, patterns: patternLines(frame),
    happened: happened.length ? happened : ["(nothing between this turn and the next)"],
    ports: portsText(frame), memory: memoryLines(frame),
    answered: answeredLines(frame),
    alignmentTitle: alignmentTitle(frame), alignment: alignmentLines(frame),
    observed: observedLines(frame),
    card: cardLines(frame),
    session: frame.record?.session_id || "?",
  };
}

function add<K extends keyof HTMLElementTagNameMap>(parent: HTMLElement, tag: K, cls?: string, text?: string): HTMLElementTagNameMap[K] {
  const node = document.createElement(tag);
  if (cls) node.className = cls;
  if (text !== undefined) node.textContent = text;
  parent.append(node);
  return node;
}

/** opts.frame: 1-based frame to open on; absent means the newest frame in
 * live mode and the first in fixture mode. A reader on the newest frame
 * follows pushed frames; one reading back stays put. */
export function renderStepper(initial: Frame[], host: HTMLElement, opts: {
  frame?: number; live?: boolean; refetch?: () => Promise<Frame[]>;
  /** the page's agent (?agent=), for the lineage slice */
  agent?: string;
  subscribe?: (handlers: {
    onFrames: (frames: Frame[]) => void;
    onStatus: (status: StreamStatus) => void;
  }) => () => void;
} = {}): {goTo(i: number): void; next(): void; prev(): void; refresh(): Promise<void>; index(): number; count(): number;
           setCascade(graph: CascadeGraph | null): void; setView(view: FutonView | null): void} {
  let cascade: CascadeGraph | null = null;
  let futonView: FutonView | null = null;
  const clamp = (i: number): number => Math.max(0, Math.min(i, Math.max(0, frames.length - 1)));
  let frames = readyFrames(initial, !!opts.live);
  let current = clamp(opts.frame ? opts.frame - 1 : opts.live ? frames.length - 1 : 0);
  let refreshed = "";
  let streamState = opts.live ? "" : " · fixture";
  let busy = false;
  const status = add(host, "div", "stepper-status");
  const body = add(host, "main", "stepper-frame");
  const footer = add(host, "footer", "stepper-footer", "rewind: Emacs only");
  void footer;

  const section = (title: string, lines: string[], pre = true, parent: HTMLElement = body): HTMLElement => {
    const sectionNode = add(parent, "section");
    add(sectionNode, "h2", "", title);
    const content = add(sectionNode, pre ? "pre" : "div", "stepper-section");
    content.textContent = lines.join("\n");
    return content;
  };
  const updateUrl = (): void => {
    if (typeof history !== "undefined" && typeof location !== "undefined") {
      const url = new URL(location.href); url.searchParams.set("frame", String(current + 1));
      history.replaceState(null, "", url);
    }
  };
  const updateStatus = (): void => {
    if (!frames.length) status.textContent = `frame 0/0${refreshed}${streamState}`;
    else {
      const model = frameModel(frames[current]);
      status.textContent = `frame ${current + 1}/${frames.length} · ${short(model.session)} · ${model.status}${refreshed}${streamState}`;
    }
  };
  const draw = (): void => {
    body.textContent = "";
    if (!frames.length) { updateStatus(); add(body, "p", "", "No operator-turn frames."); return; }
    const model = frameModel(frames[current]);
    updateStatus();
    const zoom = zoomLines(frames[current], cascade, futonView, opts.agent);
    if (zoom.length) section(`FUTON'S VIEW (馬) — what a zoomed injection would add to this turn (preview; nothing is sent)`, zoom);
    const said = add(body, "section");
    const saidHeading = add(said, "h2");
    const heading = model.saidHref ? add(saidHeading, "a", "said-link") : add(saidHeading, "span");
    if (model.saidHref) (heading as HTMLAnchorElement).href = model.saidHref;
    heading.textContent = `SAID — ${model.at}`;
    add(said, "p", "said-text", model.said);
    section("OPERATORS", model.operators);
    const parseSection = add(body, "section"); add(parseSection, "h2", "", "PARSE");
    if (model.parseNote) add(parseSection, "p", "provisional", model.parseNote);
    if (!model.parse.length) add(parseSection, "pre", "stepper-section", "(no fragments)");
    for (const line of model.parse) {
      const row = add(parseSection, "div", line.provisional ? "parse-line provisional" : "parse-line");
      add(row, "span", "parse-head", `[${line.head}] `);
      for (const piece of line.pieces) add(row, "span", piece.cue ? "parse-cue" : "", piece.text);
      add(row, "div", "parse-sources", `(${line.sources})`);
    }
    section("ANSWERED", model.answered);
    section(model.alignmentTitle, model.alignment);
    if (model.observed.length) section("OBSERVED (馬)", model.observed);
    section("PATTERNS", model.patterns);
    section("HAPPENED", model.happened);
    if (model.ports) section("PORTS", model.ports.split("\n"));
    section("MEMORY", model.memory);
    // 馬's card and the Cascade slice describe the session, not this turn
    // (Joe, 2026-10-19: they got in the way of reading the turn), so they
    // sit last, folded.
    const slice = cascadeLines(frames[current], cascade, opts.agent);
    if (model.card.length || slice.length) {
      const background = add(body, "details", "stepper-background");
      add(background, "summary", "", "BACKGROUND (馬's card, Cascade slice) — the session, not this turn");
      if (model.card.length) section(`CARD (馬) — as it stood after this turn`, model.card, true, background);
      if (slice.length) section("CASCADE (slice of Cascade Live)", slice, true, background);
    }
    updateUrl();
  };
  const goTo = (i: number): void => { current = clamp(i); draw(); };
  const next = (): void => goTo(current + 1);
  const prev = (): void => goTo(current - 1);
  const refresh = async (): Promise<void> => {
    if (!opts.live || !opts.refetch) { status.textContent += " · fixture mode: no refetch"; return; }
    if (busy) return;
    busy = true;
    try {
      const received = receiveFrames({frames, current}, await opts.refetch());
      frames = received.frames;
      current = received.current;
      refreshed = ` · refreshed ${new Date().toISOString().slice(11, 19)}Z`;
      draw();
    } catch (error) { status.textContent += ` · refresh failed: ${String(error)}`; }
    finally { busy = false; }
  };
  window.addEventListener("keydown", async event => {
    if (["INPUT", "TEXTAREA", "SELECT"].includes((event.target as HTMLElement)?.tagName)) return;
    if (event.key === "n" || event.key === "ArrowRight") next();
    else if (event.key === "p" || event.key === "ArrowLeft") prev();
    else if (event.key === "Home") goTo(0);
    else if (event.key === "End") goTo(frames.length - 1);
    else if (event.key === "g") await refresh();
  });
  draw();
  const unsubscribe = opts.live && opts.subscribe ? opts.subscribe({
    onFrames: incoming => {
      const received = receiveFrames({frames, current}, incoming);
      frames = received.frames;
      current = received.current;
      refreshed = ` · pushed ${new Date().toISOString().slice(11, 19)}Z`;
      streamState = " · live"; // a frame set arriving ends "building" and "rebuild failed"
      draw();
    },
    onStatus: next => {
      streamState = next.state === "rebuild-failed"
        ? ` · rebuild failed${next.detail ? `: ${next.detail}` : ""}`
        : ` · ${next.state}`;
      updateStatus();
    },
  }) : undefined;
  void unsubscribe;
  const setCascade = (graph: CascadeGraph | null): void => { cascade = graph; if (frames.length) draw(); };
  const setView = (view: FutonView | null): void => { futonView = view; if (frames.length) draw(); };
  return {goTo, next, prev, refresh, index: () => current, count: () => frames.length, setCascade, setView};
}
