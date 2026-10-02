/**
 * The widget's view-model: pure functions from route payloads to rows the
 * page draws. Kept DOM-free so node:test covers them.
 */

import { marksFor, marksHtml } from "../src/marks.js";
import type { Analysis, Notice, TurnRecord, TurnSummary, TurnView } from "../src/types.js";

export interface WidgetConfig {
  base: string;
  agent: string;
  session: string | null;
  /** Poll interval in ms. */
  every: number;
}

/** Read the widget's configuration from its URL (Element passes widget params as query/fragment). */
export function configFromUrl(href: string, defaults: Partial<WidgetConfig> = {}): WidgetConfig {
  const url = new URL(href);
  const params = new URLSearchParams(url.search);
  // Element may put parameters after a '#'; merge both.
  for (const [k, v] of new URLSearchParams(url.hash.replace(/^#\??/, ""))) if (!params.has(k)) params.set(k, v);
  const every = Number(params.get("every") ?? defaults.every ?? 15000);
  return {
    base: (params.get("base") ?? defaults.base ?? url.origin).replace(/\/$/, ""),
    agent: params.get("agent") ?? defaults.agent ?? "",
    session: params.get("session") ?? defaults.session ?? null,
    every: Number.isFinite(every) && every >= 2000 ? every : 15000,
  };
}

export interface TurnRow {
  id: string;
  turnId: string;
  origin: "operator" | "agent";
  author: string;
  when: string;
  status: string;
  preview: string;
}

/** Rows for the turn list, oldest first, so a room reads downward. */
export function turnRows(turns: TurnSummary[]): TurnRow[] {
  return [...turns]
    .sort((a, b) => a["created-at"].localeCompare(b["created-at"]))
    .map((t) => {
      const origin: "operator" | "agent" = t["turn-id"].endsWith(":reply") ? "agent" : "operator";
      return {
        id: t.id,
        turnId: t["turn-id"],
        origin,
        author: origin === "agent" ? t["agent-id"] : "operator",
        when: t["created-at"],
        status: t["analysis-status"] ?? "not-requested",
        preview: t["source-text"].split("\n")[0]!.slice(0, 120),
      };
    });
}

export interface TurnDetail {
  id: string;
  origin: "operator" | "agent";
  author: string;
  when: string;
  status: string;
  html: string;
  marks: Array<{ mark: string; intent: string; stage: string; text: string }>;
  fragments: Array<{ sentence: string; intent: string; target: string | null; rationale: string; patterns: string[] }>;
  notices: Notice[];
  labeller: string | null;
}

interface RecordWithMarks extends TurnRecord {
  author?: string;
  operator_id?: string;
  proforma_marks?: Array<{ mark: string; intent: string; stage: string; text: string }>;
}

/** Everything the detail panel shows for one turn. */
export function turnDetail(view: TurnView): TurnDetail {
  const record = view.record as RecordWithMarks;
  const analysis: Analysis | null = view.analysis;
  const origin = record.origin === "agent" ? "agent" : "operator";
  return {
    id: view.id,
    origin,
    author: record.author ?? record.operator_id ?? (origin === "agent" ? record.agent_id : "operator"),
    when: record.created_at,
    status: record.analysis_status ?? "not-requested",
    html: marksHtml(record.source_text, marksFor(record, analysis)),
    marks: (record.proforma_marks ?? []).map((m) => ({ mark: m.mark, intent: m.intent, stage: m.stage, text: m.text })),
    fragments: analysis
      ? analysis.sentences.flatMap((s) =>
          s.fragments.map((f) => ({
            sentence: s.id,
            intent: f.intent,
            target: f.target,
            rationale: f.rationale,
            patterns: (f.pattern_refs ?? []).map((p) => p.id),
          })),
        )
      : [],
    notices: view.notices ?? [],
    labeller: analysis?.labeller ?? null,
  };
}

export interface ObligationRow {
  id: string;
  kind: string;
  counterparty: string;
  due: string | null;
  status: string;
  deliverable: string;
}

interface ObligationsResponse {
  ok?: boolean;
  owes?: Array<Record<string, unknown>>;
  owed?: Array<Record<string, unknown>>;
  unchecked?: Array<Record<string, unknown>>;
  incomplete?: unknown[];
  "as-of"?: string;
  reason?: string;
}

function str(v: unknown): string {
  return v == null ? "" : typeof v === "string" ? v : JSON.stringify(v);
}

function row(o: Record<string, unknown>, counterpartyKey: "creditor" | "debtor"): ObligationRow {
  return {
    id: str(o["obligation/id"] ?? o.id),
    kind: str(o["source/kind"] ?? (o.agreement ? "agreement" : "promise")),
    counterparty: str(o[counterpartyKey]),
    due: o["due-at"] ? str(o["due-at"]) : null,
    status: str(o.status ?? "open"),
    deliverable: str(o.deliverable ?? o["fulfilment-criterion"] ?? ""),
  };
}

export interface ObligationsPane {
  asOf: string | null;
  owes: ObligationRow[];
  owed: ObligationRow[];
  unchecked: number;
  incomplete: number;
  error: string | null;
}

/** The obligations pane from GET /api/alpha/obligations?agent=…: what the agent owes and is owed. */
export function obligationsPane(status: number, body: ObligationsResponse | null): ObligationsPane {
  if (status !== 200 || !body) {
    return { asOf: null, owes: [], owed: [], unchecked: 0, incomplete: 0, error: body?.reason ?? (status ? `http ${status}` : "unreachable") };
  }
  return {
    asOf: body["as-of"] ?? null,
    owes: (body.owes ?? []).map((o) => row(o, "creditor")),
    owed: (body.owed ?? []).map((o) => row(o, "debtor")),
    unchecked: (body.unchecked ?? []).length,
    incomplete: (body.incomplete ?? []).length,
    error: null,
  };
}

export interface HealthPane {
  seat: string;
  state: string;
  detail: string;
  benched: string[];
  outstanding: number;
}

export function healthPane(body: { seat?: string; health?: { state?: string | null; detail?: string | null }; benched?: Record<string, number>; outstanding?: number } | null): HealthPane {
  return {
    seat: body?.seat ?? "?",
    state: body?.health?.state ?? "unknown",
    detail: body?.health?.detail ?? "",
    benched: Object.keys(body?.benched ?? {}),
    outstanding: body?.outstanding ?? 0,
  };
}

/** Which turn ids changed status or appeared since the last poll. */
export function changedTurns(before: TurnSummary[], after: TurnSummary[]): string[] {
  const prev = new Map(before.map((t) => [t.id, t["analysis-status"] ?? ""]));
  return after.filter((t) => prev.get(t.id) !== (t["analysis-status"] ?? "")).map((t) => t.id);
}
