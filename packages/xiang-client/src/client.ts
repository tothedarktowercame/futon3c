/**
 * The futon3c routes the 象 frontend uses, as one client.
 *
 * Nothing here touches a filesystem or a subprocess: the record, the
 * dispatch, the reap and the publication happen on the server
 * (`futon3c.xiang.turn-service`, under /api/alpha/xiang/), and the
 * agreement, undo and prompt-line routes are the ones agent-chat.el called.
 *
 * Every method returns `{status, json}` and never throws on an HTTP status:
 * the Emacs code treated a 409 and a 422 as answers, and so does this.
 * Status 0 means the request never reached the server.
 */

import type {
  AgreementResponse,
  AnalysisSubmission,
  DispatchResult,
  HappenedRequest,
  HttpResult,
  ReapResult,
  RecordTurnRequest,
  RecordTurnResponse,
  TurnSummary,
  TurnView,
  UndoResponse,
} from "./types.js";
import type { FetchLike } from "./stream.js";

export interface XiangClientOptions {
  /** e.g. "http://localhost:7070" */
  base: string;
  fetch?: FetchLike;
  /** Per-request timeout in milliseconds (agent-chat used 3 s for most of these). */
  timeoutMs?: number;
}

export interface PromptLine {
  "prompt-line"?: string;
  [key: string]: unknown;
}

export class XiangClient {
  private readonly base: string;
  private readonly fetchImpl: FetchLike;
  private readonly timeoutMs: number;

  constructor(options: XiangClientOptions) {
    this.base = options.base.replace(/\/$/, "");
    this.fetchImpl = options.fetch ?? fetch;
    this.timeoutMs = options.timeoutMs ?? 10_000;
  }

  // -- the turn pipeline (server-side session-turn-analysis) ---------------

  /** Record an operator turn at send time (session-mode--record-turn). */
  recordTurn(request: RecordTurnRequest): Promise<HttpResult<RecordTurnResponse>> {
    return this.request("POST", "/api/alpha/xiang/turns", request);
  }

  /**
   * At reply end, attach what the agent did and dispatch the turn to 象
   * (session-mode--dispatch-analysis-after-reply).
   */
  happened(id: string, happened: HappenedRequest): Promise<HttpResult<DispatchResult & { ok: boolean }>> {
    return this.request("POST", `/api/alpha/xiang/turns/${encodeURIComponent(id)}/happened`, happened);
  }

  /** The record with its analysis, candidates and notices. */
  getTurn(id: string): Promise<HttpResult<TurnView>> {
    return this.request("GET", `/api/alpha/xiang/turns/${encodeURIComponent(id)}`);
  }

  listTurns(query: { session?: string; agent?: string; limit?: number } = {}): Promise<HttpResult<{ ok: boolean; turns: TurnSummary[] }>> {
    const params = new URLSearchParams();
    if (query.session) params.set("session", query.session);
    if (query.agent) params.set("agent", query.agent);
    if (query.limit) params.set("limit", String(query.limit));
    const qs = params.toString();
    return this.request("GET", `/api/alpha/xiang/turns${qs ? `?${qs}` : ""}`);
  }

  /** Ask what became of the dispatch (turn_dispatch_reap.py --apply). */
  reap(id: string, agent?: string): Promise<HttpResult<ReapResult>> {
    return this.request("POST", `/api/alpha/xiang/turns/${encodeURIComponent(id)}/reap`, agent ? { agent } : {});
  }

  /** Publish an interpretation (session_turn_analysis.py complete). */
  publishAnalysis(id: string, analysis: AnalysisSubmission): Promise<HttpResult<{ ok: boolean; id: string; path: string }>> {
    return this.request("POST", `/api/alpha/xiang/turns/${encodeURIComponent(id)}/analysis`, analysis);
  }

  health(): Promise<HttpResult<{ ok: boolean; health: unknown; seat: string }>> {
    return this.request("GET", "/api/alpha/xiang/health");
  }

  // -- the routes agent-chat.el called directly -----------------------------

  /** Record a classical acceptance backed by the operator turn's evidence id. */
  agreement(args: { agent: string; session: string; text: string; evidenceId: string }): Promise<HttpResult<AgreementResponse>> {
    return this.request("POST", "/api/alpha/agreement", {
      agent: args.agent,
      session: args.session,
      text: args.text,
      "evidence-id": args.evidenceId,
    });
  }

  /** Reverse one visible provisional withdrawal; `effect` names it when the operator did. */
  undo(args: { agent: string; session: string; effect?: string; idempotencyKey?: string }): Promise<HttpResult<UndoResponse>> {
    const payload: Record<string, unknown> = {
      caller: "joe",
      agent: args.agent,
      session: args.session,
      "idempotency-key": args.idempotencyKey ?? `web-undo:${args.session}:${Date.now()}`,
    };
    if (args.effect) payload.effect = args.effect;
    return this.request("POST", "/api/alpha/withdrawal/undo", payload);
  }

  promptLine(agent: string, session: string): Promise<HttpResult<PromptLine>> {
    const params = new URLSearchParams({ agent, session });
    return this.request("GET", `/api/alpha/prompt-line?${params}`);
  }

  // -------------------------------------------------------------------------

  private async request<T>(method: "GET" | "POST", path: string, body?: unknown): Promise<HttpResult<T>> {
    const controller = typeof AbortController === "function" ? new AbortController() : null;
    const timer = controller ? setTimeout(() => controller.abort(), this.timeoutMs) : null;
    try {
      const response = await this.fetchImpl(`${this.base}${path}`, {
        method,
        headers: body === undefined ? { Accept: "application/json" } : { "Content-Type": "application/json", Accept: "application/json" },
        body: body === undefined ? undefined : JSON.stringify(body),
        signal: controller?.signal,
      });
      const text = await response.text();
      let json: T | null = null;
      try {
        json = text ? (JSON.parse(text) as T) : null;
      } catch {
        json = null;
      }
      return { status: response.status, json };
    } catch (error) {
      return { status: 0, json: null, error: error instanceof Error ? error.message : String(error) };
    } finally {
      if (timer) clearTimeout(timer);
    }
  }
}
