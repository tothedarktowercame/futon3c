/**
 * The one-line outcomes the Emacs buffer inserted after an acceptance or an
 * undo, ported from `agent-chat--report-acceptance` and
 * `agent-chat--maybe-handle-undo`. Short ids, as the buffer showed them; the
 * next turn's header carries the full ones.
 */

import type { AgreementResponse, HttpResult, UndoResponse } from "./types.js";

/** `act:1234abcd…` → `act:1234abcd` (agent-chat--short-act). */
export function shortAct(id: unknown): string {
  if (typeof id !== "string" || id.length === 0) return "?";
  const m = /^(act:[0-9a-fA-F]{8})/.exec(id);
  return m ? m[1]! : id.length > 12 ? id.slice(0, 12) : id;
}

function reasonText(body: AgreementResponse | UndoResponse | null): string | null {
  const reason = body?.reason;
  if (typeof reason !== "string") return null;
  return reason.replace(/^:/, "");
}

/** The status line for an agreement response. */
export function acceptanceLine(result: HttpResult<AgreementResponse>): string {
  const { status, json } = result;
  if (status === 200) {
    const record = json?.record ?? {};
    const grant = json?.grant ?? null;
    let line = `✓ agreed: option ${record["agreement/option-id"] ?? "?"} of ${shortAct(record["agreement/offer"])} (agreement ${shortAct(record.id)})`;
    if (grant) line += `; grant ${shortAct(grant.id)} until ${grant.until ?? "?"}`;
    else if (json?.["grant-reason"] === "grant-write-failed") line += "; grant write failed";
    return line;
  }
  if (status === 409 && reasonText(json) === "ambiguous") return "? yes was ambiguous; the agent will ask which";
  if (status === 409) return `✗ yes not recorded: ${reasonText(json) ?? "conflict"}`;
  if (status && status !== 0 && reasonText(json)) return `✗ yes not checked: http ${status}, ${reasonText(json)}`;
  if (status && status !== 0) return `✗ yes not checked: http ${status}`;
  return "✗ yes not checked: timeout";
}

/** Shown when an acceptance could not be sent for want of an evidence id. */
export const ACCEPTANCE_NO_EVIDENCE = "✗ yes not checked: no evidence id";

export interface UndoOutcome {
  /** True when the undo was handled here; false means send the text to the agent unchanged. */
  consumed: boolean;
  line: string | null;
}

/**
 * HTTP failures, timeouts and :nothing-to-undo deliberately leave the turn
 * unconsumed so it continues through the ordinary agent send path.
 */
export function undoOutcome(result: HttpResult<UndoResponse>): UndoOutcome {
  const { status, json } = result;
  if (status === 200) {
    const pattern = json?.["card-as-of"]?.active?.["pattern-id"] ?? "pattern card";
    const reversal = json?.record?.id ?? "unknown";
    return { consumed: true, line: `undo: ${pattern} restored (reversal ${reversal})` };
  }
  if (status === 409 && reasonText(json) === "ambiguous") {
    return { consumed: true, line: `undo: ambiguous; name one effect: ${(json?.effects ?? []).join(", ")}` };
  }
  return { consumed: false, line: null };
}
