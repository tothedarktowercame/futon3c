/**
 * Shapes of the 象 (M-象-2000) turn pipeline, as the seam declares them.
 *
 * `TurnRecord` follows `packages/turn-seam/turn-record.schema.json` field for
 * field; the analysis shape follows what `session_turn_analysis.py complete`
 * (and now `futon3c.xiang.turn-record/validate-analysis`) publishes beside it.
 *
 * Every offset in these shapes is in Unicode codepoints, zero-based and
 * end-exclusive. JavaScript strings index UTF-16 code units, so a client must
 * convert at its boundary (see offsets.ts) and never add a codepoint offset
 * to a string index.
 */

export const OFFSET_UNIT = "unicode-codepoints-zero-based-end-exclusive" as const;

export type SentenceStatus = "unresolved" | "cue-only" | "analyzed";
export type AnalysisStatus = "requested" | "not-requested" | "analyzed" | "refused" | "failed" | "drafted";

export interface Cue {
  start: number;
  end: number;
  label: string;
  text: string;
  /** How the cue was found, e.g. "literal-phrase". */
  method: string;
}

export interface Sentence {
  id: string;
  start: number;
  end: number;
  /** Must equal source_text[start:end] in codepoints. */
  text: string;
  status: SentenceStatus;
  cues: Cue[];
}

export interface DispatchOutcome {
  state?: string;
  reason?: string;
}

export interface AnalysisDispatch {
  job_id?: string;
  outcome?: DispatchOutcome;
  attempts?: Array<{ job_id?: string; outcome?: DispatchOutcome }>;
}

export interface WithdrawalEffect {
  fragment_id: string;
  status: number;
  reason?: string | null;
  effect_id?: string | null;
  idempotency_key: string;
  header_notice_published_at?: string;
  header_notice_attempts?: number;
  header_notice_give_up_reason?: string;
}

export interface NegationInterpretation {
  fragment_id: string;
  status: number;
  evidence_id?: string | null;
  resolution?: string | null;
  reason?: string | null;
}

export interface TurnRecord {
  version: 1;
  source_text: string;
  original_text?: string;
  quotes?: string[];
  offset_unit: typeof OFFSET_UNIT;
  sentences: Sentence[];
  unmatched: string[];
  created_at: string;
  agent_id: string;
  session_id: string;
  turn_id: string;
  surface?: string;
  origin?: string;
  evidence_id?: string | null;
  vocabulary_version?: number;
  interpretation_version?: number;
  secrets_redacted?: string[];
  tagging_failed?: boolean;
  analysis_status?: AnalysisStatus;
  analysis_file?: string;
  analysis_dispatch?: AnalysisDispatch;
  happened_summary?: string;
  withdrawal_effects?: WithdrawalEffect[];
  negation_interpretations?: NegationInterpretation[];
}

export interface DisplayCue {
  start: number;
  end: number;
  text: string;
}

export interface PatternRef {
  id: string;
  rationale: string;
  status?: string;
  source_sha256?: string;
}

export interface PatternRejection {
  id: string;
  reason: string;
  query?: string;
}

export interface Fragment {
  start: number;
  end: number;
  text: string;
  intent: string;
  target: string | null;
  rationale: string;
  relations: string[];
  display_cues: DisplayCue[];
  no_surface_cue?: string;
  pattern_refs?: PatternRef[];
  pattern_rejections?: PatternRejection[];
  rnode?: Record<string, unknown>;
  /** How this fragment relates to 小象's draft; set by the server at publish. */
  basis?: FragmentBasis;
}

export interface AnalysedSentence {
  id: string;
  fragments: Fragment[];
  unresolved_reason?: string;
}

export interface Analysis {
  version: number;
  status: "analyzed";
  method?: string;
  labeller: string;
  source_text: string;
  source_sha256?: string;
  offset_unit: typeof OFFSET_UNIT;
  sentences: AnalysedSentence[];
  reusable_cues?: Array<DisplayCue & { intent: string; rationale: string }>;
  created_at?: string;
  evidence_id?: string | null;
  interpretation_version?: number;
  vocabulary_version?: number;
  human_approved?: boolean;
  draft_agreement?: DraftAgreement | null;
}

/** The analysis a delegate submits; the server canonicalises it. */
export interface AnalysisSubmission {
  labeller: string;
  sentences: AnalysedSentence[];
  reusable_cues?: Array<DisplayCue & { intent: string; rationale: string }>;
  rnode_cues?: unknown[];
}

export interface DraftFragment {
  start: number;
  end: number;
  text: string;
  /** 小象's intent when it was sure, else null with two guesses. */
  intent: string | null;
  sure: boolean;
  guesses: string[];
  precision?: number;
  sentence?: string | null;
}

/** 小象's classical draft, published beside the record before 象 reads. */
export interface Draft {
  version: 1;
  status: "drafted";
  labeller: string;
  source_text: string;
  offset_unit: typeof OFFSET_UNIT;
  created_at?: string;
  fragments: DraftFragment[];
}

export type FragmentBasis = "xiaoxiang" | "xiang-relabelled" | "xiang-resegmented" | "xiang";

export interface DraftAgreement {
  agreed: number;
  relabelled: number;
  resegmented: number;
  new: number;
  dropped: number;
  unsure: number;
  draft_fragments: number;
  published_fragments: number;
}

export interface Notice {
  kind: "effect" | "no-grant" | "unresolved";
  text: string;
  effect_id?: string;
  fragment_id?: string;
}

export interface PatternCandidate {
  id: string;
  score: number;
  title: string;
  context?: string;
  conclusion?: string;
}

export interface TurnView {
  ok: true;
  id: string;
  record: TurnRecord;
  draft?: Draft | null;
  /** BM25 hits per fragment query, precomputed before dispatch. */
  pattern_candidates?: Record<string, PatternCandidate[]> | null;
  analysis: Analysis | null;
  candidates: unknown | null;
  notices: Notice[];
}

export interface TurnSummary {
  id: string;
  "turn-id": string;
  "agent-id": string;
  "session-id": string;
  "created-at": string;
  surface?: string;
  "analysis-status"?: AnalysisStatus;
  "job-id"?: string;
  "source-text": string;
}

export interface RecordTurnRequest {
  text: string;
  "agent-id": string;
  "session-id": string;
  "turn-id": string;
  "evidence-id"?: string;
  "original-text"?: string;
  surface?: string;
  failed?: boolean;
  origin?: string;
  /** "later" (default) dispatches at `happened`; "now" dispatches at once. */
  dispatch?: "now" | "later";
}

export interface RecordTurnResponse {
  ok: boolean;
  id: string;
  record: TurnRecord;
  redacted: string[];
  dispatch: "pending" | "skipped" | DispatchResult;
}

export interface DispatchResult {
  dispatched: boolean;
  agent?: string;
  "job-id"?: string;
  reason?: string;
}

export interface HappenedRequest {
  summary?: string;
  reply?: string;
  commits?: Array<{ repo?: string; "repo-path"?: string; sha?: string; subject?: string; numstat?: string }>;
}

export interface ReapResult {
  ok: boolean;
  outcome: string;
  state?: string;
  reason?: string;
  [key: string]: unknown;
}

/** One NDJSON line of POST /api/alpha/invoke-stream. */
export type InvokeStreamEvent =
  | { type: "text"; text: string; "turn-id"?: string }
  | { type: "tool_use"; tools: string[]; "turn-id"?: string }
  | {
      type: "done";
      ok: true;
      result: string;
      "session-id": string;
      "prompt-line"?: string;
      "prompt-line-wait-ms"?: number;
      "turn-id"?: string;
      [key: string]: unknown;
    }
  | { type: "error"; ok?: false; error?: string; message?: string; [key: string]: unknown }
  | { type: string; [key: string]: unknown };

export interface AgreementRecord {
  id?: string;
  "agreement/offer"?: string;
  "agreement/option-id"?: string;
  [key: string]: unknown;
}

export interface AgreementResponse {
  ok?: boolean;
  reason?: string;
  record?: AgreementRecord;
  grant?: { id?: string; until?: string } | null;
  "grant-reason"?: string;
  [key: string]: unknown;
}

export interface UndoResponse {
  ok?: boolean;
  reason?: string;
  effects?: string[];
  record?: { id?: string };
  "card-as-of"?: { active?: { "pattern-id"?: string } };
  [key: string]: unknown;
}

export interface HttpResult<T> {
  /** 0 when the request never reached the server (network error or timeout). */
  status: number;
  json: T | null;
  error?: string;
}
