/**
 * POST /api/alpha/invoke-stream as an async iterator of NDJSON events.
 *
 * The server writes one JSON object per line; the `done` event carries the
 * reply, the session id and, for `surface=emacs-repl`, the next prompt line.
 * A line that is not JSON is yielded as `{type: "raw", line}` rather than
 * dropped, so a broken stream is visible.
 */

import type { InvokeStreamEvent } from "./types.js";

export interface InvokeStreamRequest {
  "agent-id": string;
  prompt: string;
  caller?: string;
  surface?: string;
  "turn-id"?: string;
  "mission-id"?: string;
  "timeout-ms"?: number;
  [key: string]: unknown;
}

/** Split a stream of UTF-8 bytes into NDJSON events. */
export async function* parseNdjson(
  body: ReadableStream<Uint8Array> | AsyncIterable<Uint8Array | string>,
): AsyncGenerator<InvokeStreamEvent> {
  const decoder = new TextDecoder();
  let buffer = "";
  const emit = function* (chunk: string): Generator<InvokeStreamEvent> {
    buffer += chunk;
    let nl: number;
    while ((nl = buffer.indexOf("\n")) >= 0) {
      const line = buffer.slice(0, nl).trim();
      buffer = buffer.slice(nl + 1);
      if (line.length === 0) continue;
      yield parseLine(line);
    }
  };
  const iterable: AsyncIterable<Uint8Array | string> =
    "getReader" in body ? readerIterable(body as ReadableStream<Uint8Array>) : body;
  for await (const chunk of iterable) {
    yield* emit(typeof chunk === "string" ? chunk : decoder.decode(chunk, { stream: true }));
  }
  yield* emit(decoder.decode());
  const tail = buffer.trim();
  if (tail.length > 0) yield parseLine(tail);
}

function parseLine(line: string): InvokeStreamEvent {
  try {
    const parsed = JSON.parse(line);
    if (parsed && typeof parsed === "object" && typeof parsed.type === "string") return parsed as InvokeStreamEvent;
    return { type: "raw", line };
  } catch {
    return { type: "raw", line };
  }
}

async function* readerIterable(stream: ReadableStream<Uint8Array>): AsyncGenerator<Uint8Array> {
  const reader = stream.getReader();
  try {
    for (;;) {
      const { done, value } = await reader.read();
      if (done) return;
      if (value) yield value;
    }
  } finally {
    reader.releaseLock();
  }
}

export type FetchLike = (input: string, init?: RequestInit) => Promise<Response>;

/**
 * Send one operator turn and iterate its events. The caller reads text
 * events as they arrive and the `done` event last; `prompt-line` on `done`
 * is the next prompt to draw, when the server computed one.
 */
export async function* invokeStream(
  base: string,
  request: InvokeStreamRequest,
  fetchImpl: FetchLike = fetch,
  init: { signal?: AbortSignal } = {},
): AsyncGenerator<InvokeStreamEvent> {
  const response = await fetchImpl(`${base.replace(/\/$/, "")}/api/alpha/invoke-stream`, {
    method: "POST",
    headers: { "Content-Type": "application/json", Accept: "application/x-ndjson" },
    body: JSON.stringify(request),
    signal: init.signal,
  });
  if (!response.ok || !response.body) {
    let message = `http ${response.status}`;
    try {
      const text = await response.text();
      if (text) message += `: ${text.slice(0, 400)}`;
    } catch {
      /* the status is the message */
    }
    yield { type: "error", ok: false, error: "invoke-stream-failed", message, status: response.status };
    return;
  }
  yield* parseNdjson(response.body);
}
