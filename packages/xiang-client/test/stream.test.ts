import { test } from "node:test";
import assert from "node:assert/strict";
import { invokeStream, parseNdjson } from "../src/stream.js";

async function* chunks(parts: string[]): AsyncGenerator<Uint8Array> {
  const enc = new TextEncoder();
  for (const p of parts) yield enc.encode(p);
}

async function collect<T>(it: AsyncIterable<T>): Promise<T[]> {
  const out: T[] = [];
  for await (const x of it) out.push(x);
  return out;
}

test("NDJSON lines split across chunks, and a multibyte character split mid-chunk", async () => {
  const events = await collect(
    parseNdjson(chunks(['{"type":"text","text":"he', 'llo 象"}\n{"type":"tool_use","tools":["Read"]}\n', '{"type":"done","ok":true,"result":"r","session-id":"s","prompt-line":"$> "}'])),
  );
  assert.deepEqual(events, [
    { type: "text", text: "hello 象" },
    { type: "tool_use", tools: ["Read"] },
    { type: "done", ok: true, result: "r", "session-id": "s", "prompt-line": "$> " },
  ]);
});

test("a multibyte character split across byte chunks still decodes", async () => {
  const bytes = new TextEncoder().encode('{"type":"text","text":"象"}\n');
  async function* split(): AsyncGenerator<Uint8Array> {
    yield bytes.slice(0, 18);
    yield bytes.slice(18);
  }
  const events = await collect(parseNdjson(split()));
  assert.deepEqual(events, [{ type: "text", text: "象" }]);
});

test("a line that is not JSON is yielded as raw, not dropped", async () => {
  const events = await collect(parseNdjson(chunks(["garbage\n", '{"no":"type"}\n'])));
  assert.deepEqual(events, [
    { type: "raw", line: "garbage" },
    { type: "raw", line: '{"no":"type"}' },
  ]);
});

test("invokeStream posts the turn and reads the body as a ReadableStream", async () => {
  let seen: { url: string; init?: RequestInit } | null = null;
  const fetchImpl = async (url: string, init?: RequestInit): Promise<Response> => {
    seen = { url, init };
    const body = new ReadableStream<Uint8Array>({
      start(controller) {
        controller.enqueue(new TextEncoder().encode('{"type":"text","text":"x"}\n{"type":"done","ok":true,"result":"x","session-id":"s"}\n'));
        controller.close();
      },
    });
    return new Response(body, { status: 200, headers: { "Content-Type": "application/x-ndjson" } });
  };
  const events = await collect(invokeStream("http://h:7070/", { "agent-id": "claude-17", prompt: "hi", surface: "web" }, fetchImpl));
  assert.equal(events.length, 2);
  assert.equal(events[1]!.type, "done");
  assert.equal(seen!.url, "http://h:7070/api/alpha/invoke-stream");
  assert.deepEqual(JSON.parse(String(seen!.init!.body)), { "agent-id": "claude-17", prompt: "hi", surface: "web" });
});

test("an HTTP failure is one error event", async () => {
  const fetchImpl = async (): Promise<Response> => new Response('{"ok":false,"err":"missing-agent-id"}', { status: 400 });
  const events = await collect(invokeStream("http://h", { "agent-id": "", prompt: "x" }, fetchImpl));
  assert.equal(events.length, 1);
  assert.equal(events[0]!.type, "error");
  assert.match(String((events[0] as { message: string }).message), /http 400: .*missing-agent-id/);
});
