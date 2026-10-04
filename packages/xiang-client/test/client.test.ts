import { test } from "node:test";
import assert from "node:assert/strict";
import { XiangClient } from "../src/client.js";
import { ACCEPTANCE_NO_EVIDENCE, acceptanceLine, undoOutcome } from "../src/lines.js";

interface Call {
  url: string;
  method?: string;
  body?: unknown;
}

function fakeFetch(responses: Array<[number, unknown]>): { calls: Call[]; fetch: (url: string, init?: RequestInit) => Promise<Response> } {
  const calls: Call[] = [];
  return {
    calls,
    fetch: async (url: string, init?: RequestInit) => {
      calls.push({ url, method: init?.method, body: init?.body ? JSON.parse(String(init.body)) : undefined });
      const [status, body] = responses.shift() ?? [500, { ok: false }];
      return new Response(typeof body === "string" ? body : JSON.stringify(body), { status });
    },
  };
}

test("the turn pipeline calls, in the order a REPL makes them", async () => {
  const f = fakeFetch([
    [201, { ok: true, id: "turn-abc", record: {}, redacted: [], dispatch: "pending" }],
    [200, { ok: true, dispatched: true, agent: "象-1", "job-id": "job-1" }],
    [200, { ok: true, id: "turn-abc", record: {}, analysis: null, candidates: null, notices: [] }],
    [200, { ok: true, outcome: "running" }],
    [200, { ok: true, turns: [] }],
  ]);
  const client = new XiangClient({ base: "http://h:7070/", fetch: f.fetch });
  const recorded = await client.recordTurn({ text: "yes", "agent-id": "claude-17", "session-id": "s", "turn-id": "t", "evidence-id": "ev" });
  assert.equal(recorded.status, 201);
  assert.equal(recorded.json?.id, "turn-abc");
  await client.happened("turn-abc", { reply: "ok", commits: [] });
  await client.getTurn("turn-abc");
  await client.reap("turn-abc", "象-1");
  await client.listTurns({ session: "s", limit: 5 });
  assert.deepEqual(
    f.calls.map((c) => [c.method ?? "GET", c.url]),
    [
      ["POST", "http://h:7070/api/alpha/xiang/turns"],
      ["POST", "http://h:7070/api/alpha/xiang/turns/turn-abc/happened"],
      ["GET", "http://h:7070/api/alpha/xiang/turns/turn-abc"],
      ["POST", "http://h:7070/api/alpha/xiang/turns/turn-abc/reap"],
      ["GET", "http://h:7070/api/alpha/xiang/turns?session=s&limit=5"],
    ],
  );
  assert.deepEqual(f.calls[0]!.body, { text: "yes", "agent-id": "claude-17", "session-id": "s", "turn-id": "t", "evidence-id": "ev" });
  assert.deepEqual(f.calls[3]!.body, { agent: "象-1" });
});

test("agreement and undo carry the Emacs payloads", async () => {
  const f = fakeFetch([
    [200, { ok: true, record: { id: "act:0123456789ab", "agreement/offer": "act:fedcba9876", "agreement/option-id": "2" }, grant: { id: "act:aaaaaaaa11", until: "2026-10-03" } }],
    [200, { ok: true, record: { id: "act:rev" }, "card-as-of": { active: { "pattern-id": "social/x" } } }],
  ]);
  const client = new XiangClient({ base: "http://h", fetch: f.fetch });
  const agreed = await client.agreement({ agent: "claude-17", session: "s", text: "yes 2", evidenceId: "ev" });
  assert.deepEqual(f.calls[0]!.body, { agent: "claude-17", session: "s", text: "yes 2", "evidence-id": "ev" });
  assert.equal(acceptanceLine(agreed), "✓ agreed: option 2 of act:fedcba98 (agreement act:01234567); grant act:aaaaaaaa until 2026-10-03");
  const undone = await client.undo({ agent: "claude-17", session: "s", effect: "act:w1", idempotencyKey: "k" });
  assert.deepEqual(f.calls[1]!.body, { caller: "joe", agent: "claude-17", session: "s", "idempotency-key": "k", effect: "act:w1" });
  assert.deepEqual(undoOutcome(undone), { consumed: true, line: "undo: social/x restored (reversal act:rev)" });
});

test("acceptance lines for the other outcomes", () => {
  assert.equal(acceptanceLine({ status: 409, json: { reason: "ambiguous" } }), "? yes was ambiguous; the agent will ask which");
  assert.equal(acceptanceLine({ status: 409, json: { reason: ":no-visible-offer" } }), "✗ yes not recorded: no-visible-offer");
  assert.equal(acceptanceLine({ status: 403, json: { reason: "not-operator" } }), "✗ yes not checked: http 403, not-operator");
  assert.equal(acceptanceLine({ status: 502, json: null }), "✗ yes not checked: http 502");
  assert.equal(acceptanceLine({ status: 0, json: null }), "✗ yes not checked: timeout");
  assert.equal(acceptanceLine({ status: 200, json: { record: {}, "grant-reason": "grant-write-failed" } }), "✓ agreed: option ? of ? (agreement ?); grant write failed");
  assert.equal(ACCEPTANCE_NO_EVIDENCE, "✗ yes not checked: no evidence id");
});

test("undo outcomes that are not consumed go to the agent unchanged", () => {
  assert.deepEqual(undoOutcome({ status: 409, json: { reason: "ambiguous", effects: ["act:1", "act:2"] } }), { consumed: true, line: "undo: ambiguous; name one effect: act:1, act:2" });
  assert.deepEqual(undoOutcome({ status: 409, json: { reason: "idempotency-conflict" } }), { consumed: false, line: null });
  assert.deepEqual(undoOutcome({ status: 422, json: { reason: "nothing-to-undo" } }), { consumed: false, line: null });
  assert.deepEqual(undoOutcome({ status: 0, json: null }), { consumed: false, line: null });
});

test("a network failure is status 0, never a throw", async () => {
  const client = new XiangClient({
    base: "http://h",
    fetch: async () => {
      throw new Error("ECONNREFUSED");
    },
  });
  const r = await client.promptLine("a", "s");
  assert.equal(r.status, 0);
  assert.equal(r.error, "ECONNREFUSED");
});
