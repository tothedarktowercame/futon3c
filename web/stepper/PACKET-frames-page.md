# Packet: the stepper page subscribes to the frames stream — no timer

From claude-16, 2026-10-16. Repo: /home/joe/code/futon3c, branch master, work in place. Leave the untracked `holes/labs/M-象-2000/data/moves-pilot/*` files alone (another seat's). Never amend, never stash. No Clojure changes in this packet; do not load anything into the running JVM.

Third of three packets. The first (`PACKET-frames-cache.md`) made `GET /api/alpha/xiang/frames` answer from a JVM cache rebuilt only on turn events. The second (`PACKET-frames-stream.md`, commits af8d2cfa7, 0fc2a21f2, 33c2d52a3) added `GET /api/alpha/xiang/frames/stream?session=S&limit=N`, a server-sent-event stream that sends the current frame set on connect and a new set whenever the cache version for S changes. It is live on :7070 now; try it:

    curl -N -m 8 "localhost:7070/api/alpha/xiang/frames/stream?session=00e653e3-28f7-4add-8304-47ca0475b5c2&limit=2"

You get `event: hello` with `data: {"session":...}`, then `event: frames` with `id: <cache version>` and `data:` the same JSON object the GET route returns (`ok, session, limit, count, frames, version, built-at, warnings`, plus `last-error` when a rebuild has failed), then `: keepalive` comments every 25 seconds. Other events: `event: status` with `data: {"state":"building","session":...}` when the cache has nothing yet for S (the first build is running; a `frames` event follows), and `{"state":"rebuild-failed","last-error":{...}}` when a rebuild failed (the old frames stand). The browser's `EventSource` reconnects by itself and sends `Last-Event-ID`; the server then sends nothing on connect if the version is unchanged.

## Goal

`web/stepper/index.html` opens one `EventSource` for the session and the page follows the cache from then on. The 30-second `setInterval` refetch in `stepper.ts` goes, and the `poll` URL parameter and its help text go with it. `g` stays: it re-reads the cache route once (`GET /api/alpha/xiang/frames`, a cache read, cheap) for the person who wants to be sure. Fixture mode (no `session`, no `agent`) is unchanged: no stream, no timer.

Why: Joe's standing rule for served pages (2026-10-16): the JVM caches, rebuilds on the producing event, and pushes to the browser; a page never refetches on a timer.

## Design

### `stepper.ts`

- `renderStepper(initial, host, opts)`: `opts.pollMs` is removed. New optional `opts.subscribe?: (handlers: {onFrames: (frames: Frame[]) => void; onStatus: (status: StreamStatus) => void}) => () => void`. In live mode with `subscribe` given, call it exactly once after the first `draw()`; keep the returned unsubscribe fn (nothing calls it yet; a later packet may on page hide). Never call `setInterval`.
- Export two pure functions so the behaviour is testable in node without a DOM:

```ts
export interface StreamStatus { state: "building" | "rebuild-failed" | "reconnecting" | "live"; detail?: string }
export function parseStreamEvent(name: string, data: string):
  { kind: "frames"; frames: Frame[]; version: number | null } | { kind: "status"; status: StreamStatus } | { kind: "ignore" }
// "frames" -> JSON.parse(data).frames and .version; "status" -> state from the JSON, detail from
// last-error (its :reason or :err, whichever is present, as text); anything else ("hello") -> ignore.
// Malformed JSON -> {kind: "status", status: {state: "rebuild-failed", detail: "bad event: " + first 80 chars}}; never throws.

export function receiveFrames(state: {frames: Frame[]; current: number}, incoming: Frame[]):
  {frames: Frame[]; current: number}
// frames = readyFrames(incoming, true). If state.current was the newest frame (>= state.frames.length - 1),
// current follows the newest of the new set; otherwise current stays, clamped into the new range.
// Empty incoming with a non-empty state keeps the state unchanged (a stream never replaces frames with nothing).
```

- `onFrames` applies `receiveFrames` to the live `{frames, current}`, sets the `refreshed` stamp (`· pushed HH:MM:SSZ`), and draws. `onStatus` appends the state to the status line (`· building`, `· rebuild failed: <detail>`, `· reconnecting`, `· live`) without redrawing the frame.
- `refresh()` (the `g` key) keeps using `opts.refetch` as now, but goes through `receiveFrames` too, so there is one place that decides where the cursor lands.

### `index.html`

- Keep `load()` for the first paint (one cache read; the page needs frames before it can render). Then pass `subscribe` to `renderStepper` when `session` is set:

```js
const subscribe = ({onFrames, onStatus}) => {
  const source = new EventSource(`${base}/api/alpha/xiang/frames/stream?session=${encodeURIComponent(session)}&limit=${encodeURIComponent(limit)}`);
  const relay = event => { const parsed = parseStreamEvent(event.type, event.data);
    if (parsed.kind === "frames") onFrames(parsed.frames); else if (parsed.kind === "status") onStatus(parsed.status); };
  for (const name of ["frames", "status", "hello"]) source.addEventListener(name, relay);
  source.onopen = () => onStatus({state: "live"});
  source.onerror = () => onStatus({state: "reconnecting"});   // EventSource retries by itself
  return () => source.close();
};
```

  The first `frames` event after connect repeats what `load()` fetched; applying it is harmless (same set, cursor unchanged by `receiveFrames`).
- Remove the `poll` parameter and the sentence about refetching every `poll` seconds. The help line becomes: `n / → next · p / ← previous · Home / End · g re-read.` and the paragraph says the page follows the session live and `g` re-reads once.
- Import `parseStreamEvent` alongside `renderStepper`.

## Acceptance

- `npx -p typescript tsc --noEmit --strict --target es2022 --module es2022 --moduleResolution bundler web/stepper/stepper.ts` clean; then emit `stepper.js` beside it with the same flags minus `--noEmit` and commit it (compiled output is checked in, no bundler).
- `node web/stepper/test_stepper.mjs` passes, with these added over the fixture (`fixtures/frames-claude-16-00e653e3.json`): (a) `receiveFrames` with `current` on the newest of N frames and N+1 incoming lands on index N; with `current` 2 of N and N+1 incoming stays at 2; with `current` 5 of 7 and 3 incoming clamps to 2; with empty incoming the state is returned unchanged; a trailing `missing` frame in incoming is kept (live mode) and a trailing `missing` plus `drafted-provisional` both kept, exactly as `readyFrames(…, true)` does. (b) `parseStreamEvent("frames", <JSON of {ok:true, frames: fixture, version: 29}>)` gives kind `frames`, the fixture's length, version 29; `parseStreamEvent("status", '{"state":"rebuild-failed","last-error":{"reason":"script-failed","at":"…"}}')` gives state `rebuild-failed` and detail `script-failed`; `parseStreamEvent("hello", '{"session":"x"}')` gives `ignore`; `parseStreamEvent("frames", "{not json")` gives a status, not a throw. (c) The compiled `stepper.js` contains no `setInterval` and `index.html` contains neither `setInterval` nor `poll` (read both files in the test and assert on the text; this is the removal the packet is for).
- The existing assertions keep passing unchanged.
- `renderStepper`'s DOM path is not unit-tested here (no DOM in node); claude-16 checks it in a browser after review: one pending `frames/stream` request in the network panel, no periodic `frames` requests, and a settled turn appearing without a reload.

## Gates

TypeScript: the `tsc --noEmit --strict` line above. Tests: `node web/stepper/test_stepper.mjs`. No Clojure or Lisp touched, so clj-kondo and check-parens do not apply; say so in the bellback. Commit as one or two commits on master. Bell claude-16 back with a summary, the commit shas, and the test output.
