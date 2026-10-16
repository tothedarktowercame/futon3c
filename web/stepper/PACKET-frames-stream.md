# Packet: the frames cache pushes to the browser — `frames_stream` and `GET /api/alpha/xiang/frames/stream`

From claude-16, 2026-10-16. Repo: /home/joe/code/futon3c, branch master, work in place. Leave the untracked `holes/labs/M-象-2000/data/moves-pilot/*` files alone (another seat's). Never amend, never stash. Do not load anything into the running JVM; claude-16 reloads from master after review.

Second of three packets. The first (`PACKET-frames-cache.md`, commits 5b74bb283 and ab0d78f04) made `GET /api/alpha/xiang/frames` answer from `futon3c.xiang.frames-cache/!cache`, rebuilt only when the turn service records or settles a turn in that session. This packet lets a page learn of a rebuild without asking. The third packet (not this one) changes `web/stepper/*` to subscribe instead of polling.

## Goal

A page opens one `GET /api/alpha/xiang/frames/stream?session=S&limit=N` and receives server-sent events (SSE): the current frame set once on connect (unless it already has that version), then a new frame set each time the cache's `:version` for S changes, and a status event when a rebuild fails. Nothing in this path reads futon1b: the stream only reports what the cache already holds.

Why SSE and not a websocket: every websocket upgrade reaching this JVM is routed to the Agency mesh handler before any path is looked at, so a stepper websocket would mean editing that shared routing. A one-way event stream needs none of that, passes through Caddy's existing `GET /wip/stepper/api/alpha/xiang/*` proxy as it stands (Caddy flushes `text/event-stream` responses immediately by default), and the browser's `EventSource` reconnects it by itself, sending `Last-Event-ID`.

## Design

### New namespace `src/futon3c/xiang/frames_stream.clj` — all the logic, no http-kit

```clojure
(defn frames-payload [session limit entry])
;; the same map the GET route returns for a 200 (see handle-xiang-frames in http.clj:
;; :ok :session :limit :count :frames (newest N) :version :built-at :warnings, plus
;; :last-error when present), minus :elapsed-ms and :cached.

(defn sse-event [event-name id data])
;; -> string: "event: <name>\nid: <id>\ndata: <json on one line>\n\n". id may be nil (no id line).

(defn subscribe! [session {:keys [limit last-event-id send! on-missing]} ])
;; Register a watch on frames-cache/!cache keyed [::stream <unique>].
;; On call: let entry = (get @!cache session).
;;   - entry has :frames and (:version entry) differs from last-event-id (a parsed long, or nil):
;;     (send! (sse-event "frames" (:version entry) (frames-payload session limit entry)))
;;   - entry has :frames and version equals last-event-id: send nothing (the reconnecting
;;     browser already has it).
;;   - entry absent or without :frames: (send! (sse-event "status" nil {:state "building" :session session}))
;;     and call (on-missing) if given; the route uses that to start the first build (below).
;; On each atom change where (get-in new [session :version]) differs from old's: send a "frames"
;; event with the new entry. When the version is unchanged but (get-in new [session :last-error :at])
;; differs: send (sse-event "status" nil {:state "rebuild-failed" :last-error ...}).
;; Returns a zero-arg unsubscribe fn that removes the watch. send! exceptions are caught and
;; cause unsubscribe (a closed channel must not keep a watch alive).

(defn keepalive [] ": keepalive\n\n")
```

The watch fn runs on whichever thread swapped the atom (a rebuild thread, or the request thread of a first build), so it must do nothing slow: build the string, call `send!`, return.

### `http.clj`

- In `handle-xiang`: `(and (= :get method) (= "/api/alpha/xiang/frames/stream" uri))` → `handle-xiang-frames-stream`. Validate `session` as `handle-xiang-frames` does (same 400 shapes). `limit` as there. `last-event-id` from the `Last-Event-ID` request header (ring lowercases header names: `(get-in request [:headers "last-event-id"])`), parsed with `parse-long`, nil if absent or unparseable.
- `hk/as-channel request {:on-open ... :on-close ...}` (pattern: the whistle stream at ~line 7093 and the agent stream at ~6849 of http.clj). In `:on-open`:
  1. `(hk/send! channel {:status 200 :headers {"Content-Type" "text/event-stream; charset=utf-8" "Cache-Control" "no-cache" "X-Accel-Buffering" "no"} :body (frames-stream/sse-event "hello" nil {:session session})} false)` — the headers go with the first send, `false` keeps the channel open.
  2. `(frames-stream/subscribe! session {:limit limit :last-event-id id :send! #(hk/send! channel % false) :on-missing #(future (frames-cache/frames! session (frames-cache-opts)))})`. The first build (if any) runs in that future; its completion changes the atom and the watch delivers the frames. The connecting thread never blocks on a build.
  3. A keepalive every 25 s: one shared `defonce` `ScheduledThreadPoolExecutor` (1 thread, daemon) for all stream channels; `scheduleAtFixedRate` sending `(frames-stream/keepalive)`; keep the returned `ScheduledFuture` so `:on-close` cancels it. Caddy's proxy and browsers close idle streams otherwise.
- `:on-close`: call the unsubscribe fn and cancel the keepalive. Store both in atoms created in the handler so `:on-close` can reach them whichever order http-kit calls things in.
- Nothing else in `handle-xiang-frames` changes.

## Acceptance

Tests, no http-kit channel and no python:

- `test/futon3c/xiang/frames_stream_test.clj` (new; reset `frames-cache` before and after each test; reuse the hand-rolled scheduler harness idea from `frames_cache_test.clj`): (a) a subscriber to a cached session receives one `frames` event on subscribe whose `id:` is the version and whose data has the newest `limit` frames; (b) with `:last-event-id` equal to the current version it receives nothing on subscribe; (c) after `invalidate!` + running the scheduled rebuild it receives exactly one `frames` event with the new version, and three invalidations coalesced into one rebuild give one event, not three; (d) a rebuild returning `:ok false` gives one `status` event with `:state "rebuild-failed"` and no `frames` event; (e) a session not in the cache gives a `status` `building` event and calls `on-missing` once; when the build then lands (call `frames!` with the stub runner) the subscriber gets the `frames` event; (f) after unsubscribe, a rebuild sends nothing and the atom's watch map (`(.getWatches frames-cache/!cache)`) no longer has the key; (g) a `send!` that throws unsubscribes (the next change sends nothing); (h) `sse-event` output is byte-exact for a small map: `"event: frames\nid: 3\ndata: {\"ok\":true}\n\n"`, and a newline inside the JSON string value stays escaped (one `data:` line).
- `test/futon3c/transport/xiang_frames_http_test.clj` (extend): the stream path with no `session` returns the same 400 as the frames route (this part of the handler runs before `as-channel`, so it is plain ring). Do not try to test `as-channel` itself.

Gates: `clj-kondo --lint` on every changed `.clj` (0 errors, 0 warnings), `emacs --batch -l /home/joe/code/futon4/dev/check-parens.el <files>` → `OK`, and
`clojure -M:test -n futon3c.xiang.frames-stream-test -n futon3c.xiang.frames-cache-test -n futon3c.transport.xiang-frames-http-test`
green. Commit on master, message starting `frames stream:`. Then bell claude-16 back with the summary, the commit sha, and the test command's last lines (`scripts/agency_send.py --from codex-23 --to claude-16 --kind bell`).

Out of scope: `web/stepper/*` (packet 3), `scripts/turn_frames.py`, Caddy, the turn-service seam, any change to how often the cache rebuilds.
