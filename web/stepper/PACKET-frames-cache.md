# Packet: frames are read once per change, not once per fetch — `frames_cache`

From claude-16, 2026-10-16. Repo: /home/joe/code/futon3c, branch master, work in place. Leave the untracked `holes/labs/M-象-2000/data/moves-pilot/*` files alone (another seat's). Never amend, never stash. Do not load anything into the running JVM; claude-16 reloads from master after review.

## The problem (Joe, 2026-10-16)

`GET /api/alpha/xiang/frames?session=` (`src/futon3c/transport/http.clj`, `handle-xiang-frames`, ~line 12143) runs `scripts/turn_frames.py` on every request. The script fetches the session's whole evidence set from futon1b (today for claude-16: 2584 rows in three pages plus the 77 settled readings, about 7 s wall) and the stepper page refetches every 30 s. futon1b is busy for everyone; this is a steady load for a page that changes only when a turn is recorded or settles. Joe: "Reading should happen once, not on demand when fetching the URL."

This packet is the read-once half. Push to the page (an event stream, then the page dropping its poll) is the next packet, not this one; design so it can hang off the cache's atom with `add-watch`.

## Goal

One new namespace `src/futon3c/xiang/frames_cache.clj`, a listener seam in `src/futon3c/xiang/turn_service.clj`, and `handle-xiang-frames` serving from the cache. After this lands:

- the first `GET` for a session runs the script once (the caller waits for that one build, as now);
- every later `GET` for that session answers from memory, no process, no futon1b;
- the cache for a session is rebuilt when, and only when, the turn service records a turn or settles one in that session, and only for sessions someone has fetched (a session nobody is looking at costs nothing);
- a failed rebuild keeps the last good frames and records the error; it never replaces frames with nothing.

## Design

### `turn_service.clj`: turn listeners (reload-safe, never on the turn's path)

The JVM's service is built once (`defonce` in http.clj) and survives namespace reloads, so the seam cannot be a new constructor option. Use a namespace-level registry:

```clojure
(defonce ^:private !turn-listeners (atom {}))
(defn add-turn-listener! [key f] ...)      ; f: (fn [event])
(defn remove-turn-listener! [key] ...)
(defn- notify-turn! [svc event] ...)       ; runs each listener via (cfg svc :schedule!) at delay 0,
                                           ; each in its own try; a throwing listener is noted with
                                           ; set-health! (nil status) and never reaches the caller
```

Event: `{:event :recorded|:reading|:settled :id <record-id> :session-id s :agent-id a :how :analyzed|:drafted|nil}`. Fire points:

- `:recorded` in `record-turn!` once `ts/write-record!` has returned (the record exists on disk; 小象's draft may still be in flight, which is fine, the next event catches it).
- `:reading` at the top of `settle-evidence!` (a reading or draft has landed; the evidence write may be delayed up to `:settle-evidence-delay`).
- `:settled` inside `settle-evidence!`'s `write!`, after `append-evidence!` returns (the record's happened summary is on disk by then, or the delay expired).

Read the record for `:session-id`/`:agent-id` with `ts/read-record`; a missing record means no event.

### `frames_cache.clj`

```clojure
(defonce !cache (atom {}))   ; session -> entry
;; entry: {:frames [...] :count n :version k :built-at iso :build-ms n
;;         :warnings s|nil :last-error m|nil :last-read-at ms :pending? bool}
(defn frames! [session opts])      ; cached entry, building synchronously when absent;
                                   ; concurrent first calls share one build (store a delay/promise
                                   ; in the atom under :building so exactly one runner call happens)
(defn invalidate! [session opts])  ; no-op for a session not in the cache; otherwise schedule one
                                   ; rebuild, coalescing events that arrive within opts :debounce-ms
                                   ; (default 2000) and an event that lands during a rebuild into
                                   ; exactly one more rebuild afterwards
(defn evict-idle! [max-idle-ms])   ; drop entries not read for max-idle-ms (default 24 h); call it
                                   ; from invalidate! so it needs no timer
(defn reset! [])                   ; tests
```

`opts` carry `:runner` `(fn [session] -> {:ok true :frames [...] :warnings s} | {:ok false ...})` and `:schedule!` `(fn [delay-ms thunk])`; http.clj supplies the runner (today's `run-turn-frames!` + `xiang-frames-argv` with no `--limit`, so the cache holds the whole session and the route takes the newest N) and a scheduler. A rebuild that returns `:ok false` leaves `:frames`/`:version` untouched and sets `:last-error` with the reason and a timestamp. Replace a successful rebuild's `:version` with `(inc version)` so a watcher can tell a change from a read.

### `http.clj`

- `handle-xiang-frames` calls `frames-cache/frames!`; `limit` is applied to the cached vector (newest N, the script's own semantics). Response keeps today's keys and adds `:version`, `:built-at`, `:cached` (false on the request that built it, true afterwards) and `:last-error` when present. Error shaping for a first build that fails stays as today (504 on timeout, 502 otherwise).
- At namespace load: `(xiang-turns/add-turn-listener! ::frames-cache (fn [e] (frames-cache/invalidate! (:session-id e) ...)))`. Keyed, so a reload re-registers rather than stacking.
- Remove nothing else; `run-turn-frames!` and `xiang-frames-argv` stay (the test file uses them).

## Acceptance

Tests, with the runner stubbed (no python, no futon1b):

- `test/futon3c/xiang/frames_cache_test.clj` (new): (a) two `frames!` calls for one session run the runner once and return the same `:version`; (b) `invalidate!` then `frames!` runs it once more and `:version` is incremented; (c) three `invalidate!` calls inside the debounce window cause exactly one rebuild, and an `invalidate!` that arrives while a rebuild is running causes exactly one more; (d) `invalidate!` of a session never fetched runs nothing; (e) a runner returning `:ok false` leaves the previous frames and version in place and sets `:last-error`; (f) `evict-idle!` drops an entry whose `:last-read-at` is older than the bound and keeps a fresh one; (g) concurrent first calls (two threads) run the runner once. Use a hand-rolled scheduler that records thunks and runs them on demand, so the debounce is tested without sleeping.
- `test/futon3c/xiang/turn_service_test.clj` (extend, using the existing `harness`): `record-turn!` fires a `:recorded` event carrying the record's session id; the settle path (the harness has a synchronous `:evidence-async!`) fires `:reading` then `:settled`; and the bad case the seam is named for: a listener that throws leaves `record-turn!`'s return value unchanged and the service's health noted, not failing.
- `test/futon3c/transport/xiang_frames_http_test.clj` (extend; reset the cache in each test): two GETs for one session run the redefined runner once and the second answers `:cached true`; `limit=2` on a cached session of five frames returns the newest two; the existing tests still pass.

Gates: `clj-kondo --lint` on every changed `.clj` file (no new warnings), `futon4/dev/check-parens.el` on them, and
`clojure -M:test -n futon3c.xiang.frames-cache-test -n futon3c.xiang.turn-service-test -n futon3c.transport.xiang-frames-http-test`
green. Commit on master, message starting `frames cache:`; one commit is fine, two if the turn-service seam goes first. Then bell claude-16 back with the summary, the commit shas, and the test command's last lines (`scripts/agency_send.py --from <your-id> --to claude-16 --kind bell`).

Out of scope here: the event stream to the browser, changes to `web/stepper/*`, changes to `scripts/turn_frames.py`, Caddy.
