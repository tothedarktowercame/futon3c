# Mission: Transport Adapters (HTTP + WebSocket)

## Derivation

INSTANTIATE step, wiring thin boundary adapters that connect the internal
social pipeline to external agents via HTTP and WebSocket.

Prior:
- M-agency-refactor: social pipeline complete (presence -> authenticate -> mode -> dispatch -> persist)
- M-peripheral-model: specs + hop protocol complete
- M-peripheral-behavior: all behavioral code complete (runner, tools, evidence, chain orchestration)
- M-dispatch-peripheral-bridge: dispatch routes action-mode messages into peripheral sessions
- M-forum-refactor: evidence landscape complete (store, threads, proof-tree invariants)
- social-exotype.edn: specifies I-connections as `:http-request` type; S-presence as entry point

All internal architecture is wired and tested (280 tests). The pipeline accepts
typed shapes and returns typed shapes. What's missing: nothing accepts an HTTP
request and nothing sends a WebSocket frame. The pipeline has no ears or mouth.

## Why This Mission Exists

The social exotype specifies I-connections (`:http-request`) as the entry point
for agent connection events. Currently, the pipeline is exercised only through
direct function calls in tests. No network boundary exists.

futon3's transport layer (~3000 LOC across agency/http.clj, drawbridge/core.clj,
drawbridge/claude.clj, drawbridge/codex.clj, forum/ws.clj) accumulated 30+
commits fighting WebSocket, WSS/TLS, reconnection, and session tracking issues.
The hard-won lessons from that history inform this mission's design and are
now codified as patterns in the realtime library.

## Pattern Cross-References

This mission is constrained by 8 existing patterns and 5 new ones derived
from futon3 transport archaeology:

### Existing Patterns (from futon3/library/realtime/)
| Pattern | How It Constrains This Mission |
|---------|-------------------------------|
| `realtime/rendezvous-handshake` | R7: WS readiness handshake before dispatch (Part III) |
| `realtime/transport-pivot` | Transport changes need explicit cutover, not ad-hoc (future) |
| `realtime/authoritative-transcript` | R8: evidence store is the single transcript authority |
| `realtime/liveness-heartbeats` | Heartbeat protocol for WS connections (future, not Part I-IV) |
| `realtime/listener-leases` | Prevent phantom connections via lease/TTL (Part III cleanup) |
| `realtime/structured-events-only` | R9: all WS frames are typed JSON, no free text |
| `realtime/single-line-transport` | One transport line per agent at a time (Part III) |
| `realtime/loop-failure-signals` | Error frames must surface failure at the layer that caused it |

### New Patterns (from futon3c/library/realtime/, derived from git tensions)
| Pattern | Lesson ID | Constrains |
|---------|-----------|-----------|
| `realtime/connection-state-machine` | L2 | Part III: WS connection lifecycle |
| `realtime/reconnect-with-backoff` | L5 | Client reconnection protocol spec |
| `realtime/request-param-resilience` | L1, L3 | Part I: extract-params helper |
| `realtime/verify-after-start` | L4, L7 | Part II: start-server! verification |
| `realtime/single-authority-registration` | L6 | Part III: WS registration guards |

## Hard-Won Lessons from futon3 (Transport Archaeology)

These are not theoretical concerns. Each was a production debugging session.

### L1: Emacs 31 websocket.el breaks WSS

websocket.el sends `GET ?query=...` (missing leading "/") on paths with no
explicit path component. Nginx correctly rejects with HTTP 400. Same server
works with curl, Java HttpClient, and raw TLS from Emacs 31.

**Design response:** Never rely on client libraries formatting URLs correctly.
Accept session-id in both query params AND message payload. Parse `:request-uri`
as fallback when `:query-string` is nil.

### L2: Spurious on-close during SSL handshake

During WSS through nginx, `on-close` fires before `on-open` completes. This
triggers premature reconnection attempts that cascade.

**Design response:** Track "real" connection state via first successful message
exchange, not the open/close callbacks. Use a `connected?` flag set on first
`on-message`, not `on-open`.

### L3: http-kit WebSocket upgrade drops query params

http-kit's `:query-string` is nil for WebSocket upgrade requests. Must fall
back to parsing `:request-uri`.

**Design response:** Always extract params from `:request-uri` for WS upgrades.
Provide a helper that works for both HTTP and WS requests.

### L4: AsyncChannel doesn't support metadata

http-kit's `AsyncChannel` doesn't implement `IObj`, so `with-meta` throws
`ClassCastException`.

**Design response:** Wrap channels in plain maps `{:channel ch :agent-id id}`.
Never attach metadata to library objects.

### L5: Reconnection without backoff causes storms

Fixed reconnect delay (5s) hammers the server on network partition. Multiple
agents reconnecting simultaneously after a blip creates thundering herd.

**Design response:** Exponential backoff (100ms initial, doubling, 30s cap).
Reset on successful reconnect. Distinguish manual disconnect (no reconnect)
from network disconnect (reconnect with backoff). Add jitter.

### L6: Dual registration violates single routing authority

When Drawbridge has both local HTTP and remote WS to Agency, both registration
paths fire. Agent appears in two routing stores. Messages duplicate.

**Design response:** Enforce R2 (single routing authority) at registration time.
If an agent is already registered via one transport, reject or evict the other.
Make this an invariant test, not a convention.

### L7: Async server startup hides binding failures

`server.start()` returns immediately. If port is in use, failure happens in
background thread. No error surfaces.

**Design response:** After `start()`, sleep briefly and verify the port is
actually listening. Use `SO_REUSEADDR` for quick restarts during development.

### L8: Secret/ack TTL cleanup is fire-and-forget

`(future (Thread/sleep ttl) (swap! secrets dissoc k))` doesn't run on JVM
shutdown. Secrets can leak.

**Design response:** Use bounded maps with eviction-on-access, not async
cleanup. Or use a ScheduledExecutorService that shuts down cleanly.

## Scope In

- HTTP REST adapter: thin handler that accepts JSON, calls pipeline, returns JSON
- WebSocket adapter: connection lifecycle (open/message/close) mapped to pipeline
- Session-id extraction from both query params and message payload
- Readiness handshake over WebSocket (R7)
- Delivery receipts over WebSocket (R1)
- Connection state tracking (real connection vs handshake-in-progress)
- Reconnection protocol spec (exponential backoff, jitter, manual disconnect flag)
- Graceful shutdown (close all connections, flush pending receipts)
- Error mapping: SocialError -> HTTP status codes / WebSocket error frames

## Scope Out

- TLS/WSS termination (handled by nginx reverse proxy, not the application)
- IRC bridge (separate adapter, different protocol entirely)
- Emacs client (fubar-agency.el — consumer of the adapter, not part of it)
- Claude/Codex subprocess management (drawbridge concern, builds on top of this)
- Real agent invocation backends (uses MockBackend for tests; real backends deferred)
- Persistent session store (atoms for now; durable store is a separate mission)
- Rate limiting / load shedding (important but separate hardening mission)
- Multi-JVM clustering (single-process for now)

## Conceptual Model

### Adapter as Boundary — Not Business Logic

The social pipeline is pure functions:
```
ClassifiedMessage -> dispatch -> DispatchReceipt | SocialError
```

The transport adapter is a thin translation layer:
```
HTTP Request -> [parse] -> pipeline input shapes
pipeline output shapes -> [render] -> HTTP Response | WS frame
```

The adapter MUST NOT contain routing logic, mode classification, peripheral
selection, or evidence emission. All of that lives in the pipeline. The adapter
only translates protocols.

### Two Adapters, One Pipeline

```
                HTTP REST                    WebSocket
                    |                            |
            [parse request]              [on-open / on-message]
                    |                            |
                    +-----> Social Pipeline <-----+
                    |       (pure functions)      |
            [render response]            [send frame / on-close]
                    |                            |
              HTTP Response                 WS Frame
```

### WebSocket Connection Lifecycle

```
TCP connect
    |
    v
on-open (http-kit callback)
    |-- extract agent-id from :request-uri params
    |-- DO NOT mark as "connected" yet
    |
    v
first message: readiness handshake (R7)
    |-- {:type "ready" :agent-id "claude-1" :session-id "sess-..."}
    |-- call S-presence/verify
    |-- if ok: mark connected?, send {:type "ready-ack"}
    |-- if error: send {:type "error" :code ...}, close
    |
    v
subsequent messages: classified and dispatched
    |-- parse JSON -> message map
    |-- call S-mode/classify -> S-dispatch/dispatch
    |-- send receipt or error as JSON frame
    |
    v
on-close
    |-- if connected?: clean up presence, cancel pending
    |-- if not connected?: handshake failed, no cleanup needed
```

### HTTP Endpoint Design

Stateless request-response. Each request carries full context.

```
POST /dispatch
  Body: {msg_id, payload, from, to}
  -> S-mode/classify -> S-dispatch/dispatch
  <- 200 {receipt} | 400/404/500 {error}

POST /presence
  Body: {agent_id, transport, metadata}
  -> S-presence/verify
  <- 200 {presence_record} | 400/404 {error}

GET /session/:id
  -> S-persist/get-session
  <- 200 {session} | 404 {error}

GET /health
  <- 200 {status: "ok", agents: N, sessions: N}
```

### Error Mapping

| SocialError :error/code | HTTP Status | WS Error Frame |
|-------------------------|-------------|----------------|
| :invalid-message | 400 | `{type: "error", code: "invalid-message"}` |
| :invalid-registry | 500 | (internal, not sent) |
| :agent-not-found | 404 | `{type: "error", code: "agent-not-found"}` |
| :not-ready | 403 | `{type: "error", code: "not-ready"}` |
| :invoke-failed | 502 | `{type: "error", code: "invoke-failed"}` |
| :peripheral-failed | 502 | `{type: "error", code: "peripheral-failed"}` |

## Parts

### Structure: Linear (4 parts)

```
Part I: Protocol Types + Request Parsing (pure functions)
                    |
Part II: HTTP REST Adapter (httpkit handler)
                    |
Part III: WebSocket Adapter (connection lifecycle)
                    |
Part IV: Integration Tests (end-to-end with real HTTP/WS)
```

### Part I: Protocol Types + Request Parsing

**Status:** Ready

:in  -- src/futon3c/social/shapes.clj (READ-ONLY -- existing shapes)
       src/futon3c/social/dispatch.clj (READ-ONLY -- pipeline entry point)
:out -- src/futon3c/transport/protocol.clj (NEW)
       test/futon3c/transport/protocol_test.clj (NEW)

Pure functions for protocol translation. No I/O, no server, no sockets.

```clojure
;; JSON -> pipeline input
(defn parse-dispatch-request [json-str] ...)   ;; -> ClassifiedMessage | SocialError
(defn parse-presence-request [json-str] ...)   ;; -> AgentConnection | SocialError
(defn parse-ws-message [json-str] ...)         ;; -> {:type :ready|:message, ...} | SocialError

;; Pipeline output -> JSON
(defn render-receipt [dispatch-receipt] ...)    ;; -> JSON string
(defn render-error [social-error] ...)         ;; -> JSON string + HTTP status
(defn render-ws-frame [pipeline-result] ...)   ;; -> JSON string for WS send

;; Request param extraction (L3: handles http-kit WS quirk)
(defn extract-params [http-kit-request] ...)   ;; -> {:agent-id ... :session-id ...}
```

Criteria:
- [ ] parse-dispatch-request produces valid ClassifiedMessage from well-formed JSON
- [ ] parse-dispatch-request returns SocialError for malformed/missing fields
- [ ] render-receipt produces valid JSON matching DispatchReceipt structure
- [ ] render-error maps :error/code to HTTP status codes
- [ ] extract-params works for both HTTP and WS upgrade requests (L3)
- [ ] extract-params falls back to :request-uri when :query-string is nil
- [ ] Round-trip: parse -> pipeline -> render preserves information
- [ ] 10+ tests

### Part II: HTTP REST Adapter

**Status:** Blocked on Part I

:in  -- src/futon3c/transport/protocol.clj (from Part I)
       src/futon3c/social/dispatch.clj (READ-ONLY)
       src/futon3c/social/presence.clj (READ-ONLY)
       src/futon3c/social/persist.clj (READ-ONLY)
:out -- src/futon3c/transport/http.clj (NEW)
       test/futon3c/transport/http_test.clj (NEW)

Thin httpkit handler. Routes HTTP requests to pipeline functions.

```clojure
(defn make-handler
  "Create an HTTP request handler wired to the social pipeline.
   config: {:registry AgentRegistryShape
            :patterns PatternLibrary
            :peripheral-config {...} (optional)
            :evidence-store atom (optional)}
   Returns a Ring handler fn."
  [config]
  ...)

(defn start-server!
  "Start HTTP server on port. Returns {:server s :stop-fn (fn [] ...)}.
   Verifies port is listening after start (L7)."
  [handler port]
  ...)
```

Endpoints:
- POST /dispatch -- classify + dispatch, return receipt or error
- POST /presence -- verify presence, return record or error
- GET /session/:id -- retrieve session
- GET /health -- liveness check

Criteria:
- [ ] POST /dispatch with valid JSON -> 200 + DispatchReceipt JSON
- [ ] POST /dispatch with bad JSON -> 400 + SocialError JSON
- [ ] POST /dispatch to unknown agent -> 404
- [ ] POST /presence with readiness metadata -> 200 + PresenceRecord JSON
- [ ] GET /session/:id for existing session -> 200
- [ ] GET /session/:id for missing session -> 404
- [ ] GET /health returns agent/session counts
- [ ] Server startup verifies port is listening (L7)
- [ ] Graceful shutdown closes server and returns
- [ ] Content-Type is application/json on all responses
- [ ] 10+ tests (using httpkit test client or direct handler calls)

### Part III: WebSocket Adapter

**Status:** Blocked on Part II

:in  -- src/futon3c/transport/protocol.clj (from Part I)
       src/futon3c/transport/http.clj (from Part II, server infrastructure)
       src/futon3c/social/presence.clj (READ-ONLY)
       src/futon3c/social/dispatch.clj (READ-ONLY)
:out -- src/futon3c/transport/ws.clj (NEW)
       test/futon3c/transport/ws_test.clj (NEW)

WebSocket connection lifecycle mapped to pipeline.

```clojure
(defn make-ws-handler
  "Create a WebSocket handler for httpkit's :init/:on-receive/:on-close.
   config: same as make-handler, plus:
     :on-connect (fn [agent-id] ...) -- optional hook
     :on-disconnect (fn [agent-id] ...) -- optional hook
   Returns {:init fn, :on-receive fn, :on-close fn}."
  [config]
  ...)
```

Connection state:
```clojure
;; Per-connection state, wrapped in atom (L4: no metadata on channels)
{:channel    <httpkit-channel>
 :agent-id   <string or nil>
 :session-id <string or nil>
 :connected? false            ;; L2: true only after readiness handshake
 :opened-at  <instant>}
```

Readiness handshake (R7):
1. Client sends: `{"type":"ready","agent_id":"claude-1","session_id":"sess-..."}`
2. Server calls S-presence/verify
3. On success: sets `connected? true`, sends `{"type":"ready_ack"}`
4. On failure: sends `{"type":"error","code":"..."}`, closes connection

Message flow (after handshake):
1. Client sends: `{"msg_id":"...","payload":"...","to":"..."}`
2. Server calls S-mode/classify -> S-dispatch/dispatch
3. Server sends: `{"type":"receipt",...}` or `{"type":"error",...}`

Criteria:
- [ ] WS open without readiness handshake -> no messages processed (R7)
- [ ] Readiness handshake with valid agent -> ready_ack sent
- [ ] Readiness handshake with unknown agent -> error + close
- [ ] Message after handshake -> dispatched, receipt sent as frame
- [ ] Message before handshake -> error frame (not ready)
- [ ] on-close after successful handshake -> presence cleanup
- [ ] on-close before handshake (L2) -> no cleanup needed
- [ ] extract-params handles missing :query-string (L3)
- [ ] Channel state stored in map, not metadata (L4)
- [ ] Connection tracking: list connected agents with agent-id
- [ ] 8+ tests

### Part IV: Integration Tests

**Status:** Blocked on Part III

:in  -- All files from Parts I-III
       test/futon3c/social/pipeline_test.clj (READ-ONLY -- existing patterns)
       test/futon3c/social/dispatch_integration_test.clj (READ-ONLY)
:out -- test/futon3c/transport/integration_test.clj (NEW)

End-to-end scenarios using actual HTTP requests and WebSocket connections.

Scenarios:

1. **HTTP dispatch round-trip**: POST JSON -> pipeline -> JSON receipt
2. **HTTP action message -> peripheral**: POST action payload -> peripheral
   dispatch -> receipt with session-id and fruit
3. **WS connection lifecycle**: open -> ready handshake -> message -> receipt -> close
4. **WS coordination message**: WS ready -> coordination payload -> direct-invoke receipt
5. **WS action message -> peripheral**: WS ready -> action payload -> peripheral receipt
6. **WS handshake failure**: open -> ready with unknown agent -> error + close
7. **WS message before ready**: open -> message (no handshake) -> error
8. **HTTP + WS coexistence**: both adapters serve from same pipeline config
9. **Error propagation**: peripheral failure -> WS error frame with correct code
10. **Graceful shutdown**: stop server while WS connected -> connections closed

Criteria:
- [ ] All 10 scenarios pass
- [ ] All 280 existing tests still pass
- [ ] `clojure -X:test` passes cleanly
- [ ] No transport logic leaks into pipeline (adapter is pure translation)

## Dependencies

All required libraries are already in deps.edn:
- `http-kit/http-kit 2.7.0` -- HTTP + WebSocket server
- `org.clojure/data.json 2.5.0` -- JSON parsing/rendering
- `cheshire/cheshire 5.11.0` -- alternative JSON (already used in project)

No new dependencies needed.

## What This Does NOT Do (deferred)

- TLS termination (use nginx: `proxy_pass http://localhost:PORT`)
- Exponential backoff implementation (that's the client's concern; server
  just accepts connections)
- IRC bridge (different protocol, separate mission)
- Authentication tokens / secrets (uses agent-id lookup for now)
- Rate limiting (hardening mission)
- Subprocess management for Claude/Codex (drawbridge concern)
- Persistent connection registry across JVM restarts

## Relationship to Other Missions

- **M-agency-refactor**: Built the pure pipeline. This mission adds the network ear/mouth.
- **M-dispatch-peripheral-bridge**: Action messages now route to peripherals. The WS
  adapter delivers action messages that trigger peripheral sessions.
- **M-forum-refactor**: Evidence store accumulates entries. The HTTP adapter could
  expose evidence queries (deferred, but the evidence store is available).
- **futon3 transport code**: Source material for lessons learned. NOT ported directly --
  futon3c's adapter is thinner because the pipeline does the heavy lifting.

## What This Unlocks

With transport adapters in place:
- External agents (Claude, Codex, Emacs) can connect over HTTP/WebSocket
- The social pipeline accepts real network requests, not just test function calls
- WebSocket readiness handshake (R7) is enforced at the network boundary
- Delivery receipts (R1) flow back to connected agents as WS frames
- The system can be deployed as a standalone JVM process on a server
- fuclaude / fucodex peripheral wrappers can connect via WS instead of direct invoke

## Exit Conditions

- HTTP adapter accepts POST /dispatch and returns DispatchReceipt or SocialError as JSON
- HTTP adapter accepts POST /presence and returns PresenceRecord or SocialError
- WebSocket adapter enforces readiness handshake before processing messages (R7)
- WebSocket adapter sends delivery receipts as JSON frames (R1)
- Connection state tracked correctly (L2: real connection vs handshake-in-progress)
- Request params extracted from :request-uri for WS upgrades (L3)
- Channel state stored in maps, not metadata (L4)
- Server startup verifies port is listening (L7)
- Graceful shutdown closes connections
- SocialError maps to appropriate HTTP status codes
- All existing tests (280) unaffected
- 30+ new tests across 4 parts
- `clojure -X:test` passes cleanly
