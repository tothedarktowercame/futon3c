# Mission: IRC Transport Stability

## Derivation

VERIFY step — the IRC adapter is functional (26 tests, 66 assertions) but has
6 failure modes that make it unsuitable for "always-on" human↔agent communication.

Prior:
- M-transport-adapters: HTTP + WS adapters complete (72 tests)
- IRC adapter built and tested: `transport/irc.clj` (449 lines)
- Transport-pivot gate passed (I-transport-pivot PASS): bidirectional
  joe+claude chat across Emacs and IRC proven
- TN-agency-vs-subagents: articulates why IRC stability matters —
  lateral mesh coordination requires the channel to be reliably available

## Why This Mission Exists

The IRC transport works in short sessions but fails under real-world conditions.
The requirement is: **Joe can talk with agents on IRC whenever he wants.** That
means the connection must survive laptop sleep/wake, network switches, idle
periods, and misbehaving agents — none of which the current code handles.

Six critical failure modes, in priority order:

### F1: No server-initiated PING (zombie connections)

**What happens:** The server only responds to client PINGs (passive PONG at
line 221-222). It never probes whether clients are still alive. If a TCP
connection goes half-open (laptop sleeps, network changes, WiFi drops), the
server thinks the client is still connected indefinitely. Messages sent to
the zombie nick go into the void. The client's ERC shows no error because
the FIN was never sent.

**How common:** Every laptop sleep/wake cycle. Every WiFi→ethernet switch.

**Pattern:** `realtime/liveness-heartbeats` — server MUST initiate periodic
probes, not just respond to client probes.

### F2: No socket read timeout (leaked threads)

**What happens:** `.readLine()` at line 423 blocks indefinitely. If the TCP
connection stalls (no data, no FIN — a half-open connection), the reader
thread hangs forever. Each such stall permanently consumes one thread and
one socket. Over time, the server accumulates leaked threads.

**How common:** Same conditions as F1 — every half-open connection.

### F3: Silent error swallowing (invisible failures)

**What happens:** The reader loop's catch at line 427 discards all exceptions
silently: `(catch Exception _)`. If a malformed message, encoding error, or
transient I/O error occurs, there's no log, no diagnostic, no evidence entry.
The connection just silently stops processing. Similarly, send-fn at line 379
catches and discards all send failures.

**How common:** Any non-UTF-8 byte sequence, any message parsing edge case.

### F4: Client reconnection / nick collision

**What happens:** When a client reconnects (e.g., ERC auto-reconnect after
network blip), the server still has the old zombie entry for that nick (per
F1). The reconnecting client either gets a nick collision or the old and new
entries coexist, causing duplicate/missing messages. There's no nick reclaim
or ghost-kill mechanism.

**How common:** Every reconnection after F1.

### F5: No relay timeout (one agent blocks all)

**What happens:** `relay-fn` at line 328-332 iterates all agents and calls
`ws-send-fn` synchronously for each. If one agent's WS connection is stalled
(slow, buffering, or dead-but-not-yet-detected), the relay blocks, and no
other agent in that channel receives the message.

**How common:** Any time an agent process is slow or its WS connection degrades.

### F6: No connection limits / no graceful shutdown coordination

**What happens:** The accept loop at line 404 accepts unbounded concurrent
clients. Reader threads spawned at line 420 are not tracked — `stop-fn` closes
writers but doesn't join or cancel reader futures. Orphaned threads continue
running after shutdown.

**How common:** Only matters at scale or during restart cycles.

## Pattern Cross-References

| Pattern | How It Constrains This Fix |
|---------|---------------------------|
| `realtime/liveness-heartbeats` | Server-initiated PING interval and timeout values |
| `realtime/connection-state-machine` (L2) | Nick lifecycle: connecting → registered → zombie → reclaimed |
| `realtime/reconnect-with-backoff` (L5) | Client-side concern, but server must support clean nick reclaim |
| `realtime/loop-failure-signals` | Errors must surface at the layer that caused them, not be swallowed |
| `realtime/listener-leases` | Zombie connections should be reaped, not left indefinitely |

## Scope In

- Server-initiated PING/PONG with configurable interval and timeout (F1)
- Socket read timeout via `SO_TIMEOUT` (F2)
- Error logging and evidence emission on connection errors (F3)
- Nick reclaim on reconnection: detect ghost, kill old, register new (F4)
- Relay timeout: per-agent send with deadline, skip stalled agents (F5)
- Reader thread tracking and coordinated shutdown (F6)

## Scope Out

- Client-side reconnection logic (ERC handles this; server just needs to
  accept the reconnect cleanly)
- TLS/SSL termination (nginx concern)
- Rate limiting / flood protection (separate hardening mission)
- Multi-server IRC federation (not needed; single server)
- IRCv3 capabilities negotiation (keep it RFC 1459 minimal)
- Persistent connection registry across JVM restarts

## Parts

### Structure: 3 parts, linear

```
Part I: Keepalive + Timeout (F1, F2, F3)
                |
Part II: Nick Reclaim + Relay Timeout (F4, F5)
                |
Part III: Shutdown Coordination (F6) + Integration
```

### Part I: Keepalive, Timeout, Error Logging

:in  -- src/futon3c/transport/irc.clj (MODIFY)
:out -- test/futon3c/transport/irc_test.clj (ADD TESTS)

**F1 fix — Server-initiated PING:**

Add a keepalive loop that runs per-client (or globally with per-client tracking):

```clojure
;; Config keys:
;;   :ping-interval-ms  (default 30000 — 30s)
;;   :ping-timeout-ms   (default 90000 — 90s)

;; Per-client state addition:
;;   :last-pong-at  Instant   — updated on PONG or any message received
;;   :ping-pending? boolean   — true after PING sent, false after PONG received

;; Keepalive loop (single future for all clients):
;; Every :ping-interval-ms:
;;   For each registered client:
;;     If (now - :last-pong-at) > :ping-timeout-ms → reap (call on-disconnect, close)
;;     Else if not :ping-pending? → send PING, set :ping-pending? true
;;     (Any received message resets :last-pong-at — not just PONG)
```

Update PONG handler (line 224-225) to reset `:last-pong-at` and `:ping-pending?`.
Also reset `:last-pong-at` on any received line (any traffic proves liveness).

**F2 fix — Socket read timeout:**

After creating the socket at line 407, before creating reader/writer:

```clojure
(.setSoTimeout socket 120000)  ;; 120s read timeout
```

This makes `.readLine()` throw `SocketTimeoutException` after 120s of silence.
The reader loop catch should distinguish this from real errors — a timeout
without a missed PONG means the keepalive loop handles it; a timeout with a
missed PONG means the connection is dead.

**F3 fix — Error logging:**

Replace `(catch Exception _)` at lines 379, 384, 409, 427, 430 with:

```clojure
(catch Exception e
  (when evidence-store
    (estore/append* evidence-store
                    {:evidence/id (str "e-" (UUID/randomUUID))
                     :evidence/subject {:ref/type :transport :ref/id "irc-server"}
                     :evidence/type :coordination
                     :evidence/claim-type :tension
                     :evidence/author "irc-server"
                     :evidence/at (now-str)
                     :evidence/body {:error (.getMessage e)
                                     :client-id client-id
                                     :context "reader-loop"}
                     :evidence/tags [:irc :error :transport/irc]})))
```

For non-evidence-store contexts, at minimum log to stderr. The key invariant:
**no exception is silently discarded.**

Criteria:
- [ ] Server sends PING to idle clients after :ping-interval-ms
- [ ] Client that doesn't PONG within :ping-timeout-ms is reaped
- [ ] Any received message resets the liveness timer (not just PONG)
- [ ] Socket read timeout is set (no indefinite .readLine blocks)
- [ ] SocketTimeoutException is handled gracefully (not treated as disconnect)
- [ ] All catch blocks log or emit evidence (no silent swallowing)
- [ ] Existing 26 IRC tests still pass
- [ ] New tests for: PING sent after interval, reap after timeout, timeout doesn't kill active connection

### Part II: Nick Reclaim + Relay Timeout

:in  -- src/futon3c/transport/irc.clj (MODIFY, after Part I)
:out -- test/futon3c/transport/irc_test.clj (ADD TESTS)

**F4 fix — Nick reclaim:**

When a NICK command arrives and the nick is already in use by another client-id:

```clojure
;; In "NICK" handler (line 168):
;; 1. Check if nick is already registered by another client-id
;; 2. If the existing client has :ping-pending? true or hasn't sent
;;    a message in > :ping-timeout-ms → it's a ghost
;; 3. Kill the ghost: call on-disconnect for old client-id, close-fn
;; 4. Register new client with the nick
;; 5. If the existing client is NOT a ghost → send ERR_NICKNAMEINUSE (433)
```

This means reconnecting clients get their nick back immediately if the old
connection is dead (which it almost always is after a network blip).

**F5 fix — Relay timeout:**

Wrap the relay ws-send-fn call in a timeout:

```clojure
;; In relay-fn (line 328-332):
;; Replace direct ws-send-fn call with:
(let [f (future (ws-send-fn (proto/render-irc-message channel from text)))]
  (try
    (deref f 5000 ::timeout)
    (catch Exception _
      ;; Log/emit evidence, skip this agent
      )))
```

5-second per-agent timeout. If an agent's WS is stalled, skip it and continue
to the next agent. Emit a tension evidence entry for the timeout.

Criteria:
- [ ] Reconnecting client reclaims nick from ghost connection
- [ ] Non-ghost nick collision returns ERR_NICKNAMEINUSE (433)
- [ ] Ghost detection uses liveness timer from Part I
- [ ] Relay timeout: stalled agent doesn't block other agents
- [ ] Relay timeout emits tension evidence
- [ ] Existing tests still pass

### Part III: Shutdown Coordination + Integration

:in  -- src/futon3c/transport/irc.clj (MODIFY, after Part II)
:out -- test/futon3c/transport/irc_test.clj (ADD TESTS)
       test/futon3c/transport/irc_integration_test.clj (NEW — optional)

**F6 fix — Reader thread tracking:**

```clojure
;; Add !reader-futures atom: {client-id → future}
;; In accept loop, track the reader future:
(let [reader-future (future ...)]
  (swap! !reader-futures assoc client-id reader-future))

;; In stop-fn:
(doseq [[_ f] @!reader-futures]
  (future-cancel f))
```

Also add the keepalive future to the shutdown sequence.

**Integration test scenarios** (optional, if time permits):

1. Client connects, goes idle, receives PING, sends PONG → stays alive
2. Client connects, goes idle, misses PONG → gets reaped
3. Client disconnects uncleanly, reconnects → nick reclaimed
4. Agent relay with one stalled agent → other agents still receive
5. Server shutdown → all reader threads and keepalive loop stop cleanly

Criteria:
- [ ] stop-fn cancels all reader futures
- [ ] stop-fn stops keepalive loop
- [ ] No orphaned threads after shutdown
- [ ] `clojure -X:test` passes cleanly
- [ ] All existing transport tests unaffected

## Exit Conditions

- Server-initiated PING/PONG detects dead connections within 90s
- No reader thread can block indefinitely (SO_TIMEOUT enforced)
- No exception is silently discarded anywhere in irc.clj
- Reconnecting clients reclaim their nick from ghost connections
- One slow agent cannot block IRC→agent relay for other agents
- Server shutdown is clean: all threads stopped, all sockets closed
- All existing tests pass; 10+ new tests for stability behavior
- `clojure -X:test` passes cleanly

## Relationship to Other Missions

- **M-transport-adapters**: Built the HTTP/WS adapters with lessons from futon3.
  This mission applies similar hardening to the IRC adapter, which was built
  after M-transport-adapters and inherited some but not all of its rigor.
- **M-forum-refactor**: Evidence store receives IRC transcript entries. Error
  evidence from this mission feeds into the same store — tensions become visible
  in the evidence landscape.
- **TN-agency-vs-subagents**: Articulates why always-on IRC matters: lateral
  mesh coordination between Claude, Codex, and Joe requires a reliable channel.
