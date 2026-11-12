# Blackboard — emacsclient transport notes

The blackboard projects state to Emacs buffers via `emacsclient`. It is
best-effort: failures are swallowed, the next poll redoes the work.

## "no longer connected to pipe; closed it" noise

If you see Emacs *Messages* filling with lines like

```
Process server <PID> no longer connected to pipe; closed it
Process server <PID> no longer connected to pipe; closed it
...
```

(often six in a row, sequential PIDs), the cause is the JVM `destroyForcibly`-ing
an in-flight `emacsclient` while it is still mid-handshake with the Emacs
server. SIGKILL drops the socket abnormally, Emacs's process.c logs the line.

### Fix in place (2026-11-12)

`run-emacsclient-async!` uses a serialized, latest-value mailbox per Emacs
socket.  At most one blackboard emacsclient runs at once.  While it waits,
each panel retains one pending snapshot and newer snapshots replace it.  The
running client is not killed: it exits naturally when Emacs accepts it, then
the mailbox sends the next panel.  This prevents parallel panel updates from
forming a server queue and prevents the producer from manufacturing dead
clients by timing them out.

### Sync path still reaps

`run-emacsclient!` (sync, returns the eval result) keeps the forcible reap on
its 2 s timeout — callers block on the result and need a bound. If the noise
returns, suspect this path: either bump its timeout (2 s → 5 s) or convert
the caller to `run-emacsclient-async!` if the return value is unused.

### Hot-reload, don't restart

Reload the namespace through Drawbridge:

```bash
./scripts/proof-eval.sh '(require (quote [futon3c.blackboard]) :reload)'
```

Verify the async path uses the mailbox:

```bash
./scripts/proof-eval.sh '(require (quote [clojure.repl]))
(re-find #"enqueue-async-emacsclient!"
  (with-out-str
    (clojure.repl/source futon3c.blackboard/run-emacsclient-async!)))'
```

Should return a match.

## Related

- `src/futon3c/blackboard.clj` — transport implementation
- `docs/wiring-contract.md` — what the blackboard projects and why
- Backpressure note: futon3c hot paths synchronously block on emacsclient;
  a wedged Emacs cascades everywhere. Prefer async projection where the
  return value is not needed.
