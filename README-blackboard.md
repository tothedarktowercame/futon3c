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

### Fix in place (2026-04-27)

`run-emacsclient-async!` no longer calls `reap-emacsclient!`. With `-n` the
client should exit in milliseconds; a >1 s wait means Emacs is briefly
wedged, and killing the client only manufactures broken-pipe noise. The
abandoned client exits naturally once Emacs unwedges.

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

Verify the reaper is gone from the async path:

```bash
./scripts/proof-eval.sh '(require (quote [clojure.repl]))
(re-find #"reap-emacsclient!"
  (with-out-str
    (clojure.repl/source futon3c.blackboard/run-emacsclient-async!)))'
```

Should return `nil`.

## Related

- `src/futon3c/blackboard.clj` — transport implementation
- `docs/wiring-contract.md` — what the blackboard projects and why
- Backpressure note: futon3c hot paths synchronously block on emacsclient;
  a wedged Emacs cascades everywhere. Prefer async projection where the
  return value is not needed.
