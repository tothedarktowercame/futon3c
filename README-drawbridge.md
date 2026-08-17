# Drawbridge (nREPL-over-HTTP)

Drawbridge provides a lightweight HTTP REPL into the running futon3c
JVM. Useful for diagnostics, hot-reloading code, and administrative
operations (e.g. resetting a poisoned agent session).

## Connection Details

| Setting | Default | Env var |
|---------|---------|---------|
| Port | 6768 | `FUTON3C_DRAWBRIDGE_PORT` |
| Bind | 127.0.0.1 | `FUTON3C_DRAWBRIDGE_BIND` |
| Allowlist | 127.0.0.1, ::1 | `FUTON3C_DRAWBRIDGE_ALLOW` |

## Authentication

The admin token is resolved in this order:

1. `FUTON3C_ADMIN_TOKEN` env var
2. `ADMIN_TOKEN` env var
3. `.admintoken` file in the project root
4. Falls back to `change-me` (development only)

**To set the token:** write it to `.admintoken` in the futon3c root.
This file is gitignored and readable by all scripts, Emacs, and the
server without any env var configuration.

```bash
# Generate and write a token (one-time setup)
python3 -c "import secrets; print(secrets.token_urlsafe(24), end='')" > .admintoken
chmod 600 .admintoken
```

## Quick Reference

All examples use `proof-eval.sh` which handles token resolution
automatically.

### Evaluate Clojure

```bash
bash scripts/proof-eval.sh '(+ 1 2)'
```

For anything with quotes, reader macros, or multiple lines, do not fight shell
quoting. Put the form in a file or pipe it on stdin:

```bash
cat >/tmp/proof-form.clj <<'CLJ'
(do
  (require '[futon3c.agency.registry :as reg])
  (reg/registry-status))
CLJ

bash scripts/proof-eval.sh -f /tmp/proof-form.clj
bash scripts/proof-eval.sh /tmp/proof-form.clj
bash scripts/proof-eval.sh /dev/stdin <<'CLJ'
(do
  (require '[futon3c.agency.registry :as reg])
  (reg/registry-status))
CLJ
```

Avoid command substitutions such as `CODE=$(cat file)` and avoid passing
`/dev/stdin` to older copies of the script; current `proof-eval.sh` treats
readable file arguments as input, but older copies evaluated the literal string
`/dev/stdin`.

### Check Registry

```bash
bash scripts/proof-eval.sh '(futon3c.agency.registry/registry-status)'
```

### Reset a Poisoned Session

When a `claude -p` session gets corrupted (e.g. API rejects the
conversation history), clear the session so the next invoke starts
fresh:

```bash
bash scripts/proof-eval.sh '(futon3c.agency.registry/reset-session! "claude-1")'
```

Or from Emacs: `C-c C-n` in the claude-repl buffer (tries the HTTP
endpoint first, falls back to Drawbridge automatically).

Or from IRC: `!reset claude-1`

### Hot-Reload a Source File

```bash
bash scripts/proof-eval.sh '(load-file "src/futon3c/transport/http.clj")'
```

#### Reload-safety: what a reload picks up, and what it doesn't

`load-file` redefines a namespace's vars in place. Whether a running
server sees the change depends on *how* each function is referenced:

- **Handler bodies are reload-safe.** The route dispatcher
  (`transport/http.clj` `make-handler`) is a `cond` of *direct symbol
  calls* — `(handle-dispatch request config)`, not `(#'handle-dispatch …)`.
  A bare top-level symbol inside a fn body compiles to a **call-time var
  dereference**, so editing a handler body and reloading is seen on the
  next request with no restart. The same idiom keeps the watcher
  (`(file-ingest/dispatch! …)`) and the scheduler (`(fn [] (tick!))`,
  `scheduler.clj`) reload-safe.
- **Route-table changes are NOT reload-safe.** Adding or removing a `cond`
  branch (a new endpoint) changes `make-handler`'s source, but the server
  still holds the closure returned by the *original* `make-handler` call
  at startup. The new branch appears only after the handler is re-wired
  (re-invoke `make-handler` and swap the running app's handler) or the
  server is restarted.
- **The hazard to watch: a fn captured as a value.** Stale closures bite
  only when a fn is stored in a data structure (`{:handler f}` dispatch
  map), registered once as a callback, or closed over by a thread started
  once — there the *value* is frozen at capture time. In those sites use
  the `#'var` indirection so the var is re-resolved per call. The futon3c
  serving path currently has none of these in the request path (audited
  2026-05-30, pilot cycle `cg-6965e5e6`); add `#'var` if you introduce one.

Reload-safety = reconstructibility from disk: a reload is safe iff it
equals a restart. Direct-symbol-call dispatch keeps that true for handler
bodies; a value-captured fn is the exception that needs `#'var`.

### Run Diagnostics

```bash
bash scripts/agency-diagnostic.sh
```

## Raw curl

If you need to call Drawbridge directly (e.g. from a script that
doesn't use `proof-eval.sh`):

```bash
curl -s \
  -H "x-admin-token: $(cat .admintoken)" \
  -H "Content-Type: text/plain" \
  --data-binary @- \
  "http://127.0.0.1:6768/eval" <<'CLOJURE'
(futon3c.agency.registry/registry-status)
CLOJURE
```

Use `--data-binary @-` with a heredoc to avoid shell escaping issues
with double quotes in Clojure code.

## /eval reports EVERY runtime exception as a syntax error

**This is the single most time-wasting gotcha on this endpoint.** `/eval` uses
`load-string`, which compiles and runs in one step, so an exception thrown at
*run* time surfaces wrapped in a `CompilerException`:

```
(/ 1 0)                                            -> {:ok false,
                                                       :error "Syntax error macroexpanding at (1:1)."}
(try (/ 1 0) (catch Throwable t (.getMessage t)))  -> {:ok true, :value "Divide by zero"}
```

Nothing is wrong with your parentheses. **Wrap the call in `try`/`catch` and you
get the real message.**

A genuine syntax error IS distinguishable, but only by the wording:

| what happened | what you see |
|---|---|
| runtime exception | `Syntax error macroexpanding at (1:1).` |
| actual malformed form | `Syntax error reading source at (2:1).` |

`macroexpanding` at `(1:1)` on a form you know parses = a runtime throw.

**Worked example (2026-08-17).** `(futon3c.watcher.multi/retract-flexiarg! "/tmp/x.flexiarg")`
reported a syntax error. Wrapped in `try`/`catch` it said
`ExceptionInfo :: not a library flexiarg path` — the function's own guard,
working correctly. Twenty minutes went into blaming the heredoc, which was
never involved: the same forms fail identically via `--data-binary "<form>"`.
Two unrelated mistakes had been attributed to it — a zero-arg call to a
one-arity function, and this guard.

**Corollary: a timeout is not a failure.** Long store writes exceed the eval
timeout and return `"request timed out"` while the writes land anyway. Verified
on `retract-flexiarg!`: the call timed out, and the pattern row was gone
(1345 → 1344 rows). Re-check state before retrying, or you will double-apply.
For anything long, use `scripts/bg.py` rather than a foreground eval.

## Emacs Integration

The claude-repl REPL reads `.admintoken` automatically for Drawbridge
operations (e.g. `C-c C-n` session reset). No configuration needed
beyond having the file in the project root.

If you need to customize:

```elisp
(setq claude-repl-drawbridge-url "http://localhost:6768")
(setq claude-repl-drawbridge-token "your-token-here")
```
