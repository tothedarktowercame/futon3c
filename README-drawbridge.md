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

Or from Emacs: `C-c C-n` in the futon3c-chat buffer (tries the HTTP
endpoint first, falls back to Drawbridge automatically).

Or from IRC: `!reset claude-1`

### Hot-Reload a Source File

```bash
bash scripts/proof-eval.sh '(load-file "src/futon3c/transport/http.clj")'
```

Note: reloading redefines functions but does NOT refresh the HTTP
handler closure. For routing changes, the server needs a restart.

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

## Emacs Integration

The futon3c-chat REPL reads `.admintoken` automatically for Drawbridge
operations (e.g. `C-c C-n` session reset). No configuration needed
beyond having the file in the project root.

If you need to customize:

```elisp
(setq futon3c-chat-drawbridge-url "http://localhost:6768")
(setq futon3c-chat-drawbridge-token "your-token-here")
```
