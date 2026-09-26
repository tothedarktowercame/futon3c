#!/usr/bin/env bash
# Re-source futon3c.transport.http into the live Agency JVM from THIS checkout.
#
# Why: any agent that `load-file`s http.clj from another worktree (seen:
# codex-10 via proof-eval.sh from futon3c-apm-control, 2026-08-22/23) replaces
# the shared `extra-routes` with that branch's version, and every master-only
# route (e.g. POST /api/alpha/agents/:id/compact, GET .../pouch) answers
# "Unknown endpoint" 404 until the namespace is reloaded from the shared tree.
# A restart bakes the routes in; until then this is the one-line cure.
set -euo pipefail
cd "$(dirname "$0")/.."
TOKEN=$(cat .admintoken)
curl -s -X POST localhost:6768/eval -H "x-admin-token: $TOKEN" \
  -H 'Content-Type: text/plain' \
  --data-binary "(require 'futon3c.transport.http :reload)" >/dev/null
code=$(curl -s -o /dev/null -w '%{http_code}' localhost:7070/api/alpha/agents/claude-16/pouch)
if [ "$code" = 200 ]; then echo "routes restored (pouch probe 200)"; else echo "pouch probe still $code" >&2; exit 1; fi
