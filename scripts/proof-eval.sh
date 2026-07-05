#!/usr/bin/env bash
# proof-eval.sh — evaluate Clojure code against the running futon3c server
#
# Usage:
#   ./scripts/proof-eval.sh '(require (quote [futon3c.proof.bridge :as pb]))'
#   ./scripts/proof-eval.sh '(pb/summary "FM-001")'
#   ./scripts/proof-eval.sh -f /tmp/form.clj      # read form from a FILE
#   echo '(+ 1 2)' | ./scripts/proof-eval.sh -    # read form from stdin
#
# PREFER -f/stdin for any form containing quotes or apostrophes: write the
# form with your file tool, then eval the file — no shell quoting at all.
# (Two agents lost whole turns to single-arg shell quoting; 2026-07-05.)
#
# Environment:
#   FUTON3C_DRAWBRIDGE_PORT  — default 6768
#   FUTON3C_ADMIN_TOKEN      — or reads from .admintoken file

set -euo pipefail

PORT="${FUTON3C_DRAWBRIDGE_PORT:-6768}"
HOST="${FUTON3C_DRAWBRIDGE_HOST:-127.0.0.1}"

# Resolve token
if [ -n "${FUTON3C_ADMIN_TOKEN:-}" ]; then
  TOKEN="$FUTON3C_ADMIN_TOKEN"
elif [ -n "${ADMIN_TOKEN:-}" ]; then
  TOKEN="$ADMIN_TOKEN"
elif [ -f .admintoken ]; then
  TOKEN="$(cat .admintoken | tr -d '[:space:]')"
else
  TOKEN="change-me"
fi

if [ "${1:-}" = "-f" ]; then
  CODE="$(cat "$2")"
elif [ "${1:-}" = "-" ]; then
  CODE="$(cat)"
else
  CODE="$1"
fi

curl -s \
  -H "x-admin-token: $TOKEN" \
  -H "Content-Type: text/plain" \
  -d "$CODE" \
  "http://${HOST}:${PORT}/eval"
