#!/usr/bin/env bash
# proof-eval.sh — evaluate Clojure code against the running futon3c server
#
# Usage:
#   ./scripts/proof-eval.sh '(require (quote [futon3c.proof.bridge :as pb]))'
#   ./scripts/proof-eval.sh '(pb/summary "FM-001")'
#   ./scripts/proof-eval.sh -f /tmp/form.clj      # read form from a FILE
#   ./scripts/proof-eval.sh /tmp/form.clj         # same; readable file arg
#   echo '(+ 1 2)' | ./scripts/proof-eval.sh -    # read form from stdin
#   ./scripts/proof-eval.sh /dev/stdin <<'CLJ'    # heredoc also works
#   (+ 1 2)
#   CLJ
#
# PREFER -f/stdin for any form containing quotes or apostrophes: write the
# form with your file tool, then eval the file — no shell quoting at all.
# (Two agents lost whole turns to single-arg shell quoting; 2026-07-05.)
#
# Environment:
#   FUTON3C_DRAWBRIDGE_PORT  — default 6768
#   FUTON3C_ADMIN_TOKEN      — or reads from .admintoken file

set -euo pipefail

usage() {
  cat <<'EOF'
proof-eval.sh — evaluate Clojure code against the running futon3c Drawbridge

Usage:
  scripts/proof-eval.sh '(+ 1 2)'
  scripts/proof-eval.sh -f /tmp/form.clj
  scripts/proof-eval.sh /tmp/form.clj
  scripts/proof-eval.sh - < /tmp/form.clj
  scripts/proof-eval.sh /dev/stdin <<'CLJ'
  (do
    (require '[futon3c.agency.registry :as reg])
    (reg/registry-status))
  CLJ

Guidance:
  Prefer -f, -, or a file path for anything containing quotes, apostrophes,
  reader macros, or multiple lines. Avoid cat-heredoc command substitutions.
EOF
}

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

read_code() {
  case "${1:-}" in
    "")
      if [ -t 0 ]; then
        usage >&2
        exit 2
      fi
      cat
      ;;
    -f|--file)
      if [ -z "${2:-}" ]; then
        echo "proof-eval.sh: -f requires a file path" >&2
        exit 2
      fi
      cat "$2"
      ;;
    -)
      cat
      ;;
    *)
      if [ "$#" -eq 1 ] && [ -r "$1" ] && { [ -f "$1" ] || [ "$1" = "/dev/stdin" ]; }; then
        cat "$1"
      else
        printf '%s' "$*"
      fi
      ;;
  esac
}

case "${1:-}" in
  -h|--help|help)
    usage
    exit 0
    ;;
esac

CODE="$(read_code "$@")"
CODE="${CODE//$'\r'/}"

if [ -z "${CODE//[[:space:]]/}" ]; then
  echo "proof-eval.sh: no Clojure form provided" >&2
  usage >&2
  exit 2
fi

printf '%s' "$CODE" | curl -s \
  -H "x-admin-token: $TOKEN" \
  -H "Content-Type: text/plain" \
  --data-binary @- \
  "http://${HOST}:${PORT}/eval"
