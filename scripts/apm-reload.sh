#!/usr/bin/env bash
# apm-reload.sh — reload namespaces from /home/joe/code/futon3c (master) into
# the shared futon3c JVM AND register the reload as a campaign condition.
#
# A reload changes what the running campaign executes without changing the
# git revision its next manifest will pin; the registry entry is what lets a
# later reader tell the two apart. Uses (require ns :reload), never
# :reload-all, so only the named namespaces change.
#
# Usage:
#   scripts/apm-reload.sh --by <agent-id> [--note "<why>"] [--campaign-root <dir>] ns [ns ...]
set -euo pipefail
cd /home/joe/code/futon3c
by=""; note=""; root_args=(); nss=()
while [ $# -gt 0 ]; do
  case "$1" in
    --by) by="$2"; shift 2;;
    --note) note="$2"; shift 2;;
    --campaign-root) root_args=(--campaign-root "$2"); shift 2;;
    --*) echo "unknown option $1" >&2; exit 2;;
    *) nss+=("$1"); shift;;
  esac
done
[ -n "$by" ] || { echo "--by <agent-id> is required" >&2; exit 2; }
[ ${#nss[@]} -gt 0 ] || { echo "no namespaces given" >&2; exit 2; }
branch=$(git branch --show-current)
[ "$branch" = master ] || { echo "checkout is on '$branch', not master; refusing (one JVM per repo, running master)" >&2; exit 3; }
head=$(git rev-parse --short HEAD)
for ns in "${nss[@]}"; do
  out=$(printf "(require '%s :reload)\n" "$ns" | scripts/proof-eval.sh - 2>&1) || true
  if ! grep -q ':ok true' <<<"$out"; then
    echo "reload of $ns FAILED: $out" >&2
    exit 4
  fi
  echo "reloaded $ns from master @ $head"
done
joined=$(IFS=,; echo "${nss[*]}")
bb scripts/apm-condition.bb --by "$by" --kind reload --namespaces "$joined" --loaded true \
   --note "${note:-reloaded from master @ $head: $joined}" "${root_args[@]}"
