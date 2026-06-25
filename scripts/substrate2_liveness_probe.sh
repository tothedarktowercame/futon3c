#!/usr/bin/env bash
# substrate2_liveness_probe.sh — D0 staleness check for M-populate-substrate-2.
#
# Reports the watcher's commit-ingest flag + cycle count, then for each watched
# repo asks the true question: is the repo's current HEAD commit IN substrate-2?
# Uses the O(1) membership test GET /api/alpha/hyperedge/hx:code/v05/commit:<sha>
# (not a timestamp compare — that gave false-STALEs from author-vs-commit date
# skew). Merge commits carry no diff and are skipped by the ingest, so when HEAD
# is a merge we check the latest non-merge ancestor instead.
#
# A MISSING row means the live model has fallen behind the code — the silent rot
# that froze substrate-2 from 2026-05-21. Run repeatedly to watch commits land.
#
# Usage: bash futon3c/scripts/substrate2_liveness_probe.sh
set -euo pipefail
F1A="${FUTON1A_URL:-http://localhost:7071}"
DB="${DRAWBRIDGE_URL:-http://127.0.0.1:6768}"
ROOT="/home/joe/code"
TOKEN="$(cat "$ROOT/futon3c/.admintoken" 2>/dev/null || true)"

enc(){ python3 -c "import urllib.parse,sys;print(urllib.parse.quote(sys.argv[1]))" "$1"; }

echo "=== watcher commit-ingest state (Drawbridge) ==="
if [ -n "$TOKEN" ]; then
  curl -s -H "x-admin-token: $TOKEN" -H "Content-Type: text/plain" --data-binary @- "$DB/eval" <<'CLOJURE' || echo "(drawbridge unreachable — JVM down/booting?)"
(let [s @futon3c.watcher.multi/!state]
  {:running? (some? (:executor s))
   :commit-ingest? (:commit-ingest? s)
   :cycle-n (some-> (:cycle-n s) deref)
   :last-error (:last-error s)})
CLOJURE
else
  echo "(no .admintoken)"
fi

echo
echo "=== HEAD-in-store check: is each repo's current HEAD commit in substrate-2? ==="
printf "%-16s %-12s %-20s %s\n" "repo-label" "HEAD" "head-date" "status"
# label:path pairs (the 14 watcher roots)
ROOTS="futon0-d:futon0 futon1-d:futon1 futon1a-d:futon1a futon2-d:futon2 futon3-d:futon3 futon3a-d:futon3a futon3b-d:futon3b futon3c-d:futon3c futon4-elisp-d:futon4 futon5-d2:futon5 futon5a-d:futon5a futon6-py-d:futon6 futon7-d:futon7 futon7a-d:futon7a"
fresh=0; total=0
for pair in $ROOTS; do
  label="${pair%%:*}"; path="${pair##*:}"; repo="$ROOT/$path"
  [ -d "$repo/.git" ] || { printf "%-16s %-12s %-20s %s\n" "$label" "—" "—" "no-git"; continue; }
  total=$((total+1))
  head_sha=$(git -C "$repo" rev-parse HEAD 2>/dev/null || true)
  note=""
  check_sha="$head_sha"
  if git -C "$repo" rev-parse --verify -q HEAD^2 >/dev/null 2>&1; then
    # HEAD is a merge → ingest skips it; check latest non-merge ancestor
    check_sha=$(git -C "$repo" rev-list --no-merges -1 HEAD 2>/dev/null || echo "$head_sha")
    note=" (HEAD=merge→${check_sha:0:7})"
  fi
  head_date=$(git -C "$repo" log -1 --format='%cd' --date=format:'%Y-%m-%d %H:%M' "$check_sha" 2>/dev/null || echo "—")
  resp=$(curl -s --max-time 8 "$F1A/api/alpha/hyperedge/$(enc "hx:code/v05/commit:$check_sha")" 2>/dev/null || true)
  if echo "$resp" | grep -q '"not found"\|:error'; then
    status="MISSING ✗${note}"
  elif echo "$resp" | grep -q "$check_sha"; then
    status="IN-STORE ✓${note}"; fresh=$((fresh+1))
  else
    status="UNKNOWN (probe error)${note}"
  fi
  printf "%-16s %-12s %-20s %s\n" "$label" "${check_sha:0:10}" "$head_date" "$status"
done
echo
echo "liveness: $fresh/$total repos have HEAD in substrate-2"
