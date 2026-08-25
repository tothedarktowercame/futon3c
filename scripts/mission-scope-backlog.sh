#!/usr/bin/env bash
# Backlog pass over scope trees: re-detect + per-binder ingest + true-up, one
# mission at a time, through the live futon3c JVM (mission-scope-reingest.sh).
#
# Why this exists: the debounced watcher (futon3c.watcher.scope-reingest) only
# reingests a doc when it LANDS. Missions whose trees predate the July page-cap
# freeze never re-landed, so the store held ~1/3 of the declared scope
# population (4,646 of 14,807 on Zone, 2026-08-25). This walks a list.
#
# Usage:
#   scripts/mission-scope-backlog.sh [MISSION-LIST] [LOG]
#     MISSION-LIST: one mission id per line (extra tab-separated columns are
#                   ignored; see /tmp/scope-skiplist.py). Default: every tree.
#     LOG: default /tmp/scope-backlog-<utc>.log
#   systemd-run --user --unit=scope-backlog --collect \
#     /home/joe/code/futon3c/scripts/mission-scope-backlog.sh /tmp/scope-backlog-missions.txt
#
# Each mission is independent: a failure is logged as FAIL and the walk goes on.
# Idempotent — a mission whose surface is already current is a no-op put.
set -uo pipefail

TREES=/home/joe/code/futon6/data/mission-scope-trees
LIST="${1:-}"
LOG="${2:-/tmp/scope-backlog-$(date -u +%Y%m%dT%H%M%SZ).log}"
SUBSTRATE="${FUTON_SUBSTRATE_URL:-http://127.0.0.1:7073}"
cd /home/joe/code/futon3c
ONE=$(mktemp /tmp/scope-backlog-one.XXXXXX)

census() {
  for t in eightfold-phase loose-section capability-scope map-item source-material pattern; do
    printf '%s=%s ' "$t" "$(curl -s --max-time 120 "$SUBSTRATE/api/alpha/census?type=mission-scope/$t" | grep -o ':count [0-9]*' | awk '{print $2}')"
  done
  echo
}

if [ -n "$LIST" ]; then
  mapfile -t missions < <(cut -f1 "$LIST" | grep -v '^\s*$')
else
  mapfile -t missions < <(ls "$TREES"/*.json | xargs -n1 basename | sed 's/\.json$//')
fi

{
  echo "[backlog] start $(date -u +%FT%TZ) missions=${#missions[@]} list=${LIST:-<all trees>} log=$LOG"
  echo "[backlog] census before: $(census)"
  ok=0; fail=0; skip=0; i=0
  for mission in "${missions[@]}"; do
    i=$((i+1))
    tree="$TREES/$mission.json"
    doc=$(python3 -c 'import json,sys; p=json.load(open(sys.argv[1])).get("path",""); print(p if p.startswith("/") or not p else "/home/joe/code/"+p)' "$tree" 2>/dev/null)
    if [ -z "$doc" ] || [ ! -f "$doc" ]; then
      skip=$((skip+1)); echo "[backlog] $i/${#missions[@]} SKIP $mission (doc missing: ${doc:-no tree})"; continue
    fi
    t0=$(date +%s)
    if bash scripts/mission-scope-reingest.sh "$doc" >"$ONE" 2>&1; then
      ok=$((ok+1)); echo "[backlog] $i/${#missions[@]} ok   $mission ($(( $(date +%s) - t0 ))s)"
    else
      fail=$((fail+1)); echo "[backlog] $i/${#missions[@]} FAIL $mission ($(( $(date +%s) - t0 ))s)"
      sed 's/^/    /' "$ONE" | tail -15
    fi
  done
  echo "[backlog] done $(date -u +%FT%TZ) ok=$ok fail=$fail skip=$skip"
  echo "[backlog] census after: $(census)"
} 2>&1 | tee "$LOG"
