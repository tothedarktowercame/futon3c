#!/usr/bin/env bash
# One worker JVM over one shard (see scripts/mission_scope_backlog_worker.clj).
#   scripts/mission-scope-backlog-jvm.sh SHARD LOG
set -uo pipefail
SHARD="$1"; LOG="$2"
cd /home/joe/code/futon3c
export FUTON_SUBSTRATE_URL="${FUTON_SUBSTRATE_URL:-http://127.0.0.1:7073}"
clojure -J-Xmx2g "-J-Dscope.shard=$SHARD" -M -e "(load-file \"scripts/mission_scope_backlog_worker.clj\")" 2>&1 | grep --line-buffered -v "^WARNING" | tee "$LOG"
