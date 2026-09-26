#!/usr/bin/env bash
# library-lane-loop.sh — keep the library lane working while nobody is watching.
#
# One lane invocation attempts a bounded slice of the queue and exits. That is
# the right shape for a durable step machine and the wrong shape for an
# afternoon: on 2026-08-22 the lane ran once, exited cleanly at 17:01, and sat
# idle for seventeen hours. This is the outer loop that was missing.
#
# Stop it by creating the stop file; it finishes the cycle in flight and exits.
#   touch /home/joe/code/futon3c/data/apm-lane/STOP
#
# Launch durably (re-parented to the JVM, survives pouch eviction):
#   scripts/bg.py launch "APM_AREA=singular-homology .../library-lane-loop.sh" \
#     --agent claude-14 --label lane-loop
set -uo pipefail
export PATH="$HOME/.elan/bin:$PATH"
LANE_DIR=/home/joe/code/futon3c
STOP_FILE="$LANE_DIR/data/apm-lane/STOP"
CYCLE_SLEEP="${APM_CYCLE_SLEEP:-60}"
MAX_CYCLES="${APM_MAX_CYCLES:-0}"          # 0 = unbounded
cycle=0
while true; do
  if [ -e "$STOP_FILE" ]; then
    echo "=== STOP file present; exiting after $cycle cycles ==="
    exit 0
  fi
  cycle=$((cycle + 1))
  if [ "$MAX_CYCLES" -gt 0 ] && [ "$cycle" -gt "$MAX_CYCLES" ]; then
    echo "=== reached APM_MAX_CYCLES=$MAX_CYCLES; exiting ==="
    exit 0
  fi
  echo "=== cycle $cycle @ $(date -Is) ==="
  "$LANE_DIR/scripts/library-lane-run.sh"
  status=$?
  echo "=== cycle $cycle exited $status @ $(date -Is) ==="
  # A cycle that refuses instantly would otherwise spin. Always pause.
  sleep "$CYCLE_SLEEP"
done
