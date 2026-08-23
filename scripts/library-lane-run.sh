#!/usr/bin/env bash
# library-lane-run.sh — start/status/stop the JVM-owned durable library lane.
set -euo pipefail
LANE_ROOT=/home/joe/code/futon3c
CORPUS_ROOT=/home/joe/code/apm-lean
FRAMES_ROOT=/home/joe/code/apm-frames
ACTION="${APM_ACTION:-start}"
PROBLEM="${APM_PROBLEM:-}"
TARGET="${APM_TARGET:-}"
TRUNK="${APM_TRUNK:-repair/m97A06-energy-regularity}"

if [[ ! "$ACTION" =~ ^(start|resume|status|stop)$ ]]; then
  echo "REFUSED: APM_ACTION must be start, resume, status, or stop" >&2
  exit 2
fi
if [[ ! "$PROBLEM" =~ ^[A-Za-z0-9_-]+$ ]]; then
  echo "REFUSED: set a safe APM_PROBLEM" >&2
  exit 2
fi
if [[ -z "$TARGET" ]]; then
  TARGET="apm_${PROBLEM,,}"
  SOURCE="$CORPUS_ROOT/problems/$PROBLEM/lean/Main.lean"
  if [[ ! -f "$SOURCE" ]] || ! grep -Eq "^(noncomputable )?(theorem|lemma) $TARGET\\b" "$SOURCE"; then
    echo "REFUSED: derived keying target $TARGET is not declared" >&2
    exit 2
  fi
fi
if [[ -n "$TARGET" && ! "$TARGET" =~ ^[A-Za-z0-9_.]+$ ]]; then
  echo "REFUSED: unsafe APM_TARGET" >&2
  exit 2
fi
if [[ ! "$TRUNK" =~ ^[A-Za-z0-9._/-]+$ ]]; then
  echo "REFUSED: unsafe APM_TRUNK" >&2
  exit 2
fi

COORDINATOR_ID="library-lane:$PROBLEM"
REGISTRY="$LANE_ROOT/data/apm-coordinators/registry.edn"
STATE="$LANE_ROOT/data/apm-lane/coordinators/$PROBLEM.edn"
CONTRACT="$LANE_ROOT/holes/labs/M-apm-demonstration/frame-cycle-contract-codex-only-v1.edn"

case "$ACTION" in
  start)
    FORM="(do (require 'futon3c.apm.library-lane-coordinator :reload) (futon3c.apm.library-lane-coordinator/start! {:registry-path \"$REGISTRY\" :state-path \"$STATE\" :coordinator-id \"$COORDINATOR_ID\" :problem-id \"$PROBLEM\" :keying-target \"$TARGET\" :trunk-branch \"$TRUNK\" :corpus-root \"$CORPUS_ROOT\" :frames-root \"$FRAMES_ROOT\" :state-root \"$LANE_ROOT/data/apm-lane\" :contract-path \"$CONTRACT\" :agency-base \"http://localhost:7070\"}))"
    ;;
  resume)
    FORM="(do (require 'futon3c.apm.library-lane-coordinator :reload) (futon3c.apm.library-lane-coordinator/resume! \"$REGISTRY\" \"$COORDINATOR_ID\"))"
    ;;
  status)
    FORM="(do (require 'futon3c.apm.library-lane-coordinator) (futon3c.apm.library-lane-coordinator/status \"$REGISTRY\" \"$COORDINATOR_ID\"))"
    ;;
  stop)
    FORM="(do (require 'futon3c.apm.library-lane-coordinator) (futon3c.apm.library-lane-coordinator/stop! \"$REGISTRY\" \"$COORDINATOR_ID\"))"
    ;;
esac

exec "$LANE_ROOT/scripts/proof-eval.sh" "$FORM"
