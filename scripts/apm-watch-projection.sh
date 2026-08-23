#!/usr/bin/env bash
set -u

usage() {
  echo "usage: $0 [--once] TRANSITION_LOG COORDINATOR_STATE [MAX_AGE_SECONDS]" >&2
  exit 64
}

once=false
if [[ "${1:-}" == "--once" ]]; then
  once=true
  shift
fi

[[ $# -ge 2 && $# -le 3 ]] || usage
transition_log=$1
coordinator_state=$2
max_age_seconds=${3:-120}
[[ $max_age_seconds =~ ^[0-9]+$ ]] || usage

last_lines=0
check() {
  local lines
  if [[ ! -f "$transition_log" ]]; then
    printf '{:watch/status :alert :reason :transition-log-missing :path "%s"}\n' \
      "$transition_log"
    return 2
  fi
  if [[ ! -f "$coordinator_state" ]]; then
    printf '{:watch/status :alert :reason :coordinator-state-missing :path "%s"}\n' \
      "$coordinator_state"
    return 2
  fi

  lines=$(wc -l < "$transition_log")
  if (( lines > last_lines )); then
    sed -n "$((last_lines + 1)),${lines}p" "$transition_log"
    last_lines=$lines
  fi

  clojure -M -m futon3c.apm.projection-watchdog \
    "$transition_log" "$coordinator_state" "$max_age_seconds"
}

if $once; then
  check
  exit $?
fi

while true; do
  check || true
  sleep 10
done
