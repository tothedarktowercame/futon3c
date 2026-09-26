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
  local lines form result
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

  # Evaluate in the one canonical serving JVM. Starting `clojure` here would
  # create a second application image on every poll and violate I-0.
  # The accepted paths are local filenames; quote them directly after refusing
  # embedded quotes/backslashes.
  if [[ "$transition_log" == *[\\\"]* || "$coordinator_state" == *[\\\"]* ]]; then
    printf '{:watch/status :alert :reason :watch-path-invalid}\n'
    return 2
  fi
  form="(do (require 'futon3c.apm.projection-watchdog) (futon3c.apm.projection-watchdog/evaluate (futon3c.apm.projection-watchdog/observe {:transition-log \"$transition_log\" :coordinator-state \"$coordinator_state\" :max-heartbeat-age-seconds $max_age_seconds :agency-base \"http://localhost:7070\"})))"
  result=$(printf '%s' "$form" | scripts/proof-eval.sh -)
  printf '%s\n' "$result"
  # A declared wait is an operationally successful observation. Requiring a
  # later :healthy sample makes callers occupy an agent through retry and
  # substrate deadlines even though durable state settles the liveness check.
  [[ "$result" == *":watch/status :healthy"* ||
     "$result" == *":watch/status :waiting"* ]]
}

if $once; then
  check
  exit $?
fi

while true; do
  check || true
  sleep 10
done
